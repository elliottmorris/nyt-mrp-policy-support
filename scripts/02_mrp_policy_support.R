#' G. Elliott Morris | @gelliottmorris | https://www.github.com/elliottmorris
#'
#' THIS FILE CONTAINS CODE FOR A CATEGORIAL MULTILEVEL REGRESSION MODEL WITH 
#' POST-STRATIFICATION ONTO AN AUGMENTED CENSUS-BASED POPULATION FRAME
#' 

rm(list=ls())
library(tidyverse)
library(survey)
library(brms) # install.packages('brms',version='2.9.0')
library(cmdstanr) # install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos"))); cmdstanr::install_cmdstan()
library(tidybayes)

# helper functions
source("scripts/helpers.R")

# stan stuff
mc.cores = parallel::detectCores()

# global model settings
REDO_MODELS = T # only run models if no cached version

# STATE LEVEL -------------------------------------------------------------
message("Wrangling data")

# read in crosswalk
state_region_cw <- read_csv("data/state/state_region_crosswalk.csv")

# read in post-strat targets
targets <- read_rds("data/mrp/acs_psframe_with_2020vote.rds")

# read covars
covars <- read_csv("data/state/state_covariates.csv")

# generation state-level vote equal to region average
covars = covars %>%
  left_join(state_region_cw %>% select(state_name, region)) %>%
  left_join(
    covars %>% 
      left_join(state_region_cw) %>%
      group_by(region) %>%
      summarise(region_biden_2020 = weighted.mean(state_biden_2020, total_votes_2020))
  ) %>%
  select(-c(region))


# and now scale covars -- mrp works best with scaled and centered state-level predictors
scale_unscale_vars <- names(covars)[3:ncol(covars)] # getting names of cols to scale

covars <- scale_variables(covars,scale_unscale_vars) #scale


# SURVEY DATA -------------------------------------------------------------
# source("generate_dataset.R")
ns = read_rds('nationscape.rds')

# what policies do we have?
names(ns)
# path_to_citizenship_dreamers
# legal_marijuana
# cap_carbon 
# guns_bg
# guns_assault
# raise_taxes_600k 
# state_college
# abortion_never_legal
# abortion_most_time 
# paid_maternity_12wk
# gov_health_subsidies 
# minimum_wage_15d

# cross tab for each

# get crosstabs for each state_name
state_tabs = lapply(9:ncol(ns),
                    function(i){
                      var = names(ns)[i]
                      
                      tmp = ns %>%
                        mutate(var = ns[[var]]) %>%
                        filter(!is.na(var),var != "Not Sure") %>%
                        group_by(state_name, var) %>%
                        summarise(n = sum(weight)) %>%
                        group_by(state_name) %>%
                        mutate(pct = n / sum(n)) %>%
                        filter(var == 'Agree') %>%
                        select(state_name,var,pct) 
                      
                      # negate if opinion is negative
                      tmp = tmp %>%
                        mutate(pct = ifelse(names(ns)[i] %in% c('abortion_never_legal'),
                                            1 - pct, 
                                            pct)) 
                      
                      # spread, rename and return
                      tmp = tmp %>%
                        spread(var,pct)  %>%
                        set_names(., c('state_name', var))
                      
                      return(tmp)
                      
                    }) 

state_tabs = Reduce(function(x, y) merge(x, y, by = "state_name", all = TRUE), state_tabs)


# survey checks
nrow(ns)

ns <- ns %>%
  left_join(state_region_cw)

ns %>%
  #filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n = sum(weight,na.rm=T)) %>%
  mutate(pct = n/sum(n))

ns %>%
  filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n = sum(weight,na.rm=T)) %>%
  mutate(pct = n/sum(n))

# final survey cleanup 
# check size of poll
nrow(ns)

# check to make sure each matching var IS THE SAME in both datasets
unique(ns$age) %in% unique(targets$age)
unique(ns$sex) %in% unique(targets$sex)
unique(ns$race) %in% unique(targets$race)
unique(ns$edu) %in% unique(targets$edu)
unique(ns$past_vote) %in% unique(targets$past_vote)

#some checks
ns.svy <- svydesign(~1,data=ns,weights=~weight)

svymean(~past_vote,ns.svy)
svymean(~past_vote,subset(ns.svy,past_vote != 'Non_voter'))

ns %>%
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

ns %>%
  group_by(state_name,past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

ns %>%
  group_by(state_name,past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))


svymean(~past_vote, 
        subset(ns.svy,state_name=='Minnesota'),
        na.rm=T) 

# sample size by state
sample_size_bystate <- ns %>% 
  group_by(state_name) %>%
  summarise(state_sample_size=n())

ggplot(sample_size_bystate, aes(y=reorder(state_name,state_sample_size),x=state_sample_size)) +
  geom_point()


# ADD COVARS --------------------------------------------------------------
# add the state's region to the individual data from the targets data
targets <- targets %>%
  left_join(state_region_cw %>% 
              dplyr::select(state_fips,state_name, state_abb, region))

ns <- ns %>%
  left_join(state_region_cw %>% 
              dplyr::select(state_name, state_abb, region))

# adding a few other state-level covariates, such as union household and evangelical %
targets <- targets %>%
  left_join(covars)

ns <- ns %>%
  left_join(covars)

# MODEL LIKELY VOTERS -----------------------------------------------------
generate_state_policy_estimates = function(policy = 'path_to_citizenship_dreamers'){
  message(sprintf("\tMultilevel model for %s",policy))
  
  # voter dummy variable
  ns$policy_dummy = round(ns[[policy]] =='Agree') 
  
  # model formula
  # model formula
  model_formula = as.formula(sprintf('policy_dummy ~ %s',
                                    default_model_formula_no_vote))
  
  model_formula
  
  # run model!
  if(REDO_MODELS | isFALSE(any(grepl(sprintf("%s_model.rds",policy) , list.files("models/"))))){
    this_policy_model <- brm(formula = model_formula,
                              data = ns %>% sample_n(10000) %>% ungroup(),
                              family = bernoulli(link='logit'),
                              # priors
                              prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                                        set_prior("normal(0, 1)", class = "b"),
                                        set_prior("normal(0, 1)", class = "sd")),
                              # settings
                              iter = 1000,
                              warmup = 500,
                              chains = 4,
                              cores = 4,
                              control = list(adapt_delta = 0.9, max_treedepth = 12),
                              refresh = 100,
                              thin = 1,
                              backend = 'cmdstanr',
                              threads = threading(2))
    
    write_rds(this_policy_model,sprintf("models/%s_model.rds",policy) ,compress = 'gz')
  }else{
    this_policy_model <- read_rds(sprintf("models/%s_model.rds",policy) )
  }
  
  message(sprintf("\tPost-stratification for %s",policy))
  # get 1000 draws from posterior predictive -- major party vote
  cell_pred <- rstantools::posterior_epred(
    object = this_policy_model,
    newdata = targets,
    ndraws = 250, 
    allow_new_levels = TRUE,
    transform = TRUE
  )
  
  # get medain for turnout
  cell_pred_median <- cell_pred %>% apply(.,2,median)
  
  # add share that are predicted voters to pop
  targets <- targets %>%
    mutate(est = cell_pred_median) 
  
  # check
  targets %>%
    summarise(policy_est = sum(est * n) / sum(n))
  
  
  # return state summarises
  targets %>%
    group_by(state_name) %>%
    summarise(est = sum(est * n) / sum(n)) %>%
    mutate(policy = policy) %>%
    return
}

# run for each of our policies
path_to_citizenship_dreamers.state = generate_state_policy_estimates('path_to_citizenship_dreamers')

legal_marijuana.state = generate_state_policy_estimates('legal_marijuana')

cap_carbon.state = generate_state_policy_estimates('cap_carbon')

guns_bg.state = generate_state_policy_estimates('guns_bg')

guns_assault.state = generate_state_policy_estimates('guns_assault')

raise_taxes_600k.state = generate_state_policy_estimates('raise_taxes_600k')

state_college.state = generate_state_policy_estimates('state_college')

abortion_never_legal.state = generate_state_policy_estimates('abortion_never_legal')

abortion_most_time.state = generate_state_policy_estimates('abortion_most_time')

paid_maternity_12wk.state = generate_state_policy_estimates('paid_maternity_12wk')

gov_health_subsidies.state = generate_state_policy_estimates('gov_health_subsidies')

minimum_wage_15d.state = generate_state_policy_estimates('minimum_wage_15d')

beepr::beep(2)

# inverse abortion never legal (to code in liberal direction)
abortion_never_legal.state = abortion_never_legal.state %>% mutate(est = 1 - est)

# join together
state_ests = list(
  path_to_citizenship_dreamers.state,
  legal_marijuana.state,
  cap_carbon.state,
  guns_bg.state,
  guns_assault.state,
  raise_taxes_600k.state,
  state_college.state,
  abortion_never_legal.state,
  abortion_most_time.state,
  paid_maternity_12wk.state,
  gov_health_subsidies.state,
  minimum_wage_15d.state
  ) %>% 
  bind_rows %>%
  spread(policy, est)



# add state covariates ----------------------------------------------------
# add population 
state_ests = state_ests %>%
  left_join(targets %>% group_by(state_name) %>% summarise(n = sum(n))) 

# add biden and trump vote
state_ests = read_csv('data/state/results_2020.csv') %>%
  select(state_name, state_abb, biden_pct, trump_pct) %>%
  left_join(state_ests)

# relocate columns
state_ests = state_ests %>%
  relocate(c(state_name, state_abb, biden_pct, trump_pct, n))


# sum up policies ---------------------------------------------------------

support_senate_votes = state_ests %>%
  gather(policy,pct,6:ncol(.)) %>%
  group_by(policy) %>%
  summarise(support_natl = weighted.mean(pct, n)) %>%
  # now support in median senate seat
  left_join(
    state_ests %>%
      gather(policy,pct,6:ncol(.)) %>%
      filter(state_abb != 'DC') %>%
      group_by(policy) %>%
      summarise(median_senate = median(pct))
  ) %>%
  # and support in 40th senate seat by size
  left_join(
    state_ests  %>%
      arrange(n) %>%
      filter(state_abb != 'DC',
             row_number() <= 20) %>%
      gather(policy,pct,6:ncol(.)) %>%
      group_by(policy) %>%
      summarise(senate_40th_vote_pop = weighted.mean(pct, n))
  ) %>%
  # and support in 40th senate seat by Biden 2020 margin
  left_join(
    state_ests  %>%
      arrange(biden_pct) %>%
      filter(state_abb != 'DC',
             row_number() <= 20) %>%
      gather(policy,pct,6:ncol(.)) %>%
      group_by(policy) %>%
      summarise(senate_40th_vote_biden = weighted.mean(pct, n))
  ) 

# adjust labels
support_senate_votes = support_senate_votes %>%
  mutate(policy = case_when(policy == 'abortion_never_legal' ~ "DON'T make abortion always illegal",
                            policy == 'border_wall' ~ "DON'T build border wall",
                            T ~ policy))

ggplot(support_senate_votes, 
       aes(y = fct_reorder(policy,support_natl))) +
  geom_point(aes(x = support_natl, col = 'if all adults count')) +
  geom_point(aes(x = median_senate, col = 'in median senate seat')) +
  geom_point(aes(x = senate_40th_vote_biden, col = 'in 40th senate seat (ranked by biden vote)')) +
  geom_vline(xintercept = 0.5)  +
  labs(x = 'Liberal opinion',
       y = 'Policy',
       col = 'Aggregation method') +
  theme_minimal() +
  theme(legend.position = 'top') +
  guides('col' = guide_legend(ncol = 1))

ggsave('output/fig_policy_support_by_different_institutions.pdf',width = 7, height = 4)
write_csv(support_senate_votes, 'output/policy_support_by_different_institutions.csv')

state_ests %>%
  select(state_abb,biden_pct,6:ncol(.)) %>%
  filter(state_abb != 'DC') %>%
  gather(policy,lib_opinion,3:ncol(.)) %>%
  ggplot(.,aes(x=biden_pct,y=lib_opinion)) +
  geom_text(aes(label=state_abb)) +
  facet_wrap(~policy) +
  geom_abline() +
  geom_smooth(method='lm') +
  theme_minimal() +
  labs(x = "Biden vote share 2020",
       y = 'Liberal opinion')

ggsave('output/fig_policy_support_by_state.pdf',width = 8, height = 6)
write_csv(state_ests, 'output/policy_support_by_state.csv')
