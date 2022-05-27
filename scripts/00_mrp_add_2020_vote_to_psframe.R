#' G. Elliott Morris | @gelliottmorris | https://www.github.com/elliottmorris
#'
#' THIS FILE CONTAINS CODE FOR A CATEGORIAL MULTILEVEL REGRESSION MODEL WITH 
#' POST-STRATIFICATION ONTO AN AUGMENTED CENSUS-BASED POPULATION FRAME
#' 

rm(list=ls())
library(tidyverse)
library(brms) # install.packages('brms',version='2.9.0')
library(cmdstanr)
library(survey)

# stan stuff
mc.cores = parallel::detectCores()


# global model settings
source("scripts/helpers.R")


# functions ---------------------------------------------------------------
# declare a function for scaling a data frame given vars
scale_variables <- function(df, vars_to_scale){
  # scale given cols
  df %>%
    mutate_at(vars_to_scale,
              scale) %>%
    return()
  
}

# also declare a function for UNscaling variables
unscale_variables <- function(df, vars_to_unscale){
  
  # scale cols 3 to 17
  df %>%
    mutate_at(vars_to_unscale, 
              function(x){
                x * attr(x, 'scaled:scale') + attr(x, 'scaled:center')
                
              }) %>%
    as_tibble() %>%
    return()
  
}


# STATE LEVEL -------------------------------------------------------------
message("Wrangling data")

# read in post-strat targets
targets <- read_csv("data/mrp/acs_psframe.csv")

# read and scale covars -- mrp works best with scaled and centered state-level predictors
covars <- read_csv("data/state/state_covariates.csv")

scale_unscale_vars <- names(covars)[3:18] # getting names of cols to scale

covars <- scale_variables(covars,scale_unscale_vars) #scale

# read in crosswalk
state_region_cw <- read_csv("data/state/state_region_crosswalk.csv")


# SURVEY DATA -------------------------------------------------------------
cces20.raw <- read_rds('data/ces_2020/CES20_Common_OUTPUT_vv.rds')

# mutate the vars we need
cces20 <- cces20.raw %>%
  filter(!is.na(vvweight_post)) %>%
  # mutate vars
  mutate(
    ### mutate demographic variables to match the census
    # race
    race = case_when(race==1 ~ "White, Non-Hispanic",
                     race==2 ~ "Black, Non-Hispanic",
                     race==3 ~ "Hispanic",
                     race %in% 4:8 ~ "Other, Non-Hispanic"),
    # sex
    sex = case_when(gender==1~ "Male",
                    gender==2~ "Female",
                    TRUE ~ NA_character_),
    # age
    AGE = 2020 - as.numeric(as.character(birthyr)),
    age = case_when(AGE >= 18 & AGE <= 29 ~ "18-29",
                    AGE >= 30 & AGE <= 44 ~ "30-44",
                    AGE >= 45 & AGE <=  64 ~ "45-64",
                    AGE >= 65 ~ "65+",
                    TRUE ~ NA_character_),
    
    # education
    edu = case_when(educ == 1 ~ "No HS",
                    educ == 2 ~ "HS grad",
                    educ %in% 3:4 ~ "Some college",
                    educ == 5 ~ 'College',
                    educ == 6~ "Post-grad",
                    TRUE ~ NA_character_),
    
    # marstat
    marstat = case_when(marstat %in% c(1,6)  ~ 'Married',
                        marstat %in% 2:4 ~ 'Not married',
                        marstat == 5 ~ 'Never married',
                        TRUE ~ NA_character_),
    
    # income5
    income5 = case_when(faminc_new %in% 1:3 ~ 'Under $30K',
                        faminc_new %in% 4:6 ~ "$30-60K",
                        faminc_new %in% 7:9 ~ '$60-100K',
                        faminc_new %in% 10:11 ~ "$100-150K",
                        faminc_new %in% 12:16 ~ '$150K or more',
                        TRUE ~ NA_character_
                        
    ),
    
    ### voting variables
    # vote in 2016
    past_vote = case_when(is.na(CL_2020gvm ) ~ 'Non_voter',
                               CC20_364a == 1 | CC20_364b == 1 ~ 'Trump',
                               CC20_364a == 2 | CC20_364b == 2~ 'Biden', 
                               CC20_364a == 3 | CC20_364b == 3~ 'Other',
                               T ~ 'Non_voter'),
    
    # validation variable
    #voter_validated = ifelse(CL_E2016GVM != '','Y','N'),
    
    ### MISC
    # weight
    weight = as.numeric(vvweight_post),
    
    state_name = state_region_cw$state_name[match(inputstate_post,state_region_cw$state_fips)]
    
    
    ### CONVERSIONS
    
  ) %>%
  dplyr::select(state_name,
                race, sex, age, edu, marstat,
                income5,
                past_vote,
                weight,
  ) 

nrow(cces20) / nrow(cces20.raw)

cces20 <- cces20 %>%
  left_join(state_region_cw)

cces20 %>%
  #filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n = sum(weight)) %>%
  mutate(pct = n/sum(n))

cces20 %>%
  filter(past_vote != 'Non_voter') %>%
  group_by(race, past_vote) %>%
  summarise(n = sum(weight)) %>%
  mutate(pct = n/sum(n))

cces20 = na.omit(cces20)

# final survey cleanup ----------------------------------------------------
# check size of poll
nrow(cces20)

# check to make sure each matching var IS THE SAME in both datasets
unique(cces20$age) %in% unique(targets$age)
unique(cces20$sex) %in% unique(targets$sex)
unique(cces20$race) %in% unique(targets$race)
unique(cces20$edu) %in% unique(targets$edu)
unique(cces20$past_vote) %in% unique(targets$past_vote)

#some checks
cces20.svy <- svydesign(~1,data=cces20,weights=~weight)

svymean(~past_vote,cces20.svy)
svymean(~past_vote,subset(cces20.svy,past_vote != 'Non_voter'))

svymean(~past_vote,subset(cces20.svy,past_vote != 'Non_voter'))

cces20 %>%
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

cces20 %>%
  group_by(state_name,past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

cces20 %>%
  group_by(state_name,past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

cces20 %>%
  group_by(past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

svymean(~past_vote, 
        subset(cces20.svy,state_name=='Minnesota'),
        na.rm=T) 

# sample size by state
sample_size_bystate <- cces20 %>% 
  group_by(state_name) %>%
  summarise(state_sample_size=n())

ggplot(sample_size_bystate, aes(y=reorder(state_name,state_sample_size),x=state_sample_size)) +
  geom_point()


# ADD COVARS --------------------------------------------------------------
# add the state's region to the individual data from the targets data
targets <- targets %>%
  left_join(state_region_cw %>% 
              dplyr::select(state_fips,state_name, state_abb, region))

cces20 <- cces20 %>%
  left_join(state_region_cw %>% 
              dplyr::select(state_name, state_abb, region))


# adding a few other state-level covariates, such as union household and evangelical %
targets <- targets %>%
  left_join(covars)

cces20 <- cces20 %>%
  left_join(covars)

# CHECK DATA AGAINST TARGETS ----------------------------------------------
# get cces20
cces20.svy <- svydesign(~1, data=cces20,weights=~weight)

cces20.targets.bkdown <- svytable(~edu+state_abb, cces20.svy) %>% 
  as.data.frame() %>%
  group_by(state_abb) %>%
  mutate(cces20_prop = Freq/sum(Freq)) %>%
  dplyr::select(state_abb,edu,cces20_prop) %>%
  arrange(state_abb,desc(cces20_prop)) %>%
  ungroup()

# get acs
acs.targets.bkdown <- targets %>%
  group_by(state_abb, edu) %>%
  summarise(n=sum(n)) %>%
  group_by(state_abb) %>%
  mutate(acs_prop = n/sum(n))  %>%
  dplyr::select(state_abb,edu,acs_prop) %>%
  arrange(state_abb,desc(acs_prop)) %>%
  ungroup()

acs.targets.bkdown %>% left_join(cces20.targets.bkdown) %>%
  mutate(state_abb = factor(state_abb)) %>%
  ggplot(aes(x=acs_prop,y=cces20_prop,col=edu)) +
  geom_abline() +
  geom_text(aes(label=state_abb)) +
  geom_smooth(aes(col=edu),se=F,method='lm') +
  #facet_wrap(~state_abb) +
  theme_minimal() +
  theme(legend.position = 'top')

# past vote
past_vote_compare <- targets %>%
  group_by(state_abb,past_vote) %>%
  summarise(n = sum(n)) %>%
  group_by(state_abb) %>%
  mutate(target_prop=n/sum(n)) %>%
  left_join(svytable(~past_vote+state_abb, cces20.svy) %>% 
              as.data.frame() %>%
              group_by(state_abb) %>%
              mutate(cces20_prop = Freq/sum(Freq)) %>%
              dplyr::select(state_abb,past_vote,cces20_prop) %>%
              arrange(state_abb,desc(cces20_prop)) %>%
              ungroup(),
            by=c('state_abb','past_vote')
  ) %>%
  mutate(diff = cces20_prop - target_prop) 

# plot comparison
past_vote_compare %>% 
  #dplyr::filter(diff > 0.1) %>%
  #dplyr::filter(party=='Republican') %>%
  ggplot(.,aes(x=target_prop,y=cces20_prop,col=past_vote)) +
  geom_abline() +
  geom_smooth(se=F,method='lm') +
  geom_text(aes(label=state_abb)) +
  theme_minimal() + 
  labs(x='Targets share of population',
       y='Diff b/t cces20 and target share of population')


# plot differences
past_vote_compare %>% 
  #dplyr::filter(diff > 0.1) %>%
  #dplyr::filter(party=='Republican') %>%
  ggplot(.,aes(x=target_prop,y=diff,col=past_vote)) +
  geom_hline(yintercept=0) +
  geom_smooth(se=F,method='lm') +
  geom_text(aes(label=state_abb)) +
  theme_minimal() + 
  labs(x='Targets share of population',
       y='Diff b/t cces20 and target share of population')


# RAKE WEIGHTS TO MATCH 2020 ----------------------------------------------
# weight polls to be representative at state-level
# get pop numbers
state_targets_pastvote <- unscale_variables(covars,scale_unscale_vars) %>% 
  as.data.frame() %>%
  dplyr::select(state_name,Biden=state_biden_2020,Trump=state_trump_2020,state_vap_turnout_2020) %>%
  left_join(targets %>% 
              group_by(state_name) %>% summarise(n=sum(n)))

# create "Other" and Non_voter number, too
state_targets_pastvote <- state_targets_pastvote %>%
  mutate(Other = 1 - (Biden + Trump),
         Non_voter = 1 - state_vap_turnout_2020) %>%
  mutate(Biden = Biden * n,
         Trump = Trump * n,
         Other = Other *n,
         Non_voter = Non_voter * n) %>%
  dplyr::select(state_name, Biden, Trump, Other, Non_voter) %>%
  gather(past_vote,Freq,2:5) %>%
  mutate(Freq = Freq/sum(Freq)) %>%
  ungroup() 

# can't have a weight of 0, so make v small
state_targets_pastvote <- state_targets_pastvote %>%
  mutate(Freq = ifelse(Freq==0,0.00000001,Freq))

state_targets_pastvote <- state_targets_pastvote %>%
  mutate(Freq = Freq*nrow(cces20))  

# make sure weighting vars are same levels
state_targets_pastvote <- state_targets_pastvote %>%
  mutate(past_vote = factor(past_vote,levels=c('Biden','Trump','Other','Non_voter')))

cces20 <- cces20 %>%
  mutate(past_vote = ifelse(past_vote == 'Non_voter','Non_voter',past_vote),
         past_vote = factor(past_vote,levels=c('Biden','Trump','Other','Non_voter')))

# filter out states without all 4 vote options
filter_out_these_states <- cces20 %>% group_by(state_name,past_vote) %>% summarise(n=n()) %>% group_by(state_name) %>% summarise(n=n()) %>% dplyr::filter(n<4) %>% pull(state_name)

# create survey object stratified by state
cces20.svy <- svydesign(ids=~1,
                        strata=~state_name,
                        data=cces20 %>% dplyr::filter(!state_name %in% filter_out_these_states),
                        weights=~weight)

# rake weights based on past vote
cces20.svy.raked <- postStratify(design = cces20.svy,
                                 strata = ~past_vote+state_name,
                                 population = state_targets_pastvote %>% dplyr::filter(!state_name %in% filter_out_these_states) %>% group_by(past_vote,state_name) %>% summarise(Freq = sum(Freq)), 
                                 partial=T)

# TRIM WEIGHTS? 
cces20.svy.raked <- trimWeights(design=cces20.svy.raked,lower=0.0001000195,upper=15.00007,strict=T)

# extract weights
new_weights <- attr(cces20.svy.raked$postStrata[[1]],'weights')
new_weights <- weights(cces20.svy.raked)

summary(new_weights)

# make new DF with old weights and new data
weighted_poll <- cces20.svy.raked$variables %>%
  mutate(weight=new_weights)

# check data
weighted_poll %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(state_name,past_vote) %>%
  summarise(prop=sum(weight)) %>%
  mutate(prop = prop/sum(prop)) %>%
  spread(past_vote,prop) %>%
  mutate(biden_margin.yg = Biden - Trump) %>%
  dplyr::select(state_name,
                biden_margin.yg) %>%
  left_join(unscale_variables(covars,scale_unscale_vars) %>% 
              dplyr::select(state_name,state_biden_2020,state_trump_2020)) %>%
  ggplot(.,aes(x=biden_margin.yg, y=state_biden_2020-state_trump_2020, label=state_name)) +
  geom_abline() +
  geom_text() +
  stat_smooth(method='lm') +
  coord_cartesian(xlim=c(-0.5,0.5),ylim=c(-0.5,0.5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# save newly weighted poll over the cces20 object
# and add back the rows from the unpopulated cells
cces20 <- weighted_poll %>%
  bind_rows(cces20 %>% dplyr::filter(state_name %in% filter_out_these_states)) %>%
  mutate(past_vote = ifelse(past_vote == 'Non_voter','Non_voter',
                                 as.character(past_vote))) %>%
  as.data.frame()


# check again
cces20 %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(state_name,past_vote) %>%
  summarise(prop=sum(weight)) %>%
  mutate(prop = prop/sum(prop)) %>%
  spread(past_vote,prop) %>%
  mutate(biden_margin.yg = Biden - Trump) %>%
  dplyr::select(state_name,
                biden_margin.yg) %>%
  left_join(unscale_variables(covars,scale_unscale_vars) %>% 
              dplyr::select(state_name,state_biden_2020,state_trump_2020)) %>%
  ggplot(.,aes(x=biden_margin.yg, y=state_biden_2020-state_trump_2020, label=state_name)) +
  geom_abline() +
  geom_text() +
  stat_smooth(method='lm') +
  coord_cartesian(xlim=c(-0.5,0.5),ylim=c(-0.5,0.5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)


# check some tabs and such
cces20.svy <- svydesign(~1,data=cces20,weights=~weight)
svymean(~past_vote,cces20.svy)
svymean(~past_vote,subset(cces20.svy,past_vote != "Non_voter"))
svymean(~past_vote,subset(cces20.svy,past_vote != "Non_voter" & past_vote != "Non_voter"))
svymean(~past_vote,subset(cces20.svy,past_vote != "Non_voter" & past_vote == "Non_voter"))
svymean(~past_vote,subset(cces20.svy,past_vote != "Non_voter" & state_name=='Louisiana'),na.rm=T)


# MODEL LIKELY VOTERS -----------------------------------------------------
# voter dummy variable
cces20 <- cces20 %>%
  mutate(likely_voter_dummy = round(past_vote != 'Non_voter') ) %>%
  ungroup()

# run model!
message("Running 2020 turnout MRP")

if(REDO_MODELS | isFALSE(any(grepl("turnout_2020_model.rds" , list.files("models/"))))){
  turnout_2020_model <- brm(formula = likely_voter_dummy | weights(weight) ~
                              # state-level smoothers
                              state_biden_2020 + state_vap_turnout_2016 + state_white_protestant + state_median_income +
                              income.c*state_median_income + income.c*state_biden_2020 +
                              
                              # main demographics
                              (1 | sex) + (1 | age) + (1 | edu) + age.c +
                              (1 + state_biden_2020  + income.c | race) +
                              # geography and income smoothing
                              (1 + income.c | state_name) + (1 | state_name:race) + (1 | state_name:income5) + (1 | state_name:race:edu) +
                              (1 + income.c | region) + (1 | region:race) + (1 | region:income5) + (1 | region:race:edu) +
                              # interactions
                              (1 | race:edu) + (1 | race:income5) + (1 | race:sex) +
                              (1 | age:edu) + (1 | income5:edu) + (1 | sex:age) +
                              # past vote!
                              (1 | past_vote:edu) + (1 | past_vote:age) + (1 | past_vote:race) +
                              (1 | past_vote:income5) + (1 | past_vote:race:edu),
                            data = cces20, # %>% group_by(time_stamp) %>% sample_n(500) %>% ungroup(),
                            family = bernoulli(link='logit'),
                            # priors
                            prior = c(set_prior("normal(0, 1)", class = "Intercept"),
                                      set_prior("normal(0, 1)", class = "b"),
                                      set_prior("normal(0, 1)", class = "sd")),
                            # seed=1843,
                            # iter = MAX_NUM_ITER, # use high iter in VB compared to MCMC
                            # algorithm=ALGO,
                            # tol_rel_obj = TOLERANCE) # decrease tol to improve convergence
                            iter = MCMC_ITER,
                            warmup = MCMC_WARMUP,
                            chains = MCMC_chains,
                            cores = MCMC_cores,
                            control = list(adapt_delta = MCMC_adapt_delta,
                                           max_treedepth = MCMC_MAX_TREE),
                            refresh = MCMC_refresh,
                            thin = MCMC_THIN,
                            backend = MCMC_BACKEND,
                            threads = threading(MCMC_THREADS))
  
  write_rds(turnout_2020_model,"models/turnout_2020_model.rds",compress = 'gz')
}else{
  turnout_2020_model <- read_rds("models/turnout_2020_model.rds")
}

# post-stratification stage!
message("\tPost-stratifying 2020 turnout onto targets")

# get 1000 draws from posterior predictive
cell_pred_2020_turnout.mat <- rstantools::posterior_linpred(turnout_2020_model,
                                                            newdata=targets %>% 
                                                              mutate(income5 = factor(income5, ordered = TRUE, levels=c('Under $30K',"$30-60K",'$60-100K',
                                                                                                                        "$100-150K", '$150K or more')),
                                                                     income.c = as.numeric(income5),
                                                                     age = factor(age, ordered = TRUE, levels=c('18-29',"30-44",'45-64',"65+")),
                                                                     
                                                                     age.c = as.numeric(age)) ,
                                                            nsamples=NUM_DRAWS,draws=NUM_DRAWS, 
                                                            allow_new_levels=TRUE,
                                                            transform=TRUE)

# get medain for turnout
cell_pred_2020_turnout <- cell_pred_2020_turnout.mat %>% apply(.,2,median)

mean(cell_pred_2020_turnout)

# add share that are predicted voters to pop
targets <- targets %>%
  mutate(likely_voter = cell_pred_2020_turnout)

# check
targets %>%
  summarise(likely_voter = sum(likely_voter * n) / sum(n))


targets %>%
  group_by(race) %>%
  summarise(likely_voters = sum(likely_voter*n) / sum(n))

targets %>%
  group_by(age) %>%
  summarise(likely_voters = sum(likely_voter*n) / sum(n))

targets %>%
  group_by(past_vote) %>%
  summarise(likely_voters = sum(likely_voter*n) / sum(n))

# MODEL 2020 VOTE ---------------------------------------------------------
# multilevel regression stage!
# this happens in two steps:
message("Running 2020 biden/trump/other vote MRP")

if(REDO_MODELS | isFALSE(any(grepl("pres_2020_model.rds" , list.files("models/"))))){
  pres_2020_model <- brm(formula = past_vote | weights(weight) ~
                           # state-level smoothers
                           state_biden_2020 + state_vap_turnout_2016 + state_white_protestant + state_median_income +
                           income.c*state_median_income + income.c*state_biden_2020 +
                           
                           # main demographics
                           (1 | sex) + (1 | age) + (1 | edu) + age.c +
                           (1 + state_biden_2020  + income.c | race) +
                           # geography and income smoothing
                           (1 + income.c | state_name) + (1 | state_name:race) + (1 | state_name:income5) + (1 | state_name:race:edu) +
                           (1 + income.c | region) + (1 | region:race) + (1 | region:income5) + (1 | region:race:edu) +
                           # interactions
                           (1 | race:edu) + (1 | race:income5) + (1 | race:sex) +
                           (1 | age:edu) + (1 | income5:edu) + (1 | sex:age) +
                           # past vote!
                           (1 | past_vote:edu) + (1 | past_vote:age) + (1 | past_vote:race) +
                           (1 | past_vote:income5) + (1 | past_vote:race:edu),
                         data = cces20[cces20$past_vote != 'Non_voter',], # %>% group_by(time_stamp) %>% sample_n(500) %>% ungroup(),
                         family = categorical(link='logit',
                                              refcat = 'Other'),
                         # priors
                         prior = c(set_prior("normal(0, 1)", class = "Intercept",dpar='muBiden'),
                                   set_prior("normal(0, 1)", class = "Intercept",dpar='muTrump'),
                                   set_prior("normal(0, 1)", class = "b",dpar='muBiden'),
                                   set_prior("normal(0, 1)", class = "b",dpar='muTrump'),
                                   set_prior("normal(0, 1)", class = "sd",dpar = 'muBiden'),
                                   set_prior("normal(0, 1)", class = "sd",dpar = 'muTrump')),
                         # seed=1843,
                         # iter = MAX_NUM_ITER, # use high iter in VB compared to MCMC
                         # algorithm=ALGO,
                         # tol_rel_obj = TOLERANCE) # decrease tol to improve convergence
                         iter = MCMC_ITER,
                         warmup = MCMC_WARMUP,
                         chains = MCMC_chains,
                         cores = MCMC_cores,
                         control = list(adapt_delta = MCMC_adapt_delta,
                                        max_treedepth = MCMC_MAX_TREE),
                         refresh = MCMC_refresh,
                         thin = MCMC_THIN,
                         backend = MCMC_BACKEND,
                         threads = threading(MCMC_THREADS))
  
  write_rds(pres_2020_model,"models/pres_2020_model.rds",compress = 'gz')
}else{
  pres_2020_model <- read_rds("models/pres_2020_model.rds")
}

# post-stratification stage!
message("\tPost-stratifying 2020 vote onto targets")

# get 1000 draws from posterior predictive -- major party vote
cell_pred_2020_vote.mat <- rstantools::posterior_linpred(pres_2020_model,
                                                         newdata=targets %>% 
                                                           mutate(income5 = factor(income5, ordered = TRUE, levels=c('Under $30K',"$30-60K",'$60-100K',
                                                                                                                     "$100-150K", '$150K or more')),
                                                                  income.c = as.numeric(income5),
                                                                  age = factor(age, ordered = TRUE, levels=c('18-29',"30-44",'45-64',"65+")),
                                                                  
                                                                  age.c = as.numeric(age)) ,
                                                         nsamples=NUM_DRAWS,draws=NUM_DRAWS, 
                                                         allow_new_levels=TRUE,
                                                         transform=TRUE)

# get medain for major party vote
cell_pred_2020_dem <- cell_pred_2020_vote.mat[,,1] %>% apply(.,2,median)
cell_pred_2020_rep <- cell_pred_2020_vote.mat[,,3] %>% apply(.,2,median)
cell_pred_2020_other <- cell_pred_2020_vote.mat[,,2] %>% apply(.,2,median)

mean(cell_pred_2020_dem)
mean(cell_pred_2020_rep)
mean(cell_pred_2020_other)

# make vars on targets frame
targets <- targets %>%
  mutate(pres_2020_dem = cell_pred_2020_dem,
         pres_2020_rep = cell_pred_2020_rep,
         pres_2020_other = cell_pred_2020_other)


# check
targets %>%
  dplyr::filter(past_vote != 'Non_voter') %>%
  summarise(dem = sum(pres_2020_dem * n) / sum(n),
            rep = sum(pres_2020_rep * n) / sum(n),
            other = sum(pres_2020_other * n) / sum(n),
  ) 

targets %>%
  summarise(dem = sum(pres_2020_dem * n * likely_voter) / sum(n * likely_voter),
            rep = sum(pres_2020_rep * n * likely_voter) / sum(n * likely_voter),
            other = sum(pres_2020_other * n * likely_voter) / sum(n * likely_voter),
  ) 

svymean(~past_vote,subset(cces20.svy,past_vote!='Non_voter'),na.rm=T)


targets %>%
  group_by(state_name) %>%
  summarise(dem = sum(pres_2020_dem * n * likely_voter) / sum(n * likely_voter),
            rep = sum(pres_2020_rep * n * likely_voter) / sum(n * likely_voter),
            other = sum(pres_2020_other * n * likely_voter) / sum(n * likely_voter),
  ) 


# CORRECT 2020 TURNOUT AND VOTE PROBS -------------------------------------
# some renaming
targets <- targets %>% rename(voter_pred = likely_voter)

# modify targets to match results
# first, turnout predictions
votes <- read_csv('data/state/results_2020.csv')

targets$voter_pred <- 
  correct_probs(pstrat = targets,
                weighting = 'n',
                cell_prob = 'voter_pred',
                outcome = votes,
                state_factor = 'turnout_pct'
  )

sum(targets$voter_pred * targets$n) / sum(targets$n)


# clone the df
expanded_targets <- targets

# create new cells for each vote type
expanded_targets <- expanded_targets %>%
  mutate(nonvoter_pred = 1 - voter_pred) %>%
  gather(vote_type,prop,c('nonvoter_pred','voter_pred')) 

# multiply the indiv vote prop with the cell prop to create the new prop, then drop the party prop
expanded_targets <- expanded_targets %>%
  dplyr::mutate(n = n * prop) %>%
  dplyr::select(-prop)

expanded_targets <- expanded_targets %>% 
  mutate(vote_type = case_when(vote_type == "nonvoter_pred" ~ "Non_voter",
                               vote_type == "voter_pred" ~ "Voter"))


# then,  do the same for vote
# adjust probs!
targets_voters <- expanded_targets[expanded_targets$vote_type == 'Voter',] %>%
  select(-vote_type)

# biden
targets_voters$pres_2020_dem <- 
  correct_probs(pstrat = targets_voters,
                weighting = 'n',
                cell_prob = 'pres_2020_dem',
                outcome = votes,
                state_factor = 'biden_pct'
  )

# trump
targets_voters$pres_2020_rep <- 
  correct_probs(pstrat = targets_voters,
                weighting = 'n',
                cell_prob = 'pres_2020_rep',
                outcome = votes,
                state_factor = 'trump_pct'
  )


weighted.mean(targets_voters$pres_2020_dem, 
              targets_voters$n)

weighted.mean(targets_voters$pres_2020_rep, 
              targets_voters$n)

targets_voters <- targets_voters %>%
  mutate(pres_2020_other = 1 - (pres_2020_dem + pres_2020_rep)) 


targets_voters <- targets_voters %>%
  rename(prop_biden = pres_2020_dem,
         prop_trump = pres_2020_rep,
         prop_other = pres_2020_other)


# ADJUST TARGET CELLS  
# for voters, collapse vote_type into vote
expanded_targets_voters <- targets_voters  %>%
  gather(vote_type,prop,c('prop_biden','prop_trump','prop_other')) 


# multiply the indiv party prop with the cell prop to create the new prop, then drop the party prop
expanded_targets_voters <- expanded_targets_voters %>%
  dplyr::mutate(n = n * prop) %>%
  dplyr::select(-prop)

expanded_targets_voters <- expanded_targets_voters %>% 
  mutate(vote_type = case_when(vote_type == "prop_biden" ~ "Biden",
                               vote_type == "prop_trump" ~ "Trump",
                               vote_type == "prop_other" ~ "Other"))

expanded_targets_voters %>% 
  group_by(race,vote_type) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))


# now we want to add back Non_voters where vote_type  <- 'Non_voter'
targets <- expanded_targets_voters %>%
  bind_rows(expanded_targets[expanded_targets$vote_type == 'Non_voter',] %>%
              dplyr::select(-c(pres_2020_dem,pres_2020_rep,pres_2020_other))) %>%
  rename(past_vote_16 = past_vote,
         past_vote_20 = vote_type)

unique(targets$past_vote_20)
sum(targets$n)

# check topline results
targets %>% 
  dplyr::filter(past_vote_16 != 'Non_voter') %>%
  group_by(past_vote_16) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

targets %>% 
  dplyr::filter(past_vote_20 != 'Non_voter') %>%
  group_by(past_vote_20) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

# check results by state
targets %>% 
  dplyr::filter(past_vote_20 != 'Non_voter') %>%
  group_by(state_abb, past_vote_20) %>%
  summarise(mrp_biden_2020 = sum(n)) %>%
  mutate(mrp_biden_2020 = mrp_biden_2020/sum(mrp_biden_2020)) %>%
  dplyr::filter(past_vote_20 == 'Biden') %>%
  left_join(as.data.frame(unscale_variables(covars,scale_unscale_vars))) %>%
  ggplot(.,aes(mrp_biden_2020,state_biden_2020)) +
  geom_point() +
  geom_abline()


# look at results by race
targets %>% 
  dplyr::filter(past_vote_16 != 'Non_voter') %>%
  group_by(race,past_vote_16) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

targets %>% 
  dplyr::filter(past_vote_20 != 'Non_voter') %>%
  group_by(race,past_vote_20) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))


# look at results by edu
targets %>% 
  dplyr::filter(past_vote_16 != 'Non_voter') %>%
  group_by(college  = edu %in% c('College','Post-grad'),past_vote_16) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

targets %>% 
  dplyr::filter(past_vote_20 != 'Non_voter') %>%
  group_by(college  = edu %in% c('College','Post-grad'),past_vote_20) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

# look at results by race and college
targets %>% 
  dplyr::filter(past_vote_16 != 'Non_voter') %>%
  group_by(race,college  = edu %in% c('College','Post-grad'),past_vote_16) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n)) %>%
  spread(past_vote_16,n) %>%
  mutate(clinton_margin = Clinton - Trump) %>%
  select(-c(Clinton,Other,Trump)) %>%
  left_join(
    targets %>% 
      dplyr::filter(past_vote_20 != 'Non_voter') %>%
      group_by(race,college  = edu %in% c('College','Post-grad'),past_vote_20) %>%
      summarise(n = sum(n)) %>%
      mutate(n=n/sum(n)) %>%
      spread(past_vote_20,n) %>%
      mutate(biden_margin = Biden - Trump) %>%
      select(-c(Biden,Other,Trump)) 
  ) %>%
  mutate(shift = clinton_margin - biden_margin)


# UNSCALE ALL THE DATASETS ------------------------------------------------
# using unscale function
targets <- unscale_variables(targets,scale_unscale_vars)
cces20 <- unscale_variables(cces20,scale_unscale_vars)
covars <- unscale_variables(covars,scale_unscale_vars)

# SAVE! -------------------------------------------------------------------
message("Saving data")

# save the predictions
targets %>%
  dplyr::select(state_fips,sex,age,race,edu,income5,past_vote_16,past_vote_20,n) %>%
  write_rds(.,'output/mrp/state_level_targets_expanded_past_vote_16_20.rds',compress = 'gz')

# # also save all the posterior draws from the model  
#cell_pred_2020_vote.mat %>%
#  write_rds(.,'output/mrp/model_samples/2020_vote_model_draws.rds',compress = 'gz')

# now that the matrix is written, clear it from memory
# rm(cell_pred_2020_vote.mat)
gc()

# POST-STRATIFY UP --------------------------------------------------------
message('making graphs')

targets <- read_rds('output/mrp/state_level_targets_expanded_past_vote_16_20.rds')

targets_spread <- targets %>% 
  group_by(state_fips, sex, age, race, edu, income5, past_vote_16)  %>%
  summarise(n = sum(n)) %>%
  left_join(
    targets %>% 
      filter(past_vote_20 != 'Non_voter') %>%
      group_by(state_fips, sex, age, race, edu, income5, past_vote_16) %>%
      mutate(n = n / sum(n)) %>%
      left_join(
        targets %>% 
          group_by(state_fips, sex, age, race, edu, income5, past_vote_16,
                   voters = past_vote_20 != 'Non_voter') %>%
          summarise(n = sum(n)) %>%
          group_by(state_fips, sex, age, race, edu, income5, past_vote_16) %>%
          mutate(likely_voter = n / sum(n)) %>%
          dplyr::filter(voters == TRUE) %>%
          ungroup() %>%
          select(-c(voters,n))
      ) %>%
      spread(past_vote_20, n) 
  ) 

targets_spread[is.na(targets_spread)] <- 0

weighted.mean(targets_spread$likely_voter, targets_spread$n)
weighted.mean(targets_spread$Biden, (targets_spread$n * targets_spread$likely_voter))
weighted.mean(targets_spread$Trump, (targets_spread$n * targets_spread$likely_voter))
weighted.mean(targets_spread$Other, (targets_spread$n * targets_spread$likely_voter))

targets_spread <- targets_spread %>% left_join(read_csv('data/state/state_region_crosswalk.csv')) %>% left_join(read_csv('data/state/state_covariates.csv'))
targets_spread <- targets_spread %>% 
  rename(past_vote = past_vote_16,
         pres_2020_dem = Biden,
         pres_2020_rep = Trump,
         pres_2020_other = Other)


# some turnout analyses
targets_spread %>% 
  group_by(race, turnout_2016 = past_vote!='Non_voter') %>% 
  summarise(pct=sum(n)) %>% mutate(pct = pct/sum(pct)) %>% 
  dplyr::filter(turnout_2016 == TRUE) %>%
  left_join(targets_spread %>% 
              group_by(race) %>%
              summarise(turnout_2020 = sum(n*likely_voter) / sum(n)))

targets_spread %>% 
  group_by(age, turnout_2016 = past_vote!='Non_voter') %>% 
  summarise(pct=sum(n)) %>% mutate(pct = pct/sum(pct)) %>% 
  dplyr::filter(turnout_2016 == TRUE) %>%
  left_join(targets_spread %>% 
              group_by(age) %>%
              summarise(turnout_2020 = sum(n*likely_voter) / sum(n)))


# DEMOGRAPHIC ANALYSIS ----------------------------------------------------
# some groups -- 
# region
targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  group_by(region,past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              group_by(region) %>% summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% mutate(net_2020 = biden-trump)) %>%
  mutate(swing = net_2020 - net_2016)

# age
targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  group_by(age,past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              group_by(age) %>% summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% mutate(net_2020 = biden-trump)) %>%
  mutate(swing = net_2020 - net_2016)

# race
targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  group_by(race,past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              group_by(race) %>% summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% mutate(net_2020 = biden-trump)) %>%
  mutate(shift = net_2020 - net_2016)

# race * sex 
targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  group_by(race,sex,past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              group_by(race,sex) %>% summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% mutate(net_2020 = biden-trump)) 

# race * education
targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
  group_by(racexedu = paste(race,edu),past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
              group_by(racexedu = paste(race,edu)) %>% summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% mutate(net_2020 = biden-trump)) %>%
  mutate(swing = net_2020 - net_2016)

# race * education IN RED MIDWESTERN STATES
targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter',state_abb %in% c('OH','MI','WI','PA','IA')) %>% 
  mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
  group_by(racexedu = paste(race,edu),past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              dplyr::filter(state_abb %in% c('OH','MI','WI','PA','IA')) %>%
              mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
              group_by(racexedu = paste(race,edu)) %>% summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% mutate(net_2020 = biden-trump)) %>%
  mutate(swing = net_2020 - net_2016)


# state predictions
state_preds <- targets_spread %>%
  mutate(n = n*likely_voter) %>%
  group_by(state_abb,state_name) %>%
  summarise(pres_2020_dem = sum(n*pres_2020_dem,na.rm=T)/sum(n,na.rm=T),
            pres_2020_rep = sum(n*pres_2020_rep,na.rm=T)/sum(n,na.rm=T),
            state_clinton_2016 = unique(state_clinton_2016),
            state_trump_2016 = unique(state_trump_2016),
            state_clinton_2016_margin = unique(state_clinton_2016_margin),
            n_voters = sum(n)
            
  ) %>%
  mutate(net_2020_dem = pres_2020_dem - pres_2020_rep) %>%
  #dplyr::filter(state_abb != "DC") %>%
  as.data.frame() 

# plot change by state size
state_preds %>%
  mutate(sum2016 = state_clinton_2016 + state_trump_2016,
         state_clinton_2016 = state_clinton_2016 / sum2016,
         state_trump_2016 = state_trump_2016 / sum2016,
         sum2020 = pres_2020_dem + pres_2020_rep,
         pres_2020_dem = pres_2020_dem / sum2020,
         pres_2020_rep = pres_2020_rep / sum2020) %>%
  left_join(read.csv('data/state/elec_col_votes.csv') %>%
              dplyr::select( state_abb,ec_votes)) %>%
  summarise(
    ec.weighted.swing = weighted.mean(
      (pres_2020_dem - pres_2020_rep) - (state_clinton_2016 - state_trump_2016),
      ec_votes
    ),
    voters.weighted.swing = weighted.mean(
      (pres_2020_dem - pres_2020_rep) - (state_clinton_2016 - state_trump_2016),
      n_voters
    )
  )


state_preds %>%
  mutate(sum2016 = state_clinton_2016 + state_trump_2016,
         state_clinton_2016 = state_clinton_2016 / sum2016,
         state_trump_2016 = state_trump_2016 / sum2016,
         sum2020 = pres_2020_dem + pres_2020_rep,
         pres_2020_dem = pres_2020_dem / sum2020,
         pres_2020_rep = pres_2020_rep / sum2020) %>%
  group_by(marginal_state = state_abb %in% c('AZ')) %>%
  summarise(weighted.swing = weighted.mean(
    (pres_2020_dem - pres_2020_rep) - (state_clinton_2016-state_trump_2016),
    n_voters))


state_preds %>%
  mutate(sum2016 = state_clinton_2016 + state_trump_2016,
         state_clinton_2016 = state_clinton_2016 / sum2016,
         state_trump_2016 = state_trump_2016 / sum2016,
         sum2020 = pres_2020_dem + pres_2020_rep,
         pres_2020_dem = pres_2020_dem / sum2020,
         pres_2020_rep = pres_2020_rep / sum2020) %>%
  mutate(lean_2016 = state_clinton_2016 - 0.506,
         lean_2020 = pres_2020_dem - weighted.mean(pres_2020_dem,n_voters),
         swing_lean = lean_2020 - lean_2016) %>%
  left_join(read.csv('data/state/elec_col_votes.csv') %>%
              dplyr::select( state_abb,ec_votes)) %>% 
  summarise(ec.weighted.swing_lean = weighted.mean(swing_lean,ec_votes),
            voters.weighted.swing_lean = weighted.mean(swing_lean,n_voters),
            swing_lean = mean(swing_lean))

urbnmapr::states  %>%
  left_join(
    state_preds %>%
      mutate(sum2016 = state_clinton_2016 + state_trump_2016,
             state_clinton_2016 = state_clinton_2016 / sum2016,
             state_trump_2016 = state_trump_2016 / sum2016,
             sum2020 = pres_2020_dem + pres_2020_rep,
             pres_2020_dem = pres_2020_dem / sum2020,
             pres_2020_rep = pres_2020_rep / sum2020) %>%
      mutate(lean_2016 = state_clinton_2016 - 0.506,
             lean_2020 = pres_2020_dem - weighted.mean(pres_2020_dem,n_voters),
             swing_lean = lean_2020 - lean_2016) 
  ) %>%
  ggplot(.,aes(x=long,y=lat)) +
  geom_polygon(aes(fill = swing_lean>0 ,group=group),col='gray60') +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0)) +
  scale_fill_manual(values=c('TRUE' = 'blue', 'FALSE' = 'red'))

# plot change from 2016 vs non-col white
state_preds %>%
  mutate(swing_16_20 = (pres_2020_dem-pres_2020_rep) - state_clinton_2016_margin) %>%
  left_join(targets_spread %>%
              dplyr::filter(past_vote!='Non_voter') %>%
              mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
              group_by(state_name,racexedu = paste(race,edu)) %>%
              summarise(n=sum(n)) %>% mutate(pct_voters_white_noncol = n/sum(n)) %>%
              dplyr::filter(racexedu == 'White, Non-Hispanic Non-college')) %>%
  ggplot(.,aes(x=pct_voters_white_noncol*100,y=swing_16_20*100)) +
  #geom_text(aes(label=state_abb)) +
  geom_point() +
  scale_size(range=c(1,4)) +
  geom_smooth(method='lm') +
  theme_minimal() +
  theme(legend.position='none',panel.grid.minor = element_blank()) +
  labs(x='Share of votes who are white and have no college degree',
       y='',
       subtitle='Predicted Joe Biden vote margin (2020) minus Hillary Clinton vote\nmargin (2016), percentage points, by state',
       title='A re-realignment election',
       caption='Source: cces20/The Economist; US Census Bureau')
#ggsave("temp.svg",width = 6,height=5)

# how much does shift among whites explain shift in state?
demo_state_table <- targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  group_by(state_abb,
           race,
           past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              group_by(state_abb,
                       race
              ) %>% 
              summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),
                        trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% 
              mutate(net_2020 = biden-trump)) %>%
  ungroup() %>%
  mutate(shift_among_group = net_2020 - net_2016) %>%
  dplyr::select(state_abb,group=race,shift_among_group) %>%
  left_join(state_preds %>%
              mutate(swing_16_20 = (pres_2020_dem-pres_2020_rep) - state_clinton_2016_margin,
                     state_biden_margin = pres_2020_dem-pres_2020_rep) %>%
              dplyr::select(state_abb,n_voters,
                            swing_16_20,state_clinton_2016_margin,state_biden_margin)) 


cor(demo_state_table[demo_state_table$group == 'White, Non-Hispanic',]$shift_among_group,
    demo_state_table[demo_state_table$group == 'White, Non-Hispanic',]$swing_16_20)


# how much does shift among non-col whites explain shift in state?
demo_state_table <- targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
  group_by(state_abb,
           racexedu = ifelse(grepl('White',race),
                             paste(race,edu),
                             race),
           past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              mutate(edu = ifelse(edu %in% c('College','Post-grad'),'College','Non-college')) %>%
              group_by(state_abb,
                       racexedu = ifelse(grepl('White',race),
                                         paste(race,edu),
                                         race)
              ) %>% 
              summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),
                        trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% 
              mutate(net_2020 = biden-trump)) %>%
  ungroup() %>%
  mutate(shift_among_group = net_2020 - net_2016) %>%
  dplyr::select(state_abb,group=racexedu,shift_among_group) %>%
  left_join(state_preds %>%
              mutate(swing_16_20 = (pres_2020_dem-pres_2020_rep) - state_clinton_2016_margin,
                     state_biden_margin = pres_2020_dem-pres_2020_rep) %>%
              dplyr::select(state_abb,n_voters,
                            swing_16_20,state_clinton_2016_margin,state_biden_margin)) 

## compare groups
demo_state_table %>%
  spread(group,shift_among_group) %>%
  ggplot(.,aes(x=`White, Non-Hispanic College`, y=`White, Non-Hispanic Non-college`,
               weight=n_voters,
               col = state_abb %in% c('WI','MI','PA','MN','NH') )) +
  geom_text(aes(label=state_abb)) +
  geom_abline() + 
  #geom_point(aes(size=n_voters)) +
  scale_size(range=c(1,4)) +
  geom_smooth(method='lm') +
  theme_minimal() +
  theme(legend.position='none',panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks=seq(-1,1,0.05)) +
  scale_y_continuous(breaks=seq(-1,1,0.05)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

## state swing ~ group swing
lm(swing_16_20 ~ . -n_voters -state_clinton_2016_margin -state_biden_margin,
   weights = n_voters,
   data = demo_state_table %>%
     spread(group,shift_among_group) %>%
     dplyr::select(-c(state_abb))) %>% summary


## plot
demo_state_table %>%
  ggplot(.,aes(x=shift_among_group, y=swing_16_20,col=group)) +
  geom_text(aes(label=state_abb)) +
  #geom_point(aes(size=n_voters)) +
  scale_size(range=c(1,4)) +
  geom_smooth(method='lm',show.legend = F) +
  theme_minimal() +
  theme(legend.position='top',
        panel.grid.minor = element_blank()) +
  guides('size'='none',
         'color' = guide_legend(nrow=2)) +
  scale_x_continuous(breaks=seq(-1,1,0.05)) +
  scale_y_continuous(breaks=seq(-1,1,0.05)) +
  labs(x='Swing in Biden versus Clinton vote margin among group, by state',
       y='Swing in Biden versus Clinton vote margin, by state') +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) #+ facet_wrap(~group,ncol=5)  

# by age
demo_state_table <- targets_spread %>% 
  dplyr::filter(past_vote!='Non_voter') %>% 
  group_by(state_abb,
           age,
           past_vote) %>% 
  summarise(n=sum(n)) %>% mutate(n = n/sum(n)) %>% 
  spread(past_vote,n) %>% mutate(net_2016=Clinton-Trump) %>% 
  left_join(targets_spread %>% 
              group_by(state_abb,age) %>% 
              summarise(biden = sum(pres_2020_dem*n*likely_voter)/sum(n*likely_voter),
                        trump=sum(pres_2020_rep*n*likely_voter)/sum(n*likely_voter)) %>% 
              mutate(net_2020 = biden-trump)) %>%
  ungroup() %>%
  mutate(shift_among_group = net_2020 - net_2016) %>%
  dplyr::select(state_abb,group=age,shift_among_group) %>%
  left_join(state_preds %>%
              mutate(swing_16_20 = (pres_2020_dem-pres_2020_rep) - state_clinton_2016_margin,
                     state_biden_margin = pres_2020_dem-pres_2020_rep) %>%
              dplyr::select(state_abb,n_voters,
                            swing_16_20,state_clinton_2016_margin,state_biden_margin)) 

## compare groups
demo_state_table %>%
  spread(group,shift_among_group) %>%
  ggplot(.,aes(x=`18-29`, y=`65+`,
               weight=n_voters,
               col = state_abb %in% c('WI','MI','PA','MN','NH') )) +
  geom_text(aes(label=state_abb,size=n_voters)) +
  geom_abline() + 
  #geom_point(aes(size=n_voters)) +
  scale_size(range=c(1,4)) +
  geom_smooth(method='lm') +
  theme_minimal() +
  theme(legend.position='none',panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks=seq(-1,1,0.05)) +
  scale_y_continuous(breaks=seq(-1,1,0.05)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

## state swing ~ group swing
lm(swing_16_20 ~ . -n_voters,
   weights = n_voters,
   data = demo_state_table %>%
     spread(group,shift_among_group) %>%
     dplyr::select(-c(state_abb))) %>% summary

## plot
demo_state_table %>%
  ggplot(.,aes(x=shift_among_group, y=swing_16_20,col=group)) +
  geom_text(aes(label=state_abb,size=n_voters)) +
  #geom_point(aes(size=n_voters)) +
  scale_size(range=c(1,4)) +
  geom_smooth(method='lm',show.legend = F) +
  theme_minimal() +
  theme(legend.position='top',
        panel.grid.minor = element_blank()) +
  guides('size'='none',
         'color' = guide_legend(nrow=1)) +
  scale_x_continuous(breaks=seq(-1,1,0.05)) +
  scale_y_continuous(breaks=seq(-1,1,0.05)) +
  labs(x='Swing in Biden versus Clinton vote margin among group, by state',
       y='Swing in Biden versus Clinton vote margin, by state') +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) #+ facet_wrap(~group,ncol=5)  

## young people more anti-biden in more liberal places?
demo_state_table %>%
  dplyr::filter(group %in% c("18-29","65+"),state_abb != 'DC') %>%
  ggplot(.,aes(x=state_clinton_2016_margin,y=shift_among_group)) +
  geom_text(data=demo_state_table %>%
              dplyr::filter(group %in% c("18-29","65+"),state_abb != 'DC') %>%
              dplyr::filter(state_abb %in% c('CA','TX','MI','WI','WY')),
            aes(label=state_abb),col='red',size=5) +
  geom_point(size=2) +
  #scale_size(range=c(2,4)) +
  geom_smooth(method='lm',show.legend = F,linetype=2) +
  theme_minimal() +
  theme(legend.position='top',
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=0)) +
  guides('size'='none',
         'color' = guide_legend(nrow=1)) +
  scale_x_continuous(breaks=seq(-1,1,0.1),#limits = c(-0.5,0.4),
                     labels = function(x){x*100}) +
  scale_y_continuous(breaks=seq(-1,1,0.1),#limits = c(-0.3,0.),
                     labels = function(x){x*100},
                     position='right',) +
  facet_wrap(~group,ncol=1) +
  labs(x="Hillary Clinton's vote margin in 2016, percentage points",
       y='',
       subtitle="Joe Biden's predicted vote margin against Donald\nTrump in 2020 relative to Hillary Clinton's in 2016\nBy state and age group",
       caption="Source: cces20/The Economist") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

#ggsave("~/Desktop/age_graph.svg",width=5,height=6)
#ggsave("~/Desktop/age_graph.pdf",width=5,height=6)
#demo_state_table %>%
# spread(group,shift_among_group) %>%
#  dplyr::select(state_abb,state_clinton_2016_margin,`18-29`,`65+`) %>%
#  write_csv('~/Desktop/age_data.csv')


# look at 2016 v 20 for a bunch of different demo groups

# STATE-LEVEL ANALYSIS ----------------------------------------------------
# plot in a grid
pdf('figures/2020_vote_calibration.pdf',width=12,height=10)
grid.arrange(ncol=2,
             ggplot(state_preds, aes(x=state_trump_2016, y=pres_2020_rep)) +
               stat_smooth(geom='line',col='blue',fullrange=T,linetype=2,method = 'lm') +
               geom_abline() +
               geom_text(aes(label=state_abb)) +
               labs(subtitle = sprintf("r=%s",round(cor(state_preds$pres_2020_rep,
                                                        state_preds$state_trump_2016),3))) +
               coord_cartesian(ylim=c(0.2,0.8),xlim=c(0.2,0.8)) +
               theme_minimal(),
             
             ggplot(state_preds, aes(x=state_clinton_2016, y=pres_2020_dem)) +
               stat_smooth(geom='line',col='blue',fullrange=T,linetype=2,method = 'lm') +
               geom_abline() +
               geom_text(aes(label=state_abb)) +
               labs(subtitle = sprintf("r=%s",round(cor(state_preds$pres_2020_dem,
                                                        state_preds$state_clinton_2016),3))) +
               coord_cartesian(ylim=c(0.2,0.8),xlim=c(0.2,0.8)) +
               theme_minimal(),
             
             ggplot(state_preds, aes(x=state_clinton_2016_margin, y=net_2020_dem)) +
               stat_smooth(geom='line',col='blue',fullrange=T,linetype=2,method = 'lm') +
               geom_abline(slope=1) +
               geom_text(aes(label=state_abb)) +
               labs(subtitle = sprintf("r=%s",round(cor(state_preds$state_clinton_2016_margin,
                                                        state_preds$net_2020_dem),3))) +
               coord_cartesian(ylim=c(-0.5,0.5),xlim=c(-0.5,0.5)) +
               theme_minimal(),
             
             ggplot(state_preds, aes(x=state_clinton_2016_margin,y=net_2020_dem - state_clinton_2016_margin)) +
               stat_smooth(geom='line',col='blue',fullrange=T,linetype=2,method = 'lm') +
               geom_hline(yintercept = 0)+
               geom_text(aes(label=state_abb)) +
               labs(subtitle = sprintf("r=%s",round(cor(state_preds$state_clinton_2016_margin,
                                                        state_preds$net_2020_dem - state_preds$state_clinton_2016_margin),3))) +
               theme_minimal()
             
)
dev.off();Sys.sleep(2)

# sanity check -- weighted avg of approval and disapproval compared to cces20
state_check <- targets_spread %>%
  mutate(n = n*likely_voter) %>%
  group_by(state_abb, state_name) %>%
  summarise(pres_2020_dem = sum(n*pres_2020_dem,na.rm=T)/sum(n,na.rm=T),
            pres_2020_rep = sum(n*pres_2020_rep,na.rm=T)/sum(n,na.rm=T),
            pres_2020_other = sum(n*pres_2020_other,na.rm=T)/sum(n,na.rm=T),
            n = sum(n)) %>%
  as.data.frame() %>%
  mutate(weight = n/sum(n))

weighted.mean(state_check$pres_2020_dem,state_check$weight)
weighted.mean(state_check$pres_2020_rep,state_check$weight)
weighted.mean(state_check$pres_2020_other,state_check$weight)

svymean(~past_vote, cces20.svy)
svymean(~past_vote, subset(cces20.svy, past_vote!='Non_voter')) #subset(cces20.svy, voter_reg == 1))
svymean(~past_vote, subset(cces20.svy, past_vote!='Non_voter'&past_vote!='Non_voter')) #subset(cces20.svy, voter_reg == 1))
svymean(~past_vote, subset(cces20.svy, past_vote!='Non_voter'&past_vote=='Non_voter')) #subset(cces20.svy, voter_reg == 1))


# check state observations
svytable(~past_vote+state_name,
         subset(cces20.svy, past_vote!='Non_voter')) %>% #subset(cces20.svy, voter_reg == 1)) %>%
  prop.table(margin=2) %>%
  t() %>%
  as_tibble() %>%
  spread(past_vote,n) %>%
  set_names(.,c('state_name','dem_disag','Non_voter','other','rep_disag')) %>%
  left_join(state_check) %>%
  mutate(diff = pres_2020_dem - dem_disag,
         #diff = trump_disapprove - disapprove_disag
  ) %>%
  pull(diff) %>% (function(x=.){sqrt(mean(x^2))})

# mrp map
map <- urbnmapr::states %>% 
  left_join(state_preds %>%
              mutate(net_2020_dem = pres_2020_dem-pres_2020_rep) %>%
              # mutate(net_2020_dem = case_when(state_abb %in% c('CO','OR') ~ net_2020_dem + 0.03,
              #                                 state_abb %in% c('AK','NC','MS','IA') ~ net_2020_dem - 0.02,
              #                                 TRUE ~ net_2020_dem )) %>%
              mutate(net_2020_dem_class = cut(-1*(net_2020_dem-0.00),
                                              breaks=c(-1, -0.2, -0.1, -0.05, 0,  0.05, 0.1, 0.2, 1),
                                              labels=c("Democratic +20 or higher",
                                                       "+20 to +10",
                                                       "+10 to +5",
                                                       "Even to Democratic +5",
                                                       "Even to Republican +5",
                                                       #"Even",
                                                       "+5 to +10",
                                                       "+10 to +20",
                                                       "Republican +20 or higher")))  #%>%
            #dplyr::select(state_abb,net_2020_dem,net_2020_dem_class) %>%
            #write_csv("temp.csv")
  ) %>% 
  ggplot(.,aes(x=long,y=lat)) +
  geom_polygon(aes(fill = net_2020_dem_class ,group=group),col='gray60') +
  scale_fill_manual(name="",
                    values=c("Democratic +20 or higher" = "#21618C",
                             "+20 to +10"="#2E86C1",
                             "+10 to +5" = "#A9CCE3",
                             "Even to Democratic +5" = "#D6EAF8",
                             "Even to Republican +5"= "#FADBD8", 
                             #"Even" = "gray80",
                             "+5 to +10" = "#EC7063",
                             "+10 to +20"="#CB4335",
                             "Republican +20 or higher" = "#943126")) +
  labs(title="2020 presidential vote by state",
       subtitle="Among likely voters",
       caption = "Source: cces20/The Economist") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0))

print(map)

ggsave(plot=map,filename = "figures/2020_vote_by_state.pdf",width=10,height=10)


# trump approval by demographic breakdown
bkdown <- targets_spread %>%
  mutate(n = n*likely_voter) %>%
  group_by(state_abb,state_name,region,race,edu) %>%
  summarise(pres_2020_dem = sum(n*pres_2020_dem,na.rm=T)/sum(n,na.rm=T),
            pres_2020_rep = sum(n*pres_2020_rep,na.rm=T)/sum(n,na.rm=T),
            n=sum(n)) %>%
  mutate(net_2020_dem = pres_2020_dem - pres_2020_rep) %>%
  as.data.frame() %>%
  mutate(race = factor(race,c("Black, Non-Hispanic","Hispanic","Other, Non-Hispanic","White, Non-Hispanic")),
         edu = factor(edu, c('No HS','HS grad','Some college','College','Post-grad'))) %>%
  mutate(scale = n/sum(n))


bdown_map <- urbnmapr::states %>% 
  left_join(bkdown) %>% 
  mutate(net_2020_dem_class = cut(-1*(net_2020_dem-0.00),
                                  breaks=c(-1, -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 1),
                                  labels=c("Democratic +20 or higher",
                                           "+20 to +10",
                                           "+10 to +5",
                                           "Even to Democratic +5",
                                           "Even to Republican +5",
                                           "+5 to +10",
                                           "+10 to +20",
                                           "Republican +20 or higher"))) %>%
  na.omit() %>%
  ggplot(.,aes(x=long,y=lat)) +
  geom_polygon(aes(fill = net_2020_dem_class ,group=group),col='gray60') +
  scale_fill_manual(name="",
                    values=c("Democratic +20 or higher" = "#21618C",
                             "+20 to +10"="#2E86C1",
                             "+10 to +5" = "#A9CCE3",
                             "Even to Democratic +5" = "#D6EAF8",
                             "Even to Republican +5"= "#FADBD8", 
                             "+5 to +10" = "#EC7063",
                             "+10 to +20"="#CB4335",
                             "Republican +20 or higher" = "#943126")) +
  labs(title="2020 presidential vote by state",
       subtitle="Among likely voters",
       caption = "Source: cces20/The Economist") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0)) +
  facet_wrap(~race+edu,ncol=5) 


pdf("figures/2020_vote_by_state_by_demog.pdf",width=14,height=9)
print(bdown_map)
dev.off();Sys.sleep(2)

# some scaling?
bkdown <- targets_spread %>%
  mutate(n = n*likely_voter) %>%
  group_by(state_abb,state_name,region,
           #race=ifelse(race=='White, Non-Hispanic','White','Non-white'),
           race,
           edu) %>%
  summarise(pres_2020_dem = sum(n*pres_2020_dem,na.rm=T)/sum(n,na.rm=T),
            pres_2020_rep = sum(n*pres_2020_rep,na.rm=T)/sum(n,na.rm=T),
            n=sum(n)) %>%
  mutate(net_2020_dem = pres_2020_dem - pres_2020_rep) %>%
  as.data.frame() %>%
  mutate(race = factor(race,c("Black, Non-Hispanic","Hispanic","Other, Non-Hispanic","White, Non-Hispanic")),
         #race = factor(race,c('Non-white','White')),
         edu = factor(edu, c('No HS','HS grad','Some college','College','Post-grad'))) %>%
  mutate(scale = n/sum(n)) %>% 
  mutate(net_2020_dem_class = cut(-1*(net_2020_dem-0.00),
                                  breaks=c(-1, -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 1),
                                  labels=c("Democratic +20 or higher",
                                           "+20 to +10",
                                           "+10 to +5",
                                           "Even to Democratic +5",
                                           "Even to Republican +5",
                                           "+5 to +10",
                                           "+10 to +20",
                                           "Republican +20 or higher")))


# map data
bdown_map <- urbnmapr::states %>% 
  left_join(bkdown) 

# centroids
states <- urbnmapr::states

cntrd <- function(x) {
  data.frame(
    centroid(as.matrix(x[,c("long", "lat")]))
  ) %>%
    mutate(state_abb = str_to_upper(unique(x$state_abbv))) %>%
    rename(long=lon)
}

centroids <- by(states, states$group, cntrd)  %>% do.call('bind_rows',.) %>%
  group_by(state_abb) %>%
  dplyr::filter(row_number() == 1) 


bkdown_centroids <- bkdown %>%
  left_join(centroids)

# combine
bdown_map_scaled <- ggplot() +
  geom_polygon(data = bdown_map,# %>% dplyr::filter(grepl('East North Central',region)),
               aes(x=long,y=lat,group=group),col='gray60',fill=NA)  +
  # add points on top +
  geom_point(data=bkdown_centroids,# %>% dplyr::filter(grepl('East North Central',region)),
             aes(x=long,y=lat,col = net_2020_dem_class, 
                 size = n)) +
  scale_size(range=c(0.1,5)) + 
  # now colors
  scale_color_manual(name="",
                     values=c("Democratic +20 or higher" = "#21618C",
                              "+20 to +10"="#2E86C1",
                              "+10 to +5" = "#A9CCE3",
                              "Even to Democratic +5" = "#D6EAF8",
                              "Even to Republican +5"= "#FADBD8", 
                              "+5 to +10" = "#EC7063",
                              "+10 to +20"="#CB4335",
                              "Republican +20 or higher" = "#943126")) +
  labs(title="2020 presidential vote by state",
       subtitle="Among likely voters",
       caption = "Source: cces20/The Economist") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides('col'=guide_legend(override.aes = list(size = 10)),
         'size'='none') +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0)) +
  facet_wrap(~race+edu,ncol=5) 

pdf("figures/2020_vote_by_state_by_demog_scaled.pdf",width=14,height=9)
print(bdown_map_scaled)
dev.off();Sys.sleep(2)


# some scaling? not breakdown this time
state_bdown <- targets_spread %>%
  mutate(n = n*likely_voter) %>%
  group_by(state_abb,state_name,region) %>%
  summarise(pres_2020_dem = sum(n*pres_2020_dem,na.rm=T)/sum(n,na.rm=T),
            pres_2020_rep = sum(n*pres_2020_rep,na.rm=T)/sum(n,na.rm=T),
            n=sum(n)) %>%
  mutate(net_2020_dem = pres_2020_dem - pres_2020_rep) %>%
  as.data.frame() %>%
  mutate(scale = n/sum(n)) %>% 
  mutate(net_2020_dem_class = cut(-1*(net_2020_dem-0.00),
                                  breaks=c(-1, -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 1),
                                  labels=c("Democratic +20 or higher",
                                           "+20 to +10",
                                           "+10 to +5",
                                           "Even to Democratic +5",
                                           "Even to Republican +5",
                                           "+5 to +10",
                                           "+10 to +20",
                                           "Republican +20 or higher")))


# map data
state_map <- urbnmapr::states %>% 
  left_join(state_bdown) 

# centroids
states <- urbnmapr::states

cntrd <- function(x) {
  data.frame(
    centroid(as.matrix(x[,c("long", "lat")]))
  ) %>%
    mutate(state_abb = str_to_upper(unique(x$state_abbv))) %>%
    rename(long=lon)
}

centroids <- by(states, states$group, cntrd)  %>% 
  do.call('bind_rows',.) %>%
  group_by(state_abb) %>%
  dplyr::filter(row_number() == 1) 


state_bdown_centroids <- state_bdown %>%
  left_join(centroids)

# non-overlapping centroids
# combine
bdown_map_scaled <- ggplot() +
  geom_polygon(data = state_map,# %>% dplyr::filter(grepl('East North Central',region)),
               aes(x=long,y=lat,group=group),col='gray60',fill=NA)  +
  # add points on top +
  geom_point(data=state_bdown_centroids,# %>% dplyr::filter(grepl('East North Central',region)),
             aes(x=long,y=lat,col = net_2020_dem_class, 
                 size = n)) +
  scale_size(range=c(0.1,20)) + 
  # now colors
  scale_color_manual(name="",
                     values=c("Democratic +20 or higher" = "#21618C",
                              "+20 to +10"="#2E86C1",
                              "+10 to +5" = "#A9CCE3",
                              "Even to Democratic +5" = "#D6EAF8",
                              "Even to Republican +5"= "#FADBD8", 
                              "+5 to +10" = "#EC7063",
                              "+10 to +20"="#CB4335",
                              "Republican +20 or higher" = "#943126")) +
  labs(title="2020 presidential vote by state",
       subtitle="Among likely voters",
       caption = "Source: cces20/The Economist") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides('col'=guide_legend(override.aes = list(size = 10)),
         'size'='none') +
  theme_void() +
  theme(legend.position = 'top',
        plot.caption=element_text(hjust=0)) 

pdf("figures/2020_vote_by_state_scaled.pdf",width=10,height=10)
print(bdown_map_scaled)
dev.off();Sys.sleep(2)



# OVER TIME ---------------------------------------------------------------

# ---- LEFT OFF HERE ----


# UNCERTAINTY -------------------------------------------------------------

if(FALSE){
  message('Generating confidence intervals')
  # the voter turnout draws are stord in the `cell_pred_2020_turnout.mat` object
  
  # and here are the vote choice probs
  cell_pred_2020_vote.mat.dem <- cell_pred_2020_vote.mat[,,1]
  cell_pred_2020_vote.mat.rep <- cell_pred_2020_vote.mat[,,3]
  
  temp_targets_spread <- targets_spread
  
  # get pred and model CI for each state
  mcmc_state_preds <- pblapply(1:nrow(cell_pred_2020_vote.mat),
                               cl = parallel::detectCores()-1,
                               function(x){
                                 
                                 # change voter population to that implied by get turnout draw
                                 temp_targets_spread$n <- temp_targets_spread$n * cell_pred_2020_turnout.mat[x,]
                                 
                                 # get vote draws
                                 temp_targets_spread$pres_2020_dem <- cell_pred_2020_vote.mat.dem[x,]
                                 temp_targets_spread$pres_2020_rep <- cell_pred_2020_vote.mat.rep[x,]
                                 
                                 # check
                                 state_preds_temp <- temp_targets_spread %>%
                                   group_by(state_name) %>%
                                   summarise(pres_2020_dem = sum(n*pres_2020_dem,na.rm=T)/sum(n,na.rm=T),
                                             pres_2020_rep = sum(n*pres_2020_rep,na.rm=T)/sum(n,na.rm=T),
                                             n=sum(n))
                                 
                                 return(state_preds_temp %>% mutate(iter=x))
                                 
                               })
  
  write_rds(mcmc_state_preds,'output/mrp/model_samples/2020_vote_uncertainty.rds',compress = 'gz')
  
  mcmc_state_preds <- read_rds('output/mrp/model_samples/2020_vote_uncertainty.rds')
  
  # get means and plot
  uncertainty.gg <- mcmc_state_preds %>%
    do.call('bind_rows',.) %>%
    dplyr::filter(!state_name %in% c("District of Columbia")) %>% #,"Utah"
    group_by(state_name) %>%
    mutate(net_2020_dem = pres_2020_dem - pres_2020_rep) %>%
    # mutate(net_2020_dem = case_when(state_name %in% c('Colorado','Oregon') ~ net_2020_dem + 0.03,
    #                                 state_name %in% c('Alaska','North Carolina','Utah','Iowa') ~ net_2020_dem - 0.02,
    #                                 TRUE ~ net_2020_dem )) %>%
    summarise(net_dem_mean = median(net_2020_dem),
              net_dem_upper = quantile(net_2020_dem,0.9),
              net_dem_lower = quantile(net_2020_dem,0.1),
              net_dem_upper95 = quantile(net_2020_dem,0.975),
              net_dem_lower95 = quantile(net_2020_dem,0.025)) %>%
    #dplyr::filter(abs(net_dem_mean) < 0.2) %>%
    ggplot(.,aes(y=reorder(state_name,net_dem_mean))) +
    geom_vline(xintercept = 0) +
    #geom_label(aes(x=net_dem_mean,label=state_name)) +
    geom_point(aes(x=net_dem_mean)) + 
    geom_segment(aes(x=net_dem_lower,xend=net_dem_upper,
                     yend=state_name),size=1) +
    geom_segment(aes(x=net_dem_lower95,xend=net_dem_upper95,
                     yend=state_name),size=0.5) +
    scale_x_continuous(breaks=seq(-1,1,0.05),
                       labels=function(x){round(x*100)}) +
    theme_minimal() + 
    theme(panel.grid.minor = element_blank())  +
    labs(x='Net Democratic 2020 presidential vote',
         y='')
  
  
  pdf("figures/2020_vote_by_state_credibility.pdf",width=8,height=8)
  print(uncertainty.gg)
  dev.off();Sys.sleep(2)
  
}


# elec college sims
if(FALSE){
  message("Generating electoral college scenarios")
  # count evs
  simulated_evs <- mcmc_state_preds %>%
    do.call('bind_rows',.) %>%
    left_join(read_csv('data/state/elec_col_votes.csv')) %>%
    group_by(iter) %>%
    summarise(dem_ev = sum((pres_2020_dem > pres_2020_rep)*ec_votes),
              dem_pop_vote_margin = (sum(pres_2020_dem*n)/sum(n) - sum(pres_2020_rep*n)/sum(n)))
  
  # probabilities
  nrow(simulated_evs[simulated_evs$dem_ev>=270,])/nrow(simulated_evs)
  nrow(simulated_evs[simulated_evs$dem_pop_vote_margin>=0,])/nrow(simulated_evs)
  
  # plot
  ec_histogram.gg <- ggplot(simulated_evs,aes(x=dem_ev,
                                              fill=ifelse(dem_ev>=270,'Democratic','Republican'))) + 
    geom_histogram(aes(y = ..count..),
                   binwidth=2) +
    scale_x_continuous(breaks=seq(0,560,30)) +
    scale_y_continuous(breaks=seq(0,nrow(simulated_evs),nrow(simulated_evs) / 100),
                       labels=function(x){sprintf('%s%%',round(x/nrow(simulated_evs)*100,2))}) +
    theme_minimal() +
    scale_fill_manual(name='Electoral College victory',
                      values=c("Democratic"='blue','Republican'='red')) +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    labs(x='Democratic Electoral College votes',
         y='Probability',
         caption='Not simulating any non-sampling error')
  
  pdf('figures/2020_vote_electoral_college_histogram.pdf',width=8,height=6)
  print(ec_histogram.gg)
  dev.off();Sys.sleep(2)
  
  # pop vote histogram
  # plot
  popvote_histogram.gg <- ggplot(simulated_evs,aes(x=dem_pop_vote_margin,
                                                   fill=case_when(dem_ev>=270 & dem_pop_vote_margin>0 ~ 'Dem. EC & Dem. Pop. Vote',
                                                                  dem_ev>=270 & dem_pop_vote_margin<0 ~ 'Dem. EC & Rep. Pop. Vote',
                                                                  dem_ev<270 & dem_pop_vote_margin>0 ~ 'Rep. EC & Dem. Pop. Vote',
                                                                  dem_ev<270 & dem_pop_vote_margin<0 ~ 'Rep. EC & Rep. Vote'))) + 
    geom_histogram(aes(y = ..count..),
                   binwidth=0.001) +
    scale_x_continuous(breaks=seq(-1,1,0.01))+
    scale_y_continuous(breaks=seq(0,nrow(simulated_evs),nrow(simulated_evs) / 100),
                       labels=function(x){sprintf('%s%%',round(x/nrow(simulated_evs)*100))}) +
    theme_minimal() +
    scale_fill_manual(name='Scenario',
                      values=c('Dem. EC & Dem. Pop. Vote'='#2E86C1',
                               'Dem. EC & Rep. Pop. Vote'='#85C1E9',
                               'Rep. EC & Dem. Pop. Vote'='#F1948A',
                               'Rep. EC & Rep. Vote'='#CB4335')) +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    labs(x='Democratic popular vote margin',
         y='Probability')
  
  pdf('figures/2020_vote_popular_vote_histogram.pdf',width=8,height=6)
  print(popvote_histogram.gg)
  dev.off();Sys.sleep(2)
  
  # try different deltas for generic dem punish
  sims_w_delta <- lapply(seq(-0.05,0,0.01),
                         function(x){
                           simulated_evs <- mcmc_state_preds %>%
                             do.call('bind_rows',.) %>%
                             left_join(read_csv('data/state/elec_col_votes.csv')) %>%
                             group_by(iter) %>%
                             summarise(dem_ev = sum((pres_2020_dem+x > pres_2020_rep)*ec_votes)) %>%
                             mutate(delta = x)
                           
                         }) %>%
    do.call('bind_rows',.)
  
  # plot
  avg_d_margin <- mean(simulated_evs$dem_pop_vote_margin)
  
  hypo_ec.gg <- ggplot(sims_w_delta,aes(x=dem_ev,
                                        fill=ifelse(dem_ev>=270,'Democratic','Republican'))) + 
    geom_vline(xintercept=270,linetype=2,col='gray60') +
    geom_histogram(aes(y = ..count..),
                   binwidth=2) +
    scale_x_continuous(breaks=seq(0,560,30)) +
    scale_y_continuous(labels=function(x){sprintf('%s%%',round(x/max(sims_w_delta$iter)*100))}) +
    theme_minimal() +
    scale_fill_manual(name='Electoral College victory',
                      values=c("Democratic"='blue','Republican'='red')) +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    labs(title='Hypothetical Electoral College results, 2020',
         subtitle=sprintf('The generic Democratic candidate is polling at %s percentage points today',
                          paste0(ifelse(avg_d_margin >= 0 ,'+',''),
                                 round(avg_d_margin*100))),
         x='Democratic Electoral College votes',
         y='Probability') +
    facet_wrap(~delta,
               labeller = labeller(delta = function(x){
                 sprintf("If the eventual Democratic nominee\npolls at %s percentage points",
                         paste0(ifelse(round((as.numeric(x) + avg_d_margin)*100) >= 0 ,'+',''),
                                round((as.numeric(x) + avg_d_margin)*100)))
               }))
  
  pdf('figures/2020_vote_hypothetical_ec_histograms.pdf',width=8,height=6)
  print(hypo_ec.gg)
  dev.off();Sys.sleep(2)
  
  # EVs by popular vote, simulations with 3 points of national non-sampling error on either side
  sims_w_delta_popvotes <- lapply(seq(-0.03,0.03,0.01),
                                  function(x){
                                    mcmc_state_preds %>%
                                      do.call('bind_rows',.) %>%
                                      left_join(read_csv('data/state/elec_col_votes.csv')) %>%
                                      mutate(pres_2020_dem = pres_2020_dem - x) %>%
                                      group_by(iter) %>%
                                      summarise(dem_pop_vote_margin = (sum(pres_2020_dem*n)/sum(n) - 
                                                                         sum(pres_2020_rep*n)/sum(n)),
                                                dem_ev = sum((pres_2020_dem  > pres_2020_rep)*ec_votes),
                                                dem_pop_votes = sum(pres_2020_dem*n),
                                                rep_pop_votes = sum(pres_2020_rep*n))  %>%
                                      mutate(delta=x)
                                    
                                  }) %>%
    do.call('bind_rows',.)
  
  # plot
  vote_seat_curve.gg <- ggplot(sims_w_delta_popvotes,
                               aes(x=dem_pop_vote_margin, y=dem_ev,
                                   col=ifelse(dem_ev>=270,'Democratic','Republican'))) + 
    geom_point(aes(alpha=ifelse(delta==0,1,0.1))) +
    scale_alpha(range=c(0.1,1)) +
    scale_color_manual(name='Electoral College victory',
                       values=c("Democratic"='blue','Republican'='red'))  +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 270) +
    theme_minimal() +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    guides('alpha'='none') +
    scale_x_continuous(breaks=seq(-1,1,0.02))
  
  pdf('figures/2020_vote_ec_popvote_curve.pdf',width=8,height=8)
  print(vote_seat_curve.gg)
  dev.off();Sys.sleep(2)
  
  # probability
  pop_vote_victory_curve <- sims_w_delta_popvotes %>%
    group_by(dem_pop_vote_margin = round(dem_pop_vote_margin,3)) %>%
    summarise(prob_ec_victory = mean(dem_ev>=270),
              p=n()) %>%
    arrange(desc(dem_pop_vote_margin)) %>%
    ungroup() %>% 
    mutate(p=p/sum(p))
  
  # plot
  ev_popvote_divide.gg <- ggplot(pop_vote_victory_curve, aes(x=dem_pop_vote_margin, y=prob_ec_victory)) +
    geom_hline(yintercept = 0.5) +
    geom_point(shape=1) +
    scale_size(range=c(0.1,5)) +
    stat_smooth(geom='line',method='gam',formula=y~s(x)) +
    theme_minimal() +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks=seq(-1,1,0.01))
  
  pdf('figures/2020_vote_ev_popvote_divide.pdf',width=8,height=6)
  print(ev_popvote_divide.gg)
  dev.off();Sys.sleep(2)
  
  # plot
  ec_histogram.gg2 <- ggplot(sims_w_delta_popvotes,aes(x=dem_ev,
                                                       fill=ifelse(dem_ev>=270,'Democratic','Republican'))) + 
    geom_histogram(aes(y = ..count..),
                   binwidth=2) +
    scale_x_continuous(breaks=seq(0,560,30)) +
    scale_y_continuous(breaks=seq(0,nrow(sims_w_delta_popvotes),nrow(sims_w_delta_popvotes) / 100),
                       labels=function(x){sprintf('%s%%',round(x/nrow(sims_w_delta_popvotes)*100))}) +
    theme_minimal() +
    scale_fill_manual(name='Electoral College victory',
                      values=c("Democratic"='blue','Republican'='red')) +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    labs(x='Democratic Electoral College votes',
         y='Probability',
         caption='Simulating 3ppt non-sampling error')
  
  pdf('figures/2020_vote_electoral_college_histogram_nonsampling_error.pdf',width=8,height=6)
  print(ec_histogram.gg2)
  dev.off();Sys.sleep(2)
  
  # pop vote histogram
  # plot
  popvote_histogram.gg2 <- ggplot(sims_w_delta_popvotes,aes(x=dem_pop_vote_margin,
                                                            fill=case_when(dem_ev>=270 & dem_pop_vote_margin>0 ~ 'Dem. EC & Dem. Pop. Vote',
                                                                           dem_ev>=270 & dem_pop_vote_margin<0 ~ 'Dem. EC & Rep. Pop. Vote',
                                                                           dem_ev<270 & dem_pop_vote_margin>0 ~ 'Rep. EC & Dem. Pop. Vote',
                                                                           dem_ev<270 & dem_pop_vote_margin<0 ~ 'Rep. EC & Rep. Vote'))) + 
    geom_histogram(aes(y = ..count..),
                   binwidth=0.001) +
    scale_x_continuous(breaks=seq(-1,1,0.01))+
    scale_y_continuous(breaks=seq(0,nrow(sims_w_delta_popvotes),nrow(sims_w_delta_popvotes) / 200),
                       labels=function(x){sprintf('%s%%',round(x/nrow(sims_w_delta_popvotes)*100,2))}) +
    theme_minimal() +
    scale_fill_manual(name='Scenario',
                      values=c('Dem. EC & Dem. Pop. Vote'='#2E86C1',
                               'Dem. EC & Rep. Pop. Vote'='#85C1E9',
                               'Rep. EC & Dem. Pop. Vote'='#F1948A',
                               'Rep. EC & Rep. Vote'='#CB4335')) +
    theme(legend.position = 'top',
          legend.justification = 'left',
          panel.grid.minor = element_blank()) +
    labs(x='Democratic popular vote margin',
         y='Probability')
  
  
  #pdf('figures/2020_vote_popular_vote_histogram.pdf',width=8,height=8)
  print(popvote_histogram.gg2)
  #dev.off();Sys.sleep(2)
  
  # check probabilities again with the 3 pts of error added
  nrow(sims_w_delta_popvotes[sims_w_delta_popvotes$dem_ev>=270,])/nrow(sims_w_delta_popvotes)
  nrow(sims_w_delta_popvotes[sims_w_delta_popvotes$dem_pop_vote_margin>=0,])/nrow(sims_w_delta_popvotes)
}

# ALL DONE! ---------------------------------------------------------------
message("///// All done! /////")

# send alert if on mac
if(Sys.info()['sysname'] == "Darwin"){ for(i in 1:3){beepr::beep(2);Sys.sleep(0.5)} }
