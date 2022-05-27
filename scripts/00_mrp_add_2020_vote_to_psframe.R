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
REDO_MODELS = F

# STATE LEVEL -------------------------------------------------------------
message("Wrangling data")

# read in crosswalk
state_region_cw <- read_csv("data/state/state_region_crosswalk.csv")

# read in post-strat targets
targets <- read_csv("data/mrp/acs_psframe.csv") %>% 
  select(-c(region,region_census,region_computed))

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
ces.raw <- read_rds('data/ces_2020/CES20_Common_OUTPUT_vv.rds')

# mutate the vars we need
ces <- ces.raw %>%
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
    past_vote = ifelse(
      is.na(CL_2020gvm), 
      'Non_voter',
      case_when(
        CC20_364a == 1 | CC20_364b == 1 ~ 'Trump',
        CC20_364a == 2 | CC20_364b == 2 ~ 'Biden', 
        CC20_364a == 3 | CC20_364b == 3 ~ 'Other',
        T ~ NA_character_)
    ),
    
    # validation variable
    #voter_validated = ifelse(CL_E2016GVM != '','Y','N'),
    
    ### MISC
    # weight
    weight = as.numeric(commonweight),
    weight_voters = as.numeric(vvweight),
    
    state_name = state_region_cw$state_name[match(inputstate_post,state_region_cw$state_fips)]
    
    
    ### CONVERSIONS
    
  ) %>%
  dplyr::select(state_name,
                sex, age, race, edu, income5,
                past_vote,
                weight,weight_voters
  )  %>%
  filter(!is.na(past_vote))

prop.table(table(is.na(ces.raw$CL_2020gvm)))
prop.table(table(ces$past_vote == 'Non_voter'))

nrow(ces)
nrow(ces) / nrow(ces.raw)

ces <- ces %>%
  left_join(state_region_cw)

ces %>%
  #filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n = sum(weight,na.rm=T)) %>%
  mutate(pct = n/sum(n))

ces %>%
  filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n = sum(weight_voters,na.rm=T)) %>%
  mutate(pct = n/sum(n))

# final survey cleanup 
# check size of poll
nrow(ces)

# check to make sure each matching var IS THE SAME in both datasets
unique(ces$age) %in% unique(targets$age)
unique(ces$sex) %in% unique(targets$sex)
unique(ces$race) %in% unique(targets$race)
unique(ces$edu) %in% unique(targets$edu)
unique(ces$past_vote) %in% unique(targets$past_vote)

#some checks
ces.svy <- svydesign(~1,data=ces,weights=~weight)

svymean(~past_vote,ces.svy)
svymean(~past_vote,subset(ces.svy,past_vote != 'Non_voter'))
svymean(~past_vote,subset(ces.svy,past_vote != 'Non_voter'))

ces %>%
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

ces %>%
  group_by(state_name,past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

ces %>%
  group_by(state_name,past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

ces %>%
  group_by(past_vote) %>%
  summarise(n=sum(weight)) %>%
  mutate(prop=n/sum(n))

svymean(~past_vote, 
        subset(ces.svy,state_name=='Minnesota'),
        na.rm=T) 

# sample size by state
sample_size_bystate <- ces %>% 
  group_by(state_name) %>%
  summarise(state_sample_size=n())

ggplot(sample_size_bystate, aes(y=reorder(state_name,state_sample_size),x=state_sample_size)) +
  geom_point()


# ADD COVARS --------------------------------------------------------------
# add the state's region to the individual data from the targets data
targets <- targets %>%
  left_join(state_region_cw %>% 
              dplyr::select(state_fips,state_name, state_abb, region))

ces <- ces %>%
  left_join(state_region_cw %>% 
              dplyr::select(state_name, state_abb, region))

# adding a few other state-level covariates, such as union household and evangelical %
targets <- targets %>%
  left_join(covars)

ces <- ces %>%
  left_join(covars)

# MODEL LIKELY VOTERS -----------------------------------------------------
# voter dummy variable
ces <- ces %>%
  mutate(likely_voter_dummy = round(past_vote != 'Non_voter') ) %>%
  ungroup()

# model formula
turnout_formula = likely_voter_dummy ~ 
  # state-level smoothers
  state_biden_2020 + region_biden_2020 + state_vap_turnout_2016 + state_white_evangel + 
  state_median_income + state_urbanicity +
  # main demographics, global 
  race + edu + race:edu + race:sex + 
  # pooling across demographics
  (1 | sex) + (1 | age) + (1 | race) + (1 | edu) + (1 | income5) +
  (1 | race:edu) + (1 | sex:race) + 
  (1 | region) + (1 | state_name) # +
  # demographics that should vary by geography
  # (1 + sex + age + race + income5 + edu | region/state_name)

# run model!
message("Running 2020 turnout MRP")

if(REDO_MODELS | isFALSE(any(grepl("turnout_2020_model.rds" , list.files("models/"))))){
  turnout_2020_model <- brm(formula = turnout_formula,
                            data = ces %>% sample_n(5000) %>% ungroup(),
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
                            refresh = 10,
                            thin = 1,
                            backend = 'cmdstanr',
                            threads = threading(2))
  
  write_rds(turnout_2020_model,"models/turnout_2020_model.rds",compress = 'gz')
}else{
  turnout_2020_model <- read_rds("models/turnout_2020_model.rds")
}


# post-stratification stage!
message("\tPost-stratifying 2020 turnout onto targets")

# get 1000 draws from posterior predictive -- major party vote
cell_pred_2020_turnout.mat <- rstantools::posterior_epred(
  object = turnout_2020_model,
  newdata = targets,
  ndraws = 250, 
  allow_new_levels = TRUE,
  transform = TRUE
)

# get medain for turnout
cell_pred_2020_turnout <- cell_pred_2020_turnout.mat %>% apply(.,2,median)

# add share that are predicted voters to pop
targets <- targets %>%
  mutate(likely_voter = cell_pred_2020_turnout)

# check
targets %>%
  summarise(likely_voter = sum(likely_voter * n) / sum(n))

svymean(~past_vote, ces.svy)

targets %>%
  group_by(race) %>%
  summarise(likely_voters = sum(likely_voter*n) / sum(n))

targets %>%
  group_by(age) %>%
  summarise(likely_voters = sum(likely_voter*n) / sum(n))


# MODEL 2020 VOTE ---------------------------------------------------------
# multilevel regression stage!
# this happens in two steps:
message("Running 2020 biden/trump/other vote MRP")

# model formula
vote_formula = past_vote  | weights(weight_voters) ~
  # state-level smoothers
  state_biden_2020 + region_biden_2020 + state_vap_turnout_2016 + state_white_evangel + 
  state_median_income + state_urbanicity +
  # main demographics, global 
  race + edu + race:edu + race:sex + 
  # pooling across demographics
  (1 | sex) + (1 | age) + (1 | race) + (1 | edu) + (1 | income5) +
  (1 | race:edu) + (1 | sex:race) + 
  (1 | region) + (1 | state_name) # +
  # demographics that should vary by geography
  # (1 + sex + age + race + income5 + edu | region/state_name) + race + s(state_biden_2020) 


if(REDO_MODELS | isFALSE(any(grepl("pres_2020_model.rds" , list.files("models/"))))){
  pres_2020_model <- brm(formula = vote_formula,
                         data = ces[ces$past_vote != 'Non_voter',] %>% sample_n(2000) %>% ungroup(),
                         family = categorical(link='logit', refcat = 'Other'),
                         # priors
                         prior = c(set_prior("normal(0, 1)", class = "Intercept",dpar='muBiden'),
                                   set_prior("normal(0, 1)", class = "Intercept",dpar='muTrump'),
                                   set_prior("normal(0, 1)", class = "b",dpar='muBiden'),
                                   set_prior("normal(0, 1)", class = "b",dpar='muTrump'),
                                   set_prior("normal(0, 1)", class = "sd",dpar = 'muBiden'),
                                   set_prior("normal(0, 1)", class = "sd",dpar = 'muTrump')),
                         # settings
                         iter = 1000,
                         warmup = 500,
                         chains = 4,
                         cores = 4,
                         control = list(adapt_delta = 0.9, max_treedepth = 12),
                         refresh = 10,
                         thin = 1,
                         backend = 'cmdstanr',
                         threads = threading(2))
  
  write_rds(pres_2020_model,"models/pres_2020_model.rds",compress = 'gz')
}else{
  pres_2020_model <- read_rds("models/pres_2020_model.rds")
}

# post-stratification stage!
message("\tPost-stratifying 2020 vote onto targets")

# get 1000 draws from posterior predictive 
cell_pred_2020_vote.mat <- rstantools::posterior_epred(
  object = pres_2020_model,
  newdata = targets,
  ndraws = 250, 
  allow_new_levels = TRUE,
  transform = TRUE
)

# get medain for major party vote
cell_pred_2020_dem <- cell_pred_2020_vote.mat[,,1] %>% apply(.,2,median)
cell_pred_2020_rep <- cell_pred_2020_vote.mat[,,3] %>% apply(.,2,median)
cell_pred_2020_other <- cell_pred_2020_vote.mat[,,2] %>% apply(.,2,median)


# make vars on targets frame
targets <- targets %>%
  mutate(pres_2020_dem = cell_pred_2020_dem,
         pres_2020_rep = cell_pred_2020_rep,
         pres_2020_other = cell_pred_2020_other)

# check
targets %>%
  mutate(n = n * likely_voter) %>%
  summarise(dem = sum(pres_2020_dem * n) / sum(n),
            rep = sum(pres_2020_rep * n) / sum(n),
            other = sum(pres_2020_other * n) / sum(n),
  ) 

svymean(~past_vote,subset(ces.svy,past_vote!='Non_voter'),na.rm=T)

targets %>%
  mutate(n * likely_voter) %>%
  group_by(state_name) %>%
  summarise(dem = sum(pres_2020_dem * n) / sum(n),
            rep = sum(pres_2020_rep * n) / sum(n),
            other = sum(pres_2020_other * n) / sum(n),
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
  rename(past_vote = vote_type)

unique(targets$past_vote)
sum(targets$n)

# check topline results
targets %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(past_vote) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

# check results by state
targets %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(state_abb, past_vote) %>%
  summarise(mrp_biden_2020 = sum(n)) %>%
  mutate(mrp_biden_2020 = mrp_biden_2020/sum(mrp_biden_2020)) %>%
  dplyr::filter(past_vote == 'Biden') %>%
  left_join(as.data.frame(unscale_variables(covars,scale_unscale_vars))) %>%
  ggplot(.,aes(mrp_biden_2020,state_biden_2020)) +
  geom_point() +
  geom_abline()


# look at results by race
targets %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(race,past_vote) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))


# look at results by edu
targets %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(college  = edu %in% c('College','Post-grad'),past_vote) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n))

# look at results by race and college
targets %>% 
  dplyr::filter(past_vote != 'Non_voter') %>%
  group_by(race,college  = edu %in% c('College','Post-grad'),past_vote) %>%
  summarise(n = sum(n)) %>%
  mutate(n=n/sum(n)) %>%
  spread(past_vote,n) %>%
  mutate(biden_margin = Biden - Trump) %>%
  select(-c(Biden,Other,Trump)) 


# SAVE! -------------------------------------------------------------------
message("Saving data")

# save the predictions
targets %>%
  dplyr::select(state_fips, sex, age, race, edu, income5, past_vote, n) %>%
  write_rds(.,'data/mrp/acs_psframe_with_2020vote.rds',compress = 'gz')


gc()


# what is hispanic vote in florida
targets %>%
  left_join(state_region_cw) %>%
  filter(state_abb == 'FL',
         past_vote != 'Non_voter') %>%
  group_by(race, past_vote) %>%
  summarise(n = sum(n)) %>%
  mutate(pct = n / sum(n))
