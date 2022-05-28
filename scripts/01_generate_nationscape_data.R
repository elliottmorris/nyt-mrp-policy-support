library(tidyverse)
library(readstata13)
library(tidycensus) # remotes::install_github("walkerke/tidycensus")
library(survey)

# read all of the phase 2 & 3 files -------------------------------------------
# liest files in folder
# files = c(list.files(list.files('Nationscape-Weekly-Materials-DTA-2021Dec/phase_3_v20210301/',
#                               full.names=T),full.names = T),
#           list.files(list.files('Nationscape-Weekly-Materials-DTA-2021Dec/phase_2_v20210301//',
#                                 full.names=T),full.names = T))

files = list.files(list.files('data/archive-nationscape/phase_3_v20210301/',full.names=T),full.names = T)

files

# filter to .dta
files = files[grepl('.dta',files)]
files

# read everything and combine into mega-poll
dat = map_df(files, readstata13::read.dta13)
dat = as_tibble(dat)
head(dat)

# remove "Not Asked" from all factor columns
factor_columns = lapply(1:ncol(dat),
                        function(x){
                          (dat %>% pull(x) %>% class)[1]
                        }
) %>% do.call('c',.)

factor_columns = which(factor_columns == 'factor')

dat = dat %>%
  mutate_at(factor_columns,
            function(x){
              case_when(x == 'Not Asked' ~ NA_character_,
                        T ~ as.character(x)
              )
            }
  )


# recode survey questions we're interested in -----------------------------
# need to this converting state abbs to names
state_region_cw <- read_csv("data/state/state_region_crosswalk.csv")


# presidential vote: past_vote or past_vote_retro
# cap on carbon? cap_carbon
ns = dat %>% 
  mutate(
    # demos
    ### mutate demographic variables to match the census
    # race
    race = case_when(hispanic != "Not Hispanic" ~ "Hispanic",
                     race_ethnicity == "White" ~ "White, Non-Hispanic",
                     race_ethnicity == "Black, or African American" ~ "Black, Non-Hispanic",
                     T ~ "Other, Non-Hispanic"),
    # sex
    sex = case_when(gender == "Male" ~ "Male",
                    gender == "Female" ~ "Female",
                    TRUE ~ NA_character_),
    # age
    AGE = age,
    age = case_when(AGE >= 18 & AGE <= 29 ~ "18-29",
                    AGE >= 30 & AGE <= 44 ~ "30-44",
                    AGE >= 45 & AGE <=  64 ~ "45-64",
                    AGE >= 65 ~ "65+",
                    TRUE ~ NA_character_),
    
    # education
    edu = case_when(education %in% c("Middle School - Grades 4 - 8",
                                     "Completed some high school",
                                     "3rd Grade or less") ~ "No HS",
                    education %in% c("High school graduate",
                                     "Other post high school vocational training") ~ "HS grad",
                    education %in% c("Completed some college, but no degree",
                                     "Associate Degree") ~ "Some college",
                    education %in% c("Completed some graduate, but no degree",
                                     "College Degree (such as B.A., B.S.)") ~ 'College',
                    education %in% c("Masters degree",
                                     "Doctorate degree") ~ "Post-grad",
                    TRUE ~ NA_character_),
    
    # income5
    income5 = case_when(household_income %in% c("$25,000 to $29,999",
                                          "Less than $14,999",
                                          "$15,000 to $19,999",
                                          "$35,000 to $39,999",
                                          "$20,000 to $24,999") ~ 'Under $30K',
                        household_income %in% c("$55,000 to $59,999",
                                          "$50,000 to $54,999",
                                          "$40,000 to $44,999",
                                          "$30,000 to $34,999",
                                          "$45,000 to $49,999") ~ "$30-60K",
                        household_income %in% c("$65,000 to $69,999",
                                          "$85,000 to $89,999",
                                          "$70,000 to $74,999",
                                          "$75,000 to $79,999",
                                          "$60,000 to $64,999",
                                          "$90,000 to $94,999",
                                          "$95,000 to $99,999",
                                          "$80,000 to $84,999") ~ '$60-100K',
                        household_income %in% c("$100,000 to $124,999",
                                          "$125,000 to $149,999") ~ "$100-150K",
                        household_income %in% c("$150,000 to $174,999",
                                          "$175,000 to $199,999",
                                          "$250,000 and above",
                                          "$200,000 to $249,999") ~ '$150K or more',
                        TRUE ~ NA_character_
                        
    ),
    
    
    state_name = state_region_cw$state_name[match(state, state_region_cw$state_abb)],
    
    # vote before election and after
    past_vote = case_when(is.na(vote_2020) ~ as.character(vote_2020_retro), 
                          T ~ as.character(vote_2020)),
    
    past_vote = case_when(past_vote == 'Donald Trump' ~ 'Trump',
                          past_vote == 'Joe Biden' ~ 'Biden',
                          past_vote %in% c('Someone else','Someone else:') ~ 'Other',
                          past_vote %in% c("I would not vote","I am not sure/don't know",
                                          "I abstained","I don't recall") ~ "Non_voter"),
    
    # policies
    path_to_citizenship_dreamers = dreamers,
    legal_marijuana = marijuana,
    cap_carbon = cap_carbon,
    guns_bg = guns_bg,
    guns_assault = ban_assault_rifles,
    raise_taxes_600k = raise_upper_tax,
    state_college = college,
    abortion_never_legal = abortion_never,
    abortion_most_time = abortion_conditions,
    paid_maternity_12wk = maternityleave,
    gov_health_subsidies = health_subsidies,
    minimum_wage_15d = minwage
  ) %>%
  select(weight, state_name,
         # demos
         sex, race, age, edu, income5,
         # vote
         past_vote,
         # group favs
         #group_favorability_blm, group_favorability_blacks, group_favorability_whites,
         #group_favorability_muslims, group_favorability_undocumented, group_favorability_lgbt,
         #group_favorability_latinos, group_favorability_jews,
         # policies
         path_to_citizenship_dreamers, legal_marijuana,
         cap_carbon, guns_bg,
         guns_assault, raise_taxes_600k, 
         state_college, abortion_never_legal,
         abortion_most_time, paid_maternity_12wk,
         gov_health_subsidies, minimum_wage_15d) %>%
  filter(!is.na(past_vote),
         state_name != '')

# write -------------------------------------------------------------------
write_rds(ns,'nationscape.rds',compress='gz')
