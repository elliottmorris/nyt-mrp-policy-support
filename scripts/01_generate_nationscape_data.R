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

files = list.files(list.files('Nationscape-Weekly-Materials-DTA-2021Dec/phase_3_v20210301/',
                                full.names=T),full.names = T)

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
# presidential vote: vote_2020 or vote_2020_retro
# cap on carbon? cap_carbon
ns = dat %>% 
  mutate(
    # demos
    pid_lean = case_when(grepl('Democrat',pid7) ~ 'Democrat',
                         grepl('Republican',pid7) ~ 'Republican',
                         pid7 == 'Independent' ~ 'Independent'),
    
    # vote before election and after
    vote_2020 = case_when(is.na(vote_2020) ~ as.character(vote_2020_retro), 
                          T ~ as.character(vote_2020)),
    
    vote_2020 = case_when(vote_2020 == 'Donald Trump' ~ 'Trump',
                          vote_2020 == 'Joe Biden' ~ 'Biden',
                          vote_2020 %in% c('Someone else','Someone else:') ~ 'Other',
                          vote_2020 %in% c("I would not vote","I am not sure/don't know",
                                          "I abstained","I don't recall") ~ "Non-voter"),
    
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
  select(weight, 
         # demos
         state_abb = state, pid_lean, 
         # vote
         vote_2020,
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
  filter(!is.na(vote_2020),
         !is.na(pid_lean),
         state_abb != '')


# re-weight data to match results of 2020 election -------------------------
# state and region cw
state_cw = tibble(state_abb = c(state.abb,'DC'),
                  state_name = c(state.name,'District of Columbia'))


# weight polls to be representative at state-level
# get pop numbers
state_results_2020 <- read_csv('state/results_2020.csv') %>%
  mutate(Biden = biden/total_votes,
         Trump = trump/total_votes,
         turnout_pct = total_votes / vep_pop) %>%
  dplyr::select(state_abb, Biden, Trump, turnout_pct, n = vep_pop) 

weighted.mean(state_results_2020$turnout_pct,state_results_2020$n)

# create "Other" and non-voter number, too
state_targets_pastvote <- state_results_2020 %>%
  mutate(Other = 1 - (Biden + Trump),
         Non_voter = 1 - turnout_pct,
         Biden = Biden * turnout_pct,
         Trump = Trump * turnout_pct,
         Other = Other * turnout_pct) %>%
  mutate(Biden = Biden * n,
         Trump = Trump * n,
         Other = Other *n,
         Non_voter = Non_voter * n) %>%
  dplyr::select(state_abb, Biden, Trump, Other, Non_voter) %>%
  gather(vote_2020,Freq,2:5) %>%
  mutate(Freq = Freq/sum(Freq)) %>%
  ungroup() 

weighted.mean(state_targets_pastvote$vote_2020 == 'Non_voter',state_targets_pastvote$Freq)

# can't have a weight of 0, so make v small
state_targets_pastvote <- state_targets_pastvote %>%
  mutate(Freq = ifelse(Freq==0,0.00000001,Freq))

state_targets_pastvote <- state_targets_pastvote %>%
  mutate(Freq = Freq*nrow(ns))  

# make sure weighting vars are same levels
state_targets_pastvote <- state_targets_pastvote %>%
  mutate(vote_2020 = factor(as.character(vote_2020),levels=c('Biden','Trump','Other','Non_voter')))

ns <- ns %>%
  mutate(vote_2020 = ifelse(vote_2020 == 'Non-voter','Non_voter',as.character(vote_2020)),
         vote_2020 = factor(as.character(vote_2020),levels=c('Biden','Trump','Other','Non_voter')))


# create survey object stratified by state
ns.svy <- svydesign(ids = ~1,
                    strata = ~state_abb,
                    data = ns,
                    weights = ~weight) # start with current weights

# rake weights based on past vote
ns.svy.raked <- postStratify(design = ns.svy,
                                 strata = ~vote_2020+state_abb,
                                 population = state_targets_pastvote %>% group_by(vote_2020,state_abb) %>% summarise(Freq = sum(Freq)), 
                                 partial = T)

# TRIM WEIGHTS? 
ns.svy.raked <- trimWeights(design=ns.svy.raked,lower=0.001,upper=12,strict=T)

# extract weights
new_weights <- attr(ns.svy.raked$postStrata[[1]],'weights')
new_weights <- weights(ns.svy.raked)

# make new DF with old weights and new data, only for states with full strata
weighted_poll <- ns.svy.raked$variables %>%
  mutate(weight = new_weights)

# check biden pct
weighted_poll %>% 
  dplyr::filter(vote_2020 != 'Non_voter') %>%
  group_by(state_abb,vote_2020) %>%
  summarise(prop=sum(weight)) %>%
  mutate(prop = prop/sum(prop)) %>%
  spread(vote_2020,prop) %>%
  mutate(biden_margin.yg = Biden - Trump) %>%
  dplyr::select(state_abb,
                biden_margin.yg) %>%
  # add actual results
  left_join(state_results_2020 %>% mutate(biden_margin.actual = Biden - Trump)) %>%
  # chart
  ggplot(.,aes(x=biden_margin.yg, y=biden_margin.actual, label=state_abb)) +
  geom_abline() +
  geom_text() +
  stat_smooth(method='lm') +
  coord_cartesian(xlim=c(-0.5,0.5),ylim=c(-0.5,0.5)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# check non-voter pct
weighted_poll %>% 
  group_by(state_abb,vote_2020) %>%
  summarise(prop=sum(weight)) %>%
  mutate(prop = prop/sum(prop)) %>%
  spread(vote_2020,prop) %>%
  # add actual results
  left_join(state_results_2020 %>% select(state_abb, turnout_pct)) %>%
  # chart
  ggplot(.,aes(x=Non_voter, y=1-turnout_pct, label=state_abb)) +
  geom_abline() +
  geom_text() +
  stat_smooth(method='lm') 

# save newly weighted poll over the ns object
# and add back the rows from the unpopulated cells
ns <- weighted_poll %>%
  bind_rows(ns) %>%
  mutate(vote_2020 = ifelse(vote_2020 == 'Non-voter','Non_voter',
                               as.character(vote_2020))) %>%
  as.data.frame()


# write -------------------------------------------------------------------
write_rds(ns,'nationscape.rds',compress='gz')

# also generate census data -----------------------------------------------
# # codes to add up: AM0NE015
acs = read_csv('state/nhgis0030_ds250_20205_state.csv') %>%
  select(state_name = STATE,
         state_abb = STUSAB,
         n = AM0NE001)

write_csv(acs, 'state/state_populations.csv')

