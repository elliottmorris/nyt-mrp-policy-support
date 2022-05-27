library(tidyverse)

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
# state_abb_college
# abortion_never_legal
# abortion_most_time 
# paid_maternity_12wk
# gov_health_subsidies 
# minimum_wage_15d

# cross tab for each


# get crosstabs for each state_abb
state_tabs = lapply(5:ncol(ns),
       function(i){
         var = names(ns)[i]
         
         tmp = ns %>%
           mutate(var = ns[[var]]) %>%
           filter(!is.na(var),var != "Not Sure") %>%
           group_by(state_abb, var) %>%
           summarise(n = sum(weight)) %>%
           group_by(state_abb) %>%
           mutate(pct = n / sum(n)) %>%
           filter(var == 'Agree') %>%
           select(state_abb,var,pct) 
          
         # negate if opinion is negative
         tmp = tmp %>%
           mutate(pct = ifelse(names(ns)[i] %in% c('abortion_never_legal'),
                               1 - pct, 
                               pct)) 
         
         # spread, rename and return
         tmp = tmp %>%
           spread(var,pct)  %>%
           set_names(., c('state_abb', var))
         
         return(tmp)
         
       }) 

state_tabs = Reduce(function(x, y) merge(x, y, by = "state_abb", all = TRUE), state_tabs)


# add state covariates ----------------------------------------------------
# population 
state_pops = read_csv('state/state_populations.csv')

state_tabs = state_pops %>%
  left_join(state_tabs)

# biden and trump vote
state_tabs = read_csv('state/results_2020.csv') %>%
  select(state_name, state_abb, biden_pct, trump_pct) %>%
  left_join(state_tabs)


support_senate_votes = state_tabs %>%
  gather(policy,pct,6:ncol(.)) %>%
  group_by(policy) %>%
  summarise(support_natl = weighted.mean(pct, n)) %>%
  # now support in median senate seat
  left_join(
    state_tabs %>%
      gather(policy,pct,6:ncol(.)) %>%
      filter(state_abb != 'DC') %>%
      group_by(policy) %>%
      summarise(median_senate = median(pct))
  ) %>%
  # and support in 40th senate seat by size
  left_join(
    state_tabs  %>%
      arrange(n) %>%
      filter(state_abb != 'DC',
             row_number() <= 20) %>%
      gather(policy,pct,6:ncol(.)) %>%
      group_by(policy) %>%
      summarise(senate_40th_vote_pop = weighted.mean(pct, n))
  ) %>%
  # and support in 40th senate seat by Biden 2020 margin
  left_join(
    state_tabs  %>%
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

state_tabs %>%
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
write_csv(state_tabs, 'output/policy_support_by_state.csv')
