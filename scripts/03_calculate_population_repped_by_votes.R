library(tidyverse)
library(Rvoteview) # remotes::install_github("voteview/Rvoteview")
library(tidycensus) # remotes::install_github("walkerke/tidycensus")

# validate tidycensus
# install with census_api_key(TK,install = TRUE)
# get api key from http://api.census.gov/data/key_signup.html

# state and region cw
state_cw = tibble(state_abb = c(state.abb,'DC'),
                  state_name = c(state.name,'District of Columbia'))

# manchin-toomey 2013 gun control -----------------------
## news article https://abcnews.go.com/blogs/politics/2013/04/senators-crack-impasse-on-gun-background-check
## congressional info: https://www.congress.gov/amendment/113th-congress/senate-amendment/715/text
## vote data: https://voteview.com/rollcall/RS1130097

# get vote
vote = Rvoteview::voteview_download(ids = 'RS1130097')


# get stat pop for that year 
state_pops = tidycensus::get_acs(geography = "state",
                    variables = "B01001_001",
                    year = 2013,
                    survey = 'acs1') %>%
  select(state_name = NAME,  n = estimate) %>%
  left_join(state_cw)

# calculate pop in each bucket
guns_bg = vote$legis.long %>%
  left_join(vote$votes.long) %>% 
  filter(state_abbrev != 'USA') %>% 
  select(name, party_code, state_abb = state_abbrev, vote) %>%
  left_join(state_pops) %>%
  group_by(vote = case_when(vote == 1 ~ 'AYE',
                            vote == 6 ~ 'NAY')) %>%
  summarise(n = sum(n/2)) %>% filter(!is.na(vote))



# 2018 roll call path for dreamers ----------------------------------------
## news article: https://thehill.com/blogs/floor-action/senate/374076-senate-rejects-first-bipartisan-measure-as-immigration-votes-begin/
## congressional vote: https://www.congress.gov/bill/115th-congress/house-bill/2579/all-actions?overview=closed
## vote data: https://voteview.com/rollcall/RS1150358

# get vote
vote = Rvoteview::voteview_download(ids = 'RS1150358')

# get stat pop for that year 
state_pops = tidycensus::get_acs(geography = "state",
                                 variables = "B01001_001",
                                 year = 2018,
                                 survey = 'acs1') %>%
  select(state_name = NAME,  n = estimate) %>%
  left_join(state_cw)

# calculate pop in each bucket
path_to_citizenship_dreamers = vote$legis.long %>%
  left_join(vote$votes.long) %>% 
  filter(state_abbrev != 'USA') %>% 
  select(name, party_code, state_abb = state_abbrev, vote) %>%
  left_join(state_pops) %>%
  group_by(vote = case_when(vote == 1 ~ 'AYE',
                            vote == 6 ~ 'NAY')) %>%
  summarise(n = sum(n/2)) %>% filter(!is.na(vote))



# 2017 senate health care bill (repeal ACA) -------------------------------
## news article: https://www.nytimes.com/2017/07/27/us/politics/obamacare-partial-repeal-senate-republicans-revolt.html
## congressional vote: https://www.congress.gov/bill/115th-congress/house-bill/1628
## vote data: https://voteview.com/rollcall/RS1150179

# get vote
vote = Rvoteview::voteview_download(ids = 'RS1150179')

# get stat pop for that year 
state_pops = tidycensus::get_acs(geography = "state",
                                 variables = "B01001_001",
                                 year = 2017,
                                 survey = 'acs1') %>%
  select(state_name = NAME,  n = estimate) %>%
  left_join(state_cw)

# calculate pop in each bucket
gov_health_subsidies = vote$legis.long %>%
  left_join(vote$votes.long) %>% 
  filter(state_abbrev != 'USA') %>% 
  select(name, party_code, state_abb = state_abbrev, vote) %>%
  left_join(state_pops) %>%
  group_by(vote = case_when(vote == 1 ~ 'NAY', # flipped because bill moves policy in conservative direction
                            vote == 6 ~ 'AYE')) %>%
  summarise(n = sum(n/2))%>% filter(!is.na(vote))



# visualize alongside opinion data ----------------------------------------
pub_opinion = read_csv('output/policy_support_by_different_institutions.csv')

votes_data = guns_bg %>% 
  spread(vote, n) %>%
  mutate(outcome = 'failed',
         policy = 'guns_bg',
         pct = AYE / (AYE + NAY)) %>% 
  bind_rows(
    path_to_citizenship_dreamers %>%
      spread(vote, n) %>%
      mutate(outcome = 'failed',
             policy = 'path_to_citizenship_dreamers',
             pct = AYE / (AYE + NAY)) 
  ) %>%
  bind_rows(
    gov_health_subsidies %>% 
      spread(vote, n) %>%
      mutate(outcome = 'passed',
             policy = 'gov_health_subsidies',
             pct = AYE / (AYE + NAY)) 
  )


# combine
congruence = pub_opinion %>% 
  left_join(votes_data) 

# graph alongside
congruence %>%
  ggplot(., 
       aes(y = fct_reorder(policy,support_natl))) +
  geom_point(aes(x = support_natl, col = 'if all adults count')) +
  geom_point(aes(x = median_senate, col = 'in median senate seat')) +
  geom_point(aes(x = senate_40th_vote_biden, col = 'in 40th senate seat (ranked by biden vote)')) +
  geom_point(aes(x = pct, col = 'pop repped by senators voting aye',shape = outcome)) +
  geom_vline(xintercept = 0.5)  +
  labs(x = 'support for liberal option',
       y = 'policy',
       col = '') +
  theme_minimal() +
  theme(legend.position = 'top') +
  guides('col' = guide_legend(ncol = 1),
         'shape' = guide_legend(ncol = 1))


ggsave('output/fig_support_with_senate_populations.pdf',width = 7, height = 4)
write_csv(congruence, 'output/support_with_senate_populations.csv')

