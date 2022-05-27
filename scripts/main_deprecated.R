library(tidyverse)

# source("generate_dataset.R")
ns = read_rds('nationscape.rds')

# what policies do we have?
names(ns)
# border wall
# cap carbon
# backgroun checks guns
# raise taxes for those making more than 600k
# free state college
# maternity leave
# government run healthcare
# minimum wage

# tabs for all those vars
for(i in 1:ncol(ns)){
  var = names(ns)[i]
  print(' ################# ')
  ns %>%
    mutate(var = ns[[var]]) %>%
    filter(!is.na(var)) %>%
    group_by(vote_2020, var) %>%
    summarise(n = sum(weight)) %>%
    group_by(vote_2020) %>%
    mutate(pct = n / sum(n)) %>%
    set_names(.,c('vote_2020',var,'n','pct')) %>%
    print
  print(' ')
}


# recode everything from 0-1 for testing correlation ----------------------
recode_survey_var = function(col){
  options = unique(col)
  if('Somewhat favorable' %in% options){
    col_y = case_when(col == 'Very ufavorable' ~ 0,
                      col == 'Somewhat unfavorable' ~ 0.33,
                      col == 'Somewhat favorable' ~ 0.66,
                      col == 'Very favorable' ~ 1)
    
    return(col_y)
  }else if('Disagree' %in% options){
    col_y = case_when(col == 'Agree' ~ 1,
                      col == 'Disagree' ~ 0)
    
    return(col_y)
  }else{
    return('Error')
  }
}

recode_survey_var(ns$group_favorability_blm)


ns %>%
  mutate_at(4:ncol(.), recode_survey_var) %>%
  mutate(SUM_group_favs = 
              group_favorability_blm + group_favorability_undocumented + group_favorability_muslims +
           group_favorability_blacks +  group_favorability_lgbt + group_favorability_latinos + 
           group_favorability_jews +
           (-1 * group_favorability_whites )
            )  %>%
  gather(variable,value,4:ncol(.)) %>%
  group_by(variable) %>%
  summarise(cor_with_vote = abs(cor(round(vote_2020 == 'Joe Biden'),value, use = 'complete.obs'))) %>%
  arrange(desc(cor_with_vote))
  

