options(scipen = 999)


default_model_formula_no_vote = 
  "
  # state-level smoothers
  s(state_biden_2020) +  s(state_urbanicity) + s(state_white_evangel) + 
  state_median_income + region_biden_2020 +
  # main demographics, global 
  race + edu + race:edu +
  # demographics that should vary by geography
  (1 + sex + age + race + edu + income5 + race:sex + race:edu | region/state_name) 
"

default_model_formula_vote = 
  "
  # state-level smoothers
  s(state_biden_2020) +  s(state_urbanicity) + s(state_white_evangel) + 
  state_median_income + region_biden_2020 +
  # main demographics, global 
  race + edu + race:edu + past_vote +
  # demographics that should vary by geography
  (1 + sex + age + race + edu + income5 + race:sex + race:edu + past_vote | region/state_name) 
"

# misc --------------------------------------------------------------------
# logits
invlogit <- function(x) {
  return(plogis(x))
}

logit <- function(x) {
  return(qlogis(x))
}

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



# 1d corrections ----------------------------------------------------------
calc_delta_correction <- function(delta, data_v, weights, x0) {
  abs(x0-sum(invlogit(logit(data_v) + delta)*weights))
}

weighted_correction <- function(data_v, weights, x0) {
  # get the invlogit delta between sum probs and desired state %
  delta <- stats::optimize(calc_delta_correction, interval=c(-5,5), data_v, weights, x0)$minimum
  
  # make corrections
  corrected <- invlogit(logit(data_v) + delta)
  
  # return
  return(list(delta=delta, corrected=corrected))
}

# re-weights a post-strat frame to state outcomes
correct_probs <- function(pstrat, weighting, cell_prob, outcome, state_factor) {
  # vector to store correct probabilities
  correct_prob = rep(NA,nrow(pstrat))
  
  # for each state
  for (st in as.character(unique(pstrat$state_name))) {
    print(st)
    # selection vector
    sel_vec = pstrat$state_name == st
    
    # current cell probabilities
    probs = pstrat[sel_vec,cell_prob] %>% data.frame() %>% data.matrix() %>% drop()
    
    # current cell weights
    weights = pstrat[sel_vec,weighting] %>% data.frame() %>% data.matrix() %>% drop()
    
    # actual state outcome
    state_outcome = outcome[as.character(outcome$state_name) == st,state_factor] %>% data.matrix() %>% drop()
    
    # weight to corrected data
    corrected = weighted_correction(data_v = probs, 
                                    weights = weights / sum(weights), 
                                    x0 = state_outcome)$corrected
    
    correct_prob[sel_vec] <- corrected
  }
  return(correct_prob)
}



# extras ------------------------------------------------------------------
# this is the old formula for our models, with global mixed effects and little state variation
"# state-level smoothers
  s(state_biden_2020) +  s(state_urbanicity) + s(state_white_evangel) + 
  state_median_income + region_biden_2020 +
  # main demographics, global 
  race + edu + race:edu + past_vote +
  # pooling across demographics
  (1 | sex) + (1 | age) + (1 | race) + (1 | edu) + (1 | income5) + (1 | past_vote)
  (1 | race:edu) + (1 | race:sex) + 
  (1 + race + past_vote | region) + (1 | state_name)
"

