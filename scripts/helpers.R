options(scipen = 999)


invlogit <- function(x) {
  return(plogis(x))
}

logit <- function(x) {
  return(qlogis(x))
}

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

# raking code -- should be put in YG library
# internal raking algorithm
rake0 <- function(vars, weight, maxit=100, eps=1e-7) {
  for (iter in 1:maxit) {
    convg <- 0
    for (v in vars) {
      counts <- prop.table(xtabs(weight ~ v$data))
      mult <- v$target / sum(v$target)
      stopifnot(all(counts[mult > 0] > 0))
      convg <- max(convg, sum(abs(counts - mult)))
      ok <- mult > 0
      mult[ok] <- mult[ok] / counts[ok]
      weight <- weight * mult[ as.integer(v$data) ]
    }
    if(iter > 99) {
      print(convg)
      print(counts)
      print(mult)
    }
    if (convg < eps & all(sapply(vars, function(v) sum(abs(prop.table(
      xtabs(weight ~ v$data)) - prop.table(v$target))) < eps)))
      return(weight)
  }
  return(NULL)
}
# rake to multidimensional margins
# targets is a list of tables with labeled dimnames
# data is a dataframe containing the dimensions in targets
# weight is an optional non-negative baseweight with length = nrow(data)
# rake returns a numeric vector of weights with the same mean as the baseweight
# (or one if there is no baseweight)
# no missing values are allowed for the weighting variables
rake <- function(data, targets, weight=NULL, maxit=100, eps=1e-7) {
  if (is.null(weight)) weight <- rep(1.0, nrow(data))
  stopifnot(length(weight) == nrow(data) & all(weight >= 0))
  vars <- lapply(targets, function(target) {
    x <- with(data, eval(parse(text=paste(names(dimnames(target)),
                                          collapse=":"))))
    print(target)
    freq <- sapply(strsplit(levels(x), ":"), function(args) do.call("[",
                                                                    c(list(target), as.list(args))))
    names(freq) <- levels(x)
    return(list(data=x, target=freq))
  })
  print(head(weight))
  rake0(vars, weight, maxit=100, eps=1e-7)
}

# re-weights a post-strat frame
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

