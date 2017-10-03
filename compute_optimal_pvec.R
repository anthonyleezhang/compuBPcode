library(linprog)

####
# Solver Function
# Takes an x (grid points on unit interval) vector, utility vector and p_0 (prior) vector as input
####

compute_optimal_pvec = function(x, p0, Uvec, solve_dual = FALSE, verbose = 0) {
  
  len = length(p0)
  
  ### Building constraint matrix Amat, constraint values bvec, and constraint signs in constdir
  Amat = matrix(nrow = 2 * len + 2, ncol = len)
  bvec = numeric(2 * len + 2)
  constdir = character(2 * len + 2)
  
  ### hacky - mean equality constraint
  Amat[1,] = x
  bvec[1] = x %*% data[, p0]
  constdir[1] = ">="
  
  ### Convex dominance constraints
  conv_dom_mat = matrix(nrow = len - 1, ncol = len)
  
  for(i in 1:(len-1)) {
    # This defines a matrix whose jth row is (xi - xj)+ 
    conv_dom_mat[i, ] = pmax(data[, x] - data[i, x], 0)
  }
  
  Amat[2:len,] = conv_dom_mat
  
  # Constraint values come from the prior, so the product of the conv_dom_mat with p0
  trunc_expectations = conv_dom_mat %*% data[, p0]
  bvec[2:len] = trunc_expectations
  
  # Constraint is that the signal distr is dominated by, <=, prior
  constdir[2:len] = "<="
  
  ### Positivity constraints on probabilities
  Amat[(len+1):(2*len),] = diag(1, len)
  bvec[(len+1):(2*len)] = rep(0, len)
  constdir[(len+1):(2*len)] = ">="
  
  ### Probability sum to 1 constraint
  # Hacky - solver seems to work better with two inequality constraints than a single equality constraint
  Amat[2 * len + 1,] = rep(1, len)
  bvec[2 * len + 1] = 1
  Amat[2 * len + 2,] = rep(1, len)
  bvec[2 * len + 2] = 1
  constdir[2 * len + 1] = ">="
  constdir[2 * len + 2] = "<="
  
  ## The objective function cvec is just the utility vector
  cvec = Uvec
  
  ##### SOLVING THE LP
  out = solveLP(cvec = cvec, bvec = bvec, Amat = Amat, maximum = TRUE, const.dir = constdir, solve.dual = solve_dual, verbose = verbose)
  return(out)
}
