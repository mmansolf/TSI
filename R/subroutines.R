prepare_mem_param <- function(calibdata, wdata, n) {
  # bring in calibration parameters
  ratio = c(calibdata$reliability)
  var_os = c(calibdata$var_os)
  var_ts = c(calibdata$var_ts)
  mean_os = c(calibdata$mean_os)
  mean_ts = c(calibdata$mean_ts)
  # calculate slope
  beta1 = sqrt(ratio * var_ts / var_os)
  # calculate residual variance
  sigmasq = (1 - ratio) * var_ts
  # calculate intercept
  beta0 = mean_ts - mean_os * beta1

  # Draw mux from normal distribution
  tmp = rnorm(1)
  mean_os_obs = mean_os + sqrt(mean(var_os) / length(wdata)) * tmp
  # Draw sigmaxsq from inverse chi-square
  # distribution with df = (n-1) <- originally 2?
  var_os_obs = var_os * (length(wdata) - 1) / rchisq(1, length(wdata) - 1)

  # define parameter structure to return
  param = data.frame(beta0 = beta0, beta1 = beta1,
                     sigmasq = sigmasq, mean = mean_os_obs,
                     var_os = var_os_obs)

  # shrink it
  param_shrunk = unique(param)
  param = as.matrix(param)
  for (i in seq_len(nrow(param_shrunk))) {
    rownames(param_shrunk)[i] =
      paste(which(rowSums(
        param != matrix(param_shrunk[i, ], nrow(param), 5, byrow = T)) == 0),
        collapse = ",")
  }

  # return it; we're done
  return(param_shrunk)
}
# Draw parameters from their predictive distribution
# based on the main study data.
gen_multivar_regre_param <- function(maindata, n, p) {
  maindata = as.matrix(na.omit(as.data.frame(maindata)))
  n = nrow(maindata)
  ndraw = 1
  w = maindata[, 1]
  wmat = mat.or.vec(n, 2)
  wmat[, 1] = 1
  wmat[, 2] = w
  umat = maindata[, 2:(p + 1)]
  ww = solve(t(wmat) %*% wmat)
  coeffhat = ww %*% (t(wmat) %*% umat)
  residual = umat - wmat %*% coeffhat
  rss = t(residual) %*% residual
  # Draw covariance of U given W from inverse wishart distribution with
  # df = (n-(k+p)+1) where k is dimension of wmat and p
  # is dimension of umat
  df = n - (p + 2) + 1
  covmatrix = MCMCpack::riwish(df, rss)
  covmatrix = as.matrix(covmatrix)
  betavar = kronecker(covmatrix, ww)
  vec_coeffhat = as.vector(coeffhat)
  beta = MASS::mvrnorm(ndraw, vec_coeffhat, betavar)
  # regression coefficients of U on W
  multivar_reg_coeff = matrix(beta, 2, p)
  # residual covariance matrix of U given W
  multivar_resid_cov = covmatrix
  # get return params
  param = rbind(multivar_reg_coeff, multivar_resid_cov)
  # return 'em
  return(param)
}

# 3-D array edition
create_matrix_on_w <- function(mem_param, dm_param, p) {
  # mem_param is 3 vectors of length length(y)
  m = p + 2 + 1
  matrix_on_w = matrix(NA, nrow = m, ncol = m)
  matrix_on_w[1:(m - 1), 3:(m - 1)] = dm_param
  # pre-allocate 3-D array
  array_on_w = array(matrix_on_w, dim = c(dim(matrix_on_w), nrow(mem_param)))
  array_on_w[1, 1, ] = (1 + mem_param$mean^2 / mem_param$var_os)
  array_on_w[1, 2, ] = mem_param$mean / mem_param$var_os
  array_on_w[2, 2, ] = -1 / mem_param$var_os
  for (i in 1:2) array_on_w[i, m, ] = unlist(mem_param[, i])
  array_on_w[m, m, ] = mem_param[, 3]
  for (i in 3:(m - 1)) array_on_w[i, m, ] =
    dm_param[2, i - 2] * mem_param[, 3] / mem_param[, 2]
  dimnames(array_on_w) = list(NULL, NULL, rownames(mem_param))
  return(array_on_w)
}

# complete initinal mean and covariance matrix \\
complete_g_matrix <- function(g) {
  size = dim(g)[1]
  for (i in 1:size) {
    for (j in 1:size) {
      if (any(is.na(g[i, j, ]))) {
        if (any(is.na(g[j, i, ]) == FALSE)) {
          g[i, j, ] = g[j, i, ]
        } else {
          print(sprintf("ERROR: both elements at [%d,%d]
and [%d,%d] are null", i, j, j, i))
        }
      }
    }
  }
  g = (g + aperm(g, c(2, 1, 3))) / 2
  g
}

# perform the sweep operator \\
sweep <- function(matrix_on_w) {
  size = dim(matrix_on_w)[1]
  cur_h = complete_g_matrix(matrix_on_w)
  new_h = NA * matrix_on_w
  for (l in 3:(size - 1)) {
    for (i in 1:size) {
      for (j in 1:size) {
        if (i == l && j == l) {
          new_h[i, j, ] = -1 / cur_h[l, l, ]
        } else if (i == l) {
          new_h[i, j, ] = cur_h[i, j, ] / cur_h[i, i, ]
        } else if (j == l) {
          new_h[i, j, ] = cur_h[i, j, ] / cur_h[j, j, ]
        } else {
          new_h[i, j, ] = cur_h[i, j, ] -
            cur_h[i, l, ] * cur_h[l, j, ] / cur_h[l, l, ]
        }
      }
    }
    cur_h = new_h
    new_h = NA * matrix_on_w
  }
  param = t(cur_h[, size, ])
  rownames(param) = dimnames(matrix_on_w)[[3]]
  return(param)
}

# Create imputed values for unobserved covariate X
generate_missing_value <- function(param, maindata) {
  nsample = nrow(maindata)
  nparam = ncol(param)
  rowsets = rownames(param)
  rownames(param) = NULL
  # test whether the estimate of the residual variance
  # is negative, and display a warning message
  if (any(param[, nparam] < 0)) {
    percent_neg_vars = round(mean(param[, nparam] < 0), 3) * 100
    if(percent_neg_vars == '0') percent_neg_vars = '<0.1'
    print(sprintf("%s", paste0(
      "Estimated residual variance of mismeasured covariate is negative for ",
      percent_neg_vars,
      "% of cases; these estimates were set to zero for imputation.")))
    param[param[, nparam] < 0, nparam] = 0
  }
  # fill in matrix to collapse
  matrix_to_collapse = matrix(NA, nsample, nparam)
  for (iset in seq_len(nrow(param))) {
    rows_iset = as.numeric(unlist(strsplit(rowsets[iset], ",", fixed = T)))
    matrix_to_collapse[rows_iset, ] = matrix(param[iset, 1:(nparam)],
                                           length(rows_iset), nparam,
                                           byrow = T)
  }
  # matrix operations
  matrix_to_collapse[, 2:(nparam - 1)] =
    matrix_to_collapse[, 2:(nparam - 1)] * maindata
  imputed_x = rowSums(matrix_to_collapse[, -nparam]) +
    sqrt(matrix_to_collapse[, nparam]) * rnorm(nsample, mean = 0, sd = 1)
  # return it
  return(imputed_x)
}

# main function \\
miec <- function(maindata, calibdata, ncalib, nsample, m, n, k, s) {
  two_stage_mi_imputed_x = c()
  p = k + s
  count = 0
  while (count < m) {
    # Step-1: draw parameters by regressing X on W
    #         based on measurement error model
    mem_param = prepare_mem_param(calibdata, maindata[, 1], ncalib)
    # Step-2: draw parameters by regressing (Y, Z) on W
    #         based on main interested 'disease' model
    dm_param = gen_multivar_regre_param(maindata, nsample, p)
    # Step-4: creating sweeping matrix on W by using parameters
    #         obtained from step 1-3 and filling estimated covariance
    #         parameter between U and X given W
    sweep_matrix_on_w = create_matrix_on_w(mem_param, dm_param, p)
    # Step-5: calculate parameters of the imputation model for X
    #         given (W,Y,Z) by the sweep operator
    impmodel_param = sweep(sweep_matrix_on_w)
    # Step-6: generate random draw for unknown X
    #         from its posterior distribution, given W and Y
    for (n_i in seq_len(n)) {
      second_stage_draw_x = generate_missing_value(impmodel_param, maindata)
      two_stage_mi_imputed_x = cbind(two_stage_mi_imputed_x,
                                     second_stage_draw_x)
    }
    count = count + 1
  }
  # Output data with Y, Z, and multiply imputed X,
  # where maindata[,P+1] = U(Y,Z)
  two_stage_mi_imputed_data = two_stage_mi_imputed_x
  return(two_stage_mi_imputed_data)
}
