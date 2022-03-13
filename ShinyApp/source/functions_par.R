# get_t_input -------------------------------------------------------------
# creates the default input vector of allocations
get_t_input <- function(ngroups){
  t_vec <- round( c(0.5, rep(0.5/((ngroups-1)), (ngroups-1)) ), 4)
  return(t_vec)
}

# get_t_mat_all ---------------------------------------------------------------
# creates a matrix that contains all combinations of allocation rates
get_t_mat_all <- function(ngroups){
  myseq <- seq(0.1,0.9,.1)
  t_mat <- rep(list(myseq), ngroups) %>%
    setNames(paste0("t", 1:ngroups)) %>%
    cross_df() %>%
    as.matrix()
  t_mat <- t_mat[which(rowSums(t_mat)==1),]
  return(t_mat)
}

# get_pow_het -------------------------------------------------------------
# simulate the power in the heteroscedastic case
get_pow_het <- function(s = 10000, t, N, mu, sigma, C) {
  a <- ncol(C)
  n <- as.numeric(round(t * N))
  N <- sum(n)
  erg <- numeric(s)
  for (i in 1:s) {
    # generate data under H1
    x <- lapply(1:a, function(j) rnorm(n = n[j], mean = mu[j], sd = sigma[j])) 
    mx <- sapply(x,mean)
    # calculate the test statistic
    Cm <- C %*% mx
    varx <- sapply(x, var)
    Sigmahat <-  diag(varx/n) 
    CShat <- C %*% Sigmahat %*% t(C)
    T <- Cm / sqrt(diag(CShat))
    T0 <- max(abs(T))
    nul <- sapply(1:nrow(C), function(arg){
      c( t(C[arg,])%*%Sigmahat%*%C[arg,] )^2/ sum( C[arg,]^4*varx^2/(n^2*(n-1)) )
    })
    pvalue <- 1 - pmvt(lower = -T0, upper = T0, delta = rep(0, nrow(C)),
                       sigma = cov2cor(CShat), df = round(min(nul)))[1]
    erg[i] <- (pvalue < 0.05)
  }
  return(list(t = t, N = N, power = mean(erg)))
}

# get_diff_pow_het --------------------------------------------------------
# get the difference between the power and the intended power for heteroscedastic variances
get_diff_pow_het <- function(N, t, delta, sigma, C, sig_level = 0.05, power_inp = 0.8){
  n <- round(t*N)
  a <- length(t)
  if( (round(N)-a) <= 0 ){
    stop("There can't be more groups than total observations.")
  }else{
    Sigma <- diag(sigma^2/n)
    CS <- C%*%Sigma%*%t(C)
    R <- cov2cor(CS)
    ncp <- delta/sqrt(diag(CS))
    nul <- sapply(1:nrow(C), function(arg){
      c( t(C[arg,])%*%Sigma%*%C[arg,] )^2/ sum( C[arg,]^4*sigma^4/(n^2*(n-1)) )
    })
    df <- round(min(nul))#+1
    crit1 <- qmvt(1-sig_level, corr = R, tail = "both", df = df)$quantile
    diff <- (1 - pmvt(lower = -crit1, upper = crit1, delta = c(ncp), corr = R, df = df)[1] - power_inp)
    return(diff)
  }
}

# SingleSimHet_tfix -------------------------------------------------------------
# finds the optimal N and calculates the power based on N for one simulation run, given t
SingleSimHet_tfix <- function(mu, sigma, C, t, power_inp, seed){
  set.seed(seed)
  a <- length(t)
  delta <- C%*%mu
  res <- tryCatch({
    # find the optimal N given a fixed t
    N_opt <- uniroot(f = get_diff_pow_het, interval = c(a*8,10000), t = t, # n_i <=8, n_i>=10^4 are excluded
                     delta = delta, sigma = sigma, C = C, power = power_inp, extendInt = "no")$root
    # calculate the power based on the optimal N
     get_pow_het(t = t, N = N_opt, mu = mu, sigma = sigma, C = C)
  }, error = function(e){
    message(paste0("These input parameters lead to a sample size smaller than ", a*8,
                   " or larger than 1000, which is not recommended."))
    return(data.frame(t = t, N = NA, power = NA) )
  })
  n <- res$t * res$N
  # calculate the effect estimator
  Sigma <- diag(sigma^2/n) 
  Gammahat <- C%*%Sigma%*%t(C)
  est <- delta/sqrt(diag(Gammahat))
  return(data.frame(Contrast = attributes(C)$type,
                    mu = paste0("(", paste(round(mu, digits=4), collapse = ", "),")"), 
                    sigma = paste0("(", paste(round(sigma, digits=4), collapse = ", "),")"),
                    t = paste0("(", paste(res$t, collapse = ", "), ")"),
                    delta = paste0("(", paste(round(est, digits = 4), collapse = ", "), ")"),
                    N = res$N, n = paste0("(", paste(round(n), collapse = ", "), ")"), Power = res$power
  ))
}

# SingleSimHet_Nfix -------------------------------------------------------------
# finds the optimal t and calculates the power based on t for one simulation run, given N
SingleSimHet_Nfix <- function(mu, sigma, C, N, power_inp, seed){
  set.seed(seed)
  a <- length(mu)
  delta <- C%*%mu
  res <- tryCatch({
    t_mat <- get_t_mat_all(ngroups = length(mu))
    # grid search
    res <- apply(t_mat, 1, function(t){
      get_diff_pow_het(t = t, N = N, delta = delta, sigma = sigma, C = C, power_inp = power_inp)
    })
    t_opt <- t_mat[which.min(abs(res)), ]
    # calculate the power based on the optimal t
    get_pow_het(t = t_opt, N = N, mu = mu, sigma = sigma, C = C)
  }, error = function(e){
    message(paste0("These input parameters lead to a sample size smaller than ", a*8,
                   " or larger than 1000, which is not recommended."))
    return(data.frame(t = NA,
                      N = NA, power = NA) )
  })
  n <- res$t * res$N
  # calculate the effect estimator
  Sigma <- diag(sigma^2/n) 
  Gammahat <- C%*%Sigma%*%t(C)
  est <- delta/sqrt(diag(Gammahat))
  return(data.frame(Contrast = attributes(C)$type,
                    mu = paste0("(", paste(round(mu, digits=4), collapse = ", "),")"), 
                    sigma = paste0("(", paste(round(sigma, digits=4), collapse = ", "),")"),
                    t = paste0("(", paste(res$t, collapse = ", "), ")"),
                    delta = paste0("(", paste(round(est, digits = 4), collapse = ", "), ")"),
                    N = res$N, n = paste0("(", paste(round(n), collapse = ", "), ")"), Power = res$power
  ))
}
  
# get_pow_hom -------------------------------------------------------------
# simulate the power in the homogeneous case
get_pow_hom <- function(nsim = 10000, t, N, mu, sigma, C) {
  a <- ncol(C)
  n <- as.numeric(round(t * N))
  N <- sum(n)
  R <- cov2cor(C %*% t(C))
  erg <- numeric(nsim)
  for (i in 1:nsim) { 
    x <- lapply(1:a, function(i) rnorm(n[i], mu[i], sigma))
    mx <- sapply(x, mean)
    Cm <- C %*% mx
    # pooled variance estimator
    vx <-  1 / (N - a) * sum( unlist(sapply(1:a, function(i) (x[[i]]-mx[i])^2) ))
    Sigmahat <- 1 / n * diag(a) * vx
    CShat <- C %*% Sigmahat %*% t(C)
    T <- Cm / sqrt(diag(CShat))
    T0 <- max(abs(T))
    pvalue <- 1 - pmvt(-abs(T0), abs(T0), delta = rep(0, nrow(C)), sigma = R, df = round(N - a))[1]
    erg[i] <- (pvalue < 0.05)
  }
  # calculate the effect estimator
  Sigma <- 1/N*diag(a)*sigma^2
  Gammahat <- C%*%Sigma%*%t(C)
  est <- (C%*%mu)/sqrt(diag(Gammahat))
  return(list(t = t, N = N, power = mean(erg), est=est))
}

# get_diff_pow_hom --------------------------------------------------------
# get the difference between the power and the intended power for homoscedastic variances
get_diff_pow_hom <- function(t, N, delta, sigma = 1, C, sig_level = 0.05, power_inp = 0.8){
  a <- ncol(C)
  n <- t*N
  df <- round(N)-a
  if( df <= 0 ){
    stop("There can't be more groups than total observations.") 
  }else{
    crit1 <- qmvt(1-sig_level, corr = cov2cor(C%*%t(C)), tail = "both", df = df)$quantile
    Sigma <- 1/n*diag(a)*sigma^2
    CS <- C%*%Sigma%*%t(C)
    R <- cov2cor(CS)
    ncp <- delta/sqrt(diag(CS))
    pmvt(-crit1, crit1, delta = c(ncp), corr = R, df = df)[1]
    diff <- (1-pmvt(-crit1, crit1, delta = c(ncp), corr = R, df = df)[1]-power_inp) #abs
    return(diff)
  }
}

# SingleSimHom_tfix -------------------------------------------------------------
# finds the optimal N and calculates the power based on N for one simulation run, given t
SingleSimHom_tfix <- function(mu, sigma, C, t, power_inp, seed){
  set.seed(seed) 
  a <- length(t)
  delta <- C%*%mu
  res <- tryCatch({
    # find the optimal N given a fixed t
    N_opt <- uniroot(f = get_diff_pow_hom, interval = c(a*8,1000), t = t,  # n_i <=8, n_i>=10^4 excluded
                     delta = delta, sigma = sigma, C = C, power_inp = power_inp, extendInt = "no")$root
    # calculate the power based on the optimal N
    get_pow_hom(t = t, N = N_opt, mu = mu, sigma = sigma, C = C)
  }, error = function(e){
    message(paste0("These input parameters lead to a sample size smaller than ", a*8,
                   " or larger than 1000, which is not recommended."))
    return(data.frame(t = t, N = NA, power = NA, est=NA) )
  })
  return(data.frame(Contrast = attributes(C)$type,
                    mu = paste0("(", paste(round(mu, digits=4), collapse = ", "),")"),
                    t = paste0("(", paste(res$t, collapse = ", "), ")"),
                    delta = paste0("(", paste(round(res$est, digits = 4), collapse = ", "), ")"),
                    N = res$N, n = paste0("(", paste(round(res$t * res$N), collapse = ", "), ")"), Power = res$power))
}

# SingleSimHom_Nfix -------------------------------------------------------------
# finds the optimal t via grid search and calculates the power based on t for one simulation run, given N
SingleSimHom_Nfix <- function(mu, sigma, C, N, power_inp, seed){
  set.seed(seed)
  a <- length(mu)
  delta <- C%*%mu
  res <- tryCatch({
    t_mat <- get_t_mat_all(ngroups = length(mu))
    # find the optimal t given a fixed N - grid search
    res <- apply(t_mat, 1, function(t){
      get_diff_pow_hom(t = t, N = N, delta = delta, sigma = sigma, C = C, power_inp = power_inp)
    })
    t_opt <- t_mat[which.min(abs(res)), ]
    # calculate the power based on the optimal t
    get_pow_hom(t = t_opt, N = N, mu = mu, sigma = sigma, C = C)
  }, error = function(e){
    message(paste0("These input parameters lead to a sample size smaller than ", a*8,
                   " or larger than 1000, which is not recommended."))
    return(data.frame(t = NA, N = N, power = NA, ests = NA))
  })
  return(data.frame(Contrast = attributes(C)$type,
                    mu = paste0("(", paste(round(mu, digits=4), collapse = ", "),")"),
                    t = paste0("(", paste(res$t, collapse = ", "), ")"),
                    delta = paste0("(", paste(round(res$est, digits = 4), collapse = ", "), ")"),
                    N = res$N, n = paste0("(", paste(round(res$t * res$N), collapse = ", "), ")"), Power = res$power))

}

