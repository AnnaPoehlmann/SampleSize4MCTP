abind.data.frame <- function(...) data.frame(abind(..., along = 3),fix.empty.names = F)

# WriteList ---------------------------------------------------------------
# Writes a list to a .txt file as printed out to console
WriteList <- function (path, list, list.name = NULL) {
  if (is.null(list.name)) list.name <- deparse(quote(list))
  cat(capture.output(print(list), file = paste0(path, "/", list.name, ".txt")))
}

# get_t_mat ---------------------------------------------------------------
# creates a matrix with combinations of allocation rates per group
# 1) balanced design
# 2) 0.4 in the first group, other groups are balanced
get_t_mat <- function(ngroups){
  t_mat <- matrix(data = c(rep(1/ngroups, ngroups),
                           c(0.4, rep(0.6/((ngroups-1)), (ngroups-1)) )),
                  ncol = ngroups, byrow = TRUE) %>% 
    round(digits = 3) 
  return(t_mat)
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
# calculates the power in the heteroscedastic case
get_pow_het <- function(s = 10000, t, N, mu, sigma, C) {
  a <- ncol(C)
  n <- as.numeric(round(t * N))
  N <- sum(n)
  erg <- numeric(s)
  for (i in 1:s) {
    # generate data under H1
    x <- lapply(1:a, function(j) rnorm(n = n[j], mean = mu[j], sd = sigma[j])) 
    mx <- sapply(x,mean)##
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
    pvalue <- 1 - pmvt(-T0, T0, delta = rep(0, nrow(C)),
                       sigma = cov2cor(CShat), df = round(min(nul)))[1]
    erg[i] <- (pvalue < 0.05)
  }
  return(data.frame(t = paste0("(", paste(t, collapse = ", "), ")"), N = N, power = mean(erg)))
}


# get_diff_pow_het --------------------------------------------------------
# get the difference between the power and the intended power for heteroscedastic variances
get_diff_pow_het <- function(N, t, delta, sigma, C, sig_level = 0.05, power = 0.8){
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
    diff <- (1 - pmvt(lower = -crit1, upper = crit1, delta = c(ncp), corr = R,
                      df = df)[1] - power) #abs
    return(diff)
  }
}

# SingleSimHet_tfix -------------------------------------------------------------
# finds the optimal N and calculates the power based on N for one simulation run, given t
SingleSimHet_tfix <- function(mu, sigma, C, t, power){
  a <- length(t)
  delta <- C%*%mu
  res <- tryCatch({
    # find the optimal N given a fixed t
    N_opt <- uniroot(f = get_diff_pow_het, interval = c(a*8,10000), t = t, # n_i <=8, n_i>=10^4 excluded
                     delta = delta, sigma = sigma, C = C, power = power, extendInt = "yes")$root
    # calculate the power based on the optimal N
    get_pow_het(t = t, N = N_opt, mu = mu, sigma = sigma, C = C)
  }, error = function(e){
    message("Error in uniroot")
    return(data.frame(t = paste0("(", paste(t, collapse = ", "), ")"),
                      N = NA, power = NA) )
  })
  return(data.frame(mu = paste0("(", paste(round(mu, digits=4), collapse = ", "),")"), 
                    sigma = paste0("(", paste(round(sigma, digits=4), collapse = ", "),")"),
                    C = attributes(C)$type, t = res$t, N = res$N, power = res$power
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
    pvalue <- 1 - pmvt(-abs(T0), abs(T0), delta = rep(0, nrow(C)), 
                       sigma = R, df = round(N - a))[1]
    erg[i] <- (pvalue < 0.05)
  }
  return(data.frame(t = paste0("(", paste(t, collapse = ", "), ")"), N = N, power = mean(erg)))
}

# get_diff_pow_hom --------------------------------------------------------
# get the difference between the power and the intended power for homoscedastic variances
get_diff_pow_hom <- function(t, N, delta, sigma = 1, C, sig_level = 0.05, power = 0.8){
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
    diff <- (1-pmvt(-crit1, crit1, delta = c(ncp), corr = R, df = df)[1]-power) #abs
    return(diff)
  }
}

# SingleSimHom_tfix -------------------------------------------------------------
# finds the optimal N and calculates the power based on N for one simulation run, given t
SingleSimHom_tfix <- function(mu, sigma, C, t, power){
  a <- length(t)
  delta <- C%*%mu
  res <- tryCatch({
    # find the optimal N given a fixed t
    N_opt <- uniroot(f = get_diff_pow_hom, interval = c(a*8,1000), t = t,  # n_i <=8, n_i>=10^4 excluded
                     delta = delta, sigma = sigma, C = C, power = power, extendInt = "yes")$root
    # calculate the power based on the optimal N
    get_pow_hom(t = t, N = N_opt, mu = mu, sigma = sigma, C = C)
  }, error = function(e){
    message("Error in uniroot")
    return(data.frame(t = paste0("(", paste(t, collapse = ", "), ")"),
                      N = NA, power = NA) )
  })
  return(data.frame(mu = paste0("(", paste(round(mu, digits=4), collapse = ", "),")"),
                    sigma = paste0("(", paste(round(sigma, digits=4), collapse = ", "),")"),
                    C = attributes(C)$type, t = res$t, N = res$N, power = res$power))
}

# RunSim ------------------------------------------------------------------
# wrapper function for the simulation
RunSim <- function(case = 1, seed = 21, t = "all", N = NULL, 
                   ngroups, heteroscedastic, CC,
                   mu = NULL, sigma = NULL, power = 0.8){
  
  start_time <- Sys.time()

  # run the simulation in parallel
  cl <- makeCluster(detectCores()-2) # 10
  clusterExport(cl = cl, varlist = c("get_pow_het", "get_diff_pow_het", 
                                     "get_pow_hom", "get_diff_pow_hom",
                                     "get_t_mat_all", "%>%", "cross_df",
                                     "qmvt", "pmvt"))
  ## heteroscedastic case ----
  if(heteroscedastic){
    res_ngroups <- lapply(ngroups, function(ngr){
      # use the same seed to draw mu, sigma
      set.seed(5)
      if(is.null(mu)){
        mu_mat <- t(replicate(10, runif(n = ngr, min = 0.4, max = 1)))
      }    
      if(is.null(sigma)){
        sigma_mat <- matrix(c(1, rep(.5, ngr-1), .5, rep(1, ngr-1)), ncol = ngr, byrow=T)
      }
      set.seed(seed)
        if(t == "all"){
        t_mat <- get_t_mat(ngroups = ngr)
        }
        # nested apply functions to run all combinations (factorial design)
        res_t <- apply(t_mat, 1, function(taux){
          res_CC <- lapply(CC, function(CCaux){
            res_sigma <- apply(sigma_mat, 1, function(sigmaaux){
              print(sigmaaux) # to check progress
              # run parallel: each core for one row of mu_mat
              res_mu <- parRapply(cl = cl, x = mu_mat, FUN = SingleSimHet_tfix,
                                  t = taux, sigma = sigmaaux, power = power,
                                  C = contrMat(n = rep(10, ngr), type = CCaux))
              do.call(rbind.data.frame, res_mu)
            })
            do.call(rbind.data.frame, res_sigma)
          })
          do.call(rbind.data.frame, res_CC)
        })
        do.call(rbind.data.frame, res_t)
    })
  }else{
    ## homoscedastic case ----
    res_ngroups <- lapply(ngroups, function(ngr){
      # use the same seed to draw mu, sigma
      set.seed(5)
      if(is.null(mu)){
        mu_mat <- t(replicate(10, runif(n = ngr, min = 0.4, max = 1)))
      }    
      if(is.null(sigma)){
        sigma_mat <- as.matrix(seq(0.5,1.5,0.5))
      }
      set.seed(seed)
        if(t == "all"){
          t_mat <- get_t_mat(ngroups = ngr)
        }
        # nested apply functions to run all combinations (factorial design)
        res_t <- apply(t_mat, 1, function(taux){
          res_CC <- lapply(CC, function(CCaux){
            res_sigma <- apply(sigma_mat, 1, function(sigmaaux){
              print(sigmaaux) # to check progress
              # run parallel: each core for one row of mu_mat
              res_mu <- parRapply(cl = cl, x = mu_mat, FUN = SingleSimHom_tfix,
                                  t = taux, sigma = sigmaaux, power = power,
                                  C = contrMat(n = rep(10, ngr), type = CCaux))
              do.call(rbind.data.frame, res_mu)
            })
            do.call(rbind.data.frame, res_sigma)
          })
          do.call(rbind.data.frame, res_CC)
        })
        do.call(rbind.data.frame, res_t)
    })
  }
  names(res_ngroups) <- paste0("ngroups", ngroups)
  stopCluster(cl)
  # Create directory to save the results
  path <- ifelse(!heteroscedastic,
                 paste0(getwd(), "/01_HomoscedasticVars/"),
                 paste0(getwd(), "/02_HeteroscedasticVars/"))
  path_tN <- ifelse(is.null(N),
                    paste0(path, "fixed_t/"),
                    paste0(path, "fixed_N/"))
  path_case <- paste0(path_tN, "case", case, "/")
  dir.create(path, showWarnings = FALSE)
  dir.create(path_tN, showWarnings = FALSE)
  dir.create(path_case, showWarnings = FALSE)
  cat(" Finished after ", round(Sys.time() - start_time, 2), " ",
      units(Sys.time() - start_time), ".\n", sep = "")
  # save results
  parameter_list <- list(seed            = seed,
                         ngroups         = ngroups,
                         heteroscedastic = heteroscedastic,
                         CC              = CC,
                         t               = t,
                         N               = N,
                         power           = power)
  WriteList(path = path_case, list = parameter_list, list.name = "Input_Parameters")
  saveRDS(list(res = res_ngroups, parameter_list = parameter_list),
          file = paste0(path_case,"/res.rds")) 
}
