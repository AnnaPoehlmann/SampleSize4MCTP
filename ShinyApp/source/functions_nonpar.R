# get_t_input -------------------------------------------------------------
# creates the default input vector of allocations
get_t_input <- function(ngroups){
  t_vec <- round( c(0.5, rep(0.5/((ngroups-1)), (ngroups-1)) ), 4)
  return(t_vec)
}

# dataGen -----------------------------------------------------------------
# generates data based on a given relative effect and distribution
dataGen <- function(N,t,p, distribution){
  
  a <- length(t)
  n <- sapply(1:a,function(arg){floor(t[arg]*N)})
  
  if(distribution == "Normal") {
    x <- rnorm(n[1], 0, 1)
    for (dd in 2:a) {
      x <- c(x, rnorm(n[dd], qnorm(p[dd - 1]) * sqrt(2)))
    }
  }else if(distribution == "LogNor") {
    x <- exp(rnorm(n[1], 0, 1))
    for (dd in 2:a) {
      x <- c(x, exp(rnorm(n[dd], qnorm(p[dd - 1]) * sqrt(2))))
    }
  }else if(distribution == "Exp") {
    x <- rexp(n[1], 1)
    for (dd in 2:a) {
      x <- c(x, rexp(n[dd], (1 - p[dd - 1]) / p[dd - 1]))
    }
  }else if(distribution == "Poisson") {
    x <- rpois(n[1], 5)
    for (dd in 2:a) {
      paktA <- p[dd - 1]
      lambda2Akt <-
        EffectPoisson$lambda2[which(abs(EffectPoisson$Effect - paktA) == min(abs(EffectPoisson$Effect - paktA)))]
      x <- c(x, rpois(n[dd], lambda2Akt))
    }
  }else if(distribution == "Binom") {
    x <- rbinom(n[1], 10, 0.5)
    for (dd in 2:a) {
      paktA <- p[dd - 1]
      p2Akt <-
        EffectBinom$p2[which(abs(EffectBinom$Effect - paktA) == min(abs(EffectBinom$Effect - paktA)))]
      x <- c(x, rbinom(n[dd], 10, p2Akt))
    }
  }else if(distribution == "Beta") {
    x <- rbeta(n[1], 2, 2)
    for (dd in 2:a) {
      paktA <- p[dd - 1]
      p2Akt <-
        EffectBeta$mu2[which(abs(EffectBeta$Effect - paktA) == min(abs(EffectBeta$Effect - paktA)))]
      x <- c(x, rbeta(n[dd], 2, p2Akt))
    }
  }else if(distribution == "Ordinal") {
    x <- sample(1:5, n[1], replace = TRUE, prob = c(0.6, 0.4, 0, 0, 0))
    for (dd in 2:a) {
      paktA <- p[dd - 1]
      aktzeile <- which(abs(EffectOrdinal$Effect - paktA) == min(abs(EffectOrdinal$Effect - paktA)))
      p2Akt <- c(EffectOrdinal[aktzeile, 1:5])
      x <- c(x, sample(1:5, n[dd], replace = TRUE, prob = p2Akt))
    }
  }
  grp <- rep(1, n[1])
  for (dd in 2:a) {
    grp <- c(grp, rep(dd, n[dd]))
  }
  dat <- data.frame(x = x, group = factor(grp))
  return(dat)
}

# get_pow -------------------------------------------------------------
# simulates the power in the nonparametric case
get_pow <- function(nsim = 10000, N, t, p, C, distribution = "Normal"){
  H0rej_ind <- numeric(nsim)
  for(w in 1:nsim){
    dat <- dataGen(N, t, p, distribution)
    # many-to-one comparison
    H0rej_ind[w] <- min(steel(x ~ group, data = dat, 
                              control = 1, info = FALSE)$Analysis$p.value) < 0.05
  }
  return(list(t = t, N = N, power = mean(H0rej_ind)))
}

# get_R0 -----------------------------------------------------------------
# calculates the correlation matrix under the null hypothesis
get_R0 <- function(N,n){
  
  sigma0 <- sigma012 <- c()
  a <- length(n)
  nc <- a-1
  tmp <- expand.grid(1:a, 1:a)
  ind <- tmp[[1]] > tmp[[2]]
  vi <- tmp[[2]][ind]
  vj <- tmp[[1]][ind]
  rho.st <- diag(nc)
  for (xx in 1:(nc - 1)) {
    for (yy in (xx + 1):nc) {
      i <- vi[xx]
      j <- vj[xx]
      v <- vi[yy]
      w <- vj[yy]
      p <- c(i == v, j == w, i == w, j == v)
      if (sum(p) == 1) {
        cl <- list(function() sqrt(n[j]/(n[i]+n[j]+1))*sqrt(n[w]/(n[i]+n[w]+1)),
                   function() sqrt(n[i]/(n[i]+n[j]+1))*sqrt(n[v]/(n[v]+n[j]+1)) ,
                   function() -(sqrt(n[j]/(n[i]+n[j]+1))*sqrt(n[v]/(n[v]+n[i]+1))),
                   function() -(sqrt(n[w]/(n[w]+n[j]+1))*sqrt(n[i]/(n[j]+n[i]+1))))
        case <- (1:4)[p]
        rho.st[xx, yy] <- rho.st[yy, xx] <- 1 * cl[[case]]()}}}
  for(xx in 1:nc){
    i <- vi[xx] 
    j <- vj[xx]
    sigma0[xx] <- sqrt(N)*sqrt(((n[i]+n[j]+1)/(12*(n[i]+n[j])))*(1/n[i]+1/n[j]))
    sigma012[xx] <- sqrt(N)*sqrt(1/12*(1/n[i]+1/n[j]))
  }
  return(list(R0 = rho.st, sigma0 = sigma0, sigma012 = sigma012))
}


# estimatorstar -----------------------------------------------------------
estimatorstar <- function(formula, data) {
  dat <- model.frame(formula, data)
  response <- dat[, 1]
  factorx <- as.factor(dat[, 2])
  samples <- split(response, factorx)
  fl <- levels(factorx)
  a <- nlevels(factorx)
  n <- as.numeric(sapply(samples, length)) #updated for better stability
  N <- sum(n)
  tmp1 <- sort(rep(1:a, a))
  tmp2 <- rep(1:a, a)
  pairRanks <- lapply(1:(a ^ 2), function(arg) rank(c(samples[[tmp1[arg]]],samples[[tmp2[arg]]])))
  p <- sapply(1:(a ^ 2), function(arg) {
    x1 <- samples[[tmp1[arg]]]
    x2 <- samples[[tmp2[arg]]]
    rx1x2 <- rank(c(x1, x2))
    l1 <- length(x1)
    l2 <- length(x2)
    1 / (l1 + l2) * (mean(rx1x2[(l1 + 1):(l1 + l2)]) - mean(rx1x2[1:l1])) + 0.5
  })
  sigma0star <- sapply(1:(a ^ 2), function(arg) {
    x1 <- samples[[tmp1[arg]]]
    x2 <- samples[[tmp2[arg]]]
    rx1x2 <- rank(c(x1, x2))
    Nij <- length(rx1x2)
    1 / Nij ^ 3 * sum((rx1x2 - mean(rx1x2)) ^ 2)
  })
  V1 = V2 = rep(0, a ^ 4)
  help <- expand.grid(1:a, 1:a, 1:a, 1:a)
  h1 <- help[, 4]
  h2 <- help[, 3]
  h3 <- help[, 2]
  h4 <- help[, 1]
  for (u in 1:(a ^ 4)) {
    i <- h1[u]
    j <- h2[u]
    r <- h3[u]
    s <- h4[u]
    if (i == r && j == s && i != j && r != s) {
      xi <- samples[[i]]
      xj <- samples[[j]]
      ni <- length(xi)
      nj <- length(xj)
      ri <- rank(xi)
      rj <- rank(xj)
      rij <- rank(c(xi, xj))
      pj <- 1 / ni * (rij[(ni + 1):(ni + nj)] - rj)
      pi <- 1 / nj * (rij[1:ni] - ri)
      V1[u] <- var(pi) * (ni - 1) / ni
      V2[u] <- var(pj) * (nj - 1) / nj
    }
    if (i == s && j == r && i != j && r != s) {
      xi <- samples[[i]]
      xj <- samples[[j]]
      ni <- length(xi)
      nj <- length(xj)
      ri <- rank(xi)
      rj <- rank(xj)
      rij <- rank(c(xi, xj))
      pj <- 1 / ni * (rij[(ni + 1):(ni + nj)] - rj)
      pi <- 1 / nj * (rij[1:ni] - ri)
      V1[u] <- -var(pi) * (ni - 1) / ni
      V2[u] <- -var(pj) * (nj - 1) / nj
    }
    if (i == r && j != s && i != j && r != s) {
      xi <- samples[[i]]
      xj <- samples[[j]]
      xs <- samples[[s]]
      ni <- length(xi)
      nj <- length(xj)
      ns <- length(xs)
      ri <- rank(xi)
      rj <- rank(xj)
      rs <- rank(xs)
      rij <- rank(c(xi, xj))
      ris <- rank(c(xi, xs))
      pij <- 1 / nj * (rij[1:ni] - ri)
      pis <- 1 / ns * (ris[1:ni] - ri)
      V1[u] <-  (cov(pij, pis) * (ni - 1) / ni)
    }
    if (i != r && j == s && i != j && r != s) {
      xi <- samples[[i]]
      xj <- samples[[j]]
      xr <- samples[[r]]
      ni <- length(xi)
      nj <- length(xj)
      nr <- length(xr)
      ri <- rank(xi)
      rj <- rank(xj)
      rr <- rank(xr)
      rji <- rank(c(xj, xi))
      rjr <- rank(c(xj, xr))
      pji <- 1 / ni * (rji[1:nj] - rj)
      prj <- 1 / nr * (rjr[1:nj] - rj)
      V1[u] <- cov(pji, prj) * (nj - 1) / nj# N * (cov(pji, prj)/nj)
    }
    if (i == s && j != r && i != j && r != s) {
      xi <- samples[[i]]
      xj <- samples[[j]]
      xr <- samples[[r]]
      ni <- length(xi)
      nj <- length(xj)
      nr <- length(xr)
      ri <- rank(xi)
      rj <- rank(xj)
      rr <- rank(xr)
      rij <- rank(c(xi, xj))
      rir <- rank(c(xi, xr))
      pij <- 1 / nj * (rij[1:ni] - ri)
      pir <- 1 / nr * (rir[1:ni] - ri)
      V1[u] <- -cov(pij, pir) * (ni - 1) / ni#-N * (cov(pij, pir)/ni)
    }
    if (i != s && j == r && i != j && r != s) {
      xi <- samples[[i]]
      xj <- samples[[j]]
      xs <- samples[[s]]
      ni <- length(xi)
      nj <- length(xj)
      ns <- length(xs)
      ri <- rank(xi)
      rj <- rank(xj)
      rs <- rank(xs)
      rji <- rank(c(xj, xi))
      rjs <- rank(c(xj, xs))
      pji <- 1 / ni * (rji[1:nj] - rj)
      pjs <- 1 / ns * (rjs[1:nj] - rj)
      V1[u] <- -cov(pji, pjs) * (nj - 1) / nj#-N * (cov(pji, pjs)/nj)
    }
  }
  V1 <- matrix(V1, ncol = a ^ 2, nrow = a ^ 2)
  V2 <- matrix(V2, ncol = a ^ 2, nrow = a ^ 2)
  return(list(p = p, sigma0star = sigma0star, V1star = V1, V2star = V2))
}

# estimatorsNormal --------------------------------------------------------
estimatorsNormal <- function(pakt) {
  a <- length(pakt) + 1
  nstar <- 100000
  yart <- rnorm(nstar, 0, 1)
  grpart <- rep(1, nstar)
  for (h in 2:a) {
    yart <- c(yart, rnorm(nstar, qnorm(pakt[h - 1]) * sqrt(2)))
    grpart <- c(grpart, rep(h, nstar))
  }
  grpart <- factor(grpart)
  datenart <- data.frame(yart, grpart)
  estimatorsart <- estimatorstar(yart ~ grpart, data = datenart)
  return(estimatorsart)
}

# estimatorsExp -----------------------------------------------------------
estimatorsExp <- function(pakt) {
  a <- length(pakt) + 1
  nstar <- 100000
  yart <- rexp(nstar)
  grpart <- rep(1, nstar)
  for (h in 2:a) {
    yart <- c(yart, rexp(nstar, (1 - pakt[h - 1]) / pakt[h - 1]))
    grpart <- c(grpart, rep(h, nstar))
  }
  grpart <- factor(grpart)
  datenart <- data.frame(yart, grpart)
  estimatorsart <- estimatorstar(yart ~ grpart, data = datenart)
  return(estimatorsart)
}

# get_pilot_data ----------------------------------------------------------
# simulates pilot data based on a given distribution, relative effect, and size of the pilot data
get_pilot_data <- function(distPilot, nPilot, pPilot){
  a <- length(nPilot)
  if (distPilot == "Normal") {
    xPilot <- rnorm(nPilot[1], 0, 1)
    for (dd in 2:a) {
      xPilot <- c(xPilot, rnorm(nPilot[dd], qnorm(pPilot[dd - 1]) * sqrt(2)))
    }
  }else if (distPilot == "LogNor") {
    xPilot <- exp(rnorm(nPilot[1], 0, 1))
    for (dd in 2:a) {
      xPilot <- c(xPilot, exp(rnorm(nPilot[dd], qnorm(pPilot[dd - 1]) * sqrt(2))))
    }
  }else  if (distPilot == "Exp") {
    xPilot <- rexp(nPilot[1], 1)
    for (dd in 2:a) {
      xPilot <- c(xPilot, rexp(nPilot[dd], (1 - pPilot[dd - 1]) / pPilot[dd -  1]))
    }
  }else  if (distPilot == "Poisson") {
    xPilot <- rpois(nPilot[1], 5)
    for (dd in 2:a) {
      paktA <- pPilot[dd - 1]
      lambda2Akt <- EffectPoisson$lambda2[which(abs(EffectPoisson$Effect - paktA) == min(abs(EffectPoisson$Effect - paktA)))]
      xPilot <- c(xPilot, rpois(nPilot[dd], lambda2Akt))
    }
  }else  if (distPilot == "Binom") {
    xPilot <- rbinom(nPilot[1], 10, 0.5)
    for (dd in 2:a) {
      paktA <- pPilot[dd - 1]
      p2Akt <- EffectBinom$p2[which(abs(EffectBinom$Effect - paktA) == min(abs(EffectBinom$Effect - paktA)))]
      xPilot <- c(xPilot, rbinom(nPilot[dd], 10, p2Akt))
    }
  }else  if (distPilot == "Beta") {
    xPilot <- rbeta(nPilot[1], 2, 2)
    for (dd in 2:a) {
      paktA <- pPilot[dd - 1]
      p2Akt <- EffectBeta$mu2[which(abs(EffectBeta$Effect - paktA) == min(abs(EffectBeta$Effect - paktA)))]
      xPilot <- c(xPilot, rbeta(nPilot[dd], 2, p2Akt))
    }
  }else  if (distPilot == "Ordinal") {
    xPilot <- sample(1:5, nPilot[1], replace = TRUE, prob = c(0.6, 0.4, 0, 0, 0))
    for (dd in 2:a) {
      paktA <- pPilot[dd - 1]
      aktzeile <- which(abs(EffectOrdinal$Effect - paktA) == min(abs(EffectOrdinal$Effect - paktA)))
      p2Akt <- c(EffectOrdinal[aktzeile, 1:5])
      xPilot <- c(xPilot, sample(1:5, nPilot[dd], replace = TRUE, prob = p2Akt))
    }
  }
  grpPilot <- rep(1, nPilot[1])
  for(dd in 2:a){
    grpPilot <- c(grpPilot, rep(dd, nPilot[dd]))
    }
  return(data.frame(yPilot = xPilot, grpPilot = factor(grpPilot)))
}

# get_diff_pow_noether --------------------------------------------------------
# get the difference between the power and the intended power 
# Noether (method A)
# many-to-one only
get_diff_pow_noether <- function(N, t, p, power_inp = 0.8){ 
  a <- length(t)  
  n <- sapply(1:a, function(arg){floor(t[arg]*N)})
  nc <- a-1
  Sigmas <- get_R0(N, n)
  rho.st <- Sigmas$R0
  crit0 <- qmvnorm(0.95, corr = rho.st, tail="both")$quantile
  sigma0 <- Sigmas$sigma012
  delta <- sqrt(N)*(p-1/2)/sigma0
  diff <- 1 - pmvnorm(-crit0, crit0, mean = delta, corr = rho.st)[1] - power_inp
  return(diff) 
}

# get_diff_pow_noether_synthetic -------------------------------------------------------
# get the difference between the power and the intended power 
# Noether, but use previous information on ties (method B)
get_diff_pow_noether_synthetic <- function(N, t, ests, power_inp = 0.8) {
  sigma0 <- c()
  a <- length(t) 
  n <- sapply(1:a, function(arg) { floor(t[arg] * N)})
  nc <- a - 1
  W <- matrix(0, ncol = a ^ 2, nrow = nc)
  tmp <- expand.grid(1:a, 1:a)
  ind <- tmp[[1]] > tmp[[2]]
  vi <- tmp[[2]][ind]
  vj <- tmp[[1]][ind]
  Sigmas <- get_R0(N, n)
  rho.st <- Sigmas$R0
  crit0 <- qmvnorm(0.95, corr = rho.st, tail = "both")$quantile
  for (s in 1:nc) {
    for (ss in 1:(a ^ 2)) {
      if (ss == (s + 1)) {
        W[s, ss] <- 1 
      }
    }
  }
  pplan <- c(W %*% ests$p)
  sigma0star <- c(W %*% ests$sigma0star)
  for (xx in 1:nc) {
    i <- vi[xx]
    j <- vj[xx]
    sigma0[xx] <- sqrt(N) * sqrt(sigma0star[xx] * (1 / n[i] + 1 / n[j]))
  }
  delta <- sqrt(N) * (pplan - 1 / 2) / sigma0
  # compute the Power under the alternative hypothesis
  diff <- 1 - pmvnorm(-crit0, crit0, mean = delta, corr = rho.st)[1] - power_inp
  return(diff)
}

# get_diff_pow_synthetic ---------------------------------------------------------------
# get the difference between the power and the intended power 
# include previous information in the estimation of the variance components
get_diff_pow_synthetic <- function(N, t, ests, Ninit = NULL, type = "normal", power_inp) {
  a <- length(t)
  n <- sapply(1:a, function(arg) {
    floor(t[arg] * N)
  })
  nc <- a - 1
  W <- matrix(0, ncol = a ^ 2, nrow = nc)
  sigma0 <- c()
  # compute the quantile under the null hypothesis
  tmp <- expand.grid(1:a, 1:a)
  ind <- tmp[[1]] > tmp[[2]]
  vi <- tmp[[2]][ind]
  vj <- tmp[[1]][ind]
  Sigmas <- get_R0(N, n)
  rho.st <- Sigmas$R0
  crit0 = qmvnorm(0.95, corr = rho.st, tail = "both")$quantile
  # compute Sigma under the alternative hypothesis
  V1help = c(ests$V1star)
  V2help = c(ests$V2star)
  SigmaA <- rep(0, a ^ 4)
  help <- expand.grid(1:a, 1:a, 1:a, 1:a)
  h1 <- help[, 4]
  h2 <- help[, 3]
  h3 <- help[, 2]
  h4 <- help[, 1]
  for (u in 1:(a ^ 4)) {
    i <- h1[u]
    j <- h2[u]
    r <- h3[u]
    s <- h4[u]
    if (i == r && j == s && i != j && r != s) {
      ni <- n[i]
      nj <- n[j]
      SigmaA[u] <- N * (V1help[u] / ni + V2help[u] / nj)
    }
    if (i == s && j == r && i != j && r != s) {
      ni <- n[i]
      nj <- n[j]
      SigmaA[u] <- N * (V1help[u] / ni + V2help[u] / nj)
    }
    if (i == r && j != s && i != j && r != s) {
      ni <- n[i]
      SigmaA[u] <- N * V1help[u] / ni
    }
    if (i != r && j == s && i != j && r != s) {
      nj <- n[j]
      SigmaA[u] <- N * V1help[u] / nj
    }
    if (i == s && j != r && i != j && r != s) {
      ni <- n[i]
      SigmaA[u] <- N * V1help[u] / ni
    }
    if (i != s && j == r && i != j && r != s) {
      nj <- n[j]
      SigmaA[u] <- N * V1help[u] / nj
    }
  }
  SigmaA <- matrix(SigmaA, ncol = a ^ 2, nrow = a ^ 2)
  # compute the planning parameter
  for (s in 1:nc) {
    for (ss in 1:(a ^ 2)) {
      if (ss == (s + 1)) {
        W[s, ss] <- 1
      }
    }
  }
  pplan <- c(W %*% ests$p)
  SigmaAplan <- W %*% SigmaA %*% t(W)
  sigma0star <- c(W %*% ests$sigma0star)
  for (xx in 1:nc) {
    i <- vi[xx]
    j <- vj[xx]
    sigma0[xx] <- sqrt(N) * sqrt(sigma0star[xx] * (1 / n[i] + 1 / n[j]))
  }
  delta <- sqrt(N) * (pplan - 1 / 2) / sigma0
  SigmaFinal <- diag(1 / sigma0) %*% SigmaAplan %*% diag(1 / sigma0)
  # compute the power under the alternative hypothesis
  if (type == "normal") {
    result <- 1 - pmvnorm(-crit0, crit0, mean = delta, sigma = SigmaFinal)[1] - power_inp
  }else  if (type == "multi.t") {
    result <- 1 - pmvt(-crit0, crit0, delta = delta, sigma = SigmaFinal, df = Ninit)[1] - power_inp
  }
  return(result)
}

# SingleSim_tfix -------------------------------------------------------------
# finds the optimal N and calculates the power based on N for one simulation run, given t
# nonparametric case
SingleSim_tfix <- function(p, t, power_inp = 0.8, approx, seed, C, datPilot = NULL){
  set.seed(seed)
  a <- length(t)
  res <- tryCatch({
    # find the optimal N given a fixed t
    # n_i <=8, n_i>=10^4 are excluded
    if(approx == "A"){ # Noether
      N_opt <- uniroot(f = get_diff_pow_noether, interval = c(a*8,1000), t = t,  
                       p = p,  power_inp = power_inp, extendInt = "yes")$root
    }else{
      # use the pilot data
      if(approx == "B"){ # Noether, but uses previous information on ties
        ests <- estimatorstar(value ~ key, data = gather(datPilot))
        N_opt <- uniroot(f = get_diff_pow_noether_synthetic, interval = c(a*8,1000), t = t,  
                         ests = ests, power_inp = power_inp, extendInt = "yes")$root
      }else if(approx == "C"){
        ests <- estimatorstar(value ~ key, data = gather(datPilot))
        N_opt <- uniroot(f = get_diff_pow_synthetic, interval = c(a*8,1000), t = t,  
                         ests = ests, power_inp = power_inp, extendInt = "yes")$root
      }else if(approx == "D"){
        ests <- estimatorstar(value ~ key, data = gather(datPilot))
        nPilot <- sapply(datPilot, function(x) sum(!is.na(x)))
        n_min <- sort(nPilot)[1:2]
        N_opt <- uniroot(f = get_diff_pow_synthetic, interval = c(a*8,1000), t = t, type = "multi.t", 
                         ests = ests, Ninit = sum(n_min)-2, power_inp = power_inp, extendInt = "yes")$root
      }else{ # approx == "E"
        # create normal data
        naux <- 1000
        xPilot <- rnorm(naux, 0, 1)
        for (dd in 2:a) {
          xPilot <- c(xPilot, rnorm(naux, qnorm(p[dd - 1]) * sqrt(2))) 
        }
        datPilot <- data.frame(x = xPilot, grp = rep(1:a, each=naux))
        ests <- estimatorstar(x ~ grp, data = datPilot)
        estsNormal <- estimatorsNormal(ests$p[2:a]) # estimatorsExp(ests$p[2:a])
        N_opt <- uniroot(f = get_diff_pow_synthetic, interval = c(a*8,1000), t = t,  
                         ests = estsNormal, power_inp = power_inp, extendInt = "yes")$root
      }
    } 
    # calculate the power based on the optimal N
    get_pow(t = t, N = N_opt, p = p, C = C) # distribution = "Normal"  distribution = "Exp"
  }, error = function(e){
  # problem: sometimes uniroot finds multiple local roots 
    message(paste0("These input parameters lead to a sample size smaller than ", a*8,
                   " or larger than 1000, which is not recommended."))
    return(list(t = t, N = NA, power = NA) )
  })
  return(data.frame(Contrast = C, p = paste0("(", paste(round(p, digits=4), collapse = ", "),")"),
                    Approximation = approx, t = paste0("(", paste(res$t, collapse = ", "), ")"),
                    N = round(res$N), n = paste0("(", paste(round(res$t * res$N), collapse = ", "), ")"), Power = res$power))
}


# SingleSim_Nfix -------------------------------------------------------------
# finds the optimal t via grid search and calculates the power based on t for one simulation run, given N
SingleSim_Nfix <- function(p, N, power_inp = 0.8, approx, seed, C, datPilot = NULL){
  set.seed(seed)
  a <- length(p)+1 # Dunnett contrast only 
  t_mat <- get_t_mat_all(ngroups = a)
  
  res <- tryCatch({
    # find the optimal t given a fixed N
    # n_i <=8, n_i>=10^4 are excluded
    if(approx == "A"){ # Noether
      res <- apply(t_mat, 1, function(t){
        get_diff_pow_noether(t = t, N = N, p = p, power_inp = power_inp)
      }) 
      t_opt <- t_mat[which.min(abs(res)), ]
    }else{
      # use the pilot data
      if(approx == "B"){ # Noether, but uses previous information on ties
        ests <- estimatorstar(value ~ key, data = gather(datPilot))
        res <- apply(t_mat, 1, function(t){
          get_diff_pow_noether_synthetic(t = t, N = N, ests = ests, power_inp = power_inp)
        }) 
        t_opt <- t_mat[which.min(abs(res)), ]
      }else if(approx == "C"){
        ests <- estimatorstar(value ~ key, data = gather(datPilot))
        res <- apply(t_mat, 1, function(t){
          get_diff_pow_synthetic(t = t, N = N, ests = ests, power_inp = power_inp)
        }) 
        t_opt <- t_mat[which.min(abs(res)), ]
      }else if(approx == "D"){
        ests <- estimatorstar(value ~ key, data = gather(datPilot))
        nPilot <- sapply(datPilot, function(x) sum(!is.na(x)))
        n_min <- sort(nPilot)[1:2]
        res <- apply(t_mat, 1, function(t){
          get_diff_pow_synthetic(t = t, N = N, ests = ests, Ninit = sum(n_min)-2,
                                 power_inp = power_inp, type = "multi.t")
        }) 
        t_opt <- t_mat[which.min(abs(res)), ]
      }else{ # approx == "E"
        # create normal data
        naux <- 1000
        xPilot <- rnorm(naux, 0, 1)
        for (dd in 2:a) {
          xPilot <- c(xPilot, rnorm(naux, qnorm(p[dd - 1]) * sqrt(2))) 
        }
        datPilot <- data.frame(x = xPilot, grp = rep(1:a, each=naux))
        ests <- estimatorstar(x ~ grp, data = datPilot)
        estsNormal <- estimatorsNormal(ests$p[2:a]) # estimatorsExp(ests$p[2:a])
        res <- apply(t_mat, 1, function(t){
          get_diff_pow_synthetic(t = t, N = N, ests = estsNormal, power_inp = power_inp)
        }) 
        t_opt <- t_mat[which.min(abs(res)), ]
      }
    } 
    # calculate the power based on the optimal N
    get_pow(t = t_opt, N = N, p = p, C = C) # distribution = "Normal"  distribution = "Exp"
  }, error = function(e){
    # problem: sometimes uniroot finds multiple local roots 
    message(paste0("These input parameters lead to a sample size smaller than ", a*8,
                   " or larger than 1000, which is not recommended."))
    return(list(t = t, N = NA, power = NA) )
  })
  return(data.frame(Contrast = C, p = paste0("(", paste(round(p, digits=4), collapse = ", "),")"),
                    Approximation = approx, t = paste0("(", paste(res$t, collapse = ", "), ")"),
                    N = round(res$N), n = paste0("(", paste(round(res$t * res$N), collapse = ", "), ")"), Power = res$power))
}



