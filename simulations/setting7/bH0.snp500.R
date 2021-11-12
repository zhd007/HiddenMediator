library("parallel")
library("mixtools")
library("ggplot2")
library("SimDesign")
library("HDInterval")
library("rjags")

RNGkind("L'Ecuyer-CMRG")
set.seed(700)
set <- 1050
true_hidden_vector <- c(rep(0, set))
#true_hidden_vector <- c(rep(0.02, set), rep(0.25, set), rep(0.5, set))
#true_hidden_vector <- c(rep(0.0025, set), rep(0.005, set), rep(0.01, set))
#true_hidden_vector <- seq(from = 0.02, to = 0.5, by = 0.01)

densMode <- function(x){
  td <- density(x)
  maxDens <- which.max(td$y)
  return(td$x[maxDens])
}

mediation_simulation <- function(true_hidden) {
  # this is a vector of snp names: e.g. snp1-snp50 
  num_snp <-   500             # number of snp in the model
  em.times <- 15
  known_freq <- c(0.5, 0.6, 0.8, 0.2, 0.5, 0.8 )    # freq of m1, m2, m3, m4, m5, m_Hidden
  known_beta <- c(0.4, 0.2, 0.3, 0.2, 0.4)            # beta1 and beta2, beta3, beat4, beta5
  sample_size <- 100000
  
  # the snp names
  snp_col <- c()
  for (i in 1:num_snp) {
    snp_col[i] <- paste("snp", i, sep="")
  }
  
  temp.record <- -9
  a_coefs <- -9
  c_coefs <- -9
  
  tryCatch({
    
    df.snp <- data.frame(matrix(vector(), sample_size, length(snp_col),
                                dimnames=list(c(), snp_col)),
                         stringsAsFactors=F)
    # generate snp
    for (i in 1:length(snp_col)) {
      #### frequency of the SNPs
      #freq <- runif(n = 1, min = 0.1, max = 0.4)
      freq <- runif(n = 1, min = 0.1, max = 0.9)
      #freq <- runif(n = 1, min = 0.005, max = 0.015)
      cur_snp_v <- sample(c(0, 1, 2), size = sample_size, replace = TRUE, prob = c((1-freq)^2, 2*freq*(1-freq), freq^2))
      cur_snp_name <- paste(snp_col[i])
      assign(cur_snp_name, cur_snp_v)
      df.snp[[cur_snp_name]] <- cur_snp_v
    }
    
    
    
    
    #cor12 <- mvrnorm(n = sample_size, mu = c(50,5), Sigma = matrix(c(2,1.5,1.5,2), nrow = 2, ncol = 2))
    #cor34 <- mvrnorm(n = sample_size, mu = c(10,6), Sigma = matrix(c(3,1.2,1.2,3), nrow = 2, ncol = 2))
    
    #betaS1_2 <- mvrnorm(n = num_snp, mu = c(0.2,0.2), Sigma = matrix(c(0.05,0.005,0.005,0.05), nrow = 2, ncol = 2))
    
    upper_level_effect <- rnorm(n = length(known_beta)+1, 0.2, 0)
    
    ############# known: m1
    
    # a vector of indicator of presence
    seq1 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-known_freq[1], known_freq[1]))
    #seq1 <- rep(1, num_snp)
    # a vector of beta(snp->mediator)
    #betaS1 <- rnorm(n = num_snp, mean = 0.3, sd = 0.1)
    betaS1 <- rnorm(n = num_snp, mean = upper_level_effect[1], sd = 0.08)
    #betaS1 <- betaS1_2[,1]
    
    m1 <- data.matrix(df.snp) %*% (seq1 * betaS1)
    m1 <- m1[,1]
    m1 <- m1 + 50 # constant
    m1 <- m1 + rnorm(sample_size, mean = 0, sd = 1) # add some noise: size of noise?
    #m1 <- m1 + cor12[,1]
    #m1 <- m1 + runif(sample_size, min = -0.3, max = 0.3)
    
    ############# known: m2
    seq2 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-known_freq[2], known_freq[2]))
    #seq2 <- rep(1, num_snp)
    # a vector of beta(snp->mediator)
    #betaS2 <- rnorm(n = num_snp, mean = 0.3, sd = 0.1)
    betaS2 <- rnorm(n = num_snp, mean = upper_level_effect[2], sd = 0.08)
    #betaS2 <- betaS1_2[,2]
    
    m2 <- data.matrix(df.snp) %*% (seq2 * betaS2)
    m2 <- m2[,1]
    m2 <- m2 + 5 # constant
    m2 <- m2 + rnorm(sample_size, mean = 0, sd = 1) 
    #m2 <-  m2 + 0.5*m1                                       # add some of m1 to m2
    #m2 <- m2 + cor12[,2]
    #m2 <- m2 + runif(sample_size, min = -0.4, max = 0.4)
    
    
    ############# known: m3
    seq3 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-known_freq[3], known_freq[3]))
    #seq3 <- rep(1, num_snp)
    # a vector of beta(snp->mediator)
    #betaS3 <- rnorm(n = num_snp, mean = 0.3, sd = 0.1)
    betaS3 <- rnorm(n = num_snp, mean = upper_level_effect[3], sd = 0.08)
    
    m3 <- data.matrix(df.snp) %*% (seq3 * betaS3)
    m3 <- m3[,1]
    m3 <- m3 + 10 # constant
    m3 <- m3 + rnorm(sample_size, mean = 0, sd = 1.5)
    #m3 <- m3 + cor34[,1]
    #m3 <- m3 + runif(sample_size, min = -0.4, max = 0.4)
    
    ############# known: m4
    seq4 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-known_freq[4], known_freq[4]))
    #seq4 <- rep(1, num_snp)
    # a vector of beta(snp->mediator)
    #betaS4 <- rnorm(n = num_snp, mean = 0.3, sd = 0.1)
    betaS4 <- rnorm(n = num_snp, mean = upper_level_effect[4], sd = 0.08)
    
    m4 <- data.matrix(df.snp) %*% (seq4 * betaS4)
    m4 <- m4[,1]
    m4 <- m4 + 6 # constant
    m4 <- m4 + rnorm(sample_size, mean = 0, sd = 1.2)
    #m4 <- 0.1 * m3 + m4
    #m4 <- m4 + cor34[,2]
    #m4 <- m4 + runif(sample_size, min = -0.4, max = 0.4)
    
    ############# known: m5
    seq5 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-known_freq[5], known_freq[5]))
    #seq5 <- rep(1, num_snp)
    # a vector of beta(snp->mediator)
    #betaS5 <- rnorm(n = num_snp, mean = 0.3, sd = 0.1)
    betaS5 <- rnorm(n = num_snp, mean = upper_level_effect[5], sd = 0.08)
    
    m5 <- data.matrix(df.snp) %*% (seq5 * betaS5)
    m5 <- m5[,1]
    m5 <- m5 + 15 # constant
    m5 <- m5 + rnorm(sample_size, mean = 0, sd = 1)
    #m5 <- m5 + runif(sample_size, min = -0.4, max = 0.4)
    
    ############# hidden: m6
    seq6 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-known_freq[6], known_freq[6]))
    # a vector of beta(snp->mediator)
    #betaS6 <- rnorm(n = num_snp, mean = 0.3, sd = 0.1)
    betaS6 <- rnorm(n = num_snp, mean = upper_level_effect[6], sd = 0.08)
    
    m6 <- data.matrix(df.snp) %*% (seq6 * betaS6)
    m6 <- m6[,1]
    m6 <- m6 + 20 # constant
    m6 <- m6 + rnorm(sample_size, mean = 0, sd = 1) 
    #m6 <- m6 + runif(sample_size, min = -0.3, max = 0.3)
    
    ############ covariates
    covariate1 <- rnorm(sample_size, mean = 7, sd = 0.5)
    covariate2 <- rnorm(sample_size, mean = 4, sd = 0.4)
    
    
    ############## mediator -> outcome
    y <- known_beta[1] * m1 + known_beta[2] * m2 + known_beta[3] * m3 + 
      known_beta[4] * m4 + known_beta[5] * m5 +
      true_hidden * m6 + rnorm(sample_size, mean = 0, sd = 0.2) +
      0.8 * covariate1 - 0.3 * covariate2
    # y <- known_beta[1] * m1 + known_beta[2] * m2 + known_beta[3] * m3 + 
    #   known_beta[4] * m4 + known_beta[5] * m5 +
    #   true_hidden * m6 + runif(sample_size, min = -0.3, max = 0.3)
    
    # combined
    df.snp$m1 <- m1
    df.snp$m2 <- m2
    df.snp$m3 <- m3
    df.snp$m4 <- m4
    df.snp$m5 <- m5
    df.snp$m6 <- m6
    df.snp$y <- y
    df.snp$covariate1 <- covariate1
    df.snp$covariate2 <- covariate2
    
    
    para_decision <- vector(mode = "logical", length = num_snp)
    beta_m1 <- vector(mode = "logical", length = num_snp)
    beta_m2 <- vector(mode = "logical", length = num_snp)
    beta_m3 <- vector(mode = "logical", length = num_snp)
    beta_m4 <- vector(mode = "logical", length = num_snp)
    beta_m5 <- vector(mode = "logical", length = num_snp)
    
    
    
    # record data specific mean of right dist of a and c
    real_a_mean <- mean(c(betaS1[which(seq1 == 1)],    
                          betaS2[which(seq2 == 1)],
                          betaS3[which(seq3 == 1)],
                          betaS4[which(seq4 == 1)],
                          betaS5[which(seq5 == 1)]))
    real_c_mean <- mean(betaS1[which(seq1 == 1)] * true_hidden)
    
    
    # 500 snps
    snp_sum <- paste("snp", 1:num_snp, sep = "", collpase = "")
    snp_sum.cat <- do.call(paste, c(as.list(snp_sum), sep = "+"))
    eq.m1.all <- paste("m1~", snp_sum.cat, sep = "")
    eq.m2.all <- paste("m2~", snp_sum.cat, sep = "")
    eq.m3.all <- paste("m3~", snp_sum.cat, sep = "")
    eq.m4.all <- paste("m4~", snp_sum.cat, sep = "")
    eq.m5.all <- paste("m5~", snp_sum.cat, sep = "")
    eq.m6.all <- paste("m6~", snp_sum.cat, sep = "")
    
    m1_model <- lm(eq.m1.all, data = df.snp)
    m2_model <- lm(eq.m2.all, data = df.snp)
    m3_model <- lm(eq.m3.all, data = df.snp)
    m4_model <- lm(eq.m4.all, data = df.snp)
    m5_model <- lm(eq.m5.all, data = df.snp)
    m6_model <- lm(eq.m6.all, data = df.snp)
    
    
    m1_coefs <- summary(m1_model)$coefficient
    m2_coefs <- summary(m2_model)$coefficient
    m3_coefs <- summary(m3_model)$coefficient
    m4_coefs <- summary(m4_model)$coefficient
    m5_coefs <- summary(m5_model)$coefficient
    m6_coefs <- summary(m6_model)$coefficient
    
    df.a <- data.frame("a1.alt" = m1_coefs[2:(num_snp+1),1],
                       "a2.alt" = m2_coefs[2:(num_snp+1),1], 
                       "a3.alt" = m3_coefs[2:(num_snp+1),1],
                       "a4.alt" = m4_coefs[2:(num_snp+1),1],
                       "a5.alt" = m5_coefs[2:(num_snp+1),1],
                       "a6.alt" = m6_coefs[2:(num_snp+1),1],
                       "a1.var.alt" = m1_coefs[2:(num_snp+1),2]^2,
                       "a2.var.alt" = m2_coefs[2:(num_snp+1),2]^2, 
                       "a3.var.alt" = m3_coefs[2:(num_snp+1),2]^2,
                       "a4.var.alt" = m4_coefs[2:(num_snp+1),2]^2,
                       "a5.var.alt" = m5_coefs[2:(num_snp+1),2]^2,
                       "a6.var.alt" = m6_coefs[2:(num_snp+1),2]^2)
    
    
    # alternative way
    #eq.y.all <- "y~m1+m2 + snp1+snp2+snp3+snp4+snp5+snp6+snp7+snp8+snp9+snp10+snp11+snp12+snp13+snp14+snp15+snp16+snp17+snp18+snp19+snp20+snp21+snp22+snp23+snp24+snp25+snp26+snp27+snp28+snp29+snp30+snp31+snp32+snp33+snp34+snp35+snp36+snp37+snp38+snp39+snp40+snp41+snp42+snp43+snp44+snp45+snp46+snp47+snp48+snp49+snp50+snp51+snp52+snp53+snp54+snp55+snp56+snp57+snp58+snp59+snp60+snp61+snp62+snp63+snp64+snp65+snp66+snp67+snp68+snp69+snp70"
    eq.y.all <- paste("y~m1+m2+m3+m4+m5+", snp_sum.cat, "+covariate1+covariate2", sep = "")
    
    outcome_model <- lm(eq.y.all, data = df.snp)
    y_coefs <- summary(outcome_model)$coefficient
    
    df.b <- data.frame("b1.alt" = y_coefs[2,1], "b2.alt" = y_coefs[3,1], "b3.alt" = y_coefs[4,1], 
                       "b4.alt" = y_coefs[5,1], "b5.alt" = y_coefs[6,1], 
                       "c.alt" = y_coefs[(length(known_beta)+2):(num_snp+1+length(known_beta)),1],
                       "b1.var.alt" = y_coefs[2,2]^2, "b2.var.alt" = y_coefs[3,2]^2, "b3.var.alt" = y_coefs[4,2]^2,
                       "b4.var.alt" = y_coefs[5,2]^2, "b5.var.alt" = y_coefs[6,2]^2,
                       "c.var.alt" = y_coefs[(length(known_beta)+2):(num_snp+1+length(known_beta)),2]^2   )
    
    # plot(df.b$b1, df.b$b1.alt)
    # plot(df.b$b2, df.b$b2.alt)
    # plot(df.b$c, df.b$c.alt)
    
    
    ##### EM to estimate a 
    all_a <- c(df.a$a1.alt,df.a$a2.alt, df.a$a3.alt, df.a$a4.alt, df.a$a5.alt)
    all_c <- df.b$c.alt
    
    a_coefs <- all_a
    c_coefs <- all_c
    
    ####### for a
    emset.a <- em.times
    emLR.a <- vector(mode = "logical", length = emset.a)
    emMu1.a <- vector(mode = "logical", length = emset.a)
    emMu2.a <- vector(mode = "logical", length = emset.a)
    emSd1.a <- vector(mode = "logical", length = emset.a)
    emSd2.a <- vector(mode = "logical", length = emset.a)
    emLambda1.a <- vector(mode = "logical", length = emset.a)
    emLambda2.a <- vector(mode = "logical", length = emset.a)
    emModel.a <- vector(mode = "list", length = emset.a)
    
    for (em.a in 1:emset.a) {
      snp_em.status.a <- -999
      while (snp_em.status.a == -999) {
        tryCatch({
          snp_em.a <- quiet(normalmixEM(all_a, 
                                        lambda = .5, arbvar = TRUE), messages = TRUE)
          snp_em.status.a <- 1
        }, error=function(e){
          print("caught")
          snp_em.status.a <- -999})
      }
      # plot(snp_em, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
      #      main2="SNP", xlab2="Beta")
      emLR.a[em.a] <- snp_em.a$loglik
      emMu1.a[em.a] <- snp_em.a$mu[1]
      emSd1.a[em.a] <- snp_em.a$sigma[1]
      emLambda1.a[em.a] <- snp_em.a$lambda[1]
      emMu2.a[em.a] <- snp_em.a$mu[2]
      emSd2.a[em.a] <- snp_em.a$sigma[2]
      emLambda2.a[em.a] <- snp_em.a$lambda[2]
      emModel.a[[em.a]] <- snp_em.a
    }
    
    
    
    #maxI.a <- which(emLR.a == max(emLR.a))[1]
    maxI.a <- which(round(emMu2.a, digits = 6) == round(median(emMu2.a), digits = 6))[1]
    snp_em_summary <- data.frame("mean" = c(emMu1.a[maxI.a], emMu2.a[maxI.a]), 
                                 "sd" = c(emSd1.a[maxI.a], emSd2.a[maxI.a]), 
                                 "lambda" = c(emLambda1.a[maxI.a], emLambda2.a[maxI.a]) )
    medianModel.a <- emModel.a[[maxI.a]]
    
    ####### for c
    emset.c <- em.times
    emLR.c <- vector(mode = "logical", length = emset.c)
    emMu1.c <- vector(mode = "logical", length = emset.c)
    emMu2.c <- vector(mode = "logical", length = emset.c)
    emSd1.c <- vector(mode = "logical", length = emset.c)
    emSd2.c <- vector(mode = "logical", length = emset.c)
    emLambda1.c <- vector(mode = "logical", length = emset.c)
    emLambda2.c <- vector(mode = "logical", length = emset.c)
    emModel.c <- vector(mode = "list", length = emset.c)
    
    for (em.c in 1:emset.c) {
      snp_em.status.c <- -999
      while (snp_em.status.c == -999) {
        tryCatch({
          snp_em.c <- quiet(normalmixEM(all_c,
                                        lambda = .5, arbvar = TRUE), messages = TRUE)
          snp_em.status.c <- 1
        }, error=function(e){
          print("caught")
          snp_em.status.c <- -999})
      }
      # plot(snp_em, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
      #      main2="SNP", xlab2="Beta")
      emLR.c[em.c] <- snp_em.c$loglik
      emMu1.c[em.c] <- snp_em.c$mu[1]
      emSd1.c[em.c] <- snp_em.c$sigma[1]
      emLambda1.c[em.c] <- snp_em.c$lambda[1]
      emMu2.c[em.c] <- snp_em.c$mu[2]
      emSd2.c[em.c] <- snp_em.c$sigma[2]
      emLambda2.c[em.c] <- snp_em.c$lambda[2]
      emModel.c[[em.c]] <- snp_em.c
    }
    
    
    
    #maxI.c <- which(emLR.c == max(emLR.c))[1]
    maxI.c <- which(round(emMu2.c, digits = 6) == round(median(emMu2.c), digits = 6))[1]
    snp_em_summary.c <- data.frame("mean" = c(emMu1.c[maxI.c], emMu2.c[maxI.c]),
                                   "sd" = c(emSd1.c[maxI.c], emSd2.a[maxI.c]),
                                   "lambda" = c(emLambda1.c[maxI.c], emLambda2.c[maxI.c]) )
    medianModel.c <- emModel.c[[maxI.c]]
    
    
    ##### mcmc on bH
    ### model specification
    flipped <- 0
    mu_a <- snp_em_summary$mean[2]
    sd_a2 <- snp_em_summary$sd[2]
    sd_c1 <- snp_em_summary.c$sd[1]
    sd_c2 <- snp_em_summary.c$sd[2]
    pi <- snp_em_summary.c$lambda
    
    
    model_string = "model {
    
    for (i in 1:n) {
    c[i] ~ dnorm(mu[z[i]], prec[z[i]])
    z[i] ~ dcat(pi)
    }
    
    
    mu[2] = bH * mu_a
    mu[1] = 0
    
    prec[1] = 1/(sd_c1^2)
    prec[2] = 1/(sd_c2^2)
    
    bH ~ dnorm(0, 1/100)
    
    }"
    
    data_jags <- list(c = all_c, mu_a = mu_a, 
                      sd_c1 = sd_c1, sd_c2 = sd_c2,
                      n = length(all_c),
                      pi = pi)
    
    inits_jags <- list(bH = 0, .RNG.name = "base::Wichmann-Hill", ".RNG.seed" = 700)
    
    params <- c("bH")
    ##%% which model string?
    mod <- jags.model(textConnection(model_string), data=data_jags, n.chains=1, inits = inits_jags)
    
    ## burn in
    update(mod, n.iter=1000)
    
    ## actual chain
    mod_sim <- coda.samples(model=mod,
                            variable.names=params,
                            n.iter=10000)
    mod_csim <- as.mcmc(do.call(rbind, mod_sim))
    
    #bH_path[[k]] <- mod_csim[,1]
    #pi1_path <- mod_csim[,3]
    #pi2_path <- mod_csim[,2]
    
    ## convergence diagnostics
    # plot(mod_sim, ask=TRUE)
    # autocorr.diag(mod_sim)
    # effectiveSize(mod_sim)
    # raftery.diag(mod_csim)
    
    
    ### estimation for bH
    b_hat.median <- quantile(mod_csim[,1], 0.5)
    b_hat.mean <- mean(mod_csim[,1])
    
    # 90% quantile CI
    b.low1 <- quantile(mod_csim[,1], 0.05)
    b.high1 <- quantile(mod_csim[,1], 0.95)
    
    b.low1b <- quantile(mod_csim[,1], 0.025)
    b.high1b <- quantile(mod_csim[,1], 0.975)
    
    # 90% HDI
    b.low2 <- hdi(mod_csim[,1], credMass = 0.9)[1]
    b.high2 <- hdi(mod_csim[,1], credMass = 0.9)[2]
    
    b.low2b <- hdi(mod_csim[,1], credMass = 0.95)[1]
    b.high2b <- hdi(mod_csim[,1], credMass = 0.95)[2]
    
    
    # 95% wald CI
    
    out.stat <- summary(mod_sim)[1][[1]]
    ts.se <- out.stat[4]
    b.low3 <- b_hat.mean - 1.96 * ts.se
    b.high3 <- b_hat.mean + 1.96 * ts.se
    
    ## mode estimates
    b.mode1 <- densMode(mod_csim)
    b.mode2 <- -9
    
    
    ####------------------------ if the intervals are too wide ------------------------####
    if ( (b.high1-b.low1) > 5) {
      flipped <- 1
      sd_c1 <- snp_em_summary.c$sd[2]
      sd_c2 <- snp_em_summary.c$sd[1]
      pi <- c(snp_em_summary.c$lambda[2], snp_em_summary.c$lambda[1])
      
      model_string = "model {
    
      for (i in 1:n) {
      c[i] ~ dnorm(mu[z[i]], prec[z[i]])
      z[i] ~ dcat(pi)
      }
      
      
      mu[2] = bH * mu_a
      mu[1] = 0
      
      prec[1] = 1/(sd_c1^2)
      prec[2] = 1/(sd_c2^2)
      
      bH ~ dnorm(0, 1/100)
      
      }"
      
      data_jags <- list(c = all_c, mu_a = mu_a, 
                        sd_c1 = sd_c1, sd_c2 = sd_c2,
                        n = length(all_c),
                        pi = pi)
      
      inits_jags <- list(bH = 0, .RNG.name = "base::Wichmann-Hill", ".RNG.seed" = 700)
      
      params <- c("bH")
      ##%% which model string?
      mod <- jags.model(textConnection(model_string), data=data_jags, n.chains=1, inits = inits_jags)
      
      ## burn in
      update(mod, n.iter=1000)
      
      ## actual chain
      mod_sim <- coda.samples(model=mod,
                              variable.names=params,
                              n.iter=10000)
      mod_csim <- as.mcmc(do.call(rbind, mod_sim))
      
      #bH_path[[k]] <- mod_csim[,1]
      #pi1_path <- mod_csim[,3]
      #pi2_path <- mod_csim[,2]
      
      ## convergence diagnostics
      # plot(mod_sim, ask=TRUE)
      # autocorr.diag(mod_sim)
      # effectiveSize(mod_sim)
      # raftery.diag(mod_csim)
      
      
      ### estimation for bH
      b_hat.median <- quantile(mod_csim[,1], 0.5)
      b_hat.mean <- mean(mod_csim[,1])
      
      # 90% quantile CI
      b.low1 <- quantile(mod_csim[,1], 0.05)
      b.high1 <- quantile(mod_csim[,1], 0.95)
      
      b.low1b <- quantile(mod_csim[,1], 0.025)
      b.high1b <- quantile(mod_csim[,1], 0.975)
      
      # 90% HDI
      b.low2 <- hdi(mod_csim[,1], credMass = 0.9)[1]
      b.high2 <- hdi(mod_csim[,1], credMass = 0.9)[2]
      
      b.low2b <- hdi(mod_csim[,1], credMass = 0.95)[1]
      b.high2b <- hdi(mod_csim[,1], credMass = 0.95)[2]
      
      
      # 95% wald CI
      
      out.stat <- summary(mod_sim)[1][[1]]
      ts.se <- out.stat[4]
      b.low3 <- b_hat.mean - 1.96 * ts.se
      b.high3 <- b_hat.mean + 1.96 * ts.se
      
      ## mode estimates
      b.mode1 <- densMode(mod_csim)
      b.mode2 <- -9
    }
    
    
    #### record result
    temp.record <- data.frame("freq" = known_freq[length(known_freq)],
                              "true_freq" = mean(seq6),
                              "pi_hat" = snp_em_summary.c$lambda[2],
                              "real_a_mean" = real_a_mean, "real_c_mean" = real_c_mean,
                              "b_hat.median" = b_hat.median, "b_hat.mean" = b_hat.mean,
                              "b_hat.mode1" = b.mode1, "b_hat.mode2" = b.mode2,
                              "b.low1" = b.low1, "b.high1" = b.high1,
                              "b.low1b" = b.low1b, "b.high1b" = b.high1b,
                              "b.low2" = b.low2, "b.high2" = b.high2,
                              "b.low2b" = b.low2b, "b.high2b" = b.high2b,
                              "b.low3" = b.low3, "b.high3" = b.high3,
                              "true_hidden" = true_hidden,
                              "flipped" = flipped)
    
    
    print(temp.record)
    
    # c_overall <- c(c_overall, snp_em_summary.c$mean[2])
    # a_overall <- c(a_overall, snp_em_summary$mean[2])  
  }, error=function(e){ 
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ERROR!! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    message(e)
    return(list("temp.record" = temp.record, "a_coefs" = a_coefs, "c_coefs" = c_coefs))  
  })
  
  return(   list("temp.record" = temp.record, "a_coefs" = a_coefs, "c_coefs" = c_coefs) )
}


ptm <- proc.time()    # start timer
sim_result <- mclapply(X = true_hidden_vector, FUN = mediation_simulation, mc.cores = 20)
proc.time() - ptm    # stop timer


## gather simulation results
a_coefs <- vector(mode = "list", length = length(true_hidden_vector))
c_coefs <- vector(mode = "list", length = length(true_hidden_vector))
record <- data.frame("freq" = rep(-9, length(true_hidden_vector)),  "true_freq" = rep(-9, length(true_hidden_vector)), 
                     "pi_hat" = rep(-9, length(true_hidden_vector)), 
                     "real_a_mean" = rep(-9, length(true_hidden_vector)), "real_c_mean" = rep(-9, length(true_hidden_vector)),
                     "b_hat.median" = rep(-9, length(true_hidden_vector)), "b_hat.mean" = rep(-9, length(true_hidden_vector)),
                     "b_hat.mode1" = rep(-9, length(true_hidden_vector)), "b_hat.mode2" = rep(-9, length(true_hidden_vector)),
                     "b.low1" = rep(-9, length(true_hidden_vector)), "b.high1" = rep(-9, length(true_hidden_vector)),
                     "b.low1b" = rep(-9, length(true_hidden_vector)), "b.high1b" = rep(-9, length(true_hidden_vector)),
                     "b.low2" = rep(-9, length(true_hidden_vector)), "b.high2" = rep(-9, length(true_hidden_vector)),
                     "b.low2b" = rep(-9, length(true_hidden_vector)), "b.high2b" = rep(-9, length(true_hidden_vector)),
                     "b.low3" = rep(-9, length(true_hidden_vector)), "b.high3" = rep(-9, length(true_hidden_vector)),
                     "true_hidden" = rep(-9, length(true_hidden_vector)),
                     "flipped" = rep(-9, length(true_hidden_vector)))

for (i in 1:length(sim_result)) {
  single_result <- sim_result[[i]]
  record[i,] <- single_result$temp.record
  a_coefs[[i]] <- single_result$a_coefs
  c_coefs[[i]] <- single_result$c_coefs
}

## beta plot
record$bH.capture1 <- ifelse((record$b.low1 <= record$true_hidden & record$b.high1 >= record$true_hidden), TRUE, FALSE)
record$bH.capture2 <- ifelse((record$b.low2 <= record$true_hidden & record$b.high2 >= record$true_hidden), TRUE, FALSE)
record$bH.capture3 <- ifelse((record$b.low3 <= record$true_hidden & record$b.high3 >= record$true_hidden), TRUE, FALSE)

record$position <- seq(1:nrow(record))
record$bH.capture1b <- ifelse((record$b.low1b <= record$true_hidden & record$b.high1b >= record$true_hidden), TRUE, FALSE)
record$bH.capture2b <- ifelse((record$b.low2b <= record$true_hidden & record$b.high2b >= record$true_hidden), TRUE, FALSE)



# # save mcmc path and coeffients
write.csv(data.frame(matrix(unlist(a_coefs), nrow=length(a_coefs), byrow=T)), row.names = FALSE,
          "/home/dingzh/t2d_mediation/continuous.updated/setting7/bH0.snp500.a_coefs.csv")
write.csv(data.frame(matrix(unlist(c_coefs), nrow=length(c_coefs), byrow=T)), row.names = FALSE,
          "/home/dingzh/t2d_mediation/continuous.updated/setting7/bH0.snp500.c_coefs.csv")

# save record dataframe
write.csv(record, row.names = FALSE,
          "/home/dingzh/t2d_mediation/continuous.updated/setting7/bH0.snp500.record.csv")
