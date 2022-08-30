library("parallel")
library("mixtools")
library("ggplot2")
library("SimDesign")
library("HDInterval")
library("rjags")
library("reshape2")
set.seed(2021)



# start_time <- Sys.time()

#### ---------------------------- Step1: regressions ---------------------------- ####
data_all <- read.csv("/Users/dr/Desktop/ms_thesis/traits/cleaned_pheno_geno_data.whr.csv", stringsAsFactors = FALSE)
dim(data_all)   # 216411    538
colnames(data_all)


lo_id <- c(rep(1:20, each=26), c(20, 20))
lo_id <- sample(lo_id, size=length(lo_id))

first_snp_index <- 17



sens_median <- rep(-9, 20)
sens_mean <- rep(-9, 20)
sens_HDI1 <- rep(-9, 20)
sens_HDI2 <- rep(-9, 20)
sens_QI1 <- rep(-9, 20)
sens_QI2 <- rep(-9, 20)

for (kk in 1:20) {
  print(paste0("At iteration: ", kk))
  data <- data_all[c(1:(first_snp_index-1), which(lo_id != kk) + first_snp_index-1)]
  #ncol(data)
  
  
  colnames(data)[first_snp_index:length(colnames(data))]
  (num_snps <- length(colnames(data)) - first_snp_index + 1)   
  num_snps == length(colnames(data)[first_snp_index:length(colnames(data))])
  
  
  
  ## correlation heatmap of the SNPs
  snp_corr <- cor(data[,first_snp_index:length(colnames(data))])
  snp_corr.melt <- melt(snp_corr)
  ggplot(data = snp_corr.melt, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) + coord_fixed()
  
  # pdf("/Users/dr/Desktop/ms_thesis/data_application/plots/snp_corr.2.pdf")
  # ggplot(data = snp_corr.melt, aes(Var2, Var1, fill = value))+
  #   geom_tile(color = "white") +
  #   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
  #                        midpoint = 0, limit = c(-1,1), space = "Lab", 
  #                        name="Pearson\nCorrelation") +
  #   theme_minimal() + 
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
  #                                    size = 12, hjust = 1)) + coord_fixed()
  # dev.off()
  
  ## scale the outcome and mediators
  colnames(data)[1:(first_snp_index-1)]
  data$waist_hip_ratio <- data$waist_circumference / data$hip_circumference
  data$waist_hip_ratio <- scale(data$waist_hip_ratio)
  data$bmi <- scale(data$bmi)
  
  
  ## flip the SNPs so that they all positively affect the outcome trait
  snp_names <- colnames(data)[first_snp_index:(first_snp_index + num_snps - 1)]
  snp_string <- do.call(paste, c(as.list(snp_names), sep = "+"))
  outcome_eq_marginal <-  paste("waist_hip_ratio~", snp_string, 
                                "+sex+age+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10")
  
  outcome_model_marginal <- lm(outcome_eq_marginal, data = data)
  summary(outcome_model_marginal)
  head(summary(outcome_model_marginal)$coefficients, n = 12)
  tail(summary(outcome_model_marginal)$coefficients, n = 12)
  fip_index <- which(outcome_model_marginal$coefficients[2:(2+num_snps-1)] < 0) + first_snp_index - 1
  
  for (i in 1:length(fip_index)) {
    index <- fip_index[i]
    data[,index][data[,index] == 0 ] <- -2
    data[,index][data[,index] == 2 ] <- 0
    data[,index][data[,index] == -2 ] <- 2
  }
  
  ## check if all positive effect
  outcome_model_marginal.check <- lm(outcome_eq_marginal, data = data)
  length(which(outcome_model_marginal.check$coefficients[2:(2+num_snps-1)] < 0) ) == 0 
  
  hist(outcome_model_marginal.check$coefficients[2:(2+num_snps-1)], breaks = 40)
  
  
  
  
  #### meditor models
  colnames(data)[1:(first_snp_index-1)]
  ## the mediators are: waist_hip_ratio, bmi, met_min_per_week, alcohol_day
  
  bmi_eq <- paste("bmi~", snp_string,
                  "+sex+age+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10")
  bmi_model <- lm(bmi_eq, data = data)
  summary(bmi_model)
  head(summary(bmi_model)$coefficients, n = 12)
  tail(summary(bmi_model)$coefficients, n = 12)
  all_a <- c(bmi_model$coefficients[2:(2+num_snps-1)])
  
  hist(all_a, breaks = 40)
  # pdf("/Users/dr/Desktop/ms_thesis/data_application/out/bmi.sbp.all_a.pdf")
  # hist(all_a, breaks = 30)
  # dev.off()
  
  
  
  #### outcome model adjusting for the mediators
  colnames(data)[1:(first_snp_index-1)]
  outcome_eq <- paste("waist_hip_ratio~", snp_string, 
                      "+bmi", 
                      "+sex+age+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10")
  
  outcome_model <- lm(outcome_eq, data = data)
  tail(summary(outcome_model)$coefficient, 20)
  all_c <- outcome_model$coefficients[2:(2+num_snps-1)]
  hist(all_c, breaks = 40)
  hist(outcome_model_marginal.check$coefficients[2:(2+num_snps-1)], breaks = 40)  # without mediators
  ori_c <- outcome_model_marginal.check$coefficients[2:(2+num_snps-1)]
  hist(all_a, breaks = 40)
  
  ## detect outlier
  out.c <- boxplot.stats(all_c, coef = 3)$out
  all_c.red <- all_c[!all_c %in% out.c]
  hist(all_c.red, breaks = 40)
  print(paste("# points removed:", length(all_c) - length(all_c.red)))
  print(all_c[all_c %in% out.c])
  
  # rs55920843_G  rs56133301_A rs114760566_A    rs998584_A  rs72959041_A   rs9792666_G rs140201358_G    rs241044_A 
  # 0.05070699    0.03778114    0.06351270    0.03404967    0.10461696    0.04013190    0.04060490    0.12956512 
  # rs55938136_G 
  # 0.13734278 
  
  out.a <- boxplot.stats(all_a, coef = 3)$out
  all_a.red <- all_a[!all_a %in% out.a]
  print(paste("# points removed:", length(all_a) - length(all_a.red)))
  print(all_a[all_a %in% out.a])
  
  # rs62106258_C  rs6743060_A  rs1558902_A   rs241044_A rs55938136_G 
  # 0.09065038   0.05831361   0.07840048  -0.19467836  -0.19422755 
  
  out <- c(out.a, out.c)
  
  
  all_a.red <- all_a[!all_a %in% out]
  hist(all_a.red, breaks = 40)
  all_c.red <- all_c[!all_c %in% out]
  hist(all_c.red, breaks = 40)
  
  # heatmap(abs(vcov(outcome_model)[2:(2+num_snps-1), 2:(2+num_snps-1)]), main = "out")
  # max(abs(vcov(outcome_model)[2:(2+num_snps-1), 2:(2+num_snps-1)]))
  # heatmap(abs(vcov(bmi_model)[2:(2+num_snps-1), 2:(2+num_snps-1)]), main = "bmi")
  # max(abs(vcov(bmi_model)[2:(2+num_snps-1), 2:(2+num_snps-1)]))
  
  
  
  #### ---------------------------- Step2: EM ---------------------------- ####
  set.seed(2021)
  
  ####### for c
  emset.c <- 15
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
        snp_em.c <- quiet(normalmixEM(all_c.red,
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
                                 "sd" = c(emSd1.c[maxI.c], emSd2.c[maxI.c]),
                                 "lambda" = c(emLambda1.c[maxI.c], emLambda2.c[maxI.c]) )
  medianModel.c <- emModel.c[[maxI.c]]
  
  ####### for a
  emset.a <- 15
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
        snp_em.a <- quiet(normalmixEM(all_a.red, 
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
  snp_em_summary.a <- data.frame("mean" = c(emMu1.a[maxI.a], emMu2.a[maxI.a]), 
                                 "sd" = c(emSd1.a[maxI.a], emSd2.a[maxI.a]), 
                                 "lambda" = c(emLambda1.a[maxI.a], emLambda2.a[maxI.a]) )
  medianModel.a <- emModel.a[[maxI.a]]
  
  # # plot the two centers
  # pdf("/Users/dr/Desktop/ms_thesis/data_application/plots/effect_hist.pdf")
  # par(mfrow=c(2,2))
  # hist(all_a, breaks = 40, main = "A) Histogram of a*", xlab = NA)
  # hist(all_a.red, breaks = 40, main = "B) Histogram of a*: outliers removed", xlab = NA, ylab = NA)
  # abline(v = snp_em_summary.a$mean, col = "red")
  # 
  # hist(all_c, breaks = 40, main = "C) Histogram of c*", xlab = NA)
  # hist(all_c.red, breaks = 40, main = "D) Histogram of c*: outliers removed", xlab = NA, ylab = NA)
  # abline(v = snp_em_summary.c$mean, col = "red")
  # 
  # dev.off()
  # par(mfrow=c(1,1))
  
  par(mfrow=c(2,2))
  hist(all_a, breaks = 40, main = "A) Histogram of a*")
  hist(all_a.red, breaks = 40, main = "B) Histogram of a*: outliers removed")
  abline(v = snp_em_summary.a$mean, col = "red")
  
  hist(all_c, breaks = 40, main = "C) Histogram of c*")
  hist(all_c.red, breaks = 40, main = "D) Histogram of c*: outliers removed")
  abline(v = snp_em_summary.c$mean, col = "red")
  par(mfrow=c(1,1))
  
  
  par(mfrow=c(1,2))
  hist(ori_c, breaks = 40, main = "Marginal")
  hist(all_c, breaks = 40, main = "Adj_bmi")
  par(mfrow=c(1,1))
  
  #### ---------------------------- Step3: GMM MCMC ---------------------------- ####
  ### model specification
  RNGkind("L'Ecuyer-CMRG")
  set.seed(2021)
  mu_a <- snp_em_summary.a$mean[2]
  sd_a2 <- snp_em_summary.a$sd[2]
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
                    sd_c = sd(all_c),
                    n = length(all_c),
                    pi = pi)
  
  inits_jags <- list(bH = 0, .RNG.name = "base::Wichmann-Hill", .RNG.seed = 2021)
  
  params <- c("bH")
  mod <- jags.model(textConnection(model_string), data=data_jags, n.chains=1, inits = inits_jags)
  
  ## burn in
  update(mod, n.iter=5000)
  
  ## actual chain
  mod_sim <- coda.samples(model=mod,
                          variable.names=params,
                          n.iter=30000)
  mod_csim <- as.mcmc(do.call(rbind, mod_sim))
  
  hist(mod_csim)
  plot(1:length(mod_csim), mod_csim)
  
  
  
  #bH_path <- mod_csim[,1]
  #pi1_path <- mod_csim[,3]
  #pi2_path <- mod_csim[,2]
  
  ## convergence diagnostics
  plot(mod_sim, ask=TRUE)
  autocorr.diag(mod_sim)
  autocorr.plot(mod_sim, lag.max = 50)
  effectiveSize(mod_sim)
  raftery.diag(mod_csim, q = 0.05)
  raftery.diag(mod_csim, q = 0.95)
  raftery.diag(mod_csim, q = 0.025)
  raftery.diag(mod_csim, q = 0.975)
  
  
  ### estimation for bH
  (b_hat.median <- quantile(mod_csim[,1], 0.5))   # 1.555527 
  (b_hat.mean <- mean(mod_csim[,1]))              # 1.556179
  
  # 90% quantile QI
  (b.low1 <- quantile(mod_csim[,1], 0.05))        # 1.468258   
  (b.high1 <- quantile(mod_csim[,1], 0.95))       # 1.646895
  
  (b.low1b <- quantile(mod_csim[,1], 0.025))      # 1.452397  
  (b.high1b <- quantile(mod_csim[,1], 0.975))     # 1.665871  
  
  # 90% HDI
  (b.low2 <- hdi(mod_csim[,1], credMass = 0.9)[1])     # 1.465825
  (b.high2 <- hdi(mod_csim[,1], credMass = 0.9)[2])    # 1.644045
  
  (b.low2b <- hdi(mod_csim[,1], credMass = 0.95)[1])   # 1.451025
  (b.high2b <- hdi(mod_csim[,1], credMass = 0.95)[2])  # 1.663901
  
  
  sens_median[kk] <- b_hat.median
  sens_mean[kk] <- b_hat.mean
  sens_HDI1[kk] <- b.low2
  sens_HDI2[kk] <- b.high2 
  sens_QI1[kk] <- b.low1
  sens_QI2[kk] <- b.high1
}

df <- data.frame("sens_median"=sens_median, "sens_mean"=sens_mean,
                 "sens_HDI1"=sens_HDI1, "sens_HDI2"=sens_HDI2,
                 "sens_QI1"=sens_QI1, "sens_QI2"=sens_QI2)

write.csv(df, "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/data_application_scripts/df_sensitivity.csv", row.names = FALSE)





# #### estimate BMI's effect on WHR ####
# b_eq <- "waist_hip_ratio~bmi+sex+age+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10"
# b_model <- lm(b_eq, data = data)
# summary(b_model)$coefficient
# summary(b_model)$coefficient[2,]
# # 0.3834024
# 
# confint(b_model, "bmi", level = 0.95)
# # 2.5 %    97.5 %
# # 0.380734 0.3860708

# end_time <- Sys.time()
# end_time - start_time




