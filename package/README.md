# HiddenMediator

An R package for estimating the effect size of a potential hidden mediator between a continuous trait of interest and its associated SNPs.

To install the package, download HiddenMediator_0.1.0.tar.gz and run the following command in R:

```
install.packages("/local_path/HiddenMediator_0.1.0.tar.gz", repos = NULL, type = "source")
```


Below, we will use an example with three know mediators and one hidden mediator to illustrate how to call the main function of the package. The hidden mediator's true effect size is 0.3. We will start by simulating the data under the true model. 



```
## an simple example with three know mediators and one hidden mediator
library(HiddenMediator)
set.seed(1000)
true_hidden <- 0.3                # the true effect size of the hidden mediator
num_snp <-   70                   # number of snp in the model
freq <- c(0.5, 0.9, 0.3, 0.6)     # freq of m1, m2, m_Hidden
known_beta <- c(0.4, 0.2, 0.25)   # beta1 and beta2, beta3, beat4, beta5
sample_size <- 100000
```

First, simulate the SNPs.

```
## the snp names
snp_col <- c()
for (i in 1:num_snp) {
  snp_col[i] <- paste("snp", i, sep="")
}
df.snp <- data.frame(matrix(vector(), sample_size, length(snp_col),
                            dimnames=list(c(), snp_col)),
                     stringsAsFactors=F)
## generate snps
for (i in 1:length(snp_col)) {
  ## frequency of the SNPs
  w_freq <- runif(n = 1, min = 0.1, max = 0.9)
  cur_snp_v <- sample(c(0, 1, 2), size = sample_size, replace = TRUE,
                      prob = c((1-w_freq)^2, 2*w_freq*(1-w_freq), w_freq^2))
  cur_snp_name <- paste(snp_col[i])
  assign(cur_snp_name, cur_snp_v)
  df.snp[[cur_snp_name]] <- cur_snp_v
}

upper_level_effect <- rnorm(n = length(known_beta)+1, 0.2, 0)
```

Then generate the three known mediators and the hidden mediator. Note that the hidden mediator is unobserved in reality so we will pretent that we do not have the data of the hidden mediator when estimating the hidden mediator's effect size below.


```
############# known: m1
# a vector of indicator of presence
idc1 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-freq[1], freq[1]))
SNP_beta1 <- rnorm(n = num_snp, mean = 0.2, sd = 0.08)     # set the mean SNP effect to 0.2
m1 <- (data.matrix(df.snp) %*% (idc1 * SNP_beta1))[,1]
m1 <- m1 + rnorm(sample_size, mean = 0, sd = 1) + 5

############# known: m2
# a vector of indicator of presence
idc2 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-freq[2], freq[2]))
SNP_beta2 <- rnorm(n = num_snp, mean = 0.2, sd = 0.08)     # set the mean SNP effect to 0.2
m2 <- (data.matrix(df.snp) %*% (idc2 * SNP_beta2))[,1]
m2 <- m2 + rnorm(sample_size, mean = 0, sd = 1) + 10

############# known: m3
# a vector of indicator of presence
idc3 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-freq[3], freq[3]))
SNP_beta3 <- rnorm(n = num_snp, mean = 0.2, sd = 0.08)     # set the mean SNP effect to 0.2
m3 <- (data.matrix(df.snp) %*% (idc3 * SNP_beta3))[,1]
m3 <- m3 + rnorm(sample_size, mean = 0, sd = 1) + 2

############# hidden: m4
# a vector of indicator of presence
idc4 <- sample(c(0, 1), size = num_snp, replace = TRUE, prob = c(1-freq[4], freq[4]))
SNP_beta4 <- rnorm(n = num_snp, mean = 0.2, sd = 0.08)     # set the mean SNP effect to 0.2
m4 <- (data.matrix(df.snp) %*% (idc4 * SNP_beta4))[,1]
m4 <- m4 + rnorm(sample_size, mean = 0, sd = 1) + 8

############# covariates
covariate1 <- rnorm(sample_size, mean = 7, sd = 0.5)
covariate2 <- rnorm(sample_size, mean = 4, sd = 0.4)
```

Next, we generate the outcome trait based on the true effects.

```
############## outcome
y <- known_beta[1] * m1 + known_beta[2] * m2 + known_beta[3] * m3 +
  true_hidden * m4 + 0.8 * covariate1 - 0.3 * covariate2 +
  rnorm(sample_size, mean = 0, sd = 0.2)
```

Before we call the function to estimate the hidden mediator's effect size, we put

```
## put together with the SNPs
df.snp$m1 <- m1
df.snp$m2 <- m2
df.snp$m3 <- m3
df.snp$m4 <- m4
df.snp$y <- y
df.snp$covariate1 <- covariate1
df.snp$covariate2 <- covariate2

## 70 SNPs
outcome <- df.snp$y
snps <- as.matrix(df.snp[,1:70])
mediators <- as.matrix(df.snp[,71:73])
covariates <- as.matrix(df.snp[76:77])
```

Finally, we call the estimateHM function to estimate the hidden mediator's effect size.

```
result <- estimateHM(outcome, snps, mediators, covariates, em_iter = 15, c = 10000, burn = 1000, remove_outliers = FALSE)
result
```

