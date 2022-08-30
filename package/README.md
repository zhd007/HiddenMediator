HiddenMediator
================
Zhuoran Ding
4/3/2022

An R package for estimating the effect size of a potential hidden mediator between a continuous trait of interest and its associated SNPs.

To install the package, download HiddenMediator\_0.1.0.tar.gz and run the following command in R:

``` r
install.packages("/<local_path>/HiddenMediator_0.1.0.tar.gz", repos = NULL, type = "source")
```

<br/>

Below, we will use an example with three know mediators and one hidden mediator to illustrate how to call the main function of the package. The hidden mediator's true effect size is 0.3. We will start by simulating the data under the true model.

``` r
## an simple example with three know mediators and one hidden mediator
library(HiddenMediator)
set.seed(1000)
true_hidden <- 0.3                # the true effect size of the hidden mediator
num_snp <-   70                   # number of snp in the model
freq <- c(0.5, 0.9, 0.3, 0.6)     # freq of m1, m2, m_Hidden
known_beta <- c(0.4, 0.2, 0.25)   # beta1 and beta2, beta3, beat4, beta5
sample_size <- 100000
```

<br/>

First, simulate the SNPs.

``` r
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

<br/>

Then generate the three known mediators and the hidden mediator. Note that the hidden mediator is unobserved in reality so we will pretent that we do not have the data of the hidden mediator when estimating the hidden mediator's effect size below.

``` r
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

<br/>

Next, we generate the outcome trait based on the true effects.

``` r
############## outcome
y <- known_beta[1] * m1 + known_beta[2] * m2 + known_beta[3] * m3 +
  true_hidden * m4 + 0.8 * covariate1 - 0.3 * covariate2 +
  rnorm(sample_size, mean = 0, sd = 0.2)
```

<br/>

Before we call the function to estimate the hidden mediator's effect size, we gather the SNPs, the known mediators, and the covariates into three matrices.

``` r
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

<br/>

Finally, we call the `estimateHM` function to estimate the hidden mediator's effect size.

``` r
result <- estimateHM(outcome, snps, mediators, covariates, em_iter = 15, c = 10000, burn = 1000, remove_outliers = FALSE)
```

![](example_files/figure-markdown_github/unnamed-chunk-7-1.png)![](example_files/figure-markdown_github/unnamed-chunk-7-2.png)

    ## Compiling model graph
    ##    Resolving undeclared variables
    ##    Allocating nodes
    ## Graph information:
    ##    Observed stochastic nodes: 70
    ##    Unobserved stochastic nodes: 71
    ##    Total graph size: 298
    ## 
    ## Initializing model

``` r
result
```

    ## $bH_median
    ##      50% 
    ## 0.306968 
    ## 
    ## $bH_mean
    ## [1] 0.3067283
    ## 
    ## $bH_mode
    ## [1] 0.3083267
    ## 
    ## $GMM_summary_c
    ##            mean          sd    lambda
    ## 1 -0.0001180267 0.001736451 0.3683472
    ## 2  0.0655954089 0.025108838 0.6316528
    ## 
    ## $GMM_summary_a
    ##        mean          sd    lambda
    ## 1 0.0004878 0.005781223 0.4122229
    ## 2 0.2136547 0.076184650 0.5877771
    ## 
    ## $HDI
    ##        5%       95% 
    ## 0.2764013 0.3357038 
    ## 
    ## $QI
    ##        5%       95% 
    ## 0.2768658 0.3362223 
    ## 
    ## $flipped
    ## [1] FALSE

The result shows that the posterior median estimate is 0.3070914, the posterior mean estimate is 0.3072435, and the posterior mode estimate is 0.3051627, which are all close to the true value 0.3. The 90% highest density interval based on the posterior distribution is (0.2794063, 0.3395431), and the 90% quantile-based interval is (0.2772821, 0.3375148). The estimated Gaussian mixture model paramters for **a**<sup>\*</sup> and **c**<sup>\*</sup> are stored in `GMM_summary_c` and `GMM_summary_a`. In this case, `flipped` is `FALSE`, which indicates that the MCMC procedure was not conducted again on the label flipped GMM. Please see the manuscript for the details.

<br/>

``` r
 sessionInfo()
```

    ## R version 4.0.5 (2021-03-31)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Mojave 10.14.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] HiddenMediator_0.1.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] highr_0.9         compiler_4.0.5    pillar_1.6.3      mixtools_1.2.0   
    ##  [5] iterators_1.0.13  tools_4.0.5       digest_0.6.28     lattice_0.20-44  
    ##  [9] jsonlite_1.7.2    evaluate_0.14     lifecycle_1.0.1   tibble_3.1.5     
    ## [13] pkgconfig_2.0.3   rlang_0.4.11      Matrix_1.3-4      foreach_1.5.1    
    ## [17] cli_3.0.1         rstudioapi_0.13   curl_4.3.2        yaml_2.2.1       
    ## [21] parallel_4.0.5    xfun_0.25         fastmap_1.1.0     coda_0.19-4      
    ## [25] withr_2.4.2       dplyr_1.0.7       stringr_1.4.0     knitr_1.33       
    ## [29] rjags_4-10        generics_0.1.0    vctrs_0.3.8       segmented_1.3-4  
    ## [33] grid_4.0.5        RPushbullet_0.3.4 tidyselect_1.1.1  glue_1.4.2       
    ## [37] R6_2.5.1          fansi_0.5.0       pbapply_1.4-3     survival_3.2-13  
    ## [41] HDInterval_0.2.2  rmarkdown_2.10    sessioninfo_1.1.1 kernlab_0.9-29   
    ## [45] purrr_0.3.4       magrittr_2.0.1    splines_4.0.5     MASS_7.3-54      
    ## [49] codetools_0.2-18  htmltools_0.5.2   ellipsis_0.3.2    utf8_1.2.2       
    ## [53] stringi_1.7.4     SimDesign_2.7.1   crayon_1.4.1
