#!/bin/bash

#BSUB -J base_case              # job name
#BSUB -o /home/dingzh/t2d_mediation/continuous/base_case/base_case.out 
#BSUB -e /home/dingzh/t2d_mediation/continuous/base_case/base_case.error   


module load R/3.6.3
module load JAGS/4.3.0-gcc-6

Rscript --no-save /home/dingzh/t2d_mediation/continuous/base_case/base_case.snp70.R
