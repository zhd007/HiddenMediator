# source("/Users/dr/Desktop/mediation_summary/cluster_simulations/summary_functions.R")
source("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/summary_function.updated.R")

#### -------------------------- 20snp -------------------------- ####
record <- read.csv("/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/snp20.record.csv", 
                   stringsAsFactors = FALSE)
dim(record)

record1 <- record[record$true_hidden == 0.02,]
record2 <- record[record$true_hidden == 0.25,]
record3 <- record[record$true_hidden == 0.5,]

## how many threw an error
length(which(record$b_hat.median == -9))
1050 - nrow(record1)  # 0
1050 - nrow(record2)  # 0
1050 - nrow(record3)  # 0

record1 <- record1[record1$b_hat.median != -9,]
record2 <- record2[record2$b_hat.median != -9,]
record3 <- record3[record3$b_hat.median != -9,]

record1 <- record1[1:1000,]
record2 <- record2[1:1000,]
record3 <- record3[1:1000,]


record <- rbind(record1, record2, record3)
dim(record) == c(3000, 21)

in_path <- "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/trend/snp20.record.csv"
out_path <- "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/summary.snp20.pdf"
cplot1 <- draw_plot_70(record1, record2, record3, in_path)

## for table
table1 <- summary_table(record1, record2, record3, c("SNP20_0.02", "SNP20_0.25", "SNP20_0.5"))






#### -------------------------- 40snp -------------------------- ####
record <- read.csv("/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/snp40.record.csv", 
                   stringsAsFactors = FALSE)
dim(record)

record1 <- record[record$true_hidden == 0.02,]
record2 <- record[record$true_hidden == 0.25,]
record3 <- record[record$true_hidden == 0.5,]

## how many threw an error
length(which(record$b_hat.median == -9))
1050 - nrow(record1)  # 0
1050 - nrow(record2)  # 0
1050 - nrow(record3)  # 0

record1 <- record1[record1$b_hat.median != -9,]
record2 <- record2[record2$b_hat.median != -9,]
record3 <- record3[record3$b_hat.median != -9,]

record1 <- record1[1:1000,]
record2 <- record2[1:1000,]
record3 <- record3[1:1000,]


record <- rbind(record1, record2, record3)
dim(record) == c(3000, 21)

in_path <- "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/trend/snp40.record.csv"
out_path <- "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/summary.snp40.pdf"
cplot2 <- draw_plot_500(record1, record2, record3, in_path)


## for table
table2 <- summary_table(record1, record2, record3, c("SNP40_0.02", "SNP40_0.25", "SNP40_0.5"))

pdf("/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/summary.SNP_vary_partial.pdf", width=12, height=6)
ggarrange(cplot1, cplot2, 
          ncol = 1, nrow = 2)
dev.off()



#### -------------------------- 700snp -------------------------- ####
record <- read.csv("/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/snp700.record.csv", 
                   stringsAsFactors = FALSE)
dim(record)

record1 <- record[record$true_hidden == 0.02,]
record2 <- record[record$true_hidden == 0.25,]
record3 <- record[record$true_hidden == 0.5,]

## how many threw an error
length(which(record$b_hat.median == -9))
1050 - nrow(record1)  # 0
1050 - nrow(record2)  # 0
1050 - nrow(record3)  # 0

record1 <- record1[record1$b_hat.median != -9,]
record2 <- record2[record2$b_hat.median != -9,]
record3 <- record3[record3$b_hat.median != -9,]

record1 <- record1[1:1000,]
record2 <- record2[1:1000,]
record3 <- record3[1:1000,]


record <- rbind(record1, record2, record3)
dim(record) == c(3000, 21)

in_path <- "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/trend/snp700.record.csv"
out_path <- "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/summary.snp700.pdf"
cplot3 <- draw_plot_700(record1, record2, record3, in_path)



## for table
table3 <- summary_table(record1, record2, record3, c("SNP700_0.02", "SNP700_0.25", "SNP700_0.5"))







## output
pdf("/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/summary.SNP_vary.pdf", width=12, height=9)
ggarrange(cplot1, cplot2, cplot3,
          ncol = 1, nrow = 3)
dev.off()


#### -------------------------- output table -------------------------- ####
table <- rbind(table1, table2, table3)
write.csv(table, "/Users/dr/Desktop/ms_thesis/manuscript/BMC_submission/cluster_simulations.updated/setting8/summary_table.csv")


