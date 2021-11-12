# source("/Users/dr/Desktop/mediation_summary/cluster_simulations/summary_functions.R")
source("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/summary_function.updated.R")

#### -------------------------- 70snp n=50000 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/n50000.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/trend/n50000.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary.n50000.snp70.pdf"
cplot1 <- draw_plot_70(record1, record2, record3, in_path)


## for table
table1 <- summary_table(record1, record2, record3, c("n=50000_SNP70_0.02", "n=50000_SNP70_0.25", "n=50000_SNP70_0.5"))






#### -------------------------- 500snp n=50000 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/n50000.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/trend/n50000.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary.n50000.snp500.pdf"
cplot2 <- draw_plot_500(record1, record2, record3, in_path)

## output
pdf("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary.n50000.pdf", width=12, height=6)
ggarrange(cplot1, cplot2,
          ncol = 1, nrow = 2)
dev.off()

## for table
table2 <- summary_table(record1, record2, record3, c("n=50000_SNP500_0.02", "n=50000_SNP500_0.25", "n=50000_SNP500_0.5"))

#### -------------------------- 70snp n=25000 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/n25000.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/trend/n25000.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary.n25000.snp70.pdf"
cplot1 <- draw_plot_70(record1, record2, record3, in_path)


## for table
table3 <- summary_table(record1, record2, record3, c("n=25000_SNP70_0.02", "n=25000_SNP70_0.25", "n=25000_SNP70_0.5"))






#### -------------------------- 500snp n=25000 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/n25000.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/trend/n25000.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary.n25000.snp500.pdf"
cplot2 <- draw_plot_500(record1, record2, record3, in_path)

## output
pdf("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary.n25000.pdf", width=12, height=6)
ggarrange(cplot1, cplot2,
          ncol = 1, nrow = 2)
dev.off()

## for table
table4 <- summary_table(record1, record2, record3, c("n=25000_SNP500_0.02", "n=25000_SNP500_0.25", "n=25000_SNP500_0.5"))





#### -------------------------- output table -------------------------- ####
table <- rbind(table1, table2, table3, table4)
write.csv(table, "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting3/summary_table.csv")


