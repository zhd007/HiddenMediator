# source("/Users/dr/Desktop/mediation_summary/cluster_simulations/summary_functions.R")
source("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/summary_function.updated.R")

#### -------------------------- 70snp sd1-3 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/sd1-3.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/trend/sd1-3.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/summary.sd1-3.snp70.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table1 <- summary_table(record1, record2, record3, c("sdRatio=1:3_SNP70_0.02", "sdRatio=1:3_SNP70_0.25", "sdRatio=1:3_SNP70_0.5"))






#### -------------------------- 500snp m=10 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/sd1-3.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/trend/sd1-3.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/summary.sd1-3.snp500.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table2 <- summary_table(record1, record2, record3, c("sdRatio=1:3_SNP500_0.02", "sdRatio=1:3_SNP500_0.25", "sdRatio=1:3_SNP500_0.5"))


#### -------------------------- 70snp sd1-1 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/sd1-1.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/trend/sd1-1.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/summary.sd1-1.snp70.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table3 <- summary_table(record1, record2, record3, c("sdRatio=1:1_SNP70_0.02", "sdRatio=1:1_SNP70_0.25", "sdRatio=1:1_SNP70_0.5"))







#### -------------------------- 500snp m=1 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/sd1-1.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/trend/sd1-1.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/summary.sd1-1.snp500.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table4 <- summary_table(record1, record2, record3, c("sdRatio=1:1_SNP500_0.02", "sdRatio=1:1_SNP500_0.25", "sdRatio=1:1_SNP500_0.5"))





#### -------------------------- output table -------------------------- ####
table <- rbind(table1, table2, table3, table4)
write.csv(table, "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting6/summary_table.csv")


