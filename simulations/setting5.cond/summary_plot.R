# source("/Users/dr/Desktop/mediation_summary/cluster_simulations/summary_functions.R")
source("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/summary_function.updated.R")

#### -------------------------- 70snp m1-0.3-m2 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.3-m2.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.3-m2.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.3-m2.snp70.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table1 <- summary_table(record1, record2, record3, c("cond_m1-0.3-m2_SNP70_0.02", "cond_m1-0.3-m2_SNP70_0.25", "cond_m1-0.3-m2_SNP70_0.5"))

#### -------------------------- 500snp m1-0.3-m2 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.3-m2.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.3-m2.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.3-m2.snp500.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table2 <- summary_table(record1, record2, record3, c("cond_m1-0.3-m2_SNP500_0.02", "cond_m1-0.3-m2_SNP500_0.25", "cond_m1-0.3-m2_SNP500_0.5"))

#### -------------------------- 70snp m1-0.9-m2 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.9-m2.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.9-m2.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.9-m2.snp70.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table3 <- summary_table(record1, record2, record3, c("cond_m1-0.9-m2_SNP70_0.02", "cond_m1-0.9-m2_SNP70_0.25", "cond_m1-0.9-m2_SNP70_0.5"))

#### -------------------------- 500snp m1-0.9-m2 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.9-m2.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.9-m2.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.9-m2.snp500.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table4 <- summary_table(record1, record2, record3, c("cond_m1-0.9-m2_SNP500_0.02", "cond_m1-0.9-m2_SNP500_0.25", "cond_m1-0.9-m2_SNP500_0.5"))



#### -------------------------- 70snp m1-0.3-m2.m3-0.2-m4 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.3-m2.m3-0.2-m4.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.3-m2.m3-0.2-m4.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.3-m2.m3-0.2-m4.snp70.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table5 <- summary_table(record1, record2, record3, c("cond_m1-0.3-m2_m3-0.2-m4_SNP70_0.02", "cond_m1-0.3-m2_m3-0.2-m4_SNP70_0.25", "cond_m1-0.3-m2_m3-0.2-m4_SNP70_0.5"))

#### -------------------------- 500snp m1-0.3-m2.m3-0.2-m4 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.3-m2.m3-0.2-m4.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.3-m2.m3-0.2-m4.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.3-m2.m3-0.2-m4.snp500.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table6 <- summary_table(record1, record2, record3, c("cond_m1-0.3-m2_m3-0.2-m4_SNP500_0.02", "cond_m1-0.3-m2_m3-0.2-m4_SNP500_0.25", "cond_m1-0.3-m2_m3-0.2-m4_SNP500_0.5"))



#### -------------------------- 70snp m1-0.5-m2.m3-0.4-m4 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.5-m2.m3-0.4-m4.snp70.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.5-m2.m3-0.4-m4.snp70.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.5-m2.m3-0.4-m4.snp70.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table7 <- summary_table(record1, record2, record3, c("cond_m1-0.5-m2_m3-0.4-m4_SNP70_0.02", "cond_m1-0.5-m2_m3-0.4-m4_SNP70_0.25", "cond_m1-0.5-m2_m3-0.4-m4_SNP70_0.5"))

#### -------------------------- 500snp m1-0.3-m2.m3-0.2-m4 -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/m1-0.5-m2.m3-0.4-m4.snp500.record.csv", 
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

in_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/trend/m1-0.5-m2.m3-0.4-m4.snp500.record.csv"
out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary.m1-0.5-m2.m3-0.4-m4.snp500.pdf"
cplot <- draw_plot_no_embed(record1, record2, record3, in_path)

## output
pdf(out_path, width=25, height=5)
cplot 
dev.off()

## for table
table8 <- summary_table(record1, record2, record3, c("cond_m1-0.5-m2_m3-0.4-m4_SNP500_0.02", "cond_m1-0.5-m2_m3-0.4-m4_SNP500_0.25", "cond_m1-0.5-m2_m3-0.4-m4_SNP500_0.5"))



#### -------------------------- output table -------------------------- ####
table <- rbind(table1, table2, table3, table4, table5, table6, table7, table8)
write.csv(table, "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting5.cond/summary_table.csv")


