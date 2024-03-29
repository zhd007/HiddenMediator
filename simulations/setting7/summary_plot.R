source("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/summary_function.updated.R")


#### -------------------------- 70snp -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting7/bH0.snp70.record.csv", 
                   stringsAsFactors = FALSE)
dim(record)


## how many threw an error
length(which(record$b_hat.median == -9)) # 0

record <- record[record$b_hat.median != -9,]
record <- record[1:1000,]
dim(record) == c(1000, 21)

out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting7/summary.null.snp70.pdf"
cplot1 <- draw_plot_null_70(record, in_path)



## for table
table1<- summary_table_null(record, c("null.SNP70"))


#### -------------------------- 500snp -------------------------- ####
record <- read.csv("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting7/bH0.snp500.record.csv", 
                   stringsAsFactors = FALSE)
dim(record)


## how many threw an error
length(which(record$b_hat.median == -9)) # 0

record <- record[record$b_hat.median != -9,]
record <- record[1:1000,]
dim(record) == c(1000, 21)

out_path <- "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting7/summary.null.snp500.pdf"
cplot2 <- draw_plot_null_500(record, in_path)

## output
pdf("/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting7/summary.null.pdf", width=12, height=6)
ggarrange(cplot1, cplot2,
          ncol = 1, nrow = 2)
dev.off()

## for table
table2 <- summary_table_null(record, c("null.SNP500"))



#### -------------------------- output table -------------------------- ####
table <- rbind(table1, table2)
write.csv(table, "/Users/dr/Desktop/mediation_summary/cluster_simulations.updated/setting7/summary_table.csv")




