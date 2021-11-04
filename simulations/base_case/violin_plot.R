draw_plot.violin <- function(record1, record2, record3, in_path) {
  
  record1$width2 <- record1$b.high2 - record1$b.low2
  record1$width1 <- record1$b.high1 - record1$b.low1
  record2$width2 <- record2$b.high2 - record2$b.low2
  record2$width1 <- record2$b.high1 - record2$b.low1
  record3$width2 <- record3$b.high2 - record3$b.low2
  record3$width1 <- record3$b.high1 - record3$b.low1
  
  ## for outliers in p1
  out_count1a <- length(which(record1$b_hat.median > 1))
  out_count1b <- length(which(record1$b_hat.mean > 1))
  
  out_count2a <- length(which(record2$b_hat.median > 1))
  out_count2b <- length(which(record2$b_hat.mean > 1))
  
  out_count3a <- length(which(record3$b_hat.median > 1))
  out_count3b <- length(which(record3$b_hat.mean > 1))
  
  ## combined three df
  record <- rbind(record1, record2, record3)
  record.median <- data.frame("value" = record$b_hat.median, "type" = "median",
                              "true_hidden" = record$true_hidden)
  record.mean <- data.frame("value" = record$b_hat.mean, "type" = "mean",
                            "true_hidden" = record$true_hidden)
  p1.df <- rbind(record.median, record.mean)
  
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_violin() + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Point Estimates") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8))
  (p1b <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_violin() + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Point Estimates: zoomed") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8) +
      coord_cartesian(ylim=c(-0.2, 1)) +
      annotate("text", label = paste(out_count1a, "            "), x = "0.02", y = 1, col = "#F8766D", size = 2.5) +
      annotate("text", label = paste("             ", out_count1b), x = "0.02", y = 1, col = "#00BFC4", size = 2.5) +
      annotate("text", label = paste(out_count2a, "             "), x = "0.25", y = 1, col = "#F8766D", size = 2.5) +
      annotate("text", label = paste("            ", out_count2b), x = "0.25", y = 1, col = "#00BFC4", size = 2.5) +
      annotate("text", label = paste(out_count3a, "             "), x = "0.5", y = 1, col = "#F8766D", size = 2.5) +
      annotate("text", label = paste("            ", out_count3b), x = "0.5", y = 1, col = "#00BFC4", size = 2.5))
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% CI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_count1a <- length(which(record1$width2 > 1.5))
  out2_count1b <- length(which(record1$width1 > 1.5))
  
  out2_count2a <- length(which(record2$width2 > 1.5))
  out2_count2b <- length(which(record2$width1 > 1.5))
  
  out2_count3a <- length(which(record3$width2 > 1.5))
  out2_count3b <- length(which(record3$width1 > 1.5))
  
  p2.df <- rbind(record.hdi, record.ci)
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_violin() + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Interval Width"))
  (p2b <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_violin() + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Interval Width: zoomed") + 
      coord_cartesian(ylim=c(0, 1.5)) +
      annotate("text", label = paste(out2_count1a, "            "), x = "0.02", y = 1.5, col = "#F8766D", size = 2.5) +
      annotate("text", label = paste("             ", out2_count1b), x = "0.02", y = 1.5, col = "#00BFC4", size = 2.5) +
      annotate("text", label = paste(out2_count2a, "             "), x = "0.25", y = 1.5, col = "#F8766D", size = 2.5) +
      annotate("text", label = paste("            ", out2_count2b), x = "0.25", y = 1.5, col = "#00BFC4", size = 2.5) +
      annotate("text", label = paste(out2_count3a, "             "), x = "0.5", y = 1.5, col = "#F8766D", size = 2.5) +
      annotate("text", label = paste("            ", out2_count3b), x = "0.5", y = 1.5, col = "#00BFC4", size = 2.5)) 
  
  ## plot3,4 
  record200 <- read.csv(in_path, stringsAsFactors = FALSE)
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  
  (p3a <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Median & 90% HDI") +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  (p3b <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Median & 90% HDI: zoomed") +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  (p4a <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture1.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Mean & 90% CI")  +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  (p4b <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Mean & 90% CI: zoomed")  +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  
  
  ## put together
  return(ggarrange(p1a, p1b, p2a, p2b, p3a, p3b, p4a, p4b, 
                   labels = c("A", "", "B", "", "C", "", "D", ""),
                   ncol = 4, nrow = 2) )
  
}

violin_plot <- draw_plot.violin(record1, record2, record3, in_path)
pdf("/Users/dr/Desktop/mediation_summary/cluster_simulations/base_case/summary.base_case.snp70.violin.pdf", width=15, height=6)
violin_plot
dev.off()





