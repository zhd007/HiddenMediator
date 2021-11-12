library(ggplot2)
library(ggpubr)



#### -------------------------- plot function -------------------------- ####
draw_plot <- function(record1, record2, record3, in_path) {
  
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
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ylab("")  +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8))
  (p1b <- ggplot(p1.df.sub, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Point Estimates") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8) +
      #coord_cartesian(ylim=c(-0.2, 1)) +
      ylim(-0.1, 1)   )
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_count1a <- length(which(record1$width2 > 1.5))
  out2_count1b <- length(which(record1$width1 > 1.5))
  
  out2_count2a <- length(which(record2$width2 > 1.5))
  out2_count2b <- length(which(record2$width1 > 1.5))
  
  out2_count3a <- length(which(record3$width2 > 1.5))
  out2_count3b <- length(which(record3$width1 > 1.5))
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      xlab(expression(b[H])) )
  (p2b <- ggplot(p2.df.sub, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Interval Width: zoomed") + 
      #coord_cartesian(ylim=c(-0.5, 1.5)) +
      ylim(-0.2, 1.65) +
      geom_abline(intercept = 1.5, slope = 0) +
      annotate("text", label = "# Above 1.5:", x = 0.7, y = 1.65, size = 4) +
      annotate("text", label = paste(out2_count1a, "        "), x = "0.02", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count1b), x = "0.02", y = 1.575, col = "#00BFC4", size = 4) +
      annotate("text", label = paste(out2_count2a, "        "), x = "0.25", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count2b), x = "0.25", y = 1.575, col = "#00BFC4", size = 4) +
      annotate("text", label = paste(out2_count3a, "        "), x = "0.5", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count3b), x = "0.5", y = 1.575, col = "#00BFC4", size = 4) ) 
  
  
  ## plot3,4 
  record200 <- read.csv(in_path, stringsAsFactors = FALSE)
  record200 <- record200[record200$b_hat.median != 0,]
  #record200 <- record200[1:200,]
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  
  (p3a <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      xlab(expression(b[H])) +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()))
  (p3b <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.2, 1.0), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Mean & 90% HDI") +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()) )
  (p4a <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture1.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      xlab(expression(b[H])) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()))
  (p4b <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.2, 1.0), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Median & 90% QI")  +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  
  
  ## put together
  return(ggarrange(p1b, p2b, p3b, p4b,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}


draw_plot_embed <- function(record1, record2, record3, in_path) {
  
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
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ylab("")  +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8))
  (p1b <- ggplot(p1.df.sub, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Point Estimates") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8) +
      #coord_cartesian(ylim=c(-0.2, 1)) +
      ylim(-0.1, 1)   )
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_count1a <- length(which(record1$width2 > 1.5))
  out2_count1b <- length(which(record1$width1 > 1.5))
  
  out2_count2a <- length(which(record2$width2 > 1.5))
  out2_count2b <- length(which(record2$width1 > 1.5))
  
  out2_count3a <- length(which(record3$width2 > 1.5))
  out2_count3b <- length(which(record3$width1 > 1.5))
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      xlab(expression(b[H])) )
  (p2b <- ggplot(p2.df.sub, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Interval Width: zoomed") + 
      #coord_cartesian(ylim=c(-0.5, 1.5)) +
      ylim(-0.2, 1.65) +
      geom_abline(intercept = 1.5, slope = 0) +
      annotate("text", label = "# Above 1.5:", x = 0.7, y = 1.65, size = 4) +
      annotate("text", label = paste(out2_count1a, "        "), x = "0.02", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count1b), x = "0.02", y = 1.575, col = "#00BFC4", size = 4) +
      annotate("text", label = paste(out2_count2a, "        "), x = "0.25", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count2b), x = "0.25", y = 1.575, col = "#00BFC4", size = 4) +
      annotate("text", label = paste(out2_count3a, "        "), x = "0.5", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count3b), x = "0.5", y = 1.575, col = "#00BFC4", size = 4) +
      annotation_custom(ggplotGrob(p2a), xmin = 2.27, xmax = 3.57, 
                        ymin = -0.58 + 1.33, ymax = 0.26 + 1.23)) 
  
  
  ## plot3,4 
  record200 <- read.csv(in_path, stringsAsFactors = FALSE)
  record200 <- record200[record200$b_hat.median != 0,]
  #record200 <- record200[1:200,]
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  
  (p3a <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      xlab(expression(b[H])) +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()))
  (p3b <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.2, 1.0), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Mean & 90% HDI") +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()) )
  (p4a <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture1.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      xlab(expression(b[H])) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()))
  (p4b <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.2, 1.0), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Median & 90% QI")  +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  
  
  ## put together
  return(ggarrange(p1b, p2b, p3b, p4b,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}


draw_plot_no_embed <- function(record1, record2, record3, in_path) {
  
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
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ylab("")  +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8))
  (p1b <- ggplot(p1.df.sub, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Point Estimates") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8) +
      #coord_cartesian(ylim=c(-0.2, 1)) +
      ylim(-0.1, 1)   )
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_count1a <- length(which(record1$width2 > 1.5))
  out2_count1b <- length(which(record1$width1 > 1.5))
  
  out2_count2a <- length(which(record2$width2 > 1.5))
  out2_count2b <- length(which(record2$width1 > 1.5))
  
  out2_count3a <- length(which(record3$width2 > 1.5))
  out2_count3b <- length(which(record3$width1 > 1.5))
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      xlab(expression(b[H])) )
  (p2b <- ggplot(p2.df.sub, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("") + ggtitle("Interval Width: zoomed") + 
      #coord_cartesian(ylim=c(-0.5, 1.5)) +
      ylim(-0.2, 1.65) +
      geom_abline(intercept = 1.5, slope = 0) +
      annotate("text", label = "# Above 1.5:", x = 0.7, y = 1.65, size = 4) +
      annotate("text", label = paste(out2_count1a, "        "), x = "0.02", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count1b), x = "0.02", y = 1.575, col = "#00BFC4", size = 4) +
      annotate("text", label = paste(out2_count2a, "        "), x = "0.25", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count2b), x = "0.25", y = 1.575, col = "#00BFC4", size = 4) +
      annotate("text", label = paste(out2_count3a, "        "), x = "0.5", y = 1.575, col = "#F8766D", size = 4) +
      annotate("text", label = paste("        ", out2_count3b), x = "0.5", y = 1.575, col = "#00BFC4", size = 4) +
      annotation_custom(ggplotGrob(p2a), xmin = 2.27, xmax = 3.57, 
                        ymin = -0.58 + 1.33, ymax = 0.26 + 1.23)) 
  
  
  ## plot3,4 
  record200 <- read.csv(in_path, stringsAsFactors = FALSE)
  record200 <- record200[record200$b_hat.median != 0,]
  #record200 <- record200[1:200,]
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  
  (p3a <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      xlab(expression(b[H])) +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()))
  (p3b <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.2, 1.0), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Mean & 90% HDI") +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()) )
  (p4a <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture1.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #coord_cartesian(ylim = c(-0.1, 1.2), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      xlab(expression(b[H])) + 
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()))
  (p4b <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.2, 1.0), xlim = c(-0.1, 0.6)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Median & 90% QI")  +
      xlab(expression(b[H])) + ylab("") +
      theme(legend.title = element_blank()))
  
  
  ## put together
  return(ggarrange(p1b, p2a, p3b, p4b,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}

draw_plot_70 <- function(record1, record2, record3, in_path) {
  
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
  
  p1.df.sub <- p1.df[p1.df$value < 1,]

  (p1b <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank(),
                                      axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ggtitle("Point Estimates") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8) +
      #coord_cartesian(ylim=c(-0.2, 1)) +
      #ylim(-0.1, 1) +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)) )
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_count1a <- length(which(record1$width2 > 1.5))
  out2_count1b <- length(which(record1$width1 > 1.5))
  
  out2_count2a <- length(which(record2$width2 > 1.5))
  out2_count2b <- length(which(record2$width1 > 1.5))
  
  out2_count3a <- length(which(record3$width2 > 1.5))
  out2_count3b <- length(which(record3$width1 > 1.5))
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]

  (p2b <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank(),
                                      axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ggtitle("Interval Width") +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)) )
  
  
  ## plot3,4 
  record200 <- read.csv(in_path, stringsAsFactors = FALSE)
  record200 <- record200[record200$b_hat.median != 0,]
  #record200 <- record200[1:200,]
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  
 
  (p3b <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.25, 0.7), xlim = c(-0.25, 0.7)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Mean & 90% HDI") +
      xlab(expression(b[H])) + 
      theme(legend.title = element_blank(),
            axis.title.y = element_blank())+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)) )

  (p4b <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.25, 0.7), xlim = c(-0.25, 0.7)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("Median & 90% QI")  +
      xlab(expression(b[H])) +
      theme(legend.title = element_blank(),
            axis.title.y = element_blank())+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  
  ## put together
  return(ggarrange(p1b, p2b, p3b, p4b,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}

draw_plot_500 <- function(record1, record2, record3, in_path) {
  
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
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  
  (p1b <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank(),
                                      axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ggtitle("") +
      geom_point(x = "0.02", y = 0.02, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.25", y = 0.25, colour="purple", shape = 95, size = 8) +
      geom_point(x = "0.5", y = 0.5, colour="purple", shape = 95, size = 8) +
      #coord_cartesian(ylim=c(-0.2, 1)) +
      #ylim(-0.1, 1)  +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)) )
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_count1a <- length(which(record1$width2 > 1.5))
  out2_count1b <- length(which(record1$width1 > 1.5))
  
  out2_count2a <- length(which(record2$width2 > 1.5))
  out2_count2b <- length(which(record2$width1 > 1.5))
  
  out2_count3a <- length(which(record3$width2 > 1.5))
  out2_count3b <- length(which(record3$width1 > 1.5))
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  
  (p2b <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + theme(legend.title = element_blank(),
                                      axis.title.y = element_blank()) +
      xlab(expression(b[H])) + ggtitle("")+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)) )
  
  
  ## plot3,4 
  record200 <- read.csv(in_path, stringsAsFactors = FALSE)
  record200 <- record200[record200$b_hat.median != 0,]
  #record200 <- record200[1:200,]
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  
  
  (p3b <- ggplot(record200, aes(y=b_hat.mean, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.25, 0.7), xlim = c(-0.25, 0.7)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("") +
      xlab(expression(b[H])) + 
      theme(legend.title = element_blank(),
            axis.title.y = element_blank())+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)) )
  
  (p4b <- ggplot(record200, aes(y=b_hat.median, x=true_hidden))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      coord_cartesian(ylim = c(-0.25, 0.7), xlim = c(-0.25, 0.7)) +
      geom_abline(intercept = 0, slope = 1, color="red") +
      ggtitle("")  +
      xlab(expression(b[H])) +
      theme(legend.title = element_blank(),
            axis.title.y = element_blank())+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  
  ## put together
  return(ggarrange(p1b, p2b, p3b, p4b,
                   labels = c("E", "F", "G", "H"),
                   ncol = 4, nrow = 1) )
  
}

#### -------------------------- table function -------------------------- ####

summary_table <- function(record1, record2, record3, row_names) {
  ## mse
  median.err70.1 <- (record1$b_hat.median - record1$true_hidden)^2
  median.rmse70.1 <- sqrt(sum(median.err70.1) / length(median.err70.1))
  median.err70.2 <- (record2$b_hat.median - record2$true_hidden)^2
  median.rmse70.2 <- sqrt(sum(median.err70.2) / length(median.err70.2))
  median.err70.3 <- (record3$b_hat.median - record3$true_hidden)^2
  median.rmse70.3 <- sqrt(sum(median.err70.3) / length(median.err70.3))
  
  mean.err70.1 <- (record1$b_hat.mean - record1$true_hidden)^2
  mean.rmse70.1 <- sqrt(sum(mean.err70.1) / length(mean.err70.1))
  mean.err70.2 <- (record2$b_hat.mean - record2$true_hidden)^2
  mean.rmse70.2 <- sqrt(sum(mean.err70.2) / length(mean.err70.2))
  mean.err70.3 <- (record3$b_hat.mean - record3$true_hidden)^2
  mean.rmse70.3 <- sqrt(sum(mean.err70.3) / length(mean.err70.3))
  
  ## outlier count
  median.out70.1 <- length(boxplot.stats(record1$b_hat.median)$out)
  median.out70.2 <- length(boxplot.stats(record2$b_hat.median)$out)
  median.out70.3 <- length(boxplot.stats(record3$b_hat.median)$out)
  
  mean.out70.1 <- length(boxplot.stats(record1$b_hat.mean)$out)
  mean.out70.2 <- length(boxplot.stats(record2$b_hat.mean)$out)
  mean.out70.3 <- length(boxplot.stats(record3$b_hat.mean)$out)
  
  ## bias
  median.bias70.1 <- mean(record1$b_hat.median - record1$true_hidden)
  median.bias70.2 <- mean(record2$b_hat.median - record2$true_hidden)
  median.bias70.3 <- mean(record3$b_hat.median - record3$true_hidden)
  
  mean.bias70.1 <- mean(record1$b_hat.mean - record1$true_hidden)
  mean.bias70.2 <- mean(record2$b_hat.mean - record2$true_hidden)
  mean.bias70.3 <- mean(record3$b_hat.mean - record3$true_hidden)
  
  ## not capture
  ci.type1Err70.1 <- length(which(record1$bH.capture1 == FALSE)) / nrow(record1)
  ci.type1Err70.2 <- length(which(record2$bH.capture1 == FALSE)) / nrow(record2)
  ci.type1Err70.3 <- length(which(record3$bH.capture1 == FALSE)) / nrow(record3)
  
  hdi.type1Err70.1 <- length(which(record1$bH.capture2 == FALSE)) / nrow(record1)
  hdi.type1Err70.2 <- length(which(record2$bH.capture2 == FALSE)) / nrow(record2)
  hdi.type1Err70.3 <- length(which(record3$bH.capture2 == FALSE)) / nrow(record3)
  
  ## power
  ci.captureZero1 <- (record1$b.low1 < 0) & (record1$b.high1 > 0)
  ci.power1 <- length(which(ci.captureZero1 == FALSE)) / length(ci.captureZero1)
  ci.captureZero2 <- (record2$b.low1 < 0) & (record2$b.high1 > 0)
  ci.power2 <- length(which(ci.captureZero2 == FALSE)) / length(ci.captureZero2)
  ci.captureZero3 <- (record3$b.low1 < 0) & (record3$b.high1 > 0)
  ci.power3 <- length(which(ci.captureZero3 == FALSE)) / length(ci.captureZero3)
  
  hdi.captureZero1 <- (record1$b.low2 < 0) & (record1$b.high2 > 0)
  hdi.power1 <- length(which(hdi.captureZero1 == FALSE)) / length(hdi.captureZero1)
  hdi.captureZero2 <- (record2$b.low2 < 0) & (record2$b.high2 > 0)
  hdi.power2 <- length(which(hdi.captureZero2 == FALSE)) / length(hdi.captureZero2)
  hdi.captureZero3 <- (record3$b.low2 < 0) & (record3$b.high2 > 0)
  hdi.power3 <- length(which(hdi.captureZero3 == FALSE)) / length(hdi.captureZero3)
  
  out <- data.frame("median.rmse" = c(median.rmse70.1, median.rmse70.2, median.rmse70.3),
                    "mean.rmse" = c(mean.rmse70.1, mean.rmse70.2, mean.rmse70.3),
                    "median.bias" = c(median.bias70.1, median.bias70.2, median.bias70.3),
                    "mean.bias" = c(mean.bias70.1, mean.bias70.2, mean.bias70.3),
                    "median.outlier" = c(median.out70.1, median.out70.2, median.out70.3),
                    "mean.outlier" = c(mean.out70.1, mean.out70.2, mean.out70.3),
                    "QI90.capture_rate" = 1 - c(ci.type1Err70.1, ci.type1Err70.2, ci.type1Err70.3),
                    "HDI90.capture_rate" = 1 - c(hdi.type1Err70.1, hdi.type1Err70.2, hdi.type1Err70.3),
                    "QI90.power" = c(ci.power1, ci.power2, ci.power3),
                    "HDI90.power" = c(hdi.power1, hdi.power2, hdi.power3))
  rownames(out) <- row_names
  return(out)
}


#### -------------------------- plot function - null 1 -------------------------- ####

draw_plot_null <- function(record, in_path) {
  
  record$width2 <- record$b.high2 - record$b.low2
  record$width1 <- record$b.high1 - record$b.low1
  
  
  ## for outliers in p1
  out_counta <- length(which(record$b_hat.median > 1))
  out_countb <- length(which(record$b_hat.mean > 1))
  
  ## combined three df
  record.median <- data.frame("value" = record$b_hat.median, "type" = "median",
                              "true_hidden" = record$true_hidden)
  record.mean <- data.frame("value" = record$b_hat.mean, "type" = "mean",
                            "true_hidden" = record$true_hidden)
  p1.df <- rbind(record.median, record.mean)
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank() ) +
      xlab(expression(b[H])) + ylab("")  +
      geom_hline(yintercept=0, col = "purple") +
      #coord_cartesian(ylim=c(-0.2, 0.2)) +
      ggtitle("Point Estimates"))
  
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_counta <- length(which(record$width2 > 1.5))
  out2_countb <- length(which(record$width1 > 1.5))
  
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank()) +
      xlab(expression(b[H]))+ ylab("")  +
      ggtitle("Interval Width") )
  
  
  ## plot3,4 
  record200 <- record
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  record200$position <- runif(n = 1000, min = -1, max = 1)
  
  (p3a <- ggplot(record200, aes(y=b_hat.median, x=position))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Median & 90% HDI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank()) +
      coord_cartesian(ylim=c(-0.5, 0.5)) +
      xlab(expression(b[H]==0))+ ylab(""))
  
  (p4a <- ggplot(record200, aes(y=b_hat.mean, x=position))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Mean & 90% QI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank())+
      coord_cartesian(ylim=c(-0.5, 0.5))   +
      xlab(expression(b[H]==0))+ ylab(""))
  
  
  
  ## put together
  return(ggarrange(p1a, p2a, p3a, p4a,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}

#### -------------------------- plot function - null 2 -------------------------- ####

draw_plot_null2 <- function(record, in_path) {
  
  record$width2 <- record$b.high2 - record$b.low2
  record$width1 <- record$b.high1 - record$b.low1
  
  
  ## for outliers in p1
  out_counta <- length(which(record$b_hat.median > 1))
  out_countb <- length(which(record$b_hat.mean > 1))
  
  ## combined three df
  record.median <- data.frame("value" = record$b_hat.median, "type" = "median",
                              "true_hidden" = record$true_hidden)
  record.mean <- data.frame("value" = record$b_hat.mean, "type" = "mean",
                            "true_hidden" = record$true_hidden)
  p1.df <- rbind(record.median, record.mean)
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("")  +
      geom_hline(yintercept=0, col = "purple") +
      #coord_cartesian(ylim=c(-0.2, 0.2)) +
      ggtitle("Point Estimates"))
  
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_counta <- length(which(record$width2 > 1.5))
  out2_countb <- length(which(record$width1 > 1.5))
  
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("")+
      ggtitle("Interval Width") )
  
  
  ## plot3,4 
  record200 <- record
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  record200$position <- runif(n = 1000, min = -1, max = 1)
  
  (p3a <- ggplot(record200, aes(y=b_hat.median, x=position))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Median & 90% HDI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank()) +
      xlab("")+ ylab("") )
  
  (p3b <- ggplot(record200, aes(y=b_hat.median, x=position))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Median & 90% HDI: zoomed") +
      theme(legend.title = element_blank(), axis.text.x = element_blank()) +
      coord_cartesian(ylim=c(-0.5, 0.5))  +
      xlab(expression(b[H]==0))+ ylab(""))
  
  (p4a <- ggplot(record200, aes(y=b_hat.mean, x=position))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Mean & 90% QI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank())  +
      xlab("")+ ylab("") )
  
  (p4b <- ggplot(record200, aes(y=b_hat.mean, x=position))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Mean & 90% QI: zoomed") +
      theme(legend.title = element_blank(), axis.text.x = element_blank())+
      coord_cartesian(ylim=c(-0.5, 0.5))   +
      xlab(expression(b[H]==0))+ ylab(""))
  
  
  
  ## put together
  return(ggarrange(p1a, p2a, 
                   ggarrange(p3a, p3b, ncol = 1, nrow = 2), 
                   ggarrange(p4a, p4b, ncol = 1, nrow = 2),
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}


#### -------------------------- plot function - null 3 -------------------------- ####

draw_plot_null3 <- function(record, in_path) {
  
  record$width2 <- record$b.high2 - record$b.low2
  record$width1 <- record$b.high1 - record$b.low1
  
  
  ## for outliers in p1
  out_counta <- length(which(record$b_hat.median > 1))
  out_countb <- length(which(record$b_hat.mean > 1))
  
  ## combined three df
  record.median <- data.frame("value" = record$b_hat.median, "type" = "median",
                              "true_hidden" = record$true_hidden)
  record.mean <- data.frame("value" = record$b_hat.mean, "type" = "mean",
                            "true_hidden" = record$true_hidden)
  p1.df <- rbind(record.median, record.mean)
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("")  +
      geom_hline(yintercept=0, col = "purple") +
      #coord_cartesian(ylim=c(-0.2, 0.2)) +
      ggtitle("Point Estimates"))
  
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_counta <- length(which(record$width2 > 1.5))
  out2_countb <- length(which(record$width1 > 1.5))
  
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank()) +
      xlab(expression(b[H])) + ylab("")+
      ggtitle("Interval Width") )
  
  
  ## plot3,4 
  record200 <- record
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  record200$position <- runif(n = 1000, min = -1, max = 1)
  
  (p3a <- ggplot(record200, aes(y=b_hat.median, x=position))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Median & 90% HDI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank()) +
      xlab("")+ ylab("") )

  
  (p4a <- ggplot(record200, aes(y=b_hat.mean, x=position))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Mean & 90% QI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank())  +
      xlab("")+ ylab("") )
  
  
  ## put together
  return(ggarrange(p1a, p2a, p3a, p4a,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}


#### -------------------------- table function - null -------------------------- ####

summary_table_null <- function(record, row_names) {
  ## mse
  median.err70.1 <- (record$b_hat.median - record$true_hidden)^2
  median.rmse70.1 <- sqrt(sum(median.err70.1) / length(median.err70.1))
  
  mean.err70.1 <- (record$b_hat.mean - record$true_hidden)^2
  mean.rmse70.1 <- sqrt(sum(mean.err70.1) / length(mean.err70.1))
  
  ## outlier count
  median.out70.1 <- length(boxplot.stats(record$b_hat.median)$out)
  
  mean.out70.1 <- length(boxplot.stats(record$b_hat.mean)$out)
  
  ## bias
  median.bias70.1 <- mean(record$b_hat.median - record$true_hidden)
  
  mean.bias70.1 <- mean(record$b_hat.mean - record$true_hidden)
  
  ## not capture
  ci.type1Err70.1 <- length(which(record$bH.capture1 == FALSE)) / nrow(record)
  
  hdi.type1Err70.1 <- length(which(record$bH.capture2 == FALSE)) / nrow(record)
  
  ## power
  ci.captureZero1 <- (record$b.low1 < 0) & (record$b.high1 > 0)
  ci.power1 <- length(which(ci.captureZero1 == FALSE)) / length(ci.captureZero1)
  
  hdi.captureZero1 <- (record$b.low2 < 0) & (record$b.high2 > 0)
  hdi.power1 <- length(which(hdi.captureZero1 == FALSE)) / length(hdi.captureZero1)
  
  out <- data.frame("median.rmse" = c(median.rmse70.1),
                    "mean.rmse" = c(mean.rmse70.1),
                    "median.bias" = c(median.bias70.1),
                    "mean.bias" = c(mean.bias70.1),
                    "median.outlier" = c(median.out70.1),
                    "mean.outlier" = c(mean.out70.1),
                    "QI90.capture_rate" = 1 - c(ci.type1Err70.1),
                    "HDI90.capture_rate" = 1 - c(hdi.type1Err70.1),
                    "QI90.type_1_error" = c(ci.power1),
                    "HDI90.type_1_error" = c(hdi.power1))
  rownames(out) <- row_names
  return(out)
}


draw_plot_null_70 <- function(record, in_path) {
  
  record$width2 <- record$b.high2 - record$b.low2
  record$width1 <- record$b.high1 - record$b.low1
  
  
  ## for outliers in p1
  out_counta <- length(which(record$b_hat.median > 1))
  out_countb <- length(which(record$b_hat.mean > 1))
  
  ## combined three df
  record.median <- data.frame("value" = record$b_hat.median, "type" = "median",
                              "true_hidden" = record$true_hidden)
  record.mean <- data.frame("value" = record$b_hat.mean, "type" = "mean",
                            "true_hidden" = record$true_hidden)
  p1.df <- rbind(record.median, record.mean)
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank() ,
            axis.title.y = element_blank(),
            axis.text.x = element_blank()) +
      xlab(expression(b[H]==0)) + 
      geom_hline(yintercept=0, col = "purple") +
      #coord_cartesian(ylim=c(-0.2, 0.2)) +
      ggtitle("Point Estimates") +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10))) 
  
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_counta <- length(which(record$width2 > 1.5))
  out2_countb <- length(which(record$width1 > 1.5))
  
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank()) +
      xlab(expression(b[H]==0))+ 
      ggtitle("Interval Width") +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  
  ## plot3,4 
  record200 <- record
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  record200$position <- runif(n = 1000, min = -1, max = 1)
  
  (p3a <- ggplot(record200, aes(y=b_hat.median, x=position))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Median & 90% HDI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank()) +
      coord_cartesian(ylim=c(-0.5, 0.5)) +
      xlab(expression(b[H]==0))+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  (p4a <- ggplot(record200, aes(y=b_hat.mean, x=position))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("Mean & 90% QI") +
      theme(legend.title = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank())+
      coord_cartesian(ylim=c(-0.5, 0.5))   +
      xlab(expression(b[H]==0))+ 
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  
  
  ## put together
  return(ggarrange(p1a, p2a, p3a, p4a,
                   labels = c("A", "B", "C", "D"),
                   ncol = 4, nrow = 1) )
  
}


draw_plot_null_500 <- function(record, in_path) {
  
  record$width2 <- record$b.high2 - record$b.low2
  record$width1 <- record$b.high1 - record$b.low1
  
  
  ## for outliers in p1
  out_counta <- length(which(record$b_hat.median > 1))
  out_countb <- length(which(record$b_hat.mean > 1))
  
  ## combined three df
  record.median <- data.frame("value" = record$b_hat.median, "type" = "median",
                              "true_hidden" = record$true_hidden)
  record.mean <- data.frame("value" = record$b_hat.mean, "type" = "mean",
                            "true_hidden" = record$true_hidden)
  p1.df <- rbind(record.median, record.mean)
  
  p1.df.sub <- p1.df[p1.df$value < 1,]
  (p1a <- ggplot(p1.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank() ,
            axis.title.y = element_blank(),
            axis.text.x = element_blank()) +
      xlab(expression(b[H]==0)) + 
      geom_hline(yintercept=0, col = "purple") +
      #coord_cartesian(ylim=c(-0.2, 0.2)) +
      ggtitle("") +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10))) 
  
  
  
  ## plot 2
  record.hdi <- data.frame("value" = record$width2, "type" = "90% HDI",
                           "true_hidden" = record$true_hidden)
  record.ci <- data.frame("value" = record$width1, "type" = "90% QI",
                          "true_hidden" = record$true_hidden)
  
  ## for outliers in p2
  out2_counta <- length(which(record$width2 > 1.5))
  out2_countb <- length(which(record$width1 > 1.5))
  
  
  p2.df <- rbind(record.hdi, record.ci)
  p2.df.sub <- p2.df[p2.df$value < 1.5,]
  (p2a <- ggplot(p2.df, aes(x=factor(true_hidden), y=value, color=type)) + 
      geom_boxplot(width=0.5) + 
      theme(legend.title = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank()) +
      xlab(expression(b[H]==0))+ 
      ggtitle("") +
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  
  ## plot3,4 
  record200 <- record
  record200$bH.capture1.p <- ifelse(record200$bH.capture1 == TRUE, "contain", "no")  ## true=1, false=2
  record200$bH.capture2.p <- ifelse(record200$bH.capture2 == TRUE, "contain", "no")  ## true=1, false=2
  record200$position <- runif(n = 1000, min = -1, max = 1)
  
  (p3a <- ggplot(record200, aes(y=b_hat.median, x=position))  +
      geom_errorbar(aes(ymin=b.low2, ymax=b.high2), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("") +
      theme(legend.title = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank()) +
      coord_cartesian(ylim=c(-0.5, 0.5)) +
      xlab(expression(b[H]==0))+
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  (p4a <- ggplot(record200, aes(y=b_hat.mean, x=position))  +
      geom_errorbar(aes(ymin=b.low1, ymax=b.high1), color = "grey") +
      geom_point(aes(colour = factor(bH.capture2.p)) ) +
      scale_color_manual(values=c("#00BFC4", "#F8766D")) +
      #ylim(-0.05, 0.05) +
      geom_abline(intercept = 0, slope = 0, color="red") +
      ggtitle("") +
      theme(legend.title = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank())+
      coord_cartesian(ylim=c(-0.5, 0.5))   +
      xlab(expression(b[H]==0))+ 
      theme(legend.position="top",
            legend.justification="right",
            legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(-10,-2,-10,-10)))
  
  
  
  ## put together
  return(ggarrange(p1a, p2a, p3a, p4a,
                   labels = c("E", "F", "G", "H"),
                   ncol = 4, nrow = 1) )
  
}



