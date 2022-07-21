####### Script Information ########################
# Brandon P.M. Edwards
# qpad-distance-binning
# 02-plot-results.R
# Created July 2021
# Last Updated July 2021

####### Import Libraries and External Files #######

library(ggplot2)
library(ggpubr)
theme_set(theme_pubclean())

####### Read Data #################################

data <- read.csv("output/model_edr.csv")

####### Main Code #################################

for (s in unique(data$Species))
{
  to_plot <- data[data$Species == s, ]
  methods <- to_plot$Method
  
  full_edr <- to_plot[to_plot$Method == "Full", "EDR"]
  
  to_plot$Method <- factor(to_plot$Method, levels = methods)
  
  edr_plot <- ggplot(data = to_plot) +
    geom_point(aes(x = Method, y = exp(EDR))) +
    geom_errorbar(aes(x = Method, ymin = exp(EDR_2.5), ymax = exp(EDR_97.5))) +
    #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0)) +
    geom_hline(yintercept = exp(full_edr), color = "red") +
    ylim(c(0,400)) +
    ggtitle(s) +
    xlab("Binning Method") +
    ylab("EDR") +
    coord_flip() +
    geom_label(aes(x = Method, y = 300, label = round(exp(EDR),2))) +
    geom_label(aes(x = Method, y = 350, label = paste0("N=",N))) +
    NULL
  
  png(filename = paste0("output/", s, "_plot.png"),
      width = 12, height = 8, res = 300, units = "in")
  print(edr_plot)
  dev.off()
}


####### Output ####################################
