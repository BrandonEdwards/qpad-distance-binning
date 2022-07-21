####### Script Information ########################
# Brandon P.M. Edwards
# qpad-distance-binning
# 01-run-models.R
# Created July 2022
# Last Updated July 2022

####### Set Constants #############################

species <- c("AMRO", "SWSP", "BGGN", "AMCR")

####### Import Libraries and External Files #######

library(detect)
library(MASS)

####### Read Data #################################

#' Unfortunately these intermediate data sets are not available to the public
#' due to data sharing agreements. Please contact Brandon Edwards at NA-POPS
#' for more details.

load("data/counts.rda")
load("data/design.rda")
distance_lookup <- read.csv("data/distance_lookup.csv")

####### Main Code #################################

results_df <- data.frame(Species = character(),
                         EDR = double(),
                         EDR_2.5 = double(),
                         EDR_97.5 = double(),
                         Method = character())

for (s in species)
{
  counts_s <- counts[which(counts$Species == s), ]
  design_s <- design[which(design$Species == s), ]
  
  n_designs <- length(unique(design_s$Distance_Method))
  i <- 1
  for (d in unique(design_s$Distance_Method))
  {
    message(paste0("Species: ", s, " Method: ", i, "/", n_designs))
    method <- distance_lookup[which(distance_lookup$Method == d), "Description"][1]
    indices <- which(design_s$Distance_Method == d)
    
    counts_d <- as.matrix(counts_s[indices, 4:ncol(counts_s)])
    design_d <- as.matrix(design_s[indices, 4:ncol(design_s)])
    
    mod <- tryCatch(
      {
        cmulti(counts_d | design_d ~ 1, type = "dis")
      },
      error = function(e)
      {
        return(NA)
      }
    )
    
    if (is.na(mod))
    {
      results_df <- rbind(results_df,
                          data.frame(Species = s,
                                     EDR = NA,
                                     EDR_2.5 = NA,
                                     EDR_97.5 = NA,
                                     Method = method))      
    }else
    {
      intervals <- quantile(mvrnorm(10000, coef(mod)[1], vcov(mod)),
                            probs = c(0.025, 0.975))
      
      results_df <- rbind(results_df,
                          data.frame(Species = s,
                                     EDR = unname(coef(mod)[1]),
                                     EDR_2.5 = intervals[1],
                                     EDR_97.5 = intervals[2],
                                     Method = method))      
    }
    

    i <- i + 1
  }
  message(paste0("Species: ", s, " Method: ALL"))
  counts_full <- as.matrix(counts_s[, 4:ncol(counts_s)])
  design_full <- as.matrix(design_s[, 4:ncol(design_s)])
  
  mod <- cmulti(counts_full | design_full ~ 1, type = "dis")
  
  intervals <- quantile(mvrnorm(10000, coef(mod)[1], vcov(mod)),
                        probs = c(0.025, 0.975))
  
  method <- "Full"
  
  results_df <- rbind(results_df,
                      data.frame(Species = s,
                                 EDR = unname(coef(mod)[1]),
                                 EDR_2.5 = intervals[1],
                                 EDR_97.5 = intervals[2],
                                 Method = method))
}

####### Output ####################################

write.table(results_df,
            file = "output/model_edr.csv",
            sep = ",",
            row.names = FALSE)
