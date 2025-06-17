#=========================================================================
#INSTALL AND LOAD THE WANTED PACKAGES
#==========================================================================


want = c("rio", "haven", "janitor", "stargazer", "foreign", "magrittr",
         "nnet", "MASS", "MNLpred", "ggplot2", "ggplot2", "extrafont", "scales", "effects", "ggpubr",
         "jtools", "DAMisc", "simEd")

have = want %in% rownames(installed.packages())

if (any(!have)) { install.packages( want[!have] ) }

# load packages

junk <- lapply(want, library, character.only = TRUE)

rm(have, want, junk)


set.seed(260423)

## LOAD THE DATA ##

load("C:/Users/Merio/Desktop/Uni/Multivariate/FINAL EXAM/Paper-data/ITANES2018_ready.RData")

ITANES2018 = ITANES2018_ready

##========================================
##ANALYSIS
##========================================







