#=========================================================================
#INSTALL AND LOAD THE WANTED PACKAGES
#==========================================================================


want = c("rio", "haven", "janitor", "stargazer", "foreign", "magrittr",
         "nnet", "MASS", "MNLpred", "ggplot2", "ggplot2", "extrafont", "scales", "effects", "ggpubr",
         "jtools", "simEd", 'tidyverse', 'forcats')

have = want %in% rownames(installed.packages())

if (any(!have)) { install.packages( want[!have] ) }

# load packages

junk <- lapply(want, library, character.only = TRUE)

rm(have, want, junk)


set.seed(260423)
###########################



## LOAD THE DATA ##

load("C:/Users/Merio/Desktop/Uni/Multivariate/FINAL EXAM/Paper-data/ITANES2018_ready.RData")

#For ITANES 2018, I'm using the pre-structured dataset made available by the author. 

ITANES2018 = ITANES2018_ready


###RESTRUCTURING AND CLEANING OF ITANES2013##
##-----------------------------------------##


#*Following the same re-codification the author used for the 2018 Dataset, I'm focusing just on the needed variables
#*re-coding them in the same way. 


ITANES2013 = read.spss("C:/Users/Merio/Desktop/Uni/Multivariate/FINAL EXAM/Paper-data/ITA2013_(itvers2013_11_29).sav",
                       to.data.frame = T)




ITANES2013 %>% 
  mutate(
    fct_collapse(
      
    )
  )
