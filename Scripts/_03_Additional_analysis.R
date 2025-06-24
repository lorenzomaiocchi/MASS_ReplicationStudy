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


#important
set.seed(260423)
###########################



## LOAD THE DATA ##

load('Output/Saved data/ITANES2013_ready.RData')


ITANES2013 = ITANES2013_ready


#==========================================================================================#
##
##                                ADDITIONAL ANALYSIS                                     ##
##                                                                                        ##                                                                                      ##
##                                                                                        ##
#==========================================================================================#


#> This part of the analysis is done as an addition to the original paper by the author. In this instance the data from ITANES2013 is used




#Model of turnout in 2013

#Coefficients will be presented in the form of odds-ratio like in the original paper



model1 = glm(data = ITANES2013,
              turnout ~
                Fear_all + 
             Employment_status +
               age+
               gender_dummy+
               residence+
               Union+
               education_numeric + 
               Ideology_numeric+
               Trust_EU_numeric+
               immigration_numeric+
               incumbent_numeric+
               Populism_numeric+
               trust_parties_numeric+
               Economic_hardship_dummy,
             family = binomial(link = 'logit')
             )
               

od_model1 = list(exp(coef(model1)))            





model2 = multinom(
  voto_challenger ~
    Fear_all + 
    Employment_status +
    age+
    gender_dummy+
    residence+
    Union+
    education_numeric + 
    Ideology_numeric+
    Trust_EU_numeric+
    immigration_numeric+
    Populism_numeric+
    incumbent_numeric+
    trust_parties_numeric+
    Economic_hardship_dummy,
  data = ITANES2013,
  Hess =  T
    
)

od_model2 = list(exp(coef(model2))) 


model3 =  multinom(
  voto_Mainstream ~
    Fear_all + 
    Employment_status +
    age+
    gender_dummy+
    residence+
    Union+
    education_numeric + 
    Ideology_numeric+
    Trust_EU_numeric+
    immigration_numeric+
    Populism_numeric+
    incumbent_numeric+
    trust_parties_numeric+
    Economic_hardship_dummy,
  data = ITANES2013,
  Hess =  T
)
  
od_model3 = list(exp(coef(model3))) 

##Table B1







stargazer(model1, coef = od_model1, p.auto = F ,type = 'html', out = 'Plots/model1.html')


stargazer(model2, coef = od_model2, p.auto = F ,type = 'html', out = 'Plots/model2.html')


stargazer(model3, coef =od_model3, p.auto = F ,type = 'html', out = 'Plots/model3.html')





