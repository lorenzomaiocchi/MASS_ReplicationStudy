#=========================================================================
#INSTALL AND LOAD THE WANTED PACKAGES
#==========================================================================


want = c("rio", "haven", "janitor", "stargazer", "foreign", "magrittr",
         "nnet", "MASS", "MNLpred", "ggplot2", "ggplot2", "extrafont", "scales", "effects", "ggpubr",
         "jtools", "simEd", 'tidyverse', 'forcats', 'brglm2')

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
  Hess =  T)

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
  Hess =  T)
  
od_model3 = list(exp(coef(model3))) 

##Table B1 - data from 2013




stargazer(model1, coef = od_model1, p.auto = F ,type = 'html', out = 'Plots/model1.html')


stargazer(model2, coef = od_model2, p.auto = F ,type = 'html', out = 'Plots/model2.html')


stargazer(model3, coef =od_model3, p.auto = F ,type = 'html', out = 'Plots/model3.html')




###Models with interaction.

#turnout with interaction


model4 = glm(data = ITANES2013,
             turnout ~
               Fear_all*Economic_hardship_dummy + 
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
               trust_parties_numeric,
             family = binomial(link = 'logit'))



coef_model4od = list(exp(coef(model4)))



#table 2.1
stargazer(model4,coef = coef_model4od, p.auto = F , type = 'html', out = 'Plots/model4.html')


#model of vote with interaction.

model5 =  multinom(
  type_voto_inc ~
    Fear_all*Economic_hardship_dummy + 
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
    trust_parties_numeric,
  data = ITANES2013,
  Hess =  T)


od.model5 = list(exp(coef(model5)))


#Table 2.2
stargazer(model5 ,coef = od.model5, p.auto = F , type = 'html', out = 'Plots/model5.html')


###Model of Mainstream/challengere vs No vote


model6 =multinom(
  voto_novote ~
    Fear_all +Economic_hardship_dummy + 
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
    trust_parties_numeric,
  data = ITANES2013,
  Hess =  T) 


#with interaction
model6.1 =multinom(
  voto_novote ~
    Fear_all*Economic_hardship_dummy + 
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
    trust_parties_numeric,
  data = ITANES2013,
  Hess =  T) 


odd.model6 = list(exp(coef(model6)))

odd.model6.1 = list(exp(coef(model6.1)))


#Table 3.1


stargazer(model6, coef = odd.model6, p.auto =  F, type = 'html', out = 'Plots/model6.html')


#Table 3.2

stargazer(model6.1, coef = odd.model6.1, p.auto =  F, type = 'html', out = 'Plots/model6_1.html')