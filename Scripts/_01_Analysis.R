#=========================================================================
#INSTALL AND LOAD THE WANTED PACKAGES
#==========================================================================


want = c("rio", "haven", "janitor", "stargazer", "foreign", "magrittr",
         "nnet", "MASS", "MNLpred", "ggplot2", "ggplot2", "extrafont", "scales", "effects", "ggpubr",
         "jtools", "simEd", 'tidyverse', 'sjPlot')

have = want %in% rownames(installed.packages())

if (any(!have)) { install.packages( want[!have] ) }

# load packages

junk <- lapply(want, library, character.only = TRUE)

rm(have, want, junk)




set.seed(260423)

## LOAD THE DATA ##

load("C:/Users/Merio/Desktop/Uni/Multivariate/FINAL EXAM/Paper-data/ITANES2018_ready.RData")

ITANES2018 = ITANES2018_ready

##========##===========================
##ANALYSIS##
##========##==========================


#> Testing Hypothesis n.1: 


#Models of Table 1.


m1 = glm(turnout~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, family="binomial")

m2 = multinom(voto_challenger~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

m3 = multinom(voto_mainstream~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


#> The author represented the coefficients in the Tables as Odds-ratios.

coef_list = lapply(list(coef(m1), coef(m2), coef(m3)), exp)


#TABLE 1 
stargazer::stargazer(m1, m2, m3, coef = coef_list, p.auto = F ,type = 'text')

#Results are consistent with the findings of the Author.


#Models of Table 2

m7<-glm(turnout~fear_all_dummy_numeric*eco_hardship_dummy+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, family="binomial")


m8<-multinom(voto~fear_all_dummy_numeric*eco_hardship_dummy+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)


#convert the coefficients into odds-ratio

coef_list_t2 = lapply(list(coef(m7), coef(m8)), exp)


#TABLE 2#

stargazer::stargazer(m7, m8, coef = coef_list_t2, p.auto = F, type = 'text' )


#results are consistent with the findings of the Author.


###MODELS AND TABLES OF THE ONLINE APPENDINX##
#############################################

#> In this section I replicate the table of the online appendix of the paper and supplmenentary material



#Model 4 --> Table A1 
m4 = multinom(voto ~ fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

#TABLE A1

m4.coef = exp(coef(m4))

stargazer(m4, coef = list(m4.coef),  p.auto = F, type = 'text')



####Model 5


m5 = multinom(voto~employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

m5.coef = exp(coef(m5))


##TABLE A2
stargazer(m5, coef = list(m5.coef), p.auto = F, type = 'text')


#Model 6 --> Table A3 

m6 = multinom(voto~fear_all_dummy_numeric*employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)
m6.rrr = exp(coef(m6)) 


#TABLE A3

stargazer(m6, coef = list(m6.rrr), p.auto = F, type = 'text')



#Model 2b and 3b, with "Lega" as Challenger vs As mainstream.


m2b = multinom(voto_Lega_challenger~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


m3b = multinom(voto_Lega_mainstream~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


cf_mb = lapply(list(coef(m2b), coef(m3b)), exp)

#Table S2

stargazer(m2b, m3b, coef = cf_mb, type = 'text', p.auto = F)


### 


#"Lega" as mainstream party.
m4b = multinom(voto_Lega~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

mb4_cof = exp(coef(m4b))

#TABLE S2 - model 4b
stargazer(m4b, coef = list(mb4_cof), type = 'text', p.auto = F)





