#=========================================================================
#INSTALL AND LOAD THE WANTED PACKAGES
#==========================================================================


want = c("rio", "haven", "janitor", "stargazer", "foreign", "magrittr",
         "nnet", "MASS", "MNLpred", "ggplot2", "ggplot2", "extrafont", "scales", "effects", "ggpubr",
         "jtools", "simEd", 'tidyverse', 'sjPlot', 'margins', 'marginaleffects')

have = want %in% rownames(installed.packages())

if (any(!have)) { install.packages( want[!have] ) }

# load packages

junk <- lapply(want, library, character.only = TRUE)

rm(have, want, junk)




set.seed(260423)

## LOAD THE DATA ##

load("Output/Saved data/ITANES2018_ready2.RData")

ITANES2018 = ITANES2018_ready2

##========##===========================
##ANALYSIS##
##========##==========================


#> IMPORTANT : In this script will be replicated the ORIGINAL paper by the author. The additional informations added by me, are written in the file "_03_Additional_analysis"


#> Testing Hypothesis n.1: 
#> Operational Note: The Author presented all the coefficients in the tables
#> as ODDS-RATIOS.

#Models of Table 1.


m1 = glm(turnout~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, family="binomial")

m2 = multinom(voto_challenger~fear_all_dummy_numeric+ residence+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

m3 = multinom(voto_mainstream~fear_all_dummy_numeric +employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


#> The author represented the coefficients in the Tables as Odds-ratios.

coef_list = lapply(list(coef(m1), coef(m2), coef(m3)), exp)


#TABLE 1 
stargazer::stargazer(m1, m2, m3, coef = coef_list, p.auto = F ,type = 'html', style = 'apsr', out = 'Plots/Tables/Replication tables/Paper Tables/Paper_table1.html')

#Results are consistent with the findings of the Author.


#Models of Table 2

m7<-glm(turnout~fear_all_dummy_numeric*eco_hardship_dummy+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, family="binomial")


m8<-multinom(voto~fear_all_dummy_numeric*eco_hardship_dummy+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)


#convert the coefficients into odds-ratio

coef_list_t2 = lapply(list(coef(m7), coef(m8)), exp)


#TABLE 2#

stargazer::stargazer(m7, m8, coef = coef_list_t2, p.auto = F, type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Paper Tables/Paper_table2.html' )


#results are consistent with the findings of the Author.


###MODELS AND TABLES OF THE ONLINE APPENDINX##
#############################################

#> In this section I replicate the table of the online appendix of the paper and suplmenentary material



#Model 4 --> Table A1 
m4 = multinom(voto ~ fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

#TABLE A1

m4.coef = exp(coef(m4))

stargazer(m4, coef = list(m4.coef),  p.auto = F, type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_A1.html')



####Model 5


m5 = multinom(voto~employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

m5.coef = exp(coef(m5))


##TABLE A2
stargazer(m5, coef = list(m5.coef), p.auto = F, type = 'text', style = 'apsr',out = 'Plots/Tables/Replication tables/Online Appendix models/AM_A2.html')


#Model 6 --> Table A3 

m6 = multinom(voto~fear_all_dummy_numeric*employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)
m6.rrr = exp(coef(m6)) 


#TABLE A3

stargazer(m6, coef = list(m6.rrr), p.auto = F, type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_A3.html')



#Model 2b and 3b, with "Lega" as Challenger vs As mainstream.


m2b = multinom(voto_Lega_challenger~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


m3b = multinom(voto_Lega_mainstream~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


cf_mb = lapply(list(coef(m2b), coef(m3b)), exp)

#Table S2

stargazer(m2b, m3b, coef = cf_mb, type = 'text', p.auto = F, style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_S1.html')


### 


#"Lega" as mainstream party.
m4b = multinom(voto_Lega~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

mb4_cof = exp(coef(m4b))

#TABLE S2 - model 4b
stargazer(m4b, coef = list(mb4_cof), type = 'text', p.auto = F, style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_S2.html')


### Models 2c and 3c

m2c = multinom(voto_challenger~fear_activeworkers_dummy+eco_hardship_dummy+employment_status_nounemp+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)

m3c = multinom(voto_mainstream~fear_activeworkers_dummy+eco_hardship_dummy+employment_status_nounemp+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)


c23_coef = lapply(list(coef(m2c), coef(m3c)), exp)


##TABLE S3
stargazer(m2c, m3c, coef = c23_coef, p.auto = F, type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_S3.html')



#Model 4c for Table S4 


m4c = multinom(voto~fear_activeworkers_dummy+eco_hardship_dummy+employment_status_nounemp+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)

#TABLE S4

stargazer(m4c, coef = list(exp(coef(m4c))), p.auto = F, type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_S4.html')



## Models turnout(1d), vote choice (2d, 3d).

m1d = glm(turnout~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, family="binomial")

m2d = multinom(voto_challenger~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

m3d = multinom(voto_mainstream~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


coefficients_S6 = lapply(list(coef(m1d), coef(m2d), coef(m3d)), exp)



#TABLE S6

stargazer(m1d, m2d, m3d, coef = coefficients_S6, p.auto = F, type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_S6.html')


#Model 4b, for table S7




#Model 4d --> Table S7
m4d = multinom(voto~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


#TABLE S7

stargazer(m4d, coef = list(exp(coef(m4d))), p.auto = F,  type = 'text', style = 'apsr', out = 'Plots/Tables/Replication tables/Online Appendix models/AM_S7.html')

