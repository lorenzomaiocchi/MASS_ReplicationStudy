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




stargazer(model1, coef = od_model1, p.auto = F ,type = 'html', out = 'Plots/Tables/Additional Analysis Table/model1.html')


stargazer(model2, coef = od_model2, p.auto = F ,type = 'html', out = 'Plots/Tables/Additional Analysis Table/model2.html')


stargazer(model3, coef =od_model3, p.auto = F ,type = 'html', out = 'Plots/Tables/Additional Analysis Table/model3.html')




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
stargazer(model4,coef = coef_model4od, p.auto = F , type = 'html', out = 'Plots/Tables/Additional Analysis Table/model4.html')


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
stargazer(model5 ,coef = od.model5, p.auto = F , type = 'html', out = 'Plots/Tables/Additional Analysis Table/model5.html')


###Model of Mainstream/challenger vs No vote


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


stargazer(model6, coef = odd.model6, p.auto =  F, type = 'html', out = 'Plots/Tables/Additional Analysis Table/model6.html')


#Table 3.2

stargazer(model6.1, coef = odd.model6.1, p.auto =  F, type = 'html', out = 'Plots/Tables/Additional Analysis Table/model6_1.html')


####===================================================================================####
##################### REMAKING THE ORIGINAL MODEL WITH NO VOTE AND "OTHERS" CATEGORIES#####

#Model 2,3 


redo_m2 = multinom(voto_challenger2~fear_all_dummy_numeric+
                employment_status+age+gender+education+
                residence+union+ideology+euro+immigration+
                trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)




redo_m3 = multinom(voto_mainstream2~fear_all_dummy_numeric+
                employment_status+age+gender+education+
                residence+union+ideology+euro+immigration+
                trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

coefficienti = lapply(list(coef(redo_m2), coef(redo_m3)), exp)



stargazer(redo_m2, redo_m3, coef = coefficienti, p.auto = F, type = 'html', out = 'Plots/Tables/Re-make models/recreated_models.html')



###
red_m8 = multinom(voto_type_na~fear_all_dummy_numeric*eco_hardship_dummy+
                    employment_status+age+
                    gender+education+residence+union+
                    ideology+euro+immigration+trust+populism+incumbent, 
                  data=ITANES2018, Hess=TRUE)


stargazer(red_m8, coef = list(exp(coef(red_m8))), p.auto = F, out = 'Plots/Tables/Re-make models/model4_red.html')


#reference = 'No vote'

red_m9 = multinom(voto_notvote~fear_all_dummy_numeric*eco_hardship_dummy+
                    employment_status+age+
                    gender+education+residence+union+
                    ideology+euro+immigration+trust+populism+incumbent, 
                  data=ITANES2018, Hess=TRUE)


stargazer(red_m9, coef = list(exp(coef(red_m9))), p.auto = F, out = 'Plots/Tables/Re-make models/model5_red.html')




#####
####### MODELS WITH NON-RESPONDENT AS NAs



redo_m2.2 = multinom(voto_challenger_nr~fear_all_dummy_numeric+
                     employment_status+age+gender+education+
                     residence+union+ideology+euro+immigration+
                     trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)



redo_m3.2 = multinom(voto_mainstream_nr~fear_all_dummy_numeric+
                     employment_status+age+gender+education+
                     residence+union+ideology+euro+immigration+
                     trust+populism+incumbent+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


coefficienti.2 = lapply(list(coef(redo_m2.2), coef(redo_m3.2)), exp)



stargazer(redo_m2.2, redo_m3.2, p.auto = F, coef = coefficienti.2, type = 'html', out = 'Plots/Tables/Re-make models/recreated_models2.html')



###
red_m8.2 = multinom(voto_type_nr~fear_all_dummy_numeric*eco_hardship_dummy+
                    employment_status+age+
                    gender+education+residence+union+
                    ideology+euro+immigration+trust+populism+incumbent, 
                  data=ITANES2018, Hess=TRUE)


stargazer(red_m8.2, coef = list(exp(coef(red_m8.2))), p.auto = F, out = 'Plots/Tables/Re-make models/model4.2_red.html')


#reference = 'No vote'

red_m9.2 = multinom(voto_notvote_nr~fear_all_dummy_numeric*eco_hardship_dummy+
                    employment_status+age+
                    gender+education+residence+union+
                    ideology+euro+immigration+trust+populism+incumbent, 
                  data=ITANES2018, Hess=TRUE)


stargazer(red_m9.2, coef = list(exp(coef(red_m9.2))), p.auto = F, out = 'Plots/Tables/Re-make models/model5.2_red.html')









############################################################################################################
############################################################################################################
#################   ** PLOTS OF THE MODELS ** #########################################################


pred_model =  multinom(
    type_voto_cdx ~
      Fear_all_numeric + Economic_hardship_dummy + 
      Permanently_employed + inactive + retired + unemployed +
      age+
      gender_dummy+
      Center+ South +
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




fd = mnl_fd2_ova(
  model = pred_model,
  data = ITANES2013,
  x = 'Fear_all_numeric',
  value1 = min(ITANES2013$Fear_all_numeric, na.rm = T),
  value2 = max(ITANES2013$Fear_all_numeric, na.rm = T),
  nsim = 2000,
  seed = 260423
)


fd$plotdata$fear_all_dummy = as.factor(case_when(
  fd$plotdata$Fear_all_numeric == 0 ~ 'Not Afraid',
  fd$plotdata$Fear_all_numeric == 1 ~ 'Afraid'))

fd$plotdata$fear_all_dummy = relevel(fd$plotdata$fear_all_dummy, ref = 'Not Afraid')



#Change in probability of vote from "Not afraid" to "Afraid"



Figure1_2013 = fd$plotdata_fd %>% 
  ggplot(aes(categories, mean, ymin = lower, ymax = upper))+
  geom_pointrange(color = 'blue')+
  geom_hline(yintercept = 0, color = 'red', alpha = 0.6)+
  theme_bw()+
  scale_y_continuous(labels = percent_format())+
  labs(
    y = 'Predicted probability',
    x = 'Vote choice 2013'
  )+
  theme(axis.text.x = element_text( size=10))+ 
  theme(axis.text.y = element_text( size=14))+ 
  theme(axis.title = element_text(size = 15)) + 
  theme(
    axis.title.x = element_text(hjust=0.5),
    axis.title.y = element_text(hjust=0.5)   
  )




ggsave("FD_2013_Additional.png",
       plot = Figure1_2013,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots/Figures/Additional Analysis')



#Estimated probabilities of vote (no vote included) by fear of jobloss.

Figure2_AA = fd$plotdata %>% 
  ggplot(aes(fear_all_dummy, mean, ymin = lower, ymax = upper, shape = fear_all_dummy))+
  geom_pointrange(color = 'darkgreen')+
  theme_bw()+
  facet_wrap(~type_voto_cdx, ncol = 2)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(labels = percent_format())+
  theme(axis.text.x = element_text( size=12))+ 
  theme(axis.text.y = element_text( size=12))+ 
  theme(axis.title = element_text(size = 13))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(shape = "Fear of Job Loss", 
       y = 'Predicted Probability',
       x = '')+ 
  theme(legend.text=element_text(size=12))+ 
  theme(legend.title=element_text(size=12))





ggsave("Fear_JB_2013.png",
       plot = Figure2_AA,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots/Figures/Additional Analysis')













####
##### Pr() of vote, given the employment status

d2 = ITANES2013

d2$Permanently_employed = 0
d2$Atypically_employed= 0
d2$inactive = 0
d2$retired = 0

pred_une = mnl_fd2_ova(
  model = pred_model,
  data = d2,
  x = 'unemployed',
  value1 = min(d2$unemployed, na.rm = T),
  value2 = max(d2$unemployed, na.rm = T),
  nsim = 2000,
  seed = 260423,
  probs = c(0.025, 0.975)
  
)



##For Permanently employed

d3 = ITANES2013

d3$Atypically_employed= 0
d3$inactive = 0
d3$retired = 0
d3$unemployed = 0

pred_perm = mnl_fd2_ova(
  model = pred_model,
  data = d3,
  x = 'Permanently_employed',
  value1 = min(d3$Permanently_employed, na.rm = T),
  value2 = max(d3$Permanently_employed, na.rm = T),
  nsim = 2000,
  seed = 260423,
  probs = c(0.025, 0.975)
  
)


d5 = ITANES2013

d5$Permanently_employed= 0
d5$retired = 0
d5$unemployed = 0
d5$Atypically_employed = 0

pred_inactive = mnl_fd2_ova(
  model = pred_model,
  data = d5,
  x = 'inactive',
  value1 = min(d5$inactive, na.rm = T),
  value2 = max(d5$inactive, na.rm = T),
  nsim = 2000,
  seed = 260423,
  probs = c(0.025, 0.975)
  
)



d6 = ITANES2013

d6$Permanently_employed= 0
d6$inactive = 0
d6$unemployed = 0
d6$Atypically_employed = 0

pred_retired = mnl_fd2_ova(
  model = pred_model,
  data = d6,
  x = 'retired',
  value1 = min(d6$retired, na.rm = T),
  value2 = max(d6$retired, na.rm = T),
  nsim = 2000,
  seed = 260423,
  probs = c(0.025, 0.975))


pred_perm$plotdata$category = ifelse(pred_perm$plotdata$Permanently_employed == 1, 'Permanently Employed', 'Atypically Employed')
pred_une$plotdata$category = ifelse(pred_une$plotdata$unemployed == 1, 'Unemployed', 'Atypically Employed')
pred_retired$plotdata$category = ifelse(pred_retired$plotdata$retired == 1, 'Retired', 'Atypically Employed')
pred_inactive$plotdata$category = ifelse(pred_inactive$plotdata$inactive== 1, 'Inactive', 'Atypically Employed')

pred_une$plotdata = pred_une$plotdata %>% filter(category != 'Atypically Employed')
pred_retired$plotdata = pred_retired$plotdata %>% filter(category != 'Atypically Employed')
pred_inactive$plotdata = pred_inactive$plotdata %>% filter(category != 'Atypically Employed')


#$category = factor(pp$category, levels = c('Permanently Employed', 'Unemployed', 'Inactive', 'Retired', 'Atypically Employed') )


pp = pred_perm$plotdata %>% 
  bind_rows(pred_une$plotdata) %>% 
  bind_rows(pred_inactive$plotdata) %>% 
  bind_rows(pred_retired$plotdata)


pp$category = factor(pp$category, levels = c('Permanently Employed', 'Atypically Employed', 'Unemployed', 'Inactive', 'Retired'))



Figure3_AA = pp %>% 
  ggplot(aes(category, mean, ymin = lower, ymax = upper, shape = category))+
  geom_pointrange(color = 'blue')+
  facet_wrap(~type_voto_cdx, ncol = 2)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(labels = percent_format())+
  theme(axis.text.x = element_text( size=12))+ 
  theme(axis.text.y = element_text( size=12))+ 
  theme(axis.title = element_text(size = 13))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(shape = "Employment status", 
       y = 'Predicted Probability',
       x = '')+ 
  theme(legend.text=element_text(size=12))+ 
  theme(legend.title=element_text(size=12))


ggsave("Figure3_AA_empl.png",
       plot = Figure3_AA,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots/Figures/Additional Analysis')


