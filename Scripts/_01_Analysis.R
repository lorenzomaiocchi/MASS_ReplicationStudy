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
#> Operational Note: The Author presented all the coefficients in the tables
#> as ODDS-RATIOS.

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

#> In this section I replicate the table of the online appendix of the paper and suplmenentary material



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


### Models 2c and 3c

m2c = multinom(voto_challenger~fear_activeworkers_dummy+eco_hardship_dummy+employment_status_nounemp+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)

m3c = multinom(voto_mainstream~fear_activeworkers_dummy+eco_hardship_dummy+employment_status_nounemp+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)


c23_coef = lapply(list(coef(m2c), coef(m3c)), exp)


##TABLE S3
stargazer(m2c, m3c, coef = c23_coef, p.auto = F, type = 'text')



#Model 4c for Table S4 


m4c = multinom(voto~fear_activeworkers_dummy+eco_hardship_dummy+employment_status_nounemp+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent, data=ITANES2018, Hess=TRUE)

#TABLE S4

stargazer(m4c, coef = list(exp(coef(m4c))), p.auto = F, type = 'text')



## Models turnout(1d), vote choice (2d, 3d).

m1d = glm(turnout~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, family="binomial")

m2d = multinom(voto_challenger~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)

m3d = multinom(voto_mainstream~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


coefficients_S6 = lapply(list(coef(m1d), coef(m2d), coef(m3d)), exp)



#TABLE S6

stargazer(m1d, m2d, m3d, coef = coefficients_S6, p.auto = F, type = 'text')


#Model 4b, for table S7




#Model 4d --> Table S7
m4d = multinom(voto~fear_all_dummy_numeric+employment_status+age+gender+education+residence+union+ideology+euro+immigration+trust+populism+incumbent+voto_abstain_2013+eco_hardship_dummy, data=ITANES2018, Hess=TRUE)


#TABLE S7

stargazer(m4d, coef = list(exp(coef(m4d))), p.auto = F,  type = 'text')


##=========================================================================================
##=========================================================================================


#### FIGURES REPLICATION #######
################################


#> This section is dedicated to the replication of the Figures the Author included in the Paper.
#> The figures added as a plus to the analysis are collected in the  R file "_02_Plots"


#FIGURE 1

##**note: Figure 1 in the Paper is specified as taken from Model 4. The model used here multi1, is different from model 4.
##**I will therefore proceed to replicate as specified, and make a new version using only model 4 to check for differences 
##**This will be included in the "_02_Plots" ' script.

set.seed(260423)
multi1<-multinom(voto~fear_all_dummy_numeric+
                   eco_hardship_dummy+
                   permanently_employed+
                   atypically_employed+
                   unemployed+age+
                   gender+education+
                   Center+South+
                   union+left+centre_left+centre+centre_right+
                   right+euro+immigration+trust+populism+incumbent, 
                 data=ITANES2018, Hess=TRUE)

#mln_pred_ova() is an external function to predict marginal effects. The function cannot use factors/character variables, therefore Numerics with work-arounds must be used

pred_perceived = mnl_pred_ova(model = multi1,
                              data = ITANES2018,
                              by = 1,
                              x = 'fear_all_dummy_numeric',
                              seed = 260423,
                              nsim = 2000,
                              probs = c(0.025, 0.975))

#extract the probabilities and plot them

pred_perceived$plotdata$fear_all_dummy = pred_perceived$plotdata$fear_all_dummy_numeric

pred_perceived$plotdata = pred_perceived$plotdata %>% 
  mutate(fear_all_dummy = factor(fear_all_dummy,
                                 levels = c(0,1),
                                 labels = c('Not Afraid', 'Afraid')))



Figure1 = pred_perceived$plotdata %>% 
  ggplot(aes(fear_all_dummy, shape = fear_all_dummy, y = mean, ymin = lower, ymax = upper))+
  geom_pointrange()+
  facet_wrap(.~voto)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.6), breaks=c(0,0.2,0.4, 0.6))+
  theme_bw()+
  labs(
    y = 'Predicted  Probabilities',
    x = ''
  )+
  theme(axis.text.y = element_text( size=14))+ 
  theme(axis.title = element_text(size = 14))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(shape = "Fear of job loss")+ 
  theme(legend.text=element_text(size=12))+ 
  theme(legend.title=element_text(size=12))+
  theme(text = element_text(family = "LM Roman 10"))

#Save the plot 

ggsave("Figure1_paper_modelmulti1.png",
       plot = Figure1,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots')
  

###
###FIGURE 2

#First Differences Estimate on the Probability to vote for each party, given a shift
#from "Not afraid" of Job Loss to "Afraid".


fdif_perceived <- mnl_fd2_ova(model = multi1,
                              data = ITANES2018,
                              x = "fear_all_dummy_numeric",
                              value1 = min(ITANES2018$fear_all_dummy_numeric, na.rm=TRUE),
                              value2 = max(ITANES2018$fear_all_dummy_numeric, na.rm=TRUE),
                              nsim = 2000,
                              seed = 260423)


Figure2 = fdif_perceived$plotdata_fd %>% 
  ggplot(aes(categories, mean, ymin = lower, ymax = upper))+
  geom_pointrange()+
  geom_hline(yintercept = 0)+
  scale_y_continuous(labels = percent_format())+
  labs(y = "Predicted probabilities",
       x = "Vote choice")+ 
 theme(axis.text.x = element_text( size=14))+ 
  theme(axis.text.y = element_text( size=14))+ 
  theme(axis.title = element_text(size = 15)) + 
  theme(
    axis.title.x = element_text(hjust=0.5),
    axis.title.y = element_text(hjust=0.5)
  )   +
  theme_bw()+
  theme(text = element_text(family = "LM Roman 10"))
  

#save the plot


ggsave("Figure2_paper_modelmulti1.png",
       plot = Figure2,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots')


#PLOT A2


#Evaluate the effect just on the category = 'permanently employed'
df1 = ITANES2018


df1 = df1 %>% 
  mutate(atypically_employed = 0,
         selfemployed = 0,
         unemployed = 0 )

pred_permanently_empl = mnl_pred_ova(model = multi1,
                                     data = df1,
                                     x = 'permanently_employed',
                                     by = 1, 
                                     seed = 260423,
                                     nsim = 2000,
                                     probs = c(0.025, 0.975))


fdi_permanently_empl = mnl_fd2_ova(model = multi1,
                                   data = df1,
                                   x = "permanently_employed",
                                   value1 = min(df1$permanently_employed,na.rm=TRUE),
                                   value2 = max(df1$permanently_employed,na.rm=TRUE),
                                   nsim = 2000,
                                   seed = 260423)

##Prediction for the "Atypically Employed" category
##
set.seed(260423) #keep seed as reference

df2 = ITANES2018

df2 = df2 %>% 
  mutate(permanently_employed = 0,
         selfemployed = 0,
         unemployed =  0)

pred_atypically_employed = mnl_pred_ova(model = multi1,
                                         data = df2,
                                         x = "atypically_employed",
                                         by = 1,
                                         seed = 260423,
                                         nsim = 2000, 
                                         probs = c(0.025, 0.975)) 
                                         
fdif_atypically_employed = mnl_fd2_ova(model = multi1,
                                        data = df2,
                                        x = "atypically_employed",
                                        value1 = min(df2$atypically_employed,na.rm=TRUE),
                                        value2 = max(df2$atypically_employed,na.rm=TRUE),
                                        nsim = 2000,
                                        seed = 260423)


##
## Prediction for "unemployed" category

df3 = ITANES2018
df3$permanently_employed = 0
df3$selfemployed = 0
df3$atypically_employed = 0
set.seed(260423)
pred_unemployed = mnl_pred_ova(model = multi1,
                                data = df3,
                                x = "unemployed",
                                by = 1,
                                seed = 260423,
                                nsim = 2000, # faster
                                probs = c(0.025, 0.975)) 
set.seed(260423)
fdif_unemployed = mnl_fd2_ova(model = multi1,
                               data = df3,
                               x = "unemployed",
                               value1 = min(df3$unemployed,na.rm=TRUE),
                               value2 = max(df3$unemployed,na.rm=TRUE),
                               nsim = 2000,
                               seed = 260423)


#Recode the categories

pred_unemployed$plotdata$category = ifelse(pred_unemployed$plotdata$unemployed == 1, 'Unemployed', 'Self-employed')
pred_atypically_employed$plotdata$category = ifelse(pred_atypically_employed$plotdata$atypically_employed == 1, "Atypically employed", "Self-employed")
pred_permanently_empl$plotdata$category = ifelse(pred_permanently_empl$plotdata$permanently_empl == 1, "Permanently employed", "Self-employed")


pred_atypically_employed$plotdata = pred_atypically_employed$plotdata %>% 
  filter(category != "Self-employed")
pred_permanently_empl$plotdata = pred_permanently_empl$plotdata %>% 
  filter(category != "Self-employed")


pps$category<-factor(pps$category, levels=c("Unemployed" , "Atypically employed","Permanently employed", "Self-employed"))

#Bind all the data together.

pps = pred_unemployed$plotdata %>% 
  bind_rows(pred_atypically_employed$plotdata) %>%
  bind_rows(pred_permanently_empl$plotdata)


pps$category = as.factor(as.character(pps$category))


##PLOT 

FigureA2 = pps %>% 
  ggplot(aes(category, shape = category, y = mean, ymin = lower, ymax = upper))+
  geom_pointrange()+
geom_pointrange()+ 
facet_wrap(~voto)+
scale_shape_manual(values = c(18,15,17,20)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.6), breaks=c(0,0.2,0.4,0.6))+ 
  theme_bw() +
  labs(x = "",
       y = "Predicted probabilities")+ 
  scale_color_brewer(type = 'div', palette = 4)+ 
  theme(axis.text.x = element_text( size=14))+ 
  theme(axis.text.y = element_text( size=14))+ 
  theme(axis.title = element_text(size = 15))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(shape = "Employment status")+ 
  theme(legend.text=element_text(size=12))+ 
  theme(legend.title=element_text(size=12))+
  theme(text = element_text(family = "LM Roman 10"))



ggsave("FigureA2_paper_ES.png",
       plot = FigureA2,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots')


##
#### PLOTS AND PREDICTIONS FROM ONLINE APPENDIX ###

#This model is not present in the Paper, it's based on the predictions of the 2013 elections

#Dummies for the variables
ITANES2018$abstain_2013=ifelse(ITANES2018$voto_abstain_2013=="Abstain", 1, 0)
ITANES2018$Challengerleft_2013<-ifelse(ITANES2018$voto_abstain_2013=="Challenger-left", 1, 0)
ITANES2018$Challengerright_2013=ifelse(ITANES2018$voto_abstain_2013=="Challenger-right", 1, 0)
ITANES2018$Mainstreamright_2013=ifelse(ITANES2018$voto_abstain_2013=="Mainstream right", 1, 0)
ITANES2018$Mainstreamleft_2013=ifelse(ITANES2018$voto_abstain_2013=="Mainstream left", 1, 0)

#Model 4d from Online appendix

multi1d=multinom(voto~fear_all_dummy_numeric+eco_hardship_dummy+permanently_employed+atypically_employed+unemployed+age+gender+education+Center+South+union+left+centre_left+centre+centre_right+right+euro+immigration+trust+populism+incumbent+Mainstreamright_2013+Mainstreamleft_2013+Challengerright_2013+Challengerleft_2013, data=ITANES2018, Hess=TRUE)




pred_perceived = mnl_pred_ova(model = multi1d,
                               data = ITANES2018,
                               x = "fear_all_dummy_numeric",
                               by = 1,
                               seed = 260423,
                               nsim = 2000, 
                               probs = c(0.025, 0.975))



pred_perceived$plotdata = pred_perceived$plotdata %>% 
  mutate(fear_all_dummy = factor(fear_all_dummy_numeric,
                                         levels = c(0, 1),
                                         labels = c('Not afraid', 'Afraid')))


Pred2013 = pred_perceived$plotdata %>%
  ggplot(aes(fear_all_dummy, shape = fear_all_dummy, y = mean, ymin = lower, ymax = upper))+
  geom_pointrange()+
  facet_wrap(~voto)+
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.6), breaks=c(0,0.2,0.4, 0.6)) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "")+ 
  facet_wrap(.~ voto,  nrow = 2)+ 
  theme(axis.text.y = element_text( size=14))+ 
  theme(axis.title = element_text(size = 14))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ 
  labs(shape = "Fear of job loss")+ 
  theme(legend.text=element_text(size=12))+ 
  theme(legend.title=element_text(size=12))+
  theme(text = element_text(family = "LM Roman 10"))



ggsave("Pred2013_Nopaper.png",
       plot = Pred2013,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots')




##FIGURE S1 (2013 elections)

fdif_perceived = mnl_fd2_ova(model = multi1d,
                              data = ITANES2018,
                              x = "fear_all_dummy_numeric",
                              value1 = min(ITANES2018$fear_all_dummy_numeric, na.rm=TRUE),
                              value2 = max(ITANES2018$fear_all_dummy_numeric, na.rm=TRUE),
                              nsim = 2000,
                              seed = 260423)

FigureS1 = fdif_perceived$plotdata_fd %>% 
  ggplot(aes(categories, y = mean, ymin = lower, ymax = upper))+
  geom_pointrange()+
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Vote choice") + 
  theme(axis.text.x = element_text( size=14))+ 
  theme(axis.text.y = element_text( size=14))+ 
  theme(axis.title = element_text(size = 15)) + 
  theme(
    axis.title.x = element_text(hjust=0.5),
    axis.title.y = element_text(hjust=0.5)   
  )+
  theme(text = element_text(family = "LM Roman 10"))
  
ggsave("Fd2013_FS1_OA.png",
       plot = FigureS1,
       device = "png",
       units = "in",
       width = 8,
       height = 5,
       dpi = 600,
       path = 'Plots')



##Turnout Prediction model
#Turnout model m1, of the first section

#Holding constant (at their mean level), all the other covariates, predict the probability of turnout.


set.seed(260423)

newdata = with(ITANES2018, data.frame(
  fear_all_dummy_numeric = c(0,1),
  employment_status = 'Permanently employed',
  age = mean(age, na.rm = T),
  gender = 1,
  education=mean(education, na.rm=TRUE),
  residence="North",
  union= 0, 
  ideology="Left", 
  euro=mean(euro, na.rm=TRUE), 
  immigration=mean(immigration, na.rm=TRUE),
  trust=mean(trust, na.rm=TRUE), 
  eco_hardship_dummy=mean(eco_hardship, na.rm=TRUE),
  populism=mean(populism, na.rm=TRUE), 
  incumbent=mean(incumbent, na.rm=TRUE)))


preds = predict(m1, newdata, type="response", se.fit=TRUE)
predf = preds$fit # predicted
lower = preds$fit - (1.96*preds$se.fit) # lower bounds
upper = preds$fit + (1.96*preds$se.fit) # upper bounds

predf

#Plotting the result (##NOT INCLUDED BY THE AUTHOR##)

df_pred = tibble(
  Profile = c('Profile 1', 'Profile 2'),
  prediction = preds$fit,
  se = preds$se.fit, 
  lower = preds$fit - (1.96*preds$se.fit), # lower bounds
  upper = preds$fit + (1.96*preds$se.fit))

df_pred %>% 
  ggplot(aes(Profile, y = prediction, shape = Profile, ymin = lower, ymax = upper))+
  geom_pointrange(aes(color = Profile))+
  theme_bw()



#Their difference is not statistically significant.
#Simple model to test their difference


pr_test = function(data = df_pred){
  diff = df_pred$prediction[1] - df_pred$prediction[2]
  
  se_diff = sqrt((df_pred$se[1]^2 + df_pred$se[2]^2))
  
  z_stat = diff/se_diff
  
  p_value = 2*(1- pnorm(abs(z_stat)))#two-tailed
  
  cat("Difference:", diff, "\nZ:", z_stat, "\nP-value:", p_value, "\n")
  
  
}



pr_test(df_pred)

#the difference is not statistically significant (at 95% - 99% - 99.9%)
