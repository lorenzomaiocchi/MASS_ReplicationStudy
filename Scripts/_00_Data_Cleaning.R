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



#Gender Dummy, Region and Union

ITANES2013 = ITANES2013 %>% 
  mutate(
    
    gender_dummy =
      ifelse(sex == 'Donna', 1,0), #1 females, 0 males
    
    residence = fct_collapse(
      regioni,
      North = c('Lombardia', 'Piemonte', 'Liguria','Valle d\'Aosta', 'Trentino Alto Adige',
                'Veneto', 'Friuli Venezia Giulia', 'Emilia Romagna'),
      Center = c('Toscana', 'Umbria', 'Marche', 'Lazio', 'Abruzzo', 'Molise'),
      South = c('Campania', 'Basilicata', 'Puglia', 'Calabria', 'Sicilia', 'Sardegna') #islands included,
    ),
    
    Union = ifelse(ITANES2013$d78_7 == 'Sì', 1, 
                   ifelse(ITANES2013$d78_7 == 'No', 0, NA)),
    
    age = d1
    )
    
#Residence dummies:

ITANES2013$North = ifelse(ITANES2013$residence == 'North', 1, 0)
ITANES2013$Center = ifelse(ITANES2013$residence == 'Center', 1, 0)
ITANES2013$South = ifelse(ITANES2013$residence == 'South', 1, 0)


table(ITANES2013$Union )
table(ITANES2013$residence)
table(ITANES2013$gender_dummy)

#Education


ITANES2013 = ITANES2013 %>% 
  mutate(
    
    education = fct_collapse( stu,
      
      Diploma = c('Diploma maturità professionale (compreso istituto d\'arte)',
                  'Diploma maturità tecnica', 'Diploma maturità liceo classico o scientifico',
                  "Altro diploma maturità (istituto magistrale, liceo linguistico, liceo artistico, liceo socio-psico-p"),
      University = c("Laurea Scientifica (3/4/5 anni, laurea triennale, laurea specialistica) (include medicina, biologia ",
                    "Laurea Umanistica (3/4/5 anni, laurea triennale, laurea specialistica) (include psicologia, sociolog")
    )
      
 )


levels(ITANES2013$education)[7] = NA

ITANES2013$education_numeric = as.numeric(ITANES2013$education)


##POLITICAL IDEOLOGY##
######################


#As numeric

ITANES2013 = ITANES2013 %>% 
  mutate(Ideology_numeric = as.numeric(as.character( case_when(
    d39 == 'Sinistra' ~ "1",
    d39 == 'Destra' ~ "10",
    d39 %in% as.character(1:10) ~ d39,
    TRUE ~ NA_character_
    )
    )))

table(ITANES2013$Ideology_numeric)


#As categorical

categories = c('Left', 'Centre-Left', 'Centre', 'Centre-Right', 'Right')

ITANES2013 = ITANES2013 %>% 
  mutate(
    ideology_category = cut(Ideology_numeric, breaks = c(1, 3, 5, 7, 9, 11), right = F, labels = categories) 
  )

table(ITANES2013$ideology_category, ITANES2013$Ideology_numeric)


#Dummies

ITANES2013 = ITANES2013 %>% 
  mutate(
    Left = ifelse(ITANES2013$ideology_category == 'Left', 1, 0),
    Right = ifelse(ITANES2013$ideology_category == 'Right', 1, 0),
    Centre_Left = ifelse(ITANES2013$ideology_category == 'Centre-Left', 1, 0),
    Centre_Right = ifelse(ITANES2013$ideology_category == 'Centre-Right', 1, 0),
    Centre = ifelse(ITANES2013$ideology_category == 'Centre', 1, 0),
    None = ifelse(is.na(ITANES2013$ideology_category) == TRUE, 1, 0)
  )



##EUROPEAN UNION##
##################


ITANES2013$Trust_EU = ITANES2013$d15_5

levels(ITANES2013$Trust_EU) [5:6] = NA

table(ITANES2013$Trust_EU)


ITANES2013$Trust_EU_numeric = as.numeric(as.factor(ITANES2013$Trust_EU))

####IMMIGRATION#####
###################

#The variable in question is different from the original.


ITANES2013$immigration = ITANES2013$d44_8

levels(ITANES2013$immigration) [5:6] = NA

ITANES2013$immigration = factor(ITANES2013$immigration,
                             levels = c( "Per niente d'accordo", "Poco d'accordo",
                                         "Abbastanza d'accordo", "Molto d'accordo"))

ITANES2013$immigration_numeric = as.numeric(as.factor(ITANES2013$immigration))


##Populist attitudes##
#####################



ITANES2013$Populism = ITANES2013$d22_5

ITANES2013 <- ITANES2013 %>%
  mutate(
    Populism = case_when(
      Populism %in% c("Non sa", "Non risponde") ~ NA_character_,
      TRUE ~ as.character(Populism)
    ),
    Populism = factor(Populism),
    Populism_numeric = as.numeric(Populism)
  )


###Trust in Political Parties###
################################

ITANES2013  = ITANES2013 %>%
  mutate(
    trust_parties = case_when(
      
      d15_2 %in% c('Non sa', 'Non risponde')~ NA_character_,
      TRUE~ as.character(d15_2) 
    ),
    
    trust_parties = as.factor(trust_parties),
    trust_parties_numeric = as.numeric(trust_parties)
    
  )


  
##Economic hardship##
#####################

ITANES2013 = ITANES2013 %>% 
  mutate(
    Economic_hardship_dummy = case_when(
      
      
      lav3 %in% c('Sì, di frequente', 'Sì, qualche volta')~ 1,
      lav3 == 'No' ~ 0,
      TRUE ~ NA_integer_
      
    )
    
  )

table(ITANES2013$Economic_hardship_dummy, useNA = 'always')


##################################
###### Turnout and Vote Choice####



ITANES2013 = ITANES2013 %>% 
  mutate(
    turnout = case_when(
      d86 == 'Sì, sono andato a votare'~ 1,
      d86 == 'No, non sono andato a votare'~ 0,
      TRUE  ~ NA_integer_
    )
    
  )


table(ITANES2013$turnout)


##Political Parties:

#taking only the chamber of deputees since the law didn't permit people younger than 24 to vote for the Senate.

ITANES2013$voto = ITANES2013$d90

ITANES2013$voto = fct_collapse(
  ITANES2013$voto,
  
  Pd = 'Partito Democratico',
  Pdl = "Il Popolo Della Liberta'",
  M5s = 'Movimento 5 Stelle Beppegrillo.It',
  Fdi = "Fratelli D'italia",
  Lega = 'Lega Nord'
  
)

#Mainstream vs Challengers.


##NOTE: READ THE "IMPORTANT" MARKDOWN

#Due to the low numbers of dx- challenger parties, I'm going to incorporate all the alternative right wing parties into "Challengers"

#Challenger right is present
ITANES2013$type_voto_cdx = fct_collapse(
  ITANES2013$voto,
  
  "Mainstream Left" = c('Pd', 'Scelta Civica Con Monti Per L\'italia', 'Centro Democratico', 'Unione Di Centro'),
  "Mainstream Right" = c('Pdl', 'Lega'),
  'Challenger Left' = 'M5s',
  'Challenger Right' = c('Fdi', 'La Destra', 'Fiamma Tricolore', 'Forza Nuova', 'Casapound Italia', 'Partito Pensionati', 'Grande Sud - Mpa'),
  'Others' = c('altro', "Lista Amnistia Giustizia Liberta'", 'Partito Comunista Dei Lavoratori', 'Mir - Moderati In Rivoluzione',
               'Il Megafono Di Crocetta', 'Svp', "Futuro E Liberta'", 'Fare Per Fermare Il Declino',
               'Rivoluzione Civile'),
  'No voto' = c('Scheda Bianca/Nulla/Non Ha Votato', 'Non Indica, Non Vuole Risponde', '-1')
  
)


#Incorporated challenger right into the  mainstream right


ITANES2013$type_voto_inc = fct_collapse(
  ITANES2013$voto,
  
  "Mainstream Left" = c('Pd', 'Scelta Civica Con Monti Per L\'italia', 'Centro Democratico', 'Unione Di Centro'),
  "Mainstream Right" = c('Pdl', 'Lega', 'Fdi', 'La Destra', 'Fiamma Tricolore', 'Forza Nuova', 'Casapound Italia', 'Partito Pensionati', 'Grande Sud - Mpa'),
  'Challenger Left' = 'M5s',
  'Others' = c('altro', "Lista Amnistia Giustizia Liberta'", 'Partito Comunista Dei Lavoratori', 'Mir - Moderati In Rivoluzione',
               'Il Megafono Di Crocetta', 'Svp', "Futuro E Liberta'", 'Fare Per Fermare Il Declino',
               'Rivoluzione Civile'),
  'No voto' = c('Scheda Bianca/Nulla/Non Ha Votato', 'Non Indica, Non Vuole Risponde', '-1')
  
)


#The majority of the respondents didn't vote, this is too big of a proportion to leave out.

#we can set 'others' as NA
levels(ITANES2013$type_voto_cdx)[2] = NA

levels(ITANES2013$type_voto_inc)[2] = NA

sum(is.na(ITANES2013$type_voto_cdx)) / sum(!is.na(ITANES2013$type_voto_cdx)) *100 #leaving out just 3.7 percent of the total sample.


sum(is.na(ITANES2013$type_voto_inc)) / sum(!is.na(ITANES2013$type_voto_inc)) *100 #leaving out 3.5% of the sample

table(ITANES2013$turnout, ITANES2013$type_voto_inc, useNA = 'always')


#Set the order:

ITANES2013$type_voto_cdx = factor(
  ITANES2013$type_voto_cdx, levels= c(
    'Mainstream Left', 'Mainstream Right', 'Challenger Left', 'Challenger Right', 'No voto'
  ))


ITANES2013$type_voto_inc = factor(
  ITANES2013$type_voto_inc, levels= c(
    'Mainstream Left', 'Mainstream Right', 'Challenger Left', 'No voto'))


#Challenger / No vote vs Mainstream

ITANES2013$voto_challenger = ITANES2013$type_voto_cdx


levels(ITANES2013$voto_challenger)[1:2] = 'Mainstream'

ITANES2013$voto_challenger = relevel(ITANES2013$voto_challenger, ref = 'Mainstream')



#Mainstream/No vote  vs Challenger

ITANES2013$voto_Mainstream = ITANES2013$type_voto_cdx


levels(ITANES2013$voto_Mainstream)[3:4] = 'Challenger'

ITANES2013$voto_Mainstream = relevel(ITANES2013$voto_Mainstream, ref = 'Challenger')


#Mainstream / Challenger vs No vote

ITANES2013$voto_novote = ITANES2013$type_voto_inc


ITANES2013$voto_novote = relevel(ITANES2013$voto_novote, ref = 'No voto')



##INCUMBENT##
############

ITANES2013 = ITANES2013 %>% 
  mutate(
    incumbent_numeric = as.numeric(case_when(
      
      d9 == "0 - completamente negativo" ~ '0',
      d9 == "10 - completamente positivo" ~ '10',
      
      as.character(d9) %in% as.character(1:9) ~ as.character(d9),
      TRUE ~ NA_character_
    
        
        )
    ))

#from 0, negative to 10, positive




#####EMPLOYMENT AND UNEMPLOYMENT#####
#####################################



ITANES2013 = ITANES2013 %>% 
  mutate(
    Occupational_Status= case_when(
      
      d4 %in% c('Disoccupato/a', 'Cassa integrazione guadagni, lista di mobilità',
                'In cerca di prima occupazione') ~ 'Unemployed',
      d4 == '-1' ~ 'Employed',
      
      d4 %in% c('Inabila al lavoro', 'Benestante', 'Casalinga', 'Studente/essa' ) ~ 'Inactive',
      
      d4 == "Pensionato/a o ritirato/a dal lavoro" ~ 'Retired',
      
      TRUE ~ NA_character_
      
      
    )
  )

table(ITANES2013$Occupational_Status, useNA = 'always')


#Type of contract (2 options:  1- Unemployed = NAS, 2- Unemployed are categorized)

#Categorize the Unemployed

ITANES2013 = ITANES2013 %>% 
  mutate(
    
    type_of_contract = fct_collapse(
      cont,
      
      'Permanently employed' = 'A tempo indeterminato',
      'Atypically employed' = c('A tempo determinato', 'Lavoro senza contratto o non regolamentato'),
      'Not a worker' = '-1'
      
      
    )
    
  )

#Unemployed as NAs

ITANES2013 = ITANES2013 %>% 
  mutate(
    
    type_of_contract_na = case_when(
      
      cont == 'A tempo indeterminato' ~ 'Permanently employed',
      cont %in% c('A tempo determinato', 'Lavoro senza contratto o non regolamentato') ~ 'Atypically employed',
      TRUE ~ NA_character_
      
    ))
    

##Employment status (synthesis of occupational + type of contract) and dummies


ITANES2013 = ITANES2013 %>% 
  mutate(
    
    Employment_status = case_when(
      
      Occupational_Status == 'Employed' & type_of_contract == 'Permanently employed' ~ 'Permanently employed',
      Occupational_Status == 'Employed' & type_of_contract == 'Atypically employed' ~ 'Atypically employed',
      Occupational_Status == 'Unemployed' & type_of_contract == 'Not a worker' ~ 'Unemployed',
      Occupational_Status == 'Inactive' & type_of_contract == 'Not a worker' ~ 'Inactive',
      Occupational_Status == 'Retired' & type_of_contract == 'Not a worker' ~ 'Retired'
      ),
    Permanently_employed = ifelse(Employment_status == 'Permanently employed', 1, 0),
    Atypically_employed = ifelse(Employment_status == 'Atypically employed', 1, 0),
    unemployed = ifelse(Employment_status == 'Unemployed', 1, 0),
    inactive = ifelse(Employment_status == 'Inactive', 1, 0),
    retired = ifelse(Employment_status == 'Retired', 1, 0)
)

table(ITANES2013$Occupational_Status, ITANES2013$type_of_contract)


######################
###FEAR OF JOB LOSS###



ITANES2013 = ITANES2013 %>% 
  mutate(
    
    Fear_all = case_when(
      
      lav2 %in% c('-1', 'Non risponde')  ~ NA_character_,
      
      TRUE ~ lav2
      ),
    Fear_all =factor(
      Fear_all,
      levels = c('Non ho/ha avuto nessuna paura', "Ho/ha avuto un po' di paura di perdere il posto", 
                 "Ho/ha avuto molta paura di perdere il posto")
    ))


#Dummy active workers:


ITANES2013$Fear_all_numeric = ifelse(ITANES2013$Fear_all %in% c("Ho/ha avuto un po' di paura di perdere il posto", 
                                                                                    "Ho/ha avuto molta paura di perdere il posto"),
                                              1, 0)

ITANES2013$Fear_all = as.factor(ITANES2013$Fear_all_numeric)


table(ITANES2013$Fear_all, ITANES2013$unemployed) #10 unemployed who responded positively are Housekeepers and People in Paid Leave.

#keep also in mind that the question in phrased in such way that asks people to respond in the place of others (relatives)


#Fear of jobloss, just for active workers:


ITANES2013 = ITANES2013 %>% 
  mutate(
    
    Fear_active_workers = factor(case_when(
      
      Employment_status %in% c('Permanently employed' , 'Atypically employed') & Fear_all_numeric >= 2 ~ 'Afraid',
      Employment_status %in% c('Permanently employed' , 'Atypically employed') & Fear_all_numeric < 2 ~ 'Not afraid',
      TRUE ~ NA_character_
      
    )),
    Fear_active_workers_dummy = ifelse(Fear_active_workers == 'Afraid', 1 , 0),
    Fear_active_workers = relevel(Fear_active_workers, ref = 'Not afraid')
    
  )



#Save the final dataset


ITANES2013_ready = ITANES2013

save(ITANES2013_ready, file = 'Output/Saved data/ITANES2013_ready.RData')






#####ADDITION TO THE ITANES 2018



#include the NAs as non voters.



ITANES2018 = ITANES2018 %>% 
  mutate(
    voto_type = fct_collapse(
      votocheck_post,
      "Mainstream left" = c("Ho votato Più Europa con Emma Bonino, alleato del PD", "Ho votato Partito Democratico"),
      "Mainstream right" = c("Ho votato Forza Italia – Berlusconi Presidente", "Ho votato Noi con l’Italia"),
      'Challenger right' = c("Ho votato Lega – Salvini Premier", "Ho votato Fratelli d’Italia"),
      'Challenger left' = 'Ho votato il MoVimento 5 Stelle',
      'Other'= c("Ho votato Liberi e Uguali con Pietro Grasso", "Ho votato Civica Popolare - Lorenzin", "Ho votato un’altra lista o partito" ),
      'No vote' = c("Ho votato scheda bianca o scheda nulla", "Non sono andato a votare", 'Ho votato Insieme'))
)


table(ITANES2018$turnout, ITANES2018$voto_type, useNA = 'always')

#there are 318 respondetns who voted but of whom we do not know the party.

#In this case, non respondents are classified as "Others".

ITANES2018 = ITANES2018 %>% 
  mutate(
    voto_type_na = as.factor(case_when(
      
     is.na(turnout)== T & is.na(voto_type) == T~ 'No vote',
     
      
     is.na(turnout) == T & voto_type == 'Preferisco non rispondere'~ 'No vote',
     
     
     voto_type ==  'Preferisco non rispondere'~ 'Other', 
      
     TRUE~ voto_type 
      
    )),
    voto_type_na = relevel(voto_type_na, ref = 'Mainstream left')
  )


#Challenger, no vote, other vs Mainstream


ITANES2018$voto_challenger2 = ITANES2018$voto_type_na 
  
levels(ITANES2018$voto_challenger2)[c(1, 4)] = 'Mainstream'

ITANES2018$voto_challenger2 = relevel(ITANES2018$voto_challenger2, ref = 'Mainstream')


#Mainstream, no vote, other vs Challenger


ITANES2018$voto_mainstream2 = ITANES2018$voto_type_na 

levels(ITANES2018$voto_mainstream2)[2:3] = 'Challenger'

ITANES2018$voto_mainstream2 = relevel(ITANES2018$voto_mainstream2, ref = 'Challenger')




##Challenger, Mainstream, other vs no vote



ITANES2018$voto_notvote = ITANES2018$voto_type_na

ITANES2018$voto_notvote = relevel(ITANES2018$voto_notvote, ref = 'No vote')



##
##Make the non respondents as NAs



ITANES2018 = ITANES2018 %>% 
  mutate(
    voto_type_nr = as.factor(case_when(
      
      is.na(turnout)== T & is.na(voto_type) == T~ 'No vote',
      
      
      is.na(turnout) == T & voto_type == 'Preferisco non rispondere'~ 'No vote',
      
      
      voto_type ==  'Preferisco non rispondere'~ NA_character_, 
      
      TRUE~ voto_type 
      
    )),
    voto_type_nr = relevel(voto_type_nr, ref = 'Mainstream left')
  )


##
### Ramake the variables for single voting preferences:




ITANES2018$voto_challenger_nr = ITANES2018$voto_type_nr 

levels(ITANES2018$voto_challenger_nr)[c(1, 4)] = 'Mainstream'

ITANES2018$voto_challenger_nr = relevel(ITANES2018$voto_challenger_nr, ref = 'Mainstream')


#Mainstream, no vote, other vs Challenger


ITANES2018$voto_mainstream_nr = ITANES2018$voto_type_nr

levels(ITANES2018$voto_mainstream_nr)[2:3] = 'Challenger'

ITANES2018$voto_mainstream_nr = relevel(ITANES2018$voto_mainstream_nr, ref = 'Challenger')




##Challenger, Mainstream, other vs no vote



ITANES2018$voto_notvote_nr = ITANES2018$voto_type_nr

ITANES2018$voto_notvote_nr = relevel(ITANES2018$voto_notvote_nr, ref = 'No vote')


###Save the final dataset for 2018


ITANES2018_ready2 = ITANES2018

save(ITANES2018_ready2, file = 'Output/Saved data/ITANES2018_ready2.RData')


