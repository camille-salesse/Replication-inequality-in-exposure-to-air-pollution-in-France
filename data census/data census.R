



#the geography of the data for the years 2014 and 2015 is prior to 2019 and requires the merging of municipalities 
#to obtain the municipalities with the most recent geography  

#census 1 : population, age of population, male / female
#census 2 : family composition
#census 3 : type of activity of the working population



setwd("D:/code and data inequality in exposure to air pollution")




######census 1#####

library(readxl)
library(dplyr)


pop_2018 <- read_excel("data census/census pop structure data/structure_pop_2018.xlsx")
pop_2018<-pop_2018[,c(1:13,24,55:63)]
#geography 01/01/2021
names(pop_2018)[names(pop_2018)=="Code géographique"]<-"insee"
names(pop_2018)[names(pop_2018)=="Population en 2018 (princ)"]<-"population"
names(pop_2018)[names(pop_2018)=="Pop 0-14 ans en 2018 (princ)"]<-"pop_0_14"
names(pop_2018)[names(pop_2018)=="Pop 15-29 ans en 2018 (princ)"]<-"pop_15_29"
names(pop_2018)[names(pop_2018)=="Pop 30-44 ans en 2018 (princ)"]<-"pop_30_44"
names(pop_2018)[names(pop_2018)=="Pop 45-59 ans en 2018 (princ)"]<-"pop_45_59"
names(pop_2018)[names(pop_2018)=="Pop 60-74 ans en 2018 (princ)"]<-"pop_60_74"
names(pop_2018)[names(pop_2018)=="Pop 75-89 ans en 2018 (princ)"]<-"pop_75_89"
names(pop_2018)[names(pop_2018)=="Pop 90 ans ou plus en 2018 (princ)"]<-"pop_90_plus"
names(pop_2018)[names(pop_2018)=="Pop Hommes en 2018 (princ)"]<-"pop_homme"
names(pop_2018)[names(pop_2018)=="Pop Femmes en 2018 (princ)"]<-"pop_femme"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus en 2018 (compl)"]<-"pop_15_plus"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Agriculteurs exploitants en 2018 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2018 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2018 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Prof. intermédiaires  en 2018 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Employés en 2018 (compl)"]<-"pop_15_plus_empl"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Ouvriers en 2018 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Retraités en 2018 (compl)"]<-"pop_15_plus_retraite"
names(pop_2018)[names(pop_2018)=="Pop 15 ans ou plus Autres en 2018 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2018<-pop_2018[-1,]
pop_2018[,c(5:23)]<-sapply(pop_2018[,c(5:23)],as.numeric)
#sapply(pop_2018, class)
pop_2018$annee<-2018




pop_2017 <- read_excel("data census/census pop structure data/structure_pop_2017.xlsx")
pop_2017<-pop_2017[,c(1:13,24,55:63)]
#geography  01/01/2020
names(pop_2017)[names(pop_2017)=="Code géographique"]<-"insee"
names(pop_2017)[names(pop_2017)=="Population en 2017 (princ)"]<-"population"
names(pop_2017)[names(pop_2017)=="Pop 0-14 ans en 2017 (princ)"]<-"pop_0_14"
names(pop_2017)[names(pop_2017)=="Pop 15-29 ans en 2017 (princ)"]<-"pop_15_29"
names(pop_2017)[names(pop_2017)=="Pop 30-44 ans en 2017 (princ)"]<-"pop_30_44"
names(pop_2017)[names(pop_2017)=="Pop 45-59 ans en 2017 (princ)"]<-"pop_45_59"
names(pop_2017)[names(pop_2017)=="Pop 60-74 ans en 2017 (princ)"]<-"pop_60_74"
names(pop_2017)[names(pop_2017)=="Pop 75-89 ans en 2017 (princ)"]<-"pop_75_89"
names(pop_2017)[names(pop_2017)=="Pop 90 ans ou plus en 2017 (princ)"]<-"pop_90_plus"
names(pop_2017)[names(pop_2017)=="Pop Hommes en 2017 (princ)"]<-"pop_homme"
names(pop_2017)[names(pop_2017)=="Pop Femmes en 2017 (princ)"]<-"pop_femme"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus en 2017 (compl)"]<-"pop_15_plus"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Agriculteurs exploitants en 2017 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2017 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2017 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Prof. intermédiaires  en 2017 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Employés en 2017 (compl)"]<-"pop_15_plus_empl"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Ouvriers en 2017 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Retraités en 2017 (compl)"]<-"pop_15_plus_retraite"
names(pop_2017)[names(pop_2017)=="Pop 15 ans ou plus Autres en 2017 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2017<-pop_2017[-1,]
pop_2017[,c(5:23)]<-sapply(pop_2017[,c(5:23)],as.numeric)
pop_2017$annee<-2017



pop_2013 <- read_excel("data census/census pop structure data/structure_pop_2013_geo2021.xlsx")
pop_2013<-pop_2013[,c(1:13,24,55:63)]
#geography  01/01/2021
names(pop_2013)[names(pop_2013)=="Code géographique"]<-"insee"
names(pop_2013)[names(pop_2013)=="Population en 2013 (princ)"]<-"population"
names(pop_2013)[names(pop_2013)=="Pop 0-14 ans en 2013 (princ)"]<-"pop_0_14"
names(pop_2013)[names(pop_2013)=="Pop 15-29 ans en 2013 (princ)"]<-"pop_15_29"
names(pop_2013)[names(pop_2013)=="Pop 30-44 ans en 2013 (princ)"]<-"pop_30_44"
names(pop_2013)[names(pop_2013)=="Pop 45-59 ans en 2013 (princ)"]<-"pop_45_59"
names(pop_2013)[names(pop_2013)=="Pop 60-74 ans en 2013 (princ)"]<-"pop_60_74"
names(pop_2013)[names(pop_2013)=="Pop 75-89 ans en 2013 (princ)"]<-"pop_75_89"
names(pop_2013)[names(pop_2013)=="Pop 90 ans ou plus en 2013 (princ)"]<-"pop_90_plus"
names(pop_2013)[names(pop_2013)=="Pop Hommes en 2013 (princ)"]<-"pop_homme"
names(pop_2013)[names(pop_2013)=="Pop Femmes en 2013 (princ)"]<-"pop_femme"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus en 2013 (compl)"]<-"pop_15_plus"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Agriculteurs exploitants en 2013 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2013 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2013 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Prof. intermédiaires  en 2013 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Employés en 2013 (compl)"]<-"pop_15_plus_empl"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Ouvriers en 2013 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Retraités  en 2013 (compl)"]<-"pop_15_plus_retraite"
names(pop_2013)[names(pop_2013)=="Pop 15 ans ou plus Autres en 2013 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2013<-pop_2013[-1,]
pop_2013[,c(5:23)]<-sapply(pop_2013[,c(5:23)],as.numeric)
pop_2013$annee<-2013



pop_2016 <- read_excel("data census/census pop structure data/structure_pop_2016.xlsx")
pop_2016<-pop_2016[,c(1:13,24,55:63)]
#geography  01/01/2019
names(pop_2016)[names(pop_2016)=="Code géographique"]<-"insee"
names(pop_2016)[names(pop_2016)=="Population en 2016 (princ)"]<-"population"
names(pop_2016)[names(pop_2016)=="Pop 0-14 ans en 2016 (princ)"]<-"pop_0_14"
names(pop_2016)[names(pop_2016)=="Pop 15-29 ans en 2016 (princ)"]<-"pop_15_29"
names(pop_2016)[names(pop_2016)=="Pop 30-44 ans en 2016 (princ)"]<-"pop_30_44"
names(pop_2016)[names(pop_2016)=="Pop 45-59 ans en 2016 (princ)"]<-"pop_45_59"
names(pop_2016)[names(pop_2016)=="Pop 60-74 ans en 2016 (princ)"]<-"pop_60_74"
names(pop_2016)[names(pop_2016)=="Pop 75-89 ans en 2016 (princ)"]<-"pop_75_89"
names(pop_2016)[names(pop_2016)=="Pop 90 ans ou plus en 2016 (princ)"]<-"pop_90_plus"
names(pop_2016)[names(pop_2016)=="Pop Hommes en 2016 (princ)"]<-"pop_homme"
names(pop_2016)[names(pop_2016)=="Pop Femmes en 2016 (princ)"]<-"pop_femme"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus en 2016 (compl)"]<-"pop_15_plus"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Agriculteurs exploitants en 2016 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2016 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2016 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Prof. intermédiaires  en 2016 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Employés en 2016 (compl)"]<-"pop_15_plus_empl"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Ouvriers en 2016 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Retraités en 2016 (compl)"]<-"pop_15_plus_retraite"
names(pop_2016)[names(pop_2016)=="Pop 15 ans ou plus Autres en 2016 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2016<-pop_2016[-1,]
pop_2016[,c(5:23)]<-sapply(pop_2016[,c(5:23)],as.numeric)
pop_2016$annee<-2016






pop_2015 <- read_excel("data census/census pop structure data/structure_pop_2015.xlsx")
pop_2015<-pop_2015[,c(1:13,24,55:63)]
#geography  01/01/2017
names(pop_2015)[names(pop_2015)=="Code géographique"]<-"insee"
names(pop_2015)[names(pop_2015)=="Population en 2015 (princ)"]<-"population"
names(pop_2015)[names(pop_2015)=="Pop 0-14 ans en 2015 (princ)"]<-"pop_0_14"
names(pop_2015)[names(pop_2015)=="Pop 15-29 ans en 2015 (princ)"]<-"pop_15_29"
names(pop_2015)[names(pop_2015)=="Pop 30-44 ans en 2015 (princ)"]<-"pop_30_44"
names(pop_2015)[names(pop_2015)=="Pop 45-59 ans en 2015 (princ)"]<-"pop_45_59"
names(pop_2015)[names(pop_2015)=="Pop 60-74 ans en 2015 (princ)"]<-"pop_60_74"
names(pop_2015)[names(pop_2015)=="Pop 75-89 ans en 2015 (princ)"]<-"pop_75_89"
names(pop_2015)[names(pop_2015)=="Pop 90 ans ou plus en 2015 (princ)"]<-"pop_90_plus"
names(pop_2015)[names(pop_2015)=="Pop Hommes en 2015 (princ)"]<-"pop_homme"
names(pop_2015)[names(pop_2015)=="Pop Femmes en 2015 (princ)"]<-"pop_femme"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus en 2015 (compl)"]<-"pop_15_plus"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Agriculteurs exploitants en 2015 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2015 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2015 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Prof. intermédiaires  en 2015 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Employés en 2015 (compl)"]<-"pop_15_plus_empl"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Ouvriers en 2015 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Retraités en 2015 (compl)"]<-"pop_15_plus_retraite"
names(pop_2015)[names(pop_2015)=="Pop 15 ans ou plus Autres en 2015 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2015<-pop_2015[-1,]
pop_2015[,c(5:23)]<-sapply(pop_2015[,c(5:23)],as.numeric)


#work on merging municipalities :


library(readxl)

data_fusion <- read_excel("C:/données fusion communes/data_fusion.xlsx")
test<-left_join(data_fusion,pop_2015)
test<-test[,c(1,8:26)]

ag_1<-aggregate(.~DepComN,test,sum)

library(writexl)
#writexl::write_xlsx(ag_1, "C:/données fusion communes/ag_recensement_1_pop_2015.xlsx")

library(readxl)
ag_census_1_pop_2015 <- read_excel("C:/données fusion communes/ag_recensement_1_pop_2015.xlsx")
names(ag_census_1_pop_2015)[names(ag_census_1_pop_2015)=="DepComN"]<-"insee"
ag_census_1_pop_2015$annee<-2015
names(ag_census_1_pop_2015)[names(ag_census_1_pop_2015)=="population"]<-"population_"

ag_census_1_pop_2015_<-full_join(pop_2015,ag_census_1_pop_2015, by= c("insee"))

ag_census_1_pop_2015_$population_b<-ifelse(is.na(ag_census_1_pop_2015_$population_),ag_census_1_pop_2015_$population,ag_census_1_pop_2015_$population_)
ag_census_1_pop_2015_$pop_0_14.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_0_14.y),ag_census_1_pop_2015_$pop_0_14.x,ag_census_1_pop_2015_$pop_0_14.y)
ag_census_1_pop_2015_$pop_15_29.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_29.y),ag_census_1_pop_2015_$pop_15_29.x,ag_census_1_pop_2015_$pop_15_29.y)
ag_census_1_pop_2015_$pop_30_44.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_30_44.y),ag_census_1_pop_2015_$pop_30_44.x,ag_census_1_pop_2015_$pop_30_44.y)
ag_census_1_pop_2015_$pop_45_59.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_45_59.y),ag_census_1_pop_2015_$pop_45_59.x,ag_census_1_pop_2015_$pop_45_59.y)
ag_census_1_pop_2015_$pop_60_74.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_60_74.y),ag_census_1_pop_2015_$pop_60_74.x,ag_census_1_pop_2015_$pop_60_74.y)
ag_census_1_pop_2015_$pop_75_89.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_75_89.y),ag_census_1_pop_2015_$pop_75_89.x,ag_census_1_pop_2015_$pop_75_89.y)
ag_census_1_pop_2015_$pop_90_plus.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_90_plus.y),ag_census_1_pop_2015_$pop_90_plus.x,ag_census_1_pop_2015_$pop_90_plus.y)
ag_census_1_pop_2015_$pop_homme.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_homme.y),ag_census_1_pop_2015_$pop_homme.x,ag_census_1_pop_2015_$pop_homme.y)
ag_census_1_pop_2015_$pop_femme.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_femme.y),ag_census_1_pop_2015_$pop_femme.x,ag_census_1_pop_2015_$pop_femme.y)
ag_census_1_pop_2015_$pop_15_plus.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus.y),ag_census_1_pop_2015_$pop_15_plus.x,ag_census_1_pop_2015_$pop_15_plus.y)
ag_census_1_pop_2015_$pop_15_plus_agriculteur.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_agriculteur.y),ag_census_1_pop_2015_$pop_15_plus_agriculteur.x,ag_census_1_pop_2015_$pop_15_plus_agriculteur.y)
ag_census_1_pop_2015_$pop_15_plus_artisans_chef_ent.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_artisans_chef_ent.y),ag_census_1_pop_2015_$pop_15_plus_artisans_chef_ent.x,ag_census_1_pop_2015_$pop_15_plus_artisans_chef_ent.y)
ag_census_1_pop_2015_$pop_15_plus_cadre_prof_intel.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_cadre_prof_intel.y),ag_census_1_pop_2015_$pop_15_plus_cadre_prof_intel.x,ag_census_1_pop_2015_$pop_15_plus_cadre_prof_intel.y)
ag_census_1_pop_2015_$pop_15_plus_prof_interme.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_prof_interme.y),ag_census_1_pop_2015_$pop_15_plus_prof_interme.x,ag_census_1_pop_2015_$pop_15_plus_prof_interme.y)
ag_census_1_pop_2015_$pop_15_plus_empl.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_empl.y),ag_census_1_pop_2015_$pop_15_plus_empl.x,ag_census_1_pop_2015_$pop_15_plus_empl.y)
ag_census_1_pop_2015_$pop_15_plus_ouvrier.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_ouvrier.y),ag_census_1_pop_2015_$pop_15_plus_ouvrier.x,ag_census_1_pop_2015_$pop_15_plus_ouvrier.y)
ag_census_1_pop_2015_$pop_15_plus_retraite.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_retraite.y),ag_census_1_pop_2015_$pop_15_plus_retraite.x,ag_census_1_pop_2015_$pop_15_plus_retraite.y)
ag_census_1_pop_2015_$pop_15_plus_autre_sans_activite.b<-ifelse(is.na(ag_census_1_pop_2015_$pop_15_plus_autre_sans_activite.y),ag_census_1_pop_2015_$pop_15_plus_autre_sans_activite.x,ag_census_1_pop_2015_$pop_15_plus_autre_sans_activite.y)


ag_census_1_pop_2015_<-ag_census_1_pop_2015_[,c(1:4,43:62)]

a<-table(ag_census_1_pop_2015_$insee)
a<-as.data.frame(a)


names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="population_b"]<-"population"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_0_14.b"]<-"pop_0_14"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_29.b"]<-"pop_15_29"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_30_44.b"]<-"pop_30_44"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_45_59.b"]<-"pop_45_59"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_60_74.b"]<-"pop_60_74"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_75_89.b"]<-"pop_75_89"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_90_plus.b"]<-"pop_90_plus"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_homme.b"]<-"pop_homme"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_femme.b"]<-"pop_femme"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus.b"]<-"pop_15_plus"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_agriculteur.b"]<-"pop_15_plus_agriculteur"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_artisans_chef_ent.b"]<-"pop_15_plus_artisans_chef_ent"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_cadre_prof_intel.b"]<-"pop_15_plus_cadre_prof_intel"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_prof_interme.b"]<-"pop_15_plus_prof_interme"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_empl.b"]<-"pop_15_plus_empl"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_ouvrier.b"]<-"pop_15_plus_ouvrier"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_retraite.b"]<-"pop_15_plus_retraite"
names(ag_census_1_pop_2015_)[names(ag_census_1_pop_2015_)=="pop_15_plus_autre_sans_activite.b"]<-"pop_15_plus_autre_sans_activite"

#ag_census_1_pop_2015_$a<-rowSums(ag_census_1_pop_2015_[,c(7:13)])
#test : ok
ag_census_1_pop_2015_$annee<-2015





pop_2014 <- read_excel("data census/census pop structure data/structure_pop_2014.xlsx")
pop_2014<-pop_2014[,c(1:13,24,55:63)]
#geography  01/01/2016
names(pop_2014)[names(pop_2014)=="Code géographique"]<-"insee"
names(pop_2014)[names(pop_2014)=="Population en 2014 (princ)"]<-"population"
names(pop_2014)[names(pop_2014)=="Pop 0-14 ans en 2014 (princ)"]<-"pop_0_14"
names(pop_2014)[names(pop_2014)=="Pop 15-29 ans en 2014 (princ)"]<-"pop_15_29"
names(pop_2014)[names(pop_2014)=="Pop 30-44 ans en 2014 (princ)"]<-"pop_30_44"
names(pop_2014)[names(pop_2014)=="Pop 45-59 ans en 2014 (princ)"]<-"pop_45_59"
names(pop_2014)[names(pop_2014)=="Pop 60-74 ans en 2014 (princ)"]<-"pop_60_74"
names(pop_2014)[names(pop_2014)=="Pop 75-89 ans en 2014 (princ)"]<-"pop_75_89"
names(pop_2014)[names(pop_2014)=="Pop 90 ans ou plus en 2014 (princ)"]<-"pop_90_plus"
names(pop_2014)[names(pop_2014)=="Pop Hommes en 2014 (princ)"]<-"pop_homme"
names(pop_2014)[names(pop_2014)=="Pop Femmes en 2014 (princ)"]<-"pop_femme"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus en 2014 (compl)"]<-"pop_15_plus"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Agriculteurs exploitants en 2014 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2014 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2014 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Prof. intermédiaires  en 2014 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Employés en 2014 (compl)"]<-"pop_15_plus_empl"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Ouvriers en 2014 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Retraités  en 2014 (compl)"]<-"pop_15_plus_retraite"
names(pop_2014)[names(pop_2014)=="Pop 15 ans ou plus Autres en 2014 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2014<-pop_2014[-1,]
pop_2014[,c(5:23)]<-sapply(pop_2014[,c(5:23)],as.numeric)





library(readxl)

data_fusion <- read_excel("C:/données fusion communes/data_fusion.xlsx")
test<-left_join(data_fusion,pop_2014)
test<-test[,c(1,8:26)]

ag_1<-aggregate(.~DepComN,test,sum)

library(writexl)
#writexl::write_xlsx(ag_1, "C:/données fusion communes/ag_recensement_1_pop_2014.xlsx")

library(readxl)
ag_census_1_pop_2014 <- read_excel("C:/données fusion communes/ag_recensement_1_pop_2014.xlsx")
names(ag_census_1_pop_2014)[names(ag_census_1_pop_2014)=="DepComN"]<-"insee"
ag_census_1_pop_2014$annee<-2014
names(ag_census_1_pop_2014)[names(ag_census_1_pop_2014)=="population"]<-"population_"

ag_census_1_pop_2014_<-full_join(pop_2014,ag_census_1_pop_2014, by= c("insee"))

ag_census_1_pop_2014_$population_b<-ifelse(is.na(ag_census_1_pop_2014_$population_),ag_census_1_pop_2014_$population,ag_census_1_pop_2014_$population_)
ag_census_1_pop_2014_$pop_0_14.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_0_14.y),ag_census_1_pop_2014_$pop_0_14.x,ag_census_1_pop_2014_$pop_0_14.y)
ag_census_1_pop_2014_$pop_15_29.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_29.y),ag_census_1_pop_2014_$pop_15_29.x,ag_census_1_pop_2014_$pop_15_29.y)
ag_census_1_pop_2014_$pop_30_44.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_30_44.y),ag_census_1_pop_2014_$pop_30_44.x,ag_census_1_pop_2014_$pop_30_44.y)
ag_census_1_pop_2014_$pop_45_59.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_45_59.y),ag_census_1_pop_2014_$pop_45_59.x,ag_census_1_pop_2014_$pop_45_59.y)
ag_census_1_pop_2014_$pop_60_74.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_60_74.y),ag_census_1_pop_2014_$pop_60_74.x,ag_census_1_pop_2014_$pop_60_74.y)
ag_census_1_pop_2014_$pop_75_89.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_75_89.y),ag_census_1_pop_2014_$pop_75_89.x,ag_census_1_pop_2014_$pop_75_89.y)
ag_census_1_pop_2014_$pop_90_plus.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_90_plus.y),ag_census_1_pop_2014_$pop_90_plus.x,ag_census_1_pop_2014_$pop_90_plus.y)
ag_census_1_pop_2014_$pop_homme.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_homme.y),ag_census_1_pop_2014_$pop_homme.x,ag_census_1_pop_2014_$pop_homme.y)
ag_census_1_pop_2014_$pop_femme.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_femme.y),ag_census_1_pop_2014_$pop_femme.x,ag_census_1_pop_2014_$pop_femme.y)
ag_census_1_pop_2014_$pop_15_plus.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus.y),ag_census_1_pop_2014_$pop_15_plus.x,ag_census_1_pop_2014_$pop_15_plus.y)
ag_census_1_pop_2014_$pop_15_plus_agriculteur.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_agriculteur.y),ag_census_1_pop_2014_$pop_15_plus_agriculteur.x,ag_census_1_pop_2014_$pop_15_plus_agriculteur.y)
ag_census_1_pop_2014_$pop_15_plus_artisans_chef_ent.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_artisans_chef_ent.y),ag_census_1_pop_2014_$pop_15_plus_artisans_chef_ent.x,ag_census_1_pop_2014_$pop_15_plus_artisans_chef_ent.y)
ag_census_1_pop_2014_$pop_15_plus_cadre_prof_intel.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_cadre_prof_intel.y),ag_census_1_pop_2014_$pop_15_plus_cadre_prof_intel.x,ag_census_1_pop_2014_$pop_15_plus_cadre_prof_intel.y)
ag_census_1_pop_2014_$pop_15_plus_prof_interme.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_prof_interme.y),ag_census_1_pop_2014_$pop_15_plus_prof_interme.x,ag_census_1_pop_2014_$pop_15_plus_prof_interme.y)
ag_census_1_pop_2014_$pop_15_plus_empl.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_empl.y),ag_census_1_pop_2014_$pop_15_plus_empl.x,ag_census_1_pop_2014_$pop_15_plus_empl.y)
ag_census_1_pop_2014_$pop_15_plus_ouvrier.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_ouvrier.y),ag_census_1_pop_2014_$pop_15_plus_ouvrier.x,ag_census_1_pop_2014_$pop_15_plus_ouvrier.y)
ag_census_1_pop_2014_$pop_15_plus_retraite.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_retraite.y),ag_census_1_pop_2014_$pop_15_plus_retraite.x,ag_census_1_pop_2014_$pop_15_plus_retraite.y)
ag_census_1_pop_2014_$pop_15_plus_autre_sans_activite.b<-ifelse(is.na(ag_census_1_pop_2014_$pop_15_plus_autre_sans_activite.y),ag_census_1_pop_2014_$pop_15_plus_autre_sans_activite.x,ag_census_1_pop_2014_$pop_15_plus_autre_sans_activite.y)


ag_census_1_pop_2014_<-ag_census_1_pop_2014_[,c(1:4,43:62)]

a<-table(ag_census_1_pop_2014_$insee)
a<-as.data.frame(a)


names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="population_b"]<-"population"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_0_14.b"]<-"pop_0_14"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_29.b"]<-"pop_15_29"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_30_44.b"]<-"pop_30_44"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_45_59.b"]<-"pop_45_59"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_60_74.b"]<-"pop_60_74"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_75_89.b"]<-"pop_75_89"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_90_plus.b"]<-"pop_90_plus"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_homme.b"]<-"pop_homme"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_femme.b"]<-"pop_femme"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus.b"]<-"pop_15_plus"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_agriculteur.b"]<-"pop_15_plus_agriculteur"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_artisans_chef_ent.b"]<-"pop_15_plus_artisans_chef_ent"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_cadre_prof_intel.b"]<-"pop_15_plus_cadre_prof_intel"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_prof_interme.b"]<-"pop_15_plus_prof_interme"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_empl.b"]<-"pop_15_plus_empl"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_ouvrier.b"]<-"pop_15_plus_ouvrier"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_retraite.b"]<-"pop_15_plus_retraite"
names(ag_census_1_pop_2014_)[names(ag_census_1_pop_2014_)=="pop_15_plus_autre_sans_activite.b"]<-"pop_15_plus_autre_sans_activite"

#ag_census_1_pop_2015_$a<-rowSums(ag_census_1_pop_2015_[,c(7:13)])
#test : ok
ag_census_1_pop_2014_$annee<-2014




pop_2012 <- read_excel("data census/census pop structure data/structure_pop_2012_geo2020.xlsx")
pop_2012<-pop_2012[,c(1:13,24,55:63)]
#geography 01/01/2020
names(pop_2012)[names(pop_2012)=="Code géographique"]<-"insee"
names(pop_2012)[names(pop_2012)=="Population en 2012 (princ)"]<-"population"
names(pop_2012)[names(pop_2012)=="Pop 0-14 ans en 2012 (princ)"]<-"pop_0_14"
names(pop_2012)[names(pop_2012)=="Pop 15-29 ans en 2012 (princ)"]<-"pop_15_29"
names(pop_2012)[names(pop_2012)=="Pop 30-44 ans en 2012 (princ)"]<-"pop_30_44"
names(pop_2012)[names(pop_2012)=="Pop 45-59 ans en 2012 (princ)"]<-"pop_45_59"
names(pop_2012)[names(pop_2012)=="Pop 60-74 ans en 2012 (princ)"]<-"pop_60_74"
names(pop_2012)[names(pop_2012)=="Pop 75-89 ans en 2012 (princ)"]<-"pop_75_89"
names(pop_2012)[names(pop_2012)=="Pop 90 ans ou plus en 2012 (princ)"]<-"pop_90_plus"
names(pop_2012)[names(pop_2012)=="Pop Hommes en 2012 (princ)"]<-"pop_homme"
names(pop_2012)[names(pop_2012)=="Pop Femmes en 2012 (princ)"]<-"pop_femme"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus en 2012 (compl)"]<-"pop_15_plus"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Agriculteurs exploitants en 2012 (compl)"]<-"pop_15_plus_agriculteur"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Artisans, Comm., Chefs entr. en 2012 (compl)"]<-"pop_15_plus_artisans_chef_ent"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Cadres, Prof. intel. sup. en 2012 (compl)"]<-"pop_15_plus_cadre_prof_intel"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Prof. intermédiaires  en 2012 (compl)"]<-"pop_15_plus_prof_interme"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Employés en 2012 (compl)"]<-"pop_15_plus_empl"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Ouvriers en 2012 (compl)"]<-"pop_15_plus_ouvrier"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Retraités  en 2012 (compl)"]<-"pop_15_plus_retraite"
names(pop_2012)[names(pop_2012)=="Pop 15 ans ou plus Autres en 2012 (compl)"]<-"pop_15_plus_autre_sans_activite"
pop_2012<-pop_2012[-1,]
pop_2012[,c(5:23)]<-sapply(pop_2012[,c(5:23)],as.numeric)

pop_2012$annee<-2012













#the geography of the data for the years 2014 and 2015 is prior to 2019 and requires the merging of municipalities 




data_census_pop<-rbind(pop_2018,pop_2017)
data_census_pop<-rbind(data_census_pop,pop_2016)
data_census_pop<-rbind(data_census_pop,pop_2013)
data_census_pop<-rbind(data_census_pop,pop_2012)
data_census_pop<-rbind(data_census_pop,ag_census_1_pop_2014_)
data_census_pop<-rbind(data_census_pop,ag_census_1_pop_2015_)

a<-table(data_census_pop$insee)
a<-as.data.frame(a)
b<-filter(a, a$Freq<7)
names(b)[names(b)=="Var1"]<-"insee"
data_census_pop<-left_join(data_census_pop,b)


data_census_pop<-filter(data_census_pop, is.na(data_census_pop$Freq))

a<-table(data_census_pop$insee)
a<-as.data.frame(a)


b<-table(pop_2018$insee)
b<-as.data.frame(b)

ac<-rbind(a,b)
acd<-table(ac$Var1)
acd<-as.data.frame(acd)

#3 municipalities difference 



library(writexl)
#writexl::write_xlsx(data_census_pop, "data census/data_recensement_pop.xlsx")


#reduce the number of digits after the decimal point to reduce memory size on github. 
data_recensement_pop_arrondi <- data_census_pop %>%
  mutate_if(is.numeric, ~ round(., digits = 3))

#write.csv(data_recensement_pop_arrondi,"data census/data_recensement_pop")








#test to see if the old municipalities have gone :
r<-table(data_fusion$DepComA)
r<-as.data.frame(r)

t<-table(data_fusion$DepComN)
t<-as.data.frame(t)
tr<-rbind(r,t)
acde<-table(tr$Var1)
acde<-as.data.frame(acde)
acde<-filter(acde, acde$Freq<2)


are<-rbind(a,acde)
tf<-table(are$Var1)
tf<-as.data.frame(tf)





####census 2####



library(readxl)
library(dplyr)

library(readxl)
famille_2018 <- read_excel("data census/census family data/famille_2018.xlsx")

famille_2018<-famille_2018[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2018)[names(famille_2018)=="Code géographique"]<-"insee"

names(famille_2018)[names(famille_2018)=="Ménages en 2018 (compl)"]<-"nombre_menages"

names(famille_2018)[names(famille_2018)=="Ménages 1 personne en 2018 (compl)"]<-"menage_1_personne"

names(famille_2018)[names(famille_2018)=="Ménages Autres sans famille en 2018 (compl)"]<-"menage_autre_sans_famille"
names(famille_2018)[names(famille_2018)=="Ménages avec famille(s) en 2018 (compl)"]<-"menage_avec_famille"
names(famille_2018)[names(famille_2018)=="Mén fam princ Couple sans enfant en 2018 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2018)[names(famille_2018)=="Mén fam princ Couple avec enfant(s) en 2018 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2018)[names(famille_2018)=="Mén fam princ Famille mono en 2018 (compl)"]<-"menage_avec_famille_monoparental"

famille_2018<-famille_2018[-1,]
famille_2018[,c(5:11)]<-sapply(famille_2018[,c(5:11)],as.numeric)
#sapply(famille_2018, class)
famille_2018$annee<-2018





library(readxl)
library(dplyr)


library(readxl)
famille_2017 <- read_excel("data census/census family data/famille_2017.xlsx")

famille_2017<-famille_2017[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2017)[names(famille_2017)=="Code géographique"]<-"insee"

names(famille_2017)[names(famille_2017)=="Ménages en 2017 (compl)"]<-"nombre_menages"

names(famille_2017)[names(famille_2017)=="Ménages 1 personne en 2017 (compl)"]<-"menage_1_personne"

names(famille_2017)[names(famille_2017)=="Ménages Autres sans famille en 2017 (compl)"]<-"menage_autre_sans_famille"
names(famille_2017)[names(famille_2017)=="Ménages avec famille(s) en 2017 (compl)"]<-"menage_avec_famille"
names(famille_2017)[names(famille_2017)=="Mén fam princ Couple sans enfant en 2017 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2017)[names(famille_2017)=="Mén fam princ Couple avec enfant(s) en 2017 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2017)[names(famille_2017)=="Mén fam princ Famille mono en 2017 (compl)"]<-"menage_avec_famille_monoparental"

famille_2017<-famille_2017[-1,]
famille_2017[,c(5:11)]<-sapply(famille_2017[,c(5:11)],as.numeric)
#sapply(famille_2017, class)
famille_2017$annee<-2017





library(readxl)
library(dplyr)


library(readxl)
famille_2016 <- read_excel("data census/census family data/famille_2016.xlsx")

famille_2016<-famille_2016[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2016)[names(famille_2016)=="Code géographique"]<-"insee"

names(famille_2016)[names(famille_2016)=="Ménages en 2016 (compl)"]<-"nombre_menages"

names(famille_2016)[names(famille_2016)=="Ménages 1 personne en 2016 (compl)"]<-"menage_1_personne"

names(famille_2016)[names(famille_2016)=="Ménages Autres sans famille en 2016 (compl)"]<-"menage_autre_sans_famille"
names(famille_2016)[names(famille_2016)=="Ménages avec famille(s) en 2016 (compl)"]<-"menage_avec_famille"
names(famille_2016)[names(famille_2016)=="Mén fam princ Couple sans enfant en 2016 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2016)[names(famille_2016)=="Mén fam princ Couple avec enfant(s) en 2016 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2016)[names(famille_2016)=="Mén fam princ Famille mono en 2016 (compl)"]<-"menage_avec_famille_monoparental"

famille_2016<-famille_2016[-1,]
famille_2016[,c(5:11)]<-sapply(famille_2016[,c(5:11)],as.numeric)
#sapply(famille_2016, class)
famille_2016$annee<-2016






library(readxl)
library(dplyr)


library(readxl)
famille_2015 <- read_excel("data census/census family data/famille_2015.xlsx")

famille_2015<-famille_2015[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2015)[names(famille_2015)=="Code géographique"]<-"insee"

names(famille_2015)[names(famille_2015)=="Ménages en 2015 (compl)"]<-"nombre_menages"

names(famille_2015)[names(famille_2015)=="Ménages 1 personne en 2015 (compl)"]<-"menage_1_personne"

names(famille_2015)[names(famille_2015)=="Ménages Autres sans famille en 2015 (compl)"]<-"menage_autre_sans_famille"
names(famille_2015)[names(famille_2015)=="Ménages avec famille(s) en 2015 (compl)"]<-"menage_avec_famille"
names(famille_2015)[names(famille_2015)=="Mén fam princ Couple sans enfant en 2015 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2015)[names(famille_2015)=="Mén fam princ Couple avec enfant(s) en 2015 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2015)[names(famille_2015)=="Mén fam princ Famille mono en 2015 (compl)"]<-"menage_avec_famille_monoparental"

famille_2015<-famille_2015[-1,]
famille_2015[,c(5:11)]<-sapply(famille_2015[,c(5:11)],as.numeric)
#sapply(famille_2015, class)
famille_2015$annee<-2015







library(readxl)

data_fusion <- read_excel("C:/données fusion communes/data_fusion.xlsx")
test<-left_join(data_fusion,famille_2015)
test<-test[,c(1,8:15)]

ag_1<-aggregate(.~DepComN,test,sum)

library(writexl)
#writexl::write_xlsx(ag_1, "data census/census family data/ag_recensement_1_famille_2015.xlsx")

library(readxl)
ag_census_1_famille_2015 <- read_excel("data census/census family data/ag_recensement_1_famille_2015.xlsx")
names(ag_census_1_famille_2015)[names(ag_census_1_famille_2015)=="DepComN"]<-"insee"
ag_census_1_famille_2015$annee<-2015
#names(ag_census_1_famille_2015)[names(ag_census_1_famille_2015)=="population"]<-"population_"

#ag_census_1_
famille_2015_<-full_join(famille_2015,ag_census_1_famille_2015, by= c("insee"))





famille_2015_$nombre_menages.b<-ifelse(is.na(famille_2015_$nombre_menages.y),famille_2015_$nombre_menages.x,famille_2015_$nombre_menages.y)
famille_2015_$menage_1_personne.b<-ifelse(is.na(famille_2015_$menage_1_personne.y),famille_2015_$menage_1_personne.x,famille_2015_$menage_1_personne.y)
famille_2015_$menage_autre_sans_famille.b<-ifelse(is.na(famille_2015_$menage_autre_sans_famille.y),famille_2015_$menage_autre_sans_famille.x,famille_2015_$menage_autre_sans_famille.y)
famille_2015_$menage_avec_famille.b<-ifelse(is.na(famille_2015_$menage_avec_famille.y),famille_2015_$menage_avec_famille.x,famille_2015_$menage_avec_famille.y)
famille_2015_$menage_avec_famille_sans_enfant.b<-ifelse(is.na(famille_2015_$menage_avec_famille_sans_enfant.y),famille_2015_$menage_avec_famille_sans_enfant.x,famille_2015_$menage_avec_famille_sans_enfant.y)
famille_2015_$menage_avec_famille_avec_enfant.b<-ifelse(is.na(famille_2015_$menage_avec_famille_avec_enfant.y),famille_2015_$menage_avec_famille_avec_enfant.x,famille_2015_$menage_avec_famille_avec_enfant.y)
famille_2015_$menage_avec_famille_monoparental.b<-ifelse(is.na(famille_2015_$menage_avec_famille_monoparental.y),famille_2015_$menage_avec_famille_monoparental.x,famille_2015_$menage_avec_famille_monoparental.y)


famille_2015_<-famille_2015_[,c(1:4,21:27)]

a<-table(famille_2015_$insee)
a<-as.data.frame(a)






names(famille_2015_)[names(famille_2015_)=="nombre_menages.b"]<-"nombre_menages"
names(famille_2015_)[names(famille_2015_)=="menage_1_personne.b"]<-"menage_1_personne"
names(famille_2015_)[names(famille_2015_)=="menage_autre_sans_famille.b"]<-"menage_autre_sans_famille"
names(famille_2015_)[names(famille_2015_)=="menage_avec_famille.b"]<-"menage_avec_famille"
names(famille_2015_)[names(famille_2015_)=="menage_avec_famille_sans_enfant.b"]<-"menage_avec_famille_sans_enfant"
names(famille_2015_)[names(famille_2015_)=="menage_avec_famille_avec_enfant.b"]<-"menage_avec_famille_avec_enfant"
names(famille_2015_)[names(famille_2015_)=="menage_avec_famille_monoparental.b"]<-"menage_avec_famille_monoparental"




#ag_census_1_pop_2015_$a<-rowSums(ag_census_1_pop_2015_[,c(7:13)])
#test : ok
famille_2015_$annee<-2015






library(readxl)
library(dplyr)


library(readxl)
famille_2014 <- read_excel("data census/census family data/famille_2014.xlsx")

famille_2014<-famille_2014[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2014)[names(famille_2014)=="Code géographique"]<-"insee"

names(famille_2014)[names(famille_2014)=="Ménages en 2014 (compl)"]<-"nombre_menages"

names(famille_2014)[names(famille_2014)=="Ménages 1 personne en 2014 (compl)"]<-"menage_1_personne"

names(famille_2014)[names(famille_2014)=="Ménages Autres sans famille en 2014 (compl)"]<-"menage_autre_sans_famille"
names(famille_2014)[names(famille_2014)=="Ménages avec famille(s) en 2014 (compl)"]<-"menage_avec_famille"
names(famille_2014)[names(famille_2014)=="Mén fam princ Couple sans enfant en 2014 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2014)[names(famille_2014)=="Mén fam princ Couple avec enfant(s) en 2014 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2014)[names(famille_2014)=="Mén fam princ Famille mono en 2014 (compl)"]<-"menage_avec_famille_monoparental"

famille_2014<-famille_2014[-1,]
famille_2014[,c(5:11)]<-sapply(famille_2014[,c(5:11)],as.numeric)
#sapply(famille_2014, class)
famille_2014$annee<-2014








library(readxl)

data_fusion <- read_excel("C:/données fusion communes/data_fusion.xlsx")
test<-left_join(data_fusion,famille_2014)
test<-test[,c(1,8:15)]

ag_1<-aggregate(.~DepComN,test,sum)

library(writexl)
#writexl::write_xlsx(ag_1, "data census/census family data/ag_recensement_1_famille_2014.xlsx")

library(readxl)
ag_census_1_famille_2014<- read_excel("data census/census family data/ag_recensement_1_famille_2014.xlsx")
names(ag_census_1_famille_2014)[names(ag_census_1_famille_2014)=="DepComN"]<-"insee"
ag_census_1_famille_2014$annee<-2014
#names(ag_census_1_famille_2014)[names(ag_census_1_famille_2014)=="population"]<-"population_"

#ag_census_1_
famille_2014_<-full_join(famille_2014,ag_census_1_famille_2014, by= c("insee"))





famille_2014_$nombre_menages.b<-ifelse(is.na(famille_2014_$nombre_menages.y),famille_2014_$nombre_menages.x,famille_2014_$nombre_menages.y)
famille_2014_$menage_1_personne.b<-ifelse(is.na(famille_2014_$menage_1_personne.y),famille_2014_$menage_1_personne.x,famille_2014_$menage_1_personne.y)
famille_2014_$menage_autre_sans_famille.b<-ifelse(is.na(famille_2014_$menage_autre_sans_famille.y),famille_2014_$menage_autre_sans_famille.x,famille_2014_$menage_autre_sans_famille.y)
famille_2014_$menage_avec_famille.b<-ifelse(is.na(famille_2014_$menage_avec_famille.y),famille_2014_$menage_avec_famille.x,famille_2014_$menage_avec_famille.y)
famille_2014_$menage_avec_famille_sans_enfant.b<-ifelse(is.na(famille_2014_$menage_avec_famille_sans_enfant.y),famille_2014_$menage_avec_famille_sans_enfant.x,famille_2014_$menage_avec_famille_sans_enfant.y)
famille_2014_$menage_avec_famille_avec_enfant.b<-ifelse(is.na(famille_2014_$menage_avec_famille_avec_enfant.y),famille_2014_$menage_avec_famille_avec_enfant.x,famille_2014_$menage_avec_famille_avec_enfant.y)
famille_2014_$menage_avec_famille_monoparental.b<-ifelse(is.na(famille_2014_$menage_avec_famille_monoparental.y),famille_2014_$menage_avec_famille_monoparental.x,famille_2014_$menage_avec_famille_monoparental.y)


famille_2014_<-famille_2014_[,c(1:4,21:27)]

a<-table(famille_2014_$insee)
a<-as.data.frame(a)






names(famille_2014_)[names(famille_2014_)=="nombre_menages.b"]<-"nombre_menages"
names(famille_2014_)[names(famille_2014_)=="menage_1_personne.b"]<-"menage_1_personne"
names(famille_2014_)[names(famille_2014_)=="menage_autre_sans_famille.b"]<-"menage_autre_sans_famille"
names(famille_2014_)[names(famille_2014_)=="menage_avec_famille.b"]<-"menage_avec_famille"
names(famille_2014_)[names(famille_2014_)=="menage_avec_famille_sans_enfant.b"]<-"menage_avec_famille_sans_enfant"
names(famille_2014_)[names(famille_2014_)=="menage_avec_famille_avec_enfant.b"]<-"menage_avec_famille_avec_enfant"
names(famille_2014_)[names(famille_2014_)=="menage_avec_famille_monoparental.b"]<-"menage_avec_famille_monoparental"




#ag_census_1_pop_2014_$a<-rowSums(ag_census_1_pop_2014_[,c(7:13)])
#test : ok
famille_2014_$annee<-2014







library(readxl)
library(dplyr)


library(readxl)
famille_2013 <- read_excel("data census/census family data/famille_2013.xlsx")

famille_2013<-famille_2013[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2013)[names(famille_2013)=="Code géographique"]<-"insee"

names(famille_2013)[names(famille_2013)=="Ménages en 2013 (compl)"]<-"nombre_menages"

names(famille_2013)[names(famille_2013)=="Ménages 1 personne en 2013 (compl)"]<-"menage_1_personne"

names(famille_2013)[names(famille_2013)=="Ménages Autres sans famille en 2013 (compl)"]<-"menage_autre_sans_famille"
names(famille_2013)[names(famille_2013)=="Ménages avec famille(s) en 2013 (compl)"]<-"menage_avec_famille"
names(famille_2013)[names(famille_2013)=="Mén fam princ Couple sans enfant en 2013 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2013)[names(famille_2013)=="Mén fam princ Couple avec enfant(s) en 2013 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2013)[names(famille_2013)=="Mén fam princ Famille mono en 2013 (compl)"]<-"menage_avec_famille_monoparental"

famille_2013<-famille_2013[-1,]
famille_2013[,c(5:11)]<-sapply(famille_2013[,c(5:11)],as.numeric)
#sapply(famille_2013, class)
famille_2013$annee<-2013





library(readxl)
library(dplyr)


library(readxl)
famille_2012 <- read_excel("data census/census family data/famille_2012.xlsx")

famille_2012<-famille_2012[,c(1:6,9:13)]

#geography  01/01/2021
names(famille_2012)[names(famille_2012)=="Code géographique"]<-"insee"

names(famille_2012)[names(famille_2012)=="Ménages en 2012 (compl)"]<-"nombre_menages"

names(famille_2012)[names(famille_2012)=="Ménages 1 personne en 2012 (compl)"]<-"menage_1_personne"

names(famille_2012)[names(famille_2012)=="Ménages Autres sans famille en 2012 (compl)"]<-"menage_autre_sans_famille"
names(famille_2012)[names(famille_2012)=="Ménages avec famille(s) en 2012 (compl)"]<-"menage_avec_famille"
names(famille_2012)[names(famille_2012)=="Mén fam princ Couple sans enfant en 2012 (compl)"]<-"menage_avec_famille_sans_enfant"
names(famille_2012)[names(famille_2012)=="Mén fam princ Couple avec enfant(s) en 2012 (compl)"]<-"menage_avec_famille_avec_enfant"
names(famille_2012)[names(famille_2012)=="Mén fam princ Famille mono en 2012 (compl)"]<-"menage_avec_famille_monoparental"

famille_2012<-famille_2012[-1,]
famille_2012[,c(5:11)]<-sapply(famille_2012[,c(5:11)],as.numeric)
#sapply(famille_2012, class)
famille_2012$annee<-2012














data_census_famille<-rbind(famille_2018,famille_2017)
data_census_famille<-rbind(data_census_famille,famille_2016)
data_census_famille<-rbind(data_census_famille,famille_2013)
data_census_famille<-rbind(data_census_famille,famille_2012)
data_census_famille<-rbind(data_census_famille,famille_2014_)
data_census_famille<-rbind(data_census_famille,famille_2015_)

a<-table(data_census_famille$insee)
a<-as.data.frame(a)
b<-filter(a, a$Freq<7)
names(b)[names(b)=="Var1"]<-"insee"
data_census_famille<-left_join(data_census_famille,b)


data_census_famille<-filter(data_census_famille, is.na(data_census_famille$Freq))

a<-table(data_census_famille$insee)
a<-as.data.frame(a)


b<-table(famille_2018$insee)
b<-as.data.frame(b)

ac<-rbind(a,b)
acd<-table(ac$Var1)
acd<-as.data.frame(acd)

#3 municipalities difference 



library(writexl)
#writexl::write_xlsx(data_census_famille, "data census/data_recensement_famille.xlsx")



#reduce the number of digits after the decimal point to reduce memory size. 
data_recensement_pop_arrondi <- data_census_famille %>%
  mutate_if(is.numeric, ~ round(., digits = 3))

#write.csv(data_recensement_pop_arrondi,"data census/data_recensement_famille")



####census 3 #####




library(readxl)
library(dplyr)

pop_active_2018<- read_excel("data census/census pop active data/pop_active_2018.xlsx")

pop_active_2018<-pop_active_2018[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2021
names(pop_active_2018)[names(pop_active_2018)=="Code géographique"]<-"insee"
names(pop_active_2018)[names(pop_active_2018)=="Pop 15-64 ans en 2018 (princ)"]<-"pop_15_64"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans en 2018 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occupés 15-64 ans en 2018 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2018)[names(pop_active_2018)=="Chômeurs 15-64 ans en 2018 (princ)"]<-"chomeurs_15_64"
names(pop_active_2018)[names(pop_active_2018)=="Inactifs 15-64 ans en 2018 (princ)"]<-"inactifs_15_64"
names(pop_active_2018)[names(pop_active_2018)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2018 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2018)[names(pop_active_2018)=="Retraités Préretraités 15-64 ans en 2018 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2018)[names(pop_active_2018)=="Autres inactifs 15-64 ans en 2018 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans en 2018 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans Agriculteurs exploitants en 2018 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2018 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2018 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans Prof. intermédiaires en 2018 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans Employés en 2018 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15-64 ans Ouvriers en 2018 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occupés 15-64 ans en 2018 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2018 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2018 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2018 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occ 15-64 ans Prof. intermédiaires en 2018 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occupés 15-64 ans Employés en 2018 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occupés 15-64 ans Ouvriers en 2018 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2018)[names(pop_active_2018)=="Emplois au LT en 2018 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2018)[names(pop_active_2018)=="Actifs occupés en 2018 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2018)[names(pop_active_2018)=="Pop 15 ans ou plus en 2018 (princ)"]<-"pop_15_P_princ"
names(pop_active_2018)[names(pop_active_2018)=="Actifs 15 ans ou plus en 2018 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2018)[names(pop_active_2018)=="Emplois au LT Industrie en 2018 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2018<-pop_active_2018[-1,]
pop_active_2018[,c(5:31)]<-sapply(pop_active_2018[,c(5:31)],as.numeric)
#sapply(pop_active_2018, class)
pop_active_2018$annee<-2018







library(readxl)
library(dplyr)

pop_active_2017<- read_excel("data census/census pop active data/pop_active_2017.xlsx")

pop_active_2017<-pop_active_2017[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2021
names(pop_active_2017)[names(pop_active_2017)=="Code géographique"]<-"insee"
names(pop_active_2017)[names(pop_active_2017)=="Pop 15-64 ans en 2017 (princ)"]<-"pop_15_64"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans en 2017 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occupés 15-64 ans en 2017 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2017)[names(pop_active_2017)=="Chômeurs 15-64 ans en 2017 (princ)"]<-"chomeurs_15_64"
names(pop_active_2017)[names(pop_active_2017)=="Inactifs 15-64 ans en 2017 (princ)"]<-"inactifs_15_64"
names(pop_active_2017)[names(pop_active_2017)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2017 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2017)[names(pop_active_2017)=="Retraités Préretraités 15-64 ans en 2017 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2017)[names(pop_active_2017)=="Autres inactifs 15-64 ans en 2017 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans en 2017 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans Agriculteurs exploitants en 2017 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2017 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2017 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans Prof. intermédiaires en 2017 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans Employés en 2017 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15-64 ans Ouvriers en 2017 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occupés 15-64 ans en 2017 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2017 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2017 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2017 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occ 15-64 ans Prof. intermédiaires en 2017 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occupés 15-64 ans Employés en 2017 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occupés 15-64 ans Ouvriers en 2017 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2017)[names(pop_active_2017)=="Emplois au LT en 2017 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2017)[names(pop_active_2017)=="Actifs occupés en 2017 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2017)[names(pop_active_2017)=="Pop 15 ans ou plus en 2017 (princ)"]<-"pop_15_P_princ"
names(pop_active_2017)[names(pop_active_2017)=="Actifs 15 ans ou plus en 2017 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2017)[names(pop_active_2017)=="Emplois au LT Industrie en 2017 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2017<-pop_active_2017[-1,]
pop_active_2017[,c(5:31)]<-sapply(pop_active_2017[,c(5:31)],as.numeric)
#sapply(pop_active_2017, class)
pop_active_2017$annee<-2017







library(readxl)
library(dplyr)

pop_active_2016<- read_excel("data census/census pop active data/pop_active_2016.xlsx")

pop_active_2016<-pop_active_2016[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2021
names(pop_active_2016)[names(pop_active_2016)=="Code géographique"]<-"insee"
names(pop_active_2016)[names(pop_active_2016)=="Pop 15-64 ans en 2016 (princ)"]<-"pop_15_64"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans en 2016 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occupés 15-64 ans en 2016 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2016)[names(pop_active_2016)=="Chômeurs 15-64 ans en 2016 (princ)"]<-"chomeurs_15_64"
names(pop_active_2016)[names(pop_active_2016)=="Inactifs 15-64 ans en 2016 (princ)"]<-"inactifs_15_64"
names(pop_active_2016)[names(pop_active_2016)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2016 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2016)[names(pop_active_2016)=="Retraités Préretraités 15-64 ans en 2016 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2016)[names(pop_active_2016)=="Autres inactifs 15-64 ans en 2016 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans en 2016 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans Agriculteurs exploitants en 2016 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2016 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2016 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans Prof. intermédiaires en 2016 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans Employés en 2016 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15-64 ans Ouvriers en 2016 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occupés 15-64 ans en 2016 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2016 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2016 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2016 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occ 15-64 ans Prof. intermédiaires en 2016 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occupés 15-64 ans Employés en 2016 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occupés 15-64 ans Ouvriers en 2016 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2016)[names(pop_active_2016)=="Emplois au LT en 2016 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2016)[names(pop_active_2016)=="Actifs occupés en 2016 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2016)[names(pop_active_2016)=="Pop 15 ans ou plus en 2016 (princ)"]<-"pop_15_P_princ"
names(pop_active_2016)[names(pop_active_2016)=="Actifs 15 ans ou plus en 2016 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2016)[names(pop_active_2016)=="Emplois au LT Industrie en 2016 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2016<-pop_active_2016[-1,]
pop_active_2016[,c(5:31)]<-sapply(pop_active_2016[,c(5:31)],as.numeric)
#sapply(pop_active_2016, class)
pop_active_2016$annee<-2016






library(readxl)
library(dplyr)

pop_active_2015<- read_excel("data census/census pop active data/pop_active_2015.xlsx")

pop_active_2015<-pop_active_2015[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2017
names(pop_active_2015)[names(pop_active_2015)=="Code géographique"]<-"insee"
names(pop_active_2015)[names(pop_active_2015)=="Pop 15-64 ans en 2015 (princ)"]<-"pop_15_64"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans en 2015 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occupés 15-64 ans en 2015 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2015)[names(pop_active_2015)=="Chômeurs 15-64 ans en 2015 (princ)"]<-"chomeurs_15_64"
names(pop_active_2015)[names(pop_active_2015)=="Inactifs 15-64 ans en 2015 (princ)"]<-"inactifs_15_64"
names(pop_active_2015)[names(pop_active_2015)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2015 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2015)[names(pop_active_2015)=="Retraités Préretraités 15-64 ans en 2015 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2015)[names(pop_active_2015)=="Autres inactifs 15-64 ans en 2015 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans en 2015 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans Agriculteurs exploitants en 2015 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2015 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2015 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans Prof. intermédiaires en 2015 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans Employés en 2015 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15-64 ans Ouvriers en 2015 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occupés 15-64 ans en 2015 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2015 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2015 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2015 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occ 15-64 ans Prof. intermédiaires en 2015 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occupés 15-64 ans Employés en 2015 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occupés 15-64 ans Ouvriers en 2015 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2015)[names(pop_active_2015)=="Emplois au LT en 2015 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2015)[names(pop_active_2015)=="Actifs occupés en 2015 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2015)[names(pop_active_2015)=="Pop 15 ans ou plus en 2015 (princ)"]<-"pop_15_P_princ"
names(pop_active_2015)[names(pop_active_2015)=="Actifs 15 ans ou plus en 2015 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2015)[names(pop_active_2015)=="Emplois au LT Industrie en 2015 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2015<-pop_active_2015[-1,]
pop_active_2015[,c(5:31)]<-sapply(pop_active_2015[,c(5:31)],as.numeric)
#sapply(pop_active_2015, class)
pop_active_2015$annee<-2015














library(readxl)

data_fusion <- read_excel("C:/données fusion communes/data_fusion.xlsx")
test<-left_join(data_fusion,pop_active_2015)
test<-test[,c(1,8:34)]

ag_1<-aggregate(.~DepComN,test,sum)

library(writexl)
#writexl::write_xlsx(ag_1, "data census/census pop active data/ag_recensement_1_pop_active_2015.xlsx")

library(readxl)
ag_census_1_pop_active_2015 <- read_excel("data census/census pop active data/ag_recensement_1_pop_active_2015.xlsx")
names(ag_census_1_pop_active_2015)[names(ag_census_1_pop_active_2015)=="DepComN"]<-"insee"
ag_census_1_pop_active_2015$annee<-2015
#names(ag_census_1_pop_active_2015)[names(ag_census_1_pop_active_2015)=="population"]<-"population_"

#ag_census_1_
pop_active_2015_<-full_join(pop_active_2015,ag_census_1_pop_active_2015, by= c("insee"))





pop_active_2015_$pop_15_64.b<-ifelse(is.na(pop_active_2015_$pop_15_64.y),pop_active_2015_$pop_15_64.x,pop_active_2015_$pop_15_64.y)
pop_active_2015_$actifs_15_64_princ.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_princ.y),pop_active_2015_$actifs_15_64_princ.x,pop_active_2015_$actifs_15_64_princ.y)
pop_active_2015_$actifs_occupes_15_64.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64.y),pop_active_2015_$actifs_occupes_15_64.x,pop_active_2015_$actifs_occupes_15_64.y)
pop_active_2015_$chomeurs_15_64.b<-ifelse(is.na(pop_active_2015_$chomeurs_15_64.y),pop_active_2015_$chomeurs_15_64.x,pop_active_2015_$chomeurs_15_64.y)
pop_active_2015_$inactifs_15_64.b<-ifelse(is.na(pop_active_2015_$inactifs_15_64.y),pop_active_2015_$inactifs_15_64.x,pop_active_2015_$inactifs_15_64.y)
pop_active_2015_$inactif_15_64_etudiant_stagiaire.b<-ifelse(is.na(pop_active_2015_$inactif_15_64_etudiant_stagiaire.y),pop_active_2015_$inactif_15_64_etudiant_stagiaire.x,pop_active_2015_$inactif_15_64_etudiant_stagiaire.y)
pop_active_2015_$inactif_15_64_retraite.b<-ifelse(is.na(pop_active_2015_$inactif_15_64_retraite.y),pop_active_2015_$inactif_15_64_retraite.x,pop_active_2015_$inactif_15_64_retraite.y)
pop_active_2015_$inactif_15_64_autre.b<-ifelse(is.na(pop_active_2015_$inactif_15_64_autre.y),pop_active_2015_$inactif_15_64_autre.x,pop_active_2015_$inactif_15_64_autre.y)
pop_active_2015_$actifs_15_64_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_compl.y),pop_active_2015_$actifs_15_64_compl.x,pop_active_2015_$actifs_15_64_compl.y)
pop_active_2015_$actifs_15_64_agriculteurs_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_agriculteurs_compl.y),pop_active_2015_$actifs_15_64_agriculteurs_compl.x,pop_active_2015_$actifs_15_64_agriculteurs_compl.y)
pop_active_2015_$actifs_15_64_artisans_chef_entre_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_artisans_chef_entre_compl.y),pop_active_2015_$actifs_15_64_artisans_chef_entre_compl.x,pop_active_2015_$actifs_15_64_artisans_chef_entre_compl.y)
pop_active_2015_$actifs_15_64_cadres_prof_intel_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_cadres_prof_intel_compl.y),pop_active_2015_$actifs_15_64_cadres_prof_intel_compl.x,pop_active_2015_$actifs_15_64_cadres_prof_intel_compl.y)
pop_active_2015_$actifs_15_64_prof_inter_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_prof_inter_compl.y),pop_active_2015_$actifs_15_64_prof_inter_compl.x,pop_active_2015_$actifs_15_64_prof_inter_compl.y)
pop_active_2015_$actifs_15_64_employes_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_employes_compl.y),pop_active_2015_$actifs_15_64_employes_compl.x,pop_active_2015_$actifs_15_64_employes_compl.y)
pop_active_2015_$actifs_15_64_ouvriers_compl.b<-ifelse(is.na(pop_active_2015_$actifs_15_64_ouvriers_compl.y),pop_active_2015_$actifs_15_64_ouvriers_compl.x,pop_active_2015_$actifs_15_64_ouvriers_compl.y)
pop_active_2015_$actifs_occupes_15_64_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_compl.y),pop_active_2015_$actifs_occupes_15_64_compl.x,pop_active_2015_$actifs_occupes_15_64_compl.y)
pop_active_2015_$actifs_occupes_15_64_agriculteurs_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_agriculteurs_compl.y),pop_active_2015_$actifs_occupes_15_64_agriculteurs_compl.x,pop_active_2015_$actifs_occupes_15_64_agriculteurs_compl.y)
pop_active_2015_$actifs_occupes_15_64_artisans_chef_entre_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_artisans_chef_entre_compl.y),pop_active_2015_$actifs_occupes_15_64_artisans_chef_entre_compl.x,pop_active_2015_$actifs_occupes_15_64_artisans_chef_entre_compl.y)
pop_active_2015_$actifs_occupes_15_64_cadre_prof_intel_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_cadre_prof_intel_compl.y),pop_active_2015_$actifs_occupes_15_64_cadre_prof_intel_compl.x,pop_active_2015_$actifs_occupes_15_64_cadre_prof_intel_compl.y)
pop_active_2015_$actifs_occupes_15_64_prof_inter_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_prof_inter_compl.y),pop_active_2015_$actifs_occupes_15_64_prof_inter_compl.x,pop_active_2015_$actifs_occupes_15_64_prof_inter_compl.y)
pop_active_2015_$actifs_occupes_15_64_employes_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_employes_compl.y),pop_active_2015_$actifs_occupes_15_64_employes_compl.x,pop_active_2015_$actifs_occupes_15_64_employes_compl.y)
pop_active_2015_$actifs_occupes_15_64_ouvriers_compl.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_15_64_ouvriers_compl.y),pop_active_2015_$actifs_occupes_15_64_ouvriers_compl.x,pop_active_2015_$actifs_occupes_15_64_ouvriers_compl.y)
pop_active_2015_$emplois_au_LT_princ.b<-ifelse(is.na(pop_active_2015_$emplois_au_LT_princ.y),pop_active_2015_$emplois_au_LT_princ.x,pop_active_2015_$emplois_au_LT_princ.y)
pop_active_2015_$actifs_occupes_princ.b<-ifelse(is.na(pop_active_2015_$actifs_occupes_princ.y),pop_active_2015_$actifs_occupes_princ.x,pop_active_2015_$actifs_occupes_princ.y)
pop_active_2015_$pop_15_P_princ.b<-ifelse(is.na(pop_active_2015_$pop_15_P_princ.y),pop_active_2015_$pop_15_P_princ.x,pop_active_2015_$pop_15_P_princ.y)
pop_active_2015_$actifs_15_P_princ.b<-ifelse(is.na(pop_active_2015_$actifs_15_P_princ.y),pop_active_2015_$actifs_15_P_princ.x,pop_active_2015_$actifs_15_P_princ.y)
pop_active_2015_$emplois_LT_industrie_compl.b<-ifelse(is.na(pop_active_2015_$emplois_LT_industrie_compl.y),pop_active_2015_$emplois_LT_industrie_compl.x,pop_active_2015_$emplois_LT_industrie_compl.y)


pop_active_2015_<-pop_active_2015_[,c(1:4,61:87)]

a<-table(pop_active_2015_$insee)
a<-as.data.frame(a)

















names(pop_active_2015_)[names(pop_active_2015_)=="pop_15_64.b"]<-"pop_15_64"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_princ.b"]<-"actifs_15_64_princ"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64.b"]<-"actifs_occupes_15_64"
names(pop_active_2015_)[names(pop_active_2015_)=="chomeurs_15_64.b"]<-"chomeurs_15_64"
names(pop_active_2015_)[names(pop_active_2015_)=="inactifs_15_64.b"]<-"inactifs_15_64"
names(pop_active_2015_)[names(pop_active_2015_)=="inactif_15_64_etudiant_stagiaire.b"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2015_)[names(pop_active_2015_)=="inactif_15_64_retraite.b"]<-"inactif_15_64_retraite"
names(pop_active_2015_)[names(pop_active_2015_)=="inactif_15_64_autre.b"]<-"inactif_15_64_autre"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_compl.b"]<-"actifs_15_64_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_agriculteurs_compl.b"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_artisans_chef_entre_compl.b"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_cadres_prof_intel_compl.b"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_prof_inter_compl.b"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_employes_compl.b"]<-"actifs_15_64_employes_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_64_ouvriers_compl.b"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_compl.b"]<-"actifs_occupes_15_64_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_agriculteurs_compl.b"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_artisans_chef_entre_compl.b"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_cadre_prof_intel_compl.b"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_prof_inter_compl.b"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_employes_compl.b"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_15_64_ouvriers_compl.b"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2015_)[names(pop_active_2015_)=="emplois_au_LT_princ.b"]<-"emplois_au_LT_princ"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_occupes_princ.b"]<-"actifs_occupes_princ"
names(pop_active_2015_)[names(pop_active_2015_)=="pop_15_P_princ.b"]<-"pop_15_P_princ"
names(pop_active_2015_)[names(pop_active_2015_)=="actifs_15_P_princ.b"]<-"actifs_15_P_princ"
names(pop_active_2015_)[names(pop_active_2015_)=="emplois_LT_industrie_compl.b"]<-"emplois_LT_industrie_compl"




#ag_census_1_pop_2015_$a<-rowSums(ag_census_1_pop_2015_[,c(7:13)])
#test : ok
pop_active_2015_$annee<-2015









library(readxl)
library(dplyr)

pop_active_2014<- read_excel("data census/census pop active data/pop_active_2014.xlsx")

pop_active_2014<-pop_active_2014[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2021
names(pop_active_2014)[names(pop_active_2014)=="Code géographique"]<-"insee"
names(pop_active_2014)[names(pop_active_2014)=="Pop 15-64 ans en 2014 (princ)"]<-"pop_15_64"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans en 2014 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occupés 15-64 ans en 2014 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2014)[names(pop_active_2014)=="Chômeurs 15-64 ans en 2014 (princ)"]<-"chomeurs_15_64"
names(pop_active_2014)[names(pop_active_2014)=="Inactifs 15-64 ans en 2014 (princ)"]<-"inactifs_15_64"
names(pop_active_2014)[names(pop_active_2014)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2014 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2014)[names(pop_active_2014)=="Retraités Préretraités 15-64 ans en 2014 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2014)[names(pop_active_2014)=="Autres inactifs 15-64 ans en 2014 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans en 2014 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans Agriculteurs exploitants en 2014 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2014 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2014 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans Prof. intermédiaires en 2014 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans Employés en 2014 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15-64 ans Ouvriers en 2014 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occupés 15-64 ans en 2014 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2014 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2014 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2014 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occ 15-64 ans Prof. intermédiaires en 2014 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occupés 15-64 ans Employés en 2014 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occupés 15-64 ans Ouvriers en 2014 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2014)[names(pop_active_2014)=="Emplois au LT en 2014 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2014)[names(pop_active_2014)=="Actifs occupés en 2014 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2014)[names(pop_active_2014)=="Pop 15 ans ou plus en 2014 (princ)"]<-"pop_15_P_princ"
names(pop_active_2014)[names(pop_active_2014)=="Actifs 15 ans ou plus en 2014 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2014)[names(pop_active_2014)=="Emplois au LT Industrie en 2014 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2014<-pop_active_2014[-1,]
pop_active_2014[,c(5:31)]<-sapply(pop_active_2014[,c(5:31)],as.numeric)
#sapply(pop_active_2014, class)
pop_active_2014$annee<-2014



























library(readxl)

data_fusion <- read_excel("C:/données fusion communes/data_fusion.xlsx")
test<-left_join(data_fusion,pop_active_2014)
test<-test[,c(1,8:34)]

ag_1<-aggregate(.~DepComN,test,sum)

library(writexl)
#writexl::write_xlsx(ag_1, "data census/census pop active data/ag_recensement_1_pop_active_2014.xlsx")

library(readxl)
ag_census_1_pop_active_2014 <- read_excel("data census/census pop active data/ag_recensement_1_pop_active_2014.xlsx")
names(ag_census_1_pop_active_2014)[names(ag_census_1_pop_active_2014)=="DepComN"]<-"insee"
ag_census_1_pop_active_2014$annee<-2014
#names(ag_census_1_pop_active_2014)[names(ag_census_1_pop_active_2014)=="population"]<-"population_"

#ag_census_1_
pop_active_2014_<-full_join(pop_active_2014,ag_census_1_pop_active_2014, by= c("insee"))





pop_active_2014_$pop_15_64.b<-ifelse(is.na(pop_active_2014_$pop_15_64.y),pop_active_2014_$pop_15_64.x,pop_active_2014_$pop_15_64.y)
pop_active_2014_$actifs_15_64_princ.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_princ.y),pop_active_2014_$actifs_15_64_princ.x,pop_active_2014_$actifs_15_64_princ.y)
pop_active_2014_$actifs_occupes_15_64.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64.y),pop_active_2014_$actifs_occupes_15_64.x,pop_active_2014_$actifs_occupes_15_64.y)
pop_active_2014_$chomeurs_15_64.b<-ifelse(is.na(pop_active_2014_$chomeurs_15_64.y),pop_active_2014_$chomeurs_15_64.x,pop_active_2014_$chomeurs_15_64.y)
pop_active_2014_$inactifs_15_64.b<-ifelse(is.na(pop_active_2014_$inactifs_15_64.y),pop_active_2014_$inactifs_15_64.x,pop_active_2014_$inactifs_15_64.y)
pop_active_2014_$inactif_15_64_etudiant_stagiaire.b<-ifelse(is.na(pop_active_2014_$inactif_15_64_etudiant_stagiaire.y),pop_active_2014_$inactif_15_64_etudiant_stagiaire.x,pop_active_2014_$inactif_15_64_etudiant_stagiaire.y)
pop_active_2014_$inactif_15_64_retraite.b<-ifelse(is.na(pop_active_2014_$inactif_15_64_retraite.y),pop_active_2014_$inactif_15_64_retraite.x,pop_active_2014_$inactif_15_64_retraite.y)
pop_active_2014_$inactif_15_64_autre.b<-ifelse(is.na(pop_active_2014_$inactif_15_64_autre.y),pop_active_2014_$inactif_15_64_autre.x,pop_active_2014_$inactif_15_64_autre.y)
pop_active_2014_$actifs_15_64_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_compl.y),pop_active_2014_$actifs_15_64_compl.x,pop_active_2014_$actifs_15_64_compl.y)
pop_active_2014_$actifs_15_64_agriculteurs_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_agriculteurs_compl.y),pop_active_2014_$actifs_15_64_agriculteurs_compl.x,pop_active_2014_$actifs_15_64_agriculteurs_compl.y)
pop_active_2014_$actifs_15_64_artisans_chef_entre_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_artisans_chef_entre_compl.y),pop_active_2014_$actifs_15_64_artisans_chef_entre_compl.x,pop_active_2014_$actifs_15_64_artisans_chef_entre_compl.y)
pop_active_2014_$actifs_15_64_cadres_prof_intel_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_cadres_prof_intel_compl.y),pop_active_2014_$actifs_15_64_cadres_prof_intel_compl.x,pop_active_2014_$actifs_15_64_cadres_prof_intel_compl.y)
pop_active_2014_$actifs_15_64_prof_inter_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_prof_inter_compl.y),pop_active_2014_$actifs_15_64_prof_inter_compl.x,pop_active_2014_$actifs_15_64_prof_inter_compl.y)
pop_active_2014_$actifs_15_64_employes_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_employes_compl.y),pop_active_2014_$actifs_15_64_employes_compl.x,pop_active_2014_$actifs_15_64_employes_compl.y)
pop_active_2014_$actifs_15_64_ouvriers_compl.b<-ifelse(is.na(pop_active_2014_$actifs_15_64_ouvriers_compl.y),pop_active_2014_$actifs_15_64_ouvriers_compl.x,pop_active_2014_$actifs_15_64_ouvriers_compl.y)
pop_active_2014_$actifs_occupes_15_64_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_compl.y),pop_active_2014_$actifs_occupes_15_64_compl.x,pop_active_2014_$actifs_occupes_15_64_compl.y)
pop_active_2014_$actifs_occupes_15_64_agriculteurs_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_agriculteurs_compl.y),pop_active_2014_$actifs_occupes_15_64_agriculteurs_compl.x,pop_active_2014_$actifs_occupes_15_64_agriculteurs_compl.y)
pop_active_2014_$actifs_occupes_15_64_artisans_chef_entre_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_artisans_chef_entre_compl.y),pop_active_2014_$actifs_occupes_15_64_artisans_chef_entre_compl.x,pop_active_2014_$actifs_occupes_15_64_artisans_chef_entre_compl.y)
pop_active_2014_$actifs_occupes_15_64_cadre_prof_intel_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_cadre_prof_intel_compl.y),pop_active_2014_$actifs_occupes_15_64_cadre_prof_intel_compl.x,pop_active_2014_$actifs_occupes_15_64_cadre_prof_intel_compl.y)
pop_active_2014_$actifs_occupes_15_64_prof_inter_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_prof_inter_compl.y),pop_active_2014_$actifs_occupes_15_64_prof_inter_compl.x,pop_active_2014_$actifs_occupes_15_64_prof_inter_compl.y)
pop_active_2014_$actifs_occupes_15_64_employes_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_employes_compl.y),pop_active_2014_$actifs_occupes_15_64_employes_compl.x,pop_active_2014_$actifs_occupes_15_64_employes_compl.y)
pop_active_2014_$actifs_occupes_15_64_ouvriers_compl.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_15_64_ouvriers_compl.y),pop_active_2014_$actifs_occupes_15_64_ouvriers_compl.x,pop_active_2014_$actifs_occupes_15_64_ouvriers_compl.y)
pop_active_2014_$emplois_au_LT_princ.b<-ifelse(is.na(pop_active_2014_$emplois_au_LT_princ.y),pop_active_2014_$emplois_au_LT_princ.x,pop_active_2014_$emplois_au_LT_princ.y)
pop_active_2014_$actifs_occupes_princ.b<-ifelse(is.na(pop_active_2014_$actifs_occupes_princ.y),pop_active_2014_$actifs_occupes_princ.x,pop_active_2014_$actifs_occupes_princ.y)
pop_active_2014_$pop_15_P_princ.b<-ifelse(is.na(pop_active_2014_$pop_15_P_princ.y),pop_active_2014_$pop_15_P_princ.x,pop_active_2014_$pop_15_P_princ.y)
pop_active_2014_$actifs_15_P_princ.b<-ifelse(is.na(pop_active_2014_$actifs_15_P_princ.y),pop_active_2014_$actifs_15_P_princ.x,pop_active_2014_$actifs_15_P_princ.y)
pop_active_2014_$emplois_LT_industrie_compl.b<-ifelse(is.na(pop_active_2014_$emplois_LT_industrie_compl.y),pop_active_2014_$emplois_LT_industrie_compl.x,pop_active_2014_$emplois_LT_industrie_compl.y)


pop_active_2014_<-pop_active_2014_[,c(1:4,61:87)]

a<-table(pop_active_2014_$insee)
a<-as.data.frame(a)



names(pop_active_2014_)[names(pop_active_2014_)=="pop_15_64.b"]<-"pop_15_64"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_princ.b"]<-"actifs_15_64_princ"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64.b"]<-"actifs_occupes_15_64"
names(pop_active_2014_)[names(pop_active_2014_)=="chomeurs_15_64.b"]<-"chomeurs_15_64"
names(pop_active_2014_)[names(pop_active_2014_)=="inactifs_15_64.b"]<-"inactifs_15_64"
names(pop_active_2014_)[names(pop_active_2014_)=="inactif_15_64_etudiant_stagiaire.b"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2014_)[names(pop_active_2014_)=="inactif_15_64_retraite.b"]<-"inactif_15_64_retraite"
names(pop_active_2014_)[names(pop_active_2014_)=="inactif_15_64_autre.b"]<-"inactif_15_64_autre"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_compl.b"]<-"actifs_15_64_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_agriculteurs_compl.b"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_artisans_chef_entre_compl.b"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_cadres_prof_intel_compl.b"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_prof_inter_compl.b"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_employes_compl.b"]<-"actifs_15_64_employes_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_64_ouvriers_compl.b"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_compl.b"]<-"actifs_occupes_15_64_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_agriculteurs_compl.b"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_artisans_chef_entre_compl.b"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_cadre_prof_intel_compl.b"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_prof_inter_compl.b"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_employes_compl.b"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_15_64_ouvriers_compl.b"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2014_)[names(pop_active_2014_)=="emplois_au_LT_princ.b"]<-"emplois_au_LT_princ"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_occupes_princ.b"]<-"actifs_occupes_princ"
names(pop_active_2014_)[names(pop_active_2014_)=="pop_15_P_princ.b"]<-"pop_15_P_princ"
names(pop_active_2014_)[names(pop_active_2014_)=="actifs_15_P_princ.b"]<-"actifs_15_P_princ"
names(pop_active_2014_)[names(pop_active_2014_)=="emplois_LT_industrie_compl.b"]<-"emplois_LT_industrie_compl"




#ag_census_1_pop_2014_$a<-rowSums(ag_census_1_pop_2014_[,c(7:13)])
#test : ok
pop_active_2014_$annee<-2014









library(readxl)
library(dplyr)

pop_active_2013<- read_excel("data census/census pop active data/pop_active_2013.xlsx")

pop_active_2013<-pop_active_2013[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2021
names(pop_active_2013)[names(pop_active_2013)=="Code géographique"]<-"insee"
names(pop_active_2013)[names(pop_active_2013)=="Pop 15-64 ans en 2013 (princ)"]<-"pop_15_64"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans en 2013 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occupés 15-64 ans en 2013 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2013)[names(pop_active_2013)=="Chômeurs 15-64 ans en 2013 (princ)"]<-"chomeurs_15_64"
names(pop_active_2013)[names(pop_active_2013)=="Inactifs 15-64 ans en 2013 (princ)"]<-"inactifs_15_64"
names(pop_active_2013)[names(pop_active_2013)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2013 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2013)[names(pop_active_2013)=="Retraités Préretraités 15-64 ans en 2013 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2013)[names(pop_active_2013)=="Autres inactifs 15-64 ans en 2013 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans en 2013 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans Agriculteurs exploitants en 2013 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2013 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2013 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans Prof. intermédiaires en 2013 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans Employés en 2013 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15-64 ans Ouvriers en 2013 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occupés 15-64 ans en 2013 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2013 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2013 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2013 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occ 15-64 ans Prof. intermédiaires en 2013 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occupés 15-64 ans Employés en 2013 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occupés 15-64 ans Ouvriers en 2013 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2013)[names(pop_active_2013)=="Emplois au LT en 2013 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2013)[names(pop_active_2013)=="Actifs occupés en 2013 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2013)[names(pop_active_2013)=="Pop 15 ans ou plus en 2013 (princ)"]<-"pop_15_P_princ"
names(pop_active_2013)[names(pop_active_2013)=="Actifs 15 ans ou plus en 2013 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2013)[names(pop_active_2013)=="Emplois au LT Industrie en 2013 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2013<-pop_active_2013[-1,]
pop_active_2013[,c(5:31)]<-sapply(pop_active_2013[,c(5:31)],as.numeric)
#sapply(pop_active_2013, class)
pop_active_2013$annee<-2013









library(readxl)
library(dplyr)

pop_active_2012<- read_excel("data census/census pop active data/pop_active_2012.xlsx")

pop_active_2012<-pop_active_2012[,c(1:5,17,29,41,50:71,86)]

#geography  01/01/2021
names(pop_active_2012)[names(pop_active_2012)=="Code géographique"]<-"insee"
names(pop_active_2012)[names(pop_active_2012)=="Pop 15-64 ans en 2012 (princ)"]<-"pop_15_64"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans en 2012 (princ)"]<-"actifs_15_64_princ"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occupés 15-64 ans en 2012 (princ)"]<-"actifs_occupes_15_64"
names(pop_active_2012)[names(pop_active_2012)=="Chômeurs 15-64 ans en 2012 (princ)"]<-"chomeurs_15_64"
names(pop_active_2012)[names(pop_active_2012)=="Inactifs 15-64 ans en 2012 (princ)"]<-"inactifs_15_64"
names(pop_active_2012)[names(pop_active_2012)=="Elèv. Etud. Stag. non rémunérés 15-64 ans en 2012 (princ)"]<-"inactif_15_64_etudiant_stagiaire"
names(pop_active_2012)[names(pop_active_2012)=="Retraités Préretraités 15-64 ans en 2012 (princ)"]<-"inactif_15_64_retraite"
names(pop_active_2012)[names(pop_active_2012)=="Autres inactifs 15-64 ans en 2012 (princ)"]<-"inactif_15_64_autre"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans en 2012 (compl)"]<-"actifs_15_64_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans Agriculteurs exploitants en 2012 (compl)"]<-"actifs_15_64_agriculteurs_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans Artisans, Comm., Chefs entr. en 2012 (compl)"]<-"actifs_15_64_artisans_chef_entre_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans Cadres, Prof. intel. sup. en 2012 (compl)"]<-"actifs_15_64_cadres_prof_intel_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans Prof. intermédiaires en 2012 (compl)"]<-"actifs_15_64_prof_inter_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans Employés en 2012 (compl)"]<-"actifs_15_64_employes_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15-64 ans Ouvriers en 2012 (compl)"]<-"actifs_15_64_ouvriers_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occupés 15-64 ans en 2012 (compl)"]<-"actifs_occupes_15_64_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occ 15-64 ans Agriculteurs exploitants en 2012 (compl)"]<-"actifs_occupes_15_64_agriculteurs_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occ 15-64 ans Artisans, Comm., Chefs entr. en 2012 (compl)"]<-"actifs_occupes_15_64_artisans_chef_entre_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occ 15-64 ans Cadres Prof. intel. sup. en 2012 (compl)"]<-"actifs_occupes_15_64_cadre_prof_intel_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occ 15-64 ans Prof. intermédiaires en 2012 (compl)"]<-"actifs_occupes_15_64_prof_inter_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occupés 15-64 ans Employés en 2012 (compl)"]<-"actifs_occupes_15_64_employes_compl"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occupés 15-64 ans Ouvriers en 2012 (compl)"]<-"actifs_occupes_15_64_ouvriers_compl"
names(pop_active_2012)[names(pop_active_2012)=="Emplois au LT en 2012 (princ)"]<-"emplois_au_LT_princ"
names(pop_active_2012)[names(pop_active_2012)=="Actifs occupés en 2012 (princ)"]<-"actifs_occupes_princ"
names(pop_active_2012)[names(pop_active_2012)=="Pop 15 ans ou plus en 2012 (princ)"]<-"pop_15_P_princ"
names(pop_active_2012)[names(pop_active_2012)=="Actifs 15 ans ou plus en 2012 (princ)"]<-"actifs_15_P_princ"
names(pop_active_2012)[names(pop_active_2012)=="Emplois au LT Industrie en 2012 (compl)"]<-"emplois_LT_industrie_compl"


pop_active_2012<-pop_active_2012[-1,]
pop_active_2012[,c(5:31)]<-sapply(pop_active_2012[,c(5:31)],as.numeric)
#sapply(pop_active_2012, class)
pop_active_2012$annee<-2012
















data_census_pop_active<-rbind(pop_active_2018,pop_active_2017)
data_census_pop_active<-rbind(data_census_pop_active,pop_active_2016)
data_census_pop_active<-rbind(data_census_pop_active,pop_active_2013)
data_census_pop_active<-rbind(data_census_pop_active,pop_active_2012)
data_census_pop_active<-rbind(data_census_pop_active,pop_active_2014_)
data_census_pop_active<-rbind(data_census_pop_active,pop_active_2015_)

a<-table(data_census_pop_active$insee)
a<-as.data.frame(a)
b<-filter(a, a$Freq<7)
names(b)[names(b)=="Var1"]<-"insee"
data_census_pop_active<-left_join(data_census_pop_active,b)


data_census_pop_active<-filter(data_census_pop_active, is.na(data_census_pop_active$Freq))

a<-table(data_census_pop_active$insee)
a<-as.data.frame(a)


b<-table(pop_active_2018$insee)
b<-as.data.frame(b)

ac<-rbind(a,b)
acd<-table(ac$Var1)
acd<-as.data.frame(acd)

#3 



library(writexl)
#writexl::write_xlsx(data_census_pop_active, "data census/data_recensement_pop_active.xlsx")




#reduce the number of digits after the decimal point to reduce memory size. 
data_recensement_pop_arrondi <- data_census_pop_active %>%
  mutate_if(is.numeric, ~ round(., digits = 3))

#write.csv(data_recensement_pop_arrondi,"data census/data_recensement_pop_active")




