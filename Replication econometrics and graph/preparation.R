




library(readxl)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(effects)
library(regclass)
library(ggpubr)
library(ggeffects)
library(quantreg)
library(RColorBrewer)
library(spdep)
library(spatialreg)
library(splm)
library(plm)
library(sp)
library(maptools)
library(maps)
library(rgdal)
library(tmap)
library(FactoMineR)
library(factoextra)


setwd("D:/code and data inequality in exposure to air pollution")



data_filo<- read_excel("data filosofi (income)/base_filo_2012_2018_com.xlsx")




grille_densite<- read_excel("socio economic data municipalities/Copie de grille_densite_2021_agrege_insee.xlsx")
grille_densite<-grille_densite[,c("insee","densite")]


insee_typologie_rural_urbain <- read_excel("socio economic data municipalities/insee_typologie_rural_urbain.xlsx")

data2<-left_join(data_filo,insee_typologie_rural_urbain)
data2<-left_join(data2,grille_densite)


zones_emploi_communes <- read_excel("socio economic data municipalities/zones_emploi_communes.xlsx")
type_zone_emploi <- read_excel("socio economic data municipalities/type_zone_emploi.xlsx")
zone_emploi<-left_join(zones_emploi_communes,type_zone_emploi)
zone_emploi<-zone_emploi[,c(1,3,4,9)]

data2<-left_join(data2,zone_emploi)

data2$Orientation_economique[data2$Orientation_economique=="Grandes agglomérations à forte concentration de fonctions métropolitaines"]<-"Grandes_agglomerations"
data2$Orientation_economique[data2$Orientation_economique=="Autres grandes agglomérations et dotées de gros employeurs"]<-"Autres_grandes_agglomerations"





data_recensement_pop <- read.csv("data census/data_recensement_pop")
data_recensement_famille <- read.csv("data census/data_recensement_famille")
data_recensement_pop_active <- read.csv("data census/data_recensement_pop_active")



data_recensement_pop<-data_recensement_pop[,-c(1)]
data_recensement_famille<-data_recensement_famille[,-c(1)]
data_recensement_pop_active<-data_recensement_pop_active[,-c(1)]

data_recensement_pop$Région<-as.character(data_recensement_pop$Région) 
data_recensement_famille$Région<-as.character(data_recensement_famille$Région) 
data_recensement_pop_active$Région<-as.character(data_recensement_pop_active$Région) 

#we use csv data with fewer decimal digits for reasons of data capacity on github.

#data_recensement_pop <- read_excel("data census/data_recensement_pop.xlsx")
data2<-left_join(data2,data_recensement_pop)
#data_recensement_famille <- read_excel("data census/data_recensement_famille.xlsx")
data2<-left_join(data2,data_recensement_famille, by = c("insee", "annee"))
#data_recensement_pop_active <- read_excel("data census/data_recensement_pop_active.xlsx")
data2<-left_join(data2,data_recensement_pop_active, by = c("insee", "annee"))


donnee_seuil_jour_no2 <- read.csv("code air pollution/donnee_seuil_jour_no2")

names(donnee_seuil_jour_no2)[names(donnee_seuil_jour_no2)=="V1"]<-"insee"

data2<-left_join(data2,donnee_seuil_jour_no2)




donnee_seuil_jour_pm25<-read.csv("code air pollution/donnee_seuil_jour_pm25")
names(donnee_seuil_jour_pm25)[names(donnee_seuil_jour_pm25)=="V1"]<-"insee"

donnee_seuil_jour_pm25<-donnee_seuil_jour_pm25[,c("insee","annee","nombre_jours_au_dessus_seuils_pm25")]



data2<-left_join(data2,donnee_seuil_jour_pm25)




donnee_seuil_jour_pm10<-read.csv("code air pollution/donnee_seuil_jour_pm10")
names(donnee_seuil_jour_pm10)[names(donnee_seuil_jour_pm10)=="V1"]<-"insee"


donnee_seuil_jour_pm10<-donnee_seuil_jour_pm10[,c("insee","annee","nombre_jours_au_dessus_seuils_pm10")]


data2<-left_join(data2,donnee_seuil_jour_pm10)





donnee_seuil_jour_O3<-read.csv("code air pollution/donnee_seuil_jour_O3")
names(donnee_seuil_jour_O3)[names(donnee_seuil_jour_O3)=="V1"]<-"insee"


donnee_seuil_jour_O3<-donnee_seuil_jour_O3[,c("insee","annee","nombre_jours_au_dessus_seuils_O3")]


data2<-left_join(data2,donnee_seuil_jour_O3)




data_pauvrete <- read_excel("data filosofi (income)/data_pauvrete.xlsx")

data2<-left_join(data2,data_pauvrete)



data<-data2



data$pop_0_14_percentage<-data$pop_0_14/data$population
data$pop_15_29_percentage<-data$pop_15_29/data$population
data$pop_30_44_percentage<-data$pop_30_44/data$population
data$pop_45_59_percentage<-data$pop_60_74/data$population
data$pop_60_74_percentage<-data$pop_60_74/data$population
data$pop_75_89_percentage<-data$pop_75_89/data$population
data$pop_90_plus_percentage<-data$pop_90_plus/data$population

data$pop_homme_percentage<-data$pop_homme/data$population
data$pop_femme_percentage<-data$pop_femme/data$population

data$pop_15_plus_agriculteur_percentage<-data$pop_15_plus_agriculteur/data$pop_15_plus
data$pop_15_plus_artisans_chef_ent_percentage<-data$pop_15_plus_artisans_chef_ent/data$pop_15_plus
data$pop_15_plus_autre_sans_activite_percentage<-data$pop_15_plus_autre_sans_activite/data$pop_15_plus
data$pop_15_plus_cadre_prof_intel_percentage<-data$pop_15_plus_cadre_prof_intel/data$pop_15_plus
data$pop_15_plus_empl_percentage<-data$pop_15_plus_empl/data$pop_15_plus
data$pop_15_plus_ouvrier_percentage<-data$pop_15_plus_ouvrier/data$pop_15_plus
data$pop_15_plus_prof_interme_percentage<-data$pop_15_plus_prof_interme/data$pop_15_plus
data$pop_15_plus_retraite_percentage<-data$pop_15_plus_retraite/data$pop_15_plus


#menage1personne+%menage autre sans famille +  menage avec famille =100% menages
#menage avec famille = menage avec famille sans enfant + menage avec famille avec enfant + menage avec famille monoparental

data$menage_1_personne_percentage<-data$menage_1_personne/data$nombre_menages
data$menage_autre_sans_famille_percentage<-data$menage_autre_sans_famille/data$nombre_menages
data$menage_avec_famille_percentage<-data$menage_avec_famille/data$nombre_menages
data$menage_avec_famille_avec_enfant_percentage<-data$menage_avec_famille_avec_enfant/data$nombre_menages
data$menage_avec_famille_monoparental_percentage<-data$menage_avec_famille_monoparental/data$nombre_menages
data$menage_avec_famille_sans_enfant_percentage<-data$menage_avec_famille_sans_enfant/data$nombre_menages




data$actifs_15_64_princ_percentage<-data$actifs_15_64_princ/data$pop_15_64
data$actifs_occupes_15_64_percentage<-data$actifs_occupes_15_64/data$pop_15_64
data$chomeurs_15_64_percentage<-data$chomeurs_15_64/data$pop_15_64
data$inactifs_15_64_percentage<-data$inactifs_15_64/data$pop_15_64
data$inactif_15_64_etudiant_stagiaire_percentage<-data$inactif_15_64_etudiant_stagiaire/data$pop_15_64
data$inactif_15_64_retraite_percentage<-data$inactif_15_64_retraite/data$pop_15_64
data$inactif_15_64_autre_percentage<-data$inactif_15_64_autre/data$pop_15_64



data$actifs_occupes_15_64_agriculteurs_compl_part<-data$actifs_occupes_15_64_agriculteurs_compl/data$actifs_occupes_15_64_compl
data$actifs_occupes_15_64_artisans_chef_entre_compl_part<-data$actifs_occupes_15_64_artisans_chef_entre_compl/data$actifs_occupes_15_64_compl
data$actifs_occupes_15_64_cadre_prof_intel_compl_part<-data$actifs_occupes_15_64_cadre_prof_intel_compl/data$actifs_occupes_15_64_compl
data$actifs_occupes_15_64_employes_compl_part<-data$actifs_occupes_15_64_employes_compl/data$actifs_occupes_15_64_compl
data$actifs_occupes_15_64_ouvriers_compl_part<-data$actifs_occupes_15_64_ouvriers_compl/data$actifs_occupes_15_64_compl
data$actifs_occupes_15_64_prof_inter_compl_part<-data$actifs_occupes_15_64_prof_inter_compl/data$actifs_occupes_15_64_compl




data$actifs_occupes_15_64_agriculteurs_compl_nombre<-data$actifs_occupes_15_64_agriculteurs_compl_part*data$actifs_occupes_15_64
data$actifs_occupes_15_64_artisans_chef_entre_compl_nombre<-data$actifs_occupes_15_64_artisans_chef_entre_compl_part*data$actifs_occupes_15_64
data$actifs_occupes_15_64_cadre_prof_intel_compl_nombre<-data$actifs_occupes_15_64_cadre_prof_intel_compl_part*data$actifs_occupes_15_64
data$actifs_occupes_15_64_employes_compl_nombre<-data$actifs_occupes_15_64_employes_compl_part*data$actifs_occupes_15_64
data$actifs_occupes_15_64_ouvriers_compl_nombre<-data$actifs_occupes_15_64_ouvriers_compl_part*data$actifs_occupes_15_64
data$actifs_occupes_15_64_prof_inter_compl_nombre<-data$actifs_occupes_15_64_prof_inter_compl_part*data$actifs_occupes_15_64
#la somme est bien egale a actif occupee princ

data$actifs_occupes_15_64_agriculteurs_compl_nombre_percentage<-data$actifs_occupes_15_64_agriculteurs_compl_nombre/data$pop_15_64
data$actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage<-data$actifs_occupes_15_64_artisans_chef_entre_compl_nombre/data$pop_15_64
data$actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage<-data$actifs_occupes_15_64_cadre_prof_intel_compl_nombre/data$pop_15_64
data$actifs_occupes_15_64_employes_compl_nombre_percentage<-data$actifs_occupes_15_64_employes_compl_nombre/data$pop_15_64
data$actifs_occupes_15_64_ouvriers_compl_nombre_percentage<-data$actifs_occupes_15_64_ouvriers_compl_nombre/data$pop_15_64
data$actifs_occupes_15_64_prof_inter_compl_nombre_percentage<-data$actifs_occupes_15_64_prof_inter_compl_nombre/data$pop_15_64
#check realise avec succes, somme des parts est bien egal a part de actif occu princ

data$annee<-as.character(data$annee)
data$annee<-as.factor(data$annee)

#data$dddd<-data$actifs_occupes_15_64_percentage+data$chomeurs_15_64_percentage+data$inactifs_15_64_percentage
#check OK




data = data %>% select(insee,annee, Département, everything())









library(readr)
com_2009_2019_final_ag_final_2 <- read_csv("air pollution cocktail/com_2009_2019_final_ag_final_final_2_2")

com_2009_2019_final_ag_final_2$annee<-as.character(com_2009_2019_final_ag_final_2$annee)

data<-left_join(data, com_2009_2019_final_ag_final_2)





data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_cocktail<-(data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2)/data$sum_cocktail_day



#cleaning
data<-data[,-c(116:127,95:102,6:43,45:76,134)]


#write.csv(data,"final_data_inequality_in_exposure")


#divide in three to reduce memory weight

data1<-data[,c(1:24)]
data2<-data[,c(1,2,25:35)]
data3<-data[,c(1,2,36:70)]

#write.csv(data1,"final_data_inequality_in_exposure_1")

#write.csv(data2,"final_data_inequality_in_exposure_2")

#write.csv(data3,"final_data_inequality_in_exposure_3")








