


# Replication of 
# Salesse Camille. "Inequality in exposure to air pollution in France: bringing pollutant cocktails into the picture" (2023)

# Relevant package

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


setwd("Replication-inequality-in-exposure-to-air-pollution-in-France/Replication econometrics and graph")


# Load the original csv
# This was created after running preparation.R

data1 <- read.csv("final_data_inequality_in_exposure_1")
data2 <- read.csv("final_data_inequality_in_exposure_2")
data3 <- read.csv("final_data_inequality_in_exposure_3")

data1<-data1[,-c(1)]
data2<-data2[,-c(1)]
data3<-data3[,-c(1)]

data<-left_join(data1,data2)
data<-left_join(data,data3)


# Income and number of pollution days in rural and urban areas#
####### dense urban ######


data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain dense")





fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)





fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols2 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols4 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols5 <- lm(fm, data = data_urbain)






library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



library(stargazer)

stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))






####### urbain dense inter ######



data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain densité intermédiaire")





fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)





fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols2 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols4 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols5 <- lm(fm, data = data_urbain)






library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))








######## rural forte influence ######



data_urbain<-filter(data, data$Typologie_urbain_rural=="rural sous forte influence d'un pôle")





fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)





fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols2 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols4 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols5 <- lm(fm, data = data_urbain)






library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))




###### rural faible influence #####



data_urbain<-filter(data, data$Typologie_urbain_rural=="rural sous faible influence d'un pôle")





fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)





fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols2 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols4 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols5 <- lm(fm, data = data_urbain)






library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


###### rural ##########



data_urbain<-filter(data, data$Typologie_urbain_rural=="rural autonome très peu dense")





fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)





fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols2 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols4 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols5 <- lm(fm, data = data_urbain)






library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))

########### rural peu dense ########





data_urbain<-filter(data, data$Typologie_urbain_rural=="rural autonome peu dense")





fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)





fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols2 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols4 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols5 <- lm(fm, data = data_urbain)






library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))




























#########  analysis income - cocktail #########

######## urban dense ############

data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_no2_pm25_pm10_o3<-data$no2_pm25_pm10_o3/data$sum_cocktail_day
data$part_no2_pm10_2<-data$no2_pm10_2/data$sum_cocktail_day
data$part_pm10_o3_2<-data$pm10_o3_2/data$sum_cocktail_day
data$part_no2_pm10_o3_2<-data$no2_pm10_o3_2/data$sum_cocktail_day
data$part_no2_o3_2<-data$no2_o3_2/data$sum_cocktail_day
data$part_pm25_o3_2<-data$pm25_o3_2/data$sum_cocktail_day
data$part_o3_2<-data$o3_2/data$sum_cocktail_day
data$part_no2_2<-data$no2_2/data$sum_cocktail_day
data$part_pm10_2<-data$pm10_2/data$sum_cocktail_day
data$part_pm25_2<-data$pm25_2/data$sum_cocktail_day
data$part_pm25_no2_o3_2<-data$pm25_no2_o3_2/data$sum_cocktail_day
data$part_pm10_pm25_o3_2<-data$pm10_pm25_o3_2/data$sum_cocktail_day
data$part_pm25_pm10_2<-data$pm25_pm10_2/data$sum_cocktail_day
data$part_no2_pm25_pm10_2<-data$no2_pm25_pm10_2/data$sum_cocktail_day
data$part_no2_pm25_2<-data$no2_pm25_2/data$sum_cocktail_day






data$mediane<-scale(data$mediane)
data$mediane<-as.vector(data$mediane)

data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain dense")

#data_urbain <- within(data_urbain, decile <- as.integer(cut(sum_cocktail_day, quantile(sum_cocktail_day, prob = seq(0, 1, length = 6), na.rm = TRUE), include.lowest=TRUE)))




fm <- part_pm25_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)




fm <- part_no2_pm25_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols2 <- lm(fm, data = data_urbain)






fm <- part_pm25_pm10_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols3 <- lm(fm, data = data_urbain)




fm <- part_pm25_o3_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols4 <- lm(fm, data = data_urbain)



fm <- part_no2_pm25_pm10_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols5 <- lm(fm, data = data_urbain)




fm <- part_no2_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols6 <- lm(fm, data = data_urbain)


fm <- part_o3_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols7 <- lm(fm, data = data_urbain)




library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols6_clus <- coeftest(ols6,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols7_clus <- coeftest(ols7,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)
library(stargazer)
stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus,ols6_clus,ols7_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2,ols3,ols4,ols5,ols6,ols7, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))




####### urbain densite intermediaire##########




data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_no2_pm25_pm10_o3<-data$no2_pm25_pm10_o3/data$sum_cocktail_day
data$part_no2_pm10_2<-data$no2_pm10_2/data$sum_cocktail_day
data$part_pm10_o3_2<-data$pm10_o3_2/data$sum_cocktail_day
data$part_no2_pm10_o3_2<-data$no2_pm10_o3_2/data$sum_cocktail_day
data$part_no2_o3_2<-data$no2_o3_2/data$sum_cocktail_day
data$part_pm25_o3_2<-data$pm25_o3_2/data$sum_cocktail_day
data$part_o3_2<-data$o3_2/data$sum_cocktail_day
data$part_no2_2<-data$no2_2/data$sum_cocktail_day
data$part_pm10_2<-data$pm10_2/data$sum_cocktail_day
data$part_pm25_2<-data$pm25_2/data$sum_cocktail_day
data$part_pm25_no2_o3_2<-data$pm25_no2_o3_2/data$sum_cocktail_day
data$part_pm10_pm25_o3_2<-data$pm10_pm25_o3_2/data$sum_cocktail_day
data$part_pm25_pm10_2<-data$pm25_pm10_2/data$sum_cocktail_day
data$part_no2_pm25_pm10_2<-data$no2_pm25_pm10_2/data$sum_cocktail_day
data$part_no2_pm25_2<-data$no2_pm25_2/data$sum_cocktail_day






data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain densité intermédiaire")
#data_urbain <- within(data_urbain, decile <- as.integer(cut(sum_cocktail_day, quantile(sum_cocktail_day, prob = seq(0, 1, length = 6), na.rm = TRUE), include.lowest=TRUE)))



fm <- part_pm25_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)




fm <- part_no2_pm25_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols2 <- lm(fm, data = data_urbain)






fm <- part_pm25_pm10_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols3 <- lm(fm, data = data_urbain)




fm <- part_pm25_o3_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols4 <- lm(fm, data = data_urbain)



fm <- part_no2_pm25_pm10_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols5 <- lm(fm, data = data_urbain)




fm <- part_no2_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols6 <- lm(fm, data = data_urbain)


fm <- part_o3_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols7 <- lm(fm, data = data_urbain)




library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols6_clus <- coeftest(ols6,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols7_clus <- coeftest(ols7,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)
library(stargazer)
stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus,ols6_clus,ols7_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2,ols3,ols4,ols5,ols6,ols7, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))












####### rural ########




data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_no2_pm25_pm10_o3<-data$no2_pm25_pm10_o3/data$sum_cocktail_day
data$part_no2_pm10_2<-data$no2_pm10_2/data$sum_cocktail_day
data$part_pm10_o3_2<-data$pm10_o3_2/data$sum_cocktail_day
data$part_no2_pm10_o3_2<-data$no2_pm10_o3_2/data$sum_cocktail_day
data$part_no2_o3_2<-data$no2_o3_2/data$sum_cocktail_day
data$part_pm25_o3_2<-data$pm25_o3_2/data$sum_cocktail_day
data$part_o3_2<-data$o3_2/data$sum_cocktail_day
data$part_no2_2<-data$no2_2/data$sum_cocktail_day
data$part_pm10_2<-data$pm10_2/data$sum_cocktail_day
data$part_pm25_2<-data$pm25_2/data$sum_cocktail_day
data$part_pm25_no2_o3_2<-data$pm25_no2_o3_2/data$sum_cocktail_day
data$part_pm10_pm25_o3_2<-data$pm10_pm25_o3_2/data$sum_cocktail_day
data$part_pm25_pm10_2<-data$pm25_pm10_2/data$sum_cocktail_day
data$part_no2_pm25_pm10_2<-data$no2_pm25_pm10_2/data$sum_cocktail_day
data$part_no2_pm25_2<-data$no2_pm25_2/data$sum_cocktail_day




data_urbain<-filter(data, data$Typologie_urbain_rural=="rural sous forte influence d'un pôle"|data$Typologie_urbain_rural=="rural sous faible influence d'un pôle"|data$Typologie_urbain_rural=="rural autonome très peu dense"|data$Typologie_urbain_rural=="rural autonome peu dense")
#data_urbain <- within(data_urbain, decile <- as.integer(cut(sum_cocktail_day, quantile(sum_cocktail_day, prob = seq(0, 1, length = 6), na.rm = TRUE), include.lowest=TRUE)))



fm <- part_pm25_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)




fm <- part_no2_pm25_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols2 <- lm(fm, data = data_urbain)






fm <- part_pm25_pm10_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols3 <- lm(fm, data = data_urbain)




fm <- part_pm25_o3_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols4 <- lm(fm, data = data_urbain)



fm <- part_no2_pm25_pm10_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols5 <- lm(fm, data = data_urbain)




fm <- part_no2_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols6 <- lm(fm, data = data_urbain)


fm <- part_o3_2~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols7 <- lm(fm, data = data_urbain)




library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols6_clus <- coeftest(ols6,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols7_clus <- coeftest(ols7,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)
library(stargazer)
stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus,ols6_clus,ols7_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2,ols3,ols4,ols5,ols6,ols7, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))



















####### part cocktail nveau de vie #######
#Income and share of cocktail days in rural and urban area#


data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_cocktail<-(data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2)/data$sum_cocktail_day





#warning if "mediane" is already scale do not repeat the opperation below. 

data$mediane<-scale(data$mediane)
data$mediane<-as.vector(data$mediane)

data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain dense")
data_urbain <- within(data_urbain, decile <- as.integer(cut(sum_cocktail_day, quantile(sum_cocktail_day, prob = seq(0, 1, length = 6), na.rm = TRUE), include.lowest=TRUE)))



fm <- part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)




library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



library(stargazer)
stargazer(ols1_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))



####### densite inter ########





data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_cocktail<-(data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2)/data$sum_cocktail_day





data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain densité intermédiaire")

data_urbain <- within(data_urbain, decile <- as.integer(cut(sum_cocktail_day, quantile(sum_cocktail_day, prob = seq(0, 1, length = 6), na.rm = TRUE), include.lowest=TRUE)))



fm <- part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols2 <- lm(fm, data = data_urbain)




library(lmtest)
library(sandwich)



ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



library(stargazer)
stargazer(ols1_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))





####### rural ########





data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_cocktail<-(data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2)/data$sum_cocktail_day





data_urbain<-filter(data, data$Typologie_urbain_rural=="rural sous forte influence d'un pôle"|data$Typologie_urbain_rural=="rural sous faible influence d'un pôle"|data$Typologie_urbain_rural=="rural autonome très peu dense"|data$Typologie_urbain_rural=="rural autonome peu dense")

data_urbain <- within(data_urbain, decile <- as.integer(cut(sum_cocktail_day, quantile(sum_cocktail_day, prob = seq(0, 1, length = 6), na.rm = TRUE), include.lowest=TRUE)))



fm <- part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols3 <- lm(fm, data = data_urbain)




library(lmtest)
library(sandwich)



ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)



library(stargazer)
stargazer(ols1_clus,ols2_clus,ols3_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols2, ols3, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))





################ robustness chek #######


######### other wealth index - cocktail #####


#column ALL for urban dense and urban intermediate density subsample

data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2




data$part_cocktail<-(data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2)/data$sum_cocktail_day






#warning if "mediane" is already scale do not repeat the opperation below. 

#data$mediane<-scale(data$mediane)
#data$mediane<-as.vector(data$mediane)

data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain dense")

#or

data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain densité intermédiaire")


fm <- sum_cocktail_day~taux_pauvrete_seuil_60+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)




fm <- sum_cocktail_day~premier_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols2 <- lm(fm, data = data_urbain)



fm <- sum_cocktail_day~neuvieme_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols3 <- lm(fm, data = data_urbain)




fm <- sum_cocktail_day~part_prestation_sociale+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols4 <- lm(fm, data = data_urbain)




fm <- sum_cocktail_day~part_revenu_patrimoine_et_autre_revenu+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols5 <- lm(fm, data = data_urbain)




fm <- part_cocktail~taux_pauvrete_seuil_60+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols1 <- lm(fm, data = data_urbain)




fm <- part_cocktail~premier_decile+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols2 <- lm(fm, data = data_urbain)



fm <- part_cocktail~neuvieme_decile+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols3 <- lm(fm, data = data_urbain)




fm <- part_cocktail~part_prestation_sociale+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols4 <- lm(fm, data = data_urbain)




fm <- part_cocktail~part_revenu_patrimoine_et_autre_revenu+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(annee)+factor(Département)

ols5 <- lm(fm, data = data_urbain)





library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols2_clus <- coeftest(ols2,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols3_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols4_clus <- coeftest(ols4,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)


ols5_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data_urbain$Département)




library(stargazer)


stargazer(ols1_clus,ols2_clus,ols3_clus,ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1,ols2,ols3,ols4,ols5, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))







############ appendix regression national ###########


#data$population_scale<-scale(data$population, center = FALSE)



fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)+factor(Typologie_urbain_rural)



ols1 <- lm(fm, data = data,weights=population_scale)

VIF(ols1)
#ok




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)+factor(Typologie_urbain_rural)


ols3 <- lm(fm, data = data,weights=population_scale)



VIF(ols3)
#ok




fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)+factor(Typologie_urbain_rural)


ols5 <- lm(fm, data = data,weights=population_scale)



VIF(ols5)
#ok






fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)+factor(Typologie_urbain_rural)


ols7 <- lm(fm, data = data,weights=population_scale)



VIF(ols7)
#ok



fm <- sum_cocktail_day~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)+factor(Typologie_urbain_rural)


ols9 <- lm(fm, data = data,weights=population_scale)







library(lmtest)
library(sandwich)



ols1_clus <- coeftest(ols1,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data$Département)



ols2_clus <- coeftest(ols3,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data$Département)


ols3_clus <- coeftest(ols5,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data$Département)



ols4_clus <- coeftest(ols7,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data$Département)


ols5_clus <- coeftest(ols9,
                      vcov = vcovCL,
                      type = "HC1",
                      cluster = data$Département)



stargazer(ols1_clus, ols2_clus, ols3_clus, ols4_clus,ols5_clus, no.space = TRUE, omit = c("Département"), omit.labels= c("Département"),omit.yes.no = c("Yes", "No"),omit.stat = c("f","adj.rsq","ser"))


stargazer(ols1, ols3, ols5, ols7, ols9, no.space = TRUE)




######### test for spatial autocorelation #############





setwd("D:/code and data inequality in exposure to air pollution")



library(readr)
library(sf)
library(rgdal)
library(sp)
library(tmap)
shp <- readOGR(dsn = "shapefile france",
               layer = "communes-20190101")


data2<-data[,c("insee","annee","Région","Département","nombre_jours_au_dessus_seuils_no2","nombre_jours_au_dessus_seuils_pm25","nombre_jours_au_dessus_seuils_pm10","nombre_jours_au_dessus_seuils_O3")]
data2<-filter(data2, data2$annee=="2018")

shp1<-merge(shp,data2)


shp1@data$Département<-as.character(shp1@data$Département)
shp1@data$Région<-as.character(shp1@data$Région)


communes_01012019 <- read_csv("socio economic data municipalities/communes-01012019.csv")
communes_01012019<-communes_01012019[,c("com","reg","dep")]
names(communes_01012019)[names(communes_01012019)=="com"]<-"insee"

communes_01012019<-na.omit(communes_01012019)



shp1@data<-left_join(shp1@data,communes_01012019)
shp1@data$reg <- ifelse(is.na(shp1@data$reg), shp1@data$Région, shp1@data$reg)
shp1@data$reg[is.na(shp1@data$reg)]<-"01"


shp1@data$Département[shp1@data$insee=="75056"]<-"75"
shp1@data$Région[shp1@data$insee=="75056"]<-"11"
shp1@data$Département[shp1@data$insee=="13055"]<-"13"
shp1@data$Région[shp1@data$insee=="13055"]<-"93"
shp1@data$Département[shp1@data$insee=="69123"]<-"69"
shp1@data$Région[shp1@data$insee=="69123"]<-"84"

shp1 = subset(shp1, reg != "01")
shp1 = subset(shp1, reg != "02")
shp1 = subset(shp1, reg != "03")
shp1 = subset(shp1, reg != "04")
shp1 = subset(shp1, reg != "06")





data = data %>% select(insee,annee, Département, everything())

data_panel<-data[,c("insee","annee","Département","Région","mediane","nombre_jours_au_dessus_seuils_no2","nombre_jours_au_dessus_seuils_pm25","nombre_jours_au_dessus_seuils_pm10","nombre_jours_au_dessus_seuils_O3","Typologie_urbain_rural","menage_autre_sans_famille_percentage","menage_avec_famille_avec_enfant_percentage","menage_avec_famille_monoparental_percentage","menage_avec_famille_sans_enfant_percentage","pop_0_14_percentage","pop_15_29_percentage","pop_30_44_percentage","pop_45_59_percentage","pop_60_74_percentage","pop_75_89_percentage","pop_90_plus_percentage","pop_homme_percentage","pop_femme_percentage","inactifs_15_64_percentage","chomeurs_15_64_percentage","actifs_occupes_15_64_prof_inter_compl_nombre_percentage","actifs_occupes_15_64_ouvriers_compl_nombre_percentage","actifs_occupes_15_64_employes_compl_nombre_percentage","actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage","actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage","actifs_occupes_15_64_agriculteurs_compl_nombre_percentage","sum_cocktail_day","part_cocktail")]
data_panel<-na.omit(data_panel)
a<-table(data_panel$insee)
a<-as.data.frame(a)
names(a)[names(a)=="Var1"]<-"insee"
data_panel<-left_join(data_panel,a)
data_panel<-filter(data_panel, data_panel$Freq>6)



#for each sub-sample re run the code 
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="urbain dense")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="urbain densité intermédiaire")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural sous faible influence d'un pôle")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural sous forte influence d'un pôle")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural autonome très peu dense")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural autonome peu dense")




a<-table(data_panel$insee)
a<-as.data.frame(a)
names(a)[names(a)=="Var1"]<-"insee"


shp1@data<-left_join(shp1@data,a)
shp1 <- shp1[!is.na(shp1@data$Freq),]






row.names(shp1)<-shp1@data$insee




#k nearest neighbor matrix k=5

map_crd <- coordinates(shp1)
Points_nuts3 <- SpatialPoints(map_crd)
nuts3.knn_10 <- knearneigh(Points_nuts3, k=5)
K10_nb <- knn2nb(nuts3.knn_10)
#row.names = row.names(sub_shp1)

tra<-nb2mat(K10_nb)
row.names(tra)<-row.names(shp1)
colnames(tra)<-row.names(tra)
listw<-mat2listw(tra, style="W")


#test
#plot(shp1, border="grey")
#plot(K10_nb, map_crd, add=TRUE)



row<-row.names(tra)
row<-as.data.frame(row)
row$rownumber = 1:nrow(row)
names(row)[names(row)=="row"]<-"insee"

data_panel<-left_join(data_panel,row)
data_panel<-arrange(data_panel, rownumber, annee)


data_panel<-na.omit(data_panel)



library(splm)

slmtest(nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lml")
slmtest(nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lme")

slmtest(nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlml")
slmtest(nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlme")




slmtest(nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lml")
slmtest(nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lme")

slmtest(nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlml")
slmtest(nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlme")





slmtest(nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lml")
slmtest(nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lme")

slmtest(nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlml")
slmtest(nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlme")




slmtest(nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lml")
slmtest(nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lme")

slmtest(nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlml")
slmtest(nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlme")




slmtest(part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lml")
slmtest(part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="lme")

slmtest(part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlml")
slmtest(part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee), data=data_panel, listw = listw, test="rlme")

















#################Robustness checks : spatial regression models (SEM)###########












library(readr)

library(rgdal)
library(sp)
library(tmap)
shp <- readOGR(dsn = "shapefile france",
               layer = "communes-20190101")



data2<-data[,c("insee","annee","Région","Département","nombre_jours_au_dessus_seuils_no2","nombre_jours_au_dessus_seuils_pm25","nombre_jours_au_dessus_seuils_pm10","nombre_jours_au_dessus_seuils_O3")]
data2<-filter(data2, data2$annee=="2018")

shp1<-merge(shp,data2)


shp1@data$Département<-as.character(shp1@data$Département)
shp1@data$Région<-as.character(shp1@data$Région)


communes_01012019 <- read_csv("socio economic data municipalities/communes-01012019.csv")
communes_01012019<-communes_01012019[,c("com","reg","dep")]
names(communes_01012019)[names(communes_01012019)=="com"]<-"insee"

communes_01012019<-na.omit(communes_01012019)



shp1@data<-left_join(shp1@data,communes_01012019)
shp1@data$reg <- ifelse(is.na(shp1@data$reg), shp1@data$Région, shp1@data$reg)
shp1@data$reg[is.na(shp1@data$reg)]<-"01"


shp1@data$Département[shp1@data$insee=="75056"]<-"75"
shp1@data$Région[shp1@data$insee=="75056"]<-"11"
shp1@data$Département[shp1@data$insee=="13055"]<-"13"
shp1@data$Région[shp1@data$insee=="13055"]<-"93"
shp1@data$Département[shp1@data$insee=="69123"]<-"69"
shp1@data$Région[shp1@data$insee=="69123"]<-"84"

shp1 = subset(shp1, reg != "01")
shp1 = subset(shp1, reg != "02")
shp1 = subset(shp1, reg != "03")
shp1 = subset(shp1, reg != "04")
shp1 = subset(shp1, reg != "06")





data = data %>% select(insee,annee, Département, everything())

data_panel<-data[,c("insee","annee","Département","Région","mediane","nombre_jours_au_dessus_seuils_no2","nombre_jours_au_dessus_seuils_pm25","nombre_jours_au_dessus_seuils_pm10","nombre_jours_au_dessus_seuils_O3","Typologie_urbain_rural","menage_autre_sans_famille_percentage","menage_avec_famille_avec_enfant_percentage","menage_avec_famille_monoparental_percentage","menage_avec_famille_sans_enfant_percentage","pop_0_14_percentage","pop_15_29_percentage","pop_30_44_percentage","pop_45_59_percentage","pop_60_74_percentage","pop_75_89_percentage","pop_90_plus_percentage","pop_homme_percentage","pop_femme_percentage","inactifs_15_64_percentage","chomeurs_15_64_percentage","actifs_occupes_15_64_prof_inter_compl_nombre_percentage","actifs_occupes_15_64_ouvriers_compl_nombre_percentage","actifs_occupes_15_64_employes_compl_nombre_percentage","actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage","actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage","actifs_occupes_15_64_agriculteurs_compl_nombre_percentage","sum_cocktail_day","part_cocktail")]
data_panel<-na.omit(data_panel)
a<-table(data_panel$insee)
a<-as.data.frame(a)
names(a)[names(a)=="Var1"]<-"insee"
data_panel<-left_join(data_panel,a)
data_panel<-filter(data_panel, data_panel$Freq>6)


#for each sub-sample re run the code #
#
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="urbain dense")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="urbain densité intermédiaire")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural sous faible influence d'un pôle")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural sous forte influence d'un pôle")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural autonome très peu dense")
data_panel<-filter(data_panel, data_panel$Typologie_urbain_rural=="rural autonome peu dense")




a<-table(data_panel$insee)
a<-as.data.frame(a)
names(a)[names(a)=="Var1"]<-"insee"


shp1@data<-left_join(shp1@data,a)
shp1 <- shp1[!is.na(shp1@data$Freq),]






row.names(shp1)<-shp1@data$insee



#k nearest neighbor matrix k=5


map_crd <- coordinates(shp1)
Points_nuts3 <- SpatialPoints(map_crd)
nuts3.knn_10 <- knearneigh(Points_nuts3, k=5)
K10_nb <- knn2nb(nuts3.knn_10)
#row.names = row.names(sub_shp1)

tra<-nb2mat(K10_nb)
row.names(tra)<-row.names(shp1)
colnames(tra)<-row.names(tra)
listw<-mat2listw(tra, style="W")


#test
#plot(shp1, border="grey")
#plot(K10_nb, map_crd, add=TRUE)



row<-row.names(tra)
row<-as.data.frame(row)
row$rownumber = 1:nrow(row)
names(row)[names(row)=="row"]<-"insee"

data_panel<-left_join(data_panel,row)
data_panel<-arrange(data_panel, rownumber, annee)


data_panel<-na.omit(data_panel)








fm <- nombre_jours_au_dessus_seuils_no2~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)
test <- spml(fm, data = data_panel, listw = listw,
             model="within", effect = c("time"), lag = FALSE, spatial.error = "b", index = c("insee","annee"))




fm <- nombre_jours_au_dessus_seuils_pm25~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)
test <- spml(fm, data = data_panel, listw = listw,
             model="within", effect = c("time"), lag = FALSE, spatial.error = "b", index = c("insee","annee"))




fm <- nombre_jours_au_dessus_seuils_pm10~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)
test <- spml(fm, data = data_panel, listw = listw,
             model="within", effect = c("time"), lag = FALSE, spatial.error = "b", index = c("insee","annee"))



fm <- nombre_jours_au_dessus_seuils_O3~mediane+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)
test <- spml(fm, data = data_panel, listw = listw,
             model="within", effect = c("time"), lag = FALSE, spatial.error = "b", index = c("insee","annee"))






fm <- part_cocktail~mediane+sum_cocktail_day+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)
test <- spml(fm, data = data_panel, listw = listw,
             model="within", effect = c("time"), lag = FALSE, spatial.error = "b", index = c("insee","annee"))








############## Robustness checks : economic indicators in dense urban area (number of pollution days) #########








data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain densité intermédiaire")



data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain dense")



fm <- nombre_jours_au_dessus_seuils_no2~taux_pauvrete_seuil_60+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)



ols1 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_pm25~taux_pauvrete_seuil_60+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)


ols3 <- lm(fm, data = data_urbain)



fm <- nombre_jours_au_dessus_seuils_pm10~taux_pauvrete_seuil_60+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)


ols5 <- lm(fm, data = data_urbain)




fm <- nombre_jours_au_dessus_seuils_O3~taux_pauvrete_seuil_60+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)


ols7 <- lm(fm, data = data_urbain)


library(stargazer)



fm <- nombre_jours_au_dessus_seuils_no2~premier_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols9 <- lm(fm, data = data_urbain)

fm <- nombre_jours_au_dessus_seuils_no2~neuvieme_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols10 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_no2~part_prestation_sociale+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols11 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_no2~part_revenu_patrimoine_et_autre_revenu+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols12 <- lm(fm, data = data_urbain)







fm <- nombre_jours_au_dessus_seuils_pm25~premier_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols13 <- lm(fm, data = data_urbain)

fm <- nombre_jours_au_dessus_seuils_pm25~neuvieme_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols14 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_pm25~part_prestation_sociale+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols15 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_pm25~part_revenu_patrimoine_et_autre_revenu+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols16 <- lm(fm, data = data_urbain)











fm <- nombre_jours_au_dessus_seuils_pm10~premier_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols17 <- lm(fm, data = data_urbain)

fm <- nombre_jours_au_dessus_seuils_pm10~neuvieme_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols18 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_pm10~part_prestation_sociale+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols19 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_pm10~part_revenu_patrimoine_et_autre_revenu+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols20 <- lm(fm, data = data_urbain)










fm <- nombre_jours_au_dessus_seuils_O3~premier_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols21 <- lm(fm, data = data_urbain)

fm <- nombre_jours_au_dessus_seuils_O3~neuvieme_decile+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols22 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~part_prestation_sociale+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols23 <- lm(fm, data = data_urbain)


fm <- nombre_jours_au_dessus_seuils_O3~part_revenu_patrimoine_et_autre_revenu+menage_autre_sans_famille_percentage+menage_avec_famille_avec_enfant_percentage+menage_avec_famille_monoparental_percentage+menage_avec_famille_sans_enfant_percentage+pop_0_14_percentage+pop_15_29_percentage+pop_30_44_percentage+pop_45_59_percentage+pop_75_89_percentage+pop_90_plus_percentage+pop_femme_percentage+inactifs_15_64_percentage+chomeurs_15_64_percentage+actifs_occupes_15_64_prof_inter_compl_nombre_percentage+actifs_occupes_15_64_employes_compl_nombre_percentage+actifs_occupes_15_64_cadre_prof_intel_compl_nombre_percentage+actifs_occupes_15_64_artisans_chef_entre_compl_nombre_percentage+actifs_occupes_15_64_agriculteurs_compl_nombre_percentage+factor(Département)+factor(annee)

ols24 <- lm(fm, data = data_urbain)






stargazer(ols1, ols9, ols10, ols11, ols12, no.space = TRUE)




stargazer(ols3, ols13, ols14, ols15, ols16, no.space = TRUE)




stargazer(ols5, ols17, ols18, ols19, ols20, no.space = TRUE)



stargazer(ols7, ols21, ols22, ols23, ols24, no.space = TRUE)






################# appendix density, income, economic activity and pollution #######




library(readxl)
entreprises <- read_excel("socio economic data municipalities/entreprises.xlsx")

names(entreprises)[names(entreprises)=="CODGEO"]<-"insee"

entreprises<-entreprises[,c(1,53:62)]
entreprises$annee<-"2018"

library(readxl)
densite_communes <- read_excel("socio economic data municipalities/densite communes.xlsx")

names(densite_communes)[names(densite_communes)=="codgeo"]<-"insee"

densite_communes<-filter(densite_communes, densite_communes$an=="2018")

densite_communes<-densite_communes[,c("insee","dens_pop")]
#densite_communes$annee<-"2018"




data<-left_join(data, entreprises)
data<-left_join(data,densite_communes)





data$sum_cocktail_day<-data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$o3_2+data$no2_2+data$pm10_2+data$pm25_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2



data$part_cocktail<-(data$no2_pm25_pm10_o3+data$no2_pm10_2+data$pm10_o3_2+data$no2_pm10_o3_2+data$no2_o3_2+data$pm25_o3_2+data$pm25_no2_o3_2+data$pm10_pm25_o3_2+data$pm25_pm10_2+data$no2_pm25_pm10_2+data$no2_pm25_2)/data$sum_cocktail_day





data_urbain<-filter(data, data$Typologie_urbain_rural=="rural sous forte influence d'un pôle"|data$Typologie_urbain_rural=="rural sous faible influence d'un pôle"|data$Typologie_urbain_rural=="rural autonome très peu dense"|data$Typologie_urbain_rural=="rural autonome peu dense")


data_urbain<-filter(data_urbain, data_urbain$annee=="2018")

data_urbain<-filter(data_urbain, data_urbain$`Établissements au 31 décembre 2020`!=0)



library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), sum_cocktail_day)) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 1, label.y = 160, size=5)+
  theme_bw() +
  labs(title = "Rural area",
       x = "Density (log)",
       y = "Number of pollution days")+ theme(text=element_text(size=16), #change font size of all text
                                              #axis.text=element_text(size=15), #change font size of axis text
                                              axis.title=element_text(size=16), #change font size of axis titles
                                              plot.title=element_text(size=16), #change font size of plot title
                                              # legend.text=element_text(size=15), #change font size of legend text
                                              legend.title=element_text(size=16))





library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), log(`Établissements au 31 décembre 2020`))) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 1, label.y = 7.5, size=5)+
  theme_bw() +
  labs(title = "Rural area",
       x = "Density (log)",
       y = "Number of companies (log)")+ theme(text=element_text(size=16), #change font size of all text
                                               #axis.text=element_text(size=15), #change font size of axis text
                                               axis.title=element_text(size=16), #change font size of axis titles
                                               plot.title=element_text(size=16), #change font size of plot title
                                               # legend.text=element_text(size=15), #change font size of legend text
                                               legend.title=element_text(size=16))




library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), mediane)) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 1, label.y = 41000, size=5)+
  theme_bw() +
  labs(title = "Rural area",
       x = "Density (log)",
       y = "Income")+ theme(text=element_text(size=16), #change font size of all text
                            #axis.text=element_text(size=15), #change font size of axis text
                            axis.title=element_text(size=16), #change font size of axis titles
                            plot.title=element_text(size=16), #change font size of plot title
                            # legend.text=element_text(size=15), #change font size of legend text
                            legend.title=element_text(size=16))








library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), part_cocktail)) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 1, label.y = 0.38, size=5)+
  theme_bw() +
  labs(title = "Rural area",
       x = "Density (log)",
       y = "Share of cocktail days")+ theme(text=element_text(size=16), #change font size of all text
                                            #axis.text=element_text(size=15), #change font size of axis text
                                            axis.title=element_text(size=16), #change font size of axis titles
                                            plot.title=element_text(size=16), #change font size of plot title
                                            # legend.text=element_text(size=15), #change font size of legend text
                                            legend.title=element_text(size=16))













data_urbain<-filter(data, data$Typologie_urbain_rural=="urbain dense"|data$Typologie_urbain_rural=="urbain densité intermédiaire")

data_urbain<-filter(data_urbain, data_urbain$annee=="2018")

#data_urbain<-filter(data_urbain, data_urbain$`Établissements au 31 décembre 2020`!=0)



library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), sum_cocktail_day)) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 4, label.y = 210, size=5)+
  theme_bw() +
  labs(title = "Urban area",
       x = "Density (log)",
       y = "Number of pollution days")+ theme(text=element_text(size=16), #change font size of all text
                                              #axis.text=element_text(size=15), #change font size of axis text
                                              axis.title=element_text(size=16), #change font size of axis titles
                                              plot.title=element_text(size=16), #change font size of plot title
                                              # legend.text=element_text(size=15), #change font size of legend text
                                              legend.title=element_text(size=16))






library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), log(`Établissements au 31 décembre 2020`))) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 4, label.y = 10.3, size=5)+
  theme_bw() +
  labs(title = "Urban area",
       x = "Density (log)",
       y = "Number of companies (log)")+ theme(text=element_text(size=16), #change font size of all text
                                               #axis.text=element_text(size=15), #change font size of axis text
                                               axis.title=element_text(size=16), #change font size of axis titles
                                               plot.title=element_text(size=16), #change font size of plot title
                                               # legend.text=element_text(size=15), #change font size of legend text
                                               legend.title=element_text(size=16))




library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), mediane)) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 4, label.y = 41000, size=5)+
  theme_bw() +
  labs(title = "Urban area",
       x = "Density (log)",
       y = "Income")+ theme(text=element_text(size=16), #change font size of all text
                            #axis.text=element_text(size=15), #change font size of axis text
                            axis.title=element_text(size=16), #change font size of axis titles
                            plot.title=element_text(size=16), #change font size of plot title
                            # legend.text=element_text(size=15), #change font size of legend text
                            legend.title=element_text(size=16))








library(ggplot2)
ggplot(data_urbain,aes(log(dens_pop), part_cocktail)) +
  geom_point()+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.x = 4, label.y = 0.47, size=5)+
  theme_bw() +
  labs(title = "Urban area",
       x = "Density (log)",
       y = "Share of cocktail days")+ theme(text=element_text(size=16), #change font size of all text
                                            #axis.text=element_text(size=15), #change font size of axis text
                                            axis.title=element_text(size=16), #change font size of axis titles
                                            plot.title=element_text(size=16), #change font size of plot title
                                            # legend.text=element_text(size=15), #change font size of legend text
                                            legend.title=element_text(size=16))












