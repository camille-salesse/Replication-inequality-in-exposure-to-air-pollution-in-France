


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



########## bar plot ##########



data_bar<-data[,c("nombre_jours_au_dessus_seuils_no2","nombre_jours_au_dessus_seuils_pm25","nombre_jours_au_dessus_seuils_pm10","nombre_jours_au_dessus_seuils_O3","Typologie_urbain_rural")]

data_bar<-as.data.frame(data_bar)

data_bar_ag<-aggregate(.~Typologie_urbain_rural, mean,data=data_bar)

row.names(data_bar_ag)<-data_bar_ag$Typologie_urbain_rural

data_bar_ag<-data_bar_ag[,c("nombre_jours_au_dessus_seuils_no2","nombre_jours_au_dessus_seuils_pm25","nombre_jours_au_dessus_seuils_pm10","nombre_jours_au_dessus_seuils_O3")]

data_bar_ag<-data_bar_ag[c(2,1,3,4,6,5),]

names(data_bar_ag)[names(data_bar_ag)=="nombre_jours_au_dessus_seuils_no2"]<-"NO2"
names(data_bar_ag)[names(data_bar_ag)=="nombre_jours_au_dessus_seuils_pm25"]<-"PM25"
names(data_bar_ag)[names(data_bar_ag)=="nombre_jours_au_dessus_seuils_pm10"]<-"PM10"
names(data_bar_ag)[names(data_bar_ag)=="nombre_jours_au_dessus_seuils_O3"]<-"O3"

data_bar_ag<-as.matrix(data_bar_ag)


library(MetBrewer)



barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,120), col = met.brewer("Hokusai1", 6))



rownames(data_bar_ag)<-c("Very sparsely populated autonomous rural","Sparsely populated autonomous rural","Rural under weak influence of a pole","Rural under strong influence of a pole","Urban intermediate density","Dense urban")


barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,120), col = met.brewer("Hokusai1", 6))




######### time serie air pollution plot ############






data_plot<-aggregate(nombre_jours_au_dessus_seuils_pm25~Typologie_urbain_rural+annee, FUN = mean, data=data)




library(writexl)
writexl::write_xlsx(data_plot, "/plot_serie_pm2.5.xlsx")




data_plot<-aggregate(nombre_jours_au_dessus_seuils_pm10~Typologie_urbain_rural+annee, FUN = mean, data=data)



library(writexl)
writexl::write_xlsx(data_plot, "/plot_serie_pm10.xlsx")



data_plot<-aggregate(nombre_jours_au_dessus_seuils_no2~Typologie_urbain_rural+annee, FUN = mean, data=data)



library(writexl)
writexl::write_xlsx(data_plot, "/plot_serie_no2.xlsx")




data_plot<-aggregate(nombre_jours_au_dessus_seuils_O3~Typologie_urbain_rural+annee, FUN = mean, data=data)



library(writexl)
writexl::write_xlsx(data_plot, "/plot_serie_o3.xlsx")



#see "plot_time_serie_air_pollution.xlsx"






