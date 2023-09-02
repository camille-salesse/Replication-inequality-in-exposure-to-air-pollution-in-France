



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


# Load the original csv
# This was created after running preparation.R

data <- read.csv("final_data_inequality_in_exposure")



###### plot graph pie ##########



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








#data2<-filter(data, data$Typologie_urbain_rural=="rural autonome peu dense"| data$Typologie_urbain_rural=="rural autonome très peu dense"|data$Typologie_urbain_rural=="rural sous faible influence d'un pôle"|data$Typologie_urbain_rural=="rural sous forte influence d'un pôle")


data$somme_cocktail<-data$part_no2_pm25_pm10_o3+data$part_no2_pm10_2+data$part_pm10_o3_2+data$part_no2_pm10_o3_2+data$part_no2_o3_2+data$part_pm25_o3_2+data$part_o3_2+data$part_no2_2+data$part_pm10_2+data$part_pm25_2+data$part_pm25_no2_o3_2+data$part_pm10_pm25_o3_2+data$part_pm25_pm10_2+data$part_no2_pm25_pm10_2+data$part_no2_pm25_2
table(data$somme_cocktail)
#OK
#if obs doesn't match stat des it's because of days 0 pol pm25


data_bar<-data[,c("part_no2_pm25_pm10_o3","part_no2_pm10_2","part_pm10_o3_2" ,"part_no2_pm10_o3_2","part_no2_o3_2","part_pm25_o3_2","part_o3_2", "part_no2_2","part_pm10_2","part_pm25_2"  ,"part_pm25_no2_o3_2","part_pm10_pm25_o3_2","part_pm25_pm10_2" ,"part_no2_pm25_pm10_2","part_no2_pm25_2")]

data_bar<-as.data.frame(data_bar)

stat_des<-colMeans(data_bar,na.rm = T)
stat_des<-as.data.frame(stat_des)
stat_des$name<-row.names(stat_des)

sum(stat_des$stat_des)

library(writexl)
#writexl::write_xlsx(stat_des, "code econometrics and graph/part_moyenne_full_cocktail_national.xlsx")

########## plot dense urban ############





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








data2<-filter(data, data$Typologie_urbain_rural=="urbain dense")


data$somme_cocktail<-data$part_no2_pm25_pm10_o3+data$part_no2_pm10_2+data$part_pm10_o3_2+data$part_no2_pm10_o3_2+data$part_no2_o3_2+data$part_pm25_o3_2+data$part_o3_2+data$part_no2_2+data$part_pm10_2+data$part_pm25_2+data$part_pm25_no2_o3_2+data$part_pm10_pm25_o3_2+data$part_pm25_pm10_2+data$part_no2_pm25_pm10_2+data$part_no2_pm25_2
table(data$somme_cocktail)


data_bar<-data2[,c("part_no2_pm25_pm10_o3","part_no2_pm10_2","part_pm10_o3_2" ,"part_no2_pm10_o3_2","part_no2_o3_2","part_pm25_o3_2","part_o3_2", "part_no2_2","part_pm10_2","part_pm25_2"  ,"part_pm25_no2_o3_2","part_pm10_pm25_o3_2","part_pm25_pm10_2" ,"part_no2_pm25_pm10_2","part_no2_pm25_2")]

data_bar<-as.data.frame(data_bar)

stat_des<-colMeans(data_bar,na.rm = T)
stat_des<-as.data.frame(stat_des)
stat_des$name<-row.names(stat_des)

sum(stat_des$stat_des)

library(writexl)
#writexl::write_xlsx(stat_des, "code econometrics and graph/part_moyenne_full_cocktail_urbain_dense.xlsx")


########## plot urbain dense inter





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








data2<-filter(data, data$Typologie_urbain_rural=="urbain densité intermédiaire")


data$somme_cocktail<-data$part_no2_pm25_pm10_o3+data$part_no2_pm10_2+data$part_pm10_o3_2+data$part_no2_pm10_o3_2+data$part_no2_o3_2+data$part_pm25_o3_2+data$part_o3_2+data$part_no2_2+data$part_pm10_2+data$part_pm25_2+data$part_pm25_no2_o3_2+data$part_pm10_pm25_o3_2+data$part_pm25_pm10_2+data$part_no2_pm25_pm10_2+data$part_no2_pm25_2
table(data$somme_cocktail)


data_bar<-data2[,c("part_no2_pm25_pm10_o3","part_no2_pm10_2","part_pm10_o3_2" ,"part_no2_pm10_o3_2","part_no2_o3_2","part_pm25_o3_2","part_o3_2", "part_no2_2","part_pm10_2","part_pm25_2"  ,"part_pm25_no2_o3_2","part_pm10_pm25_o3_2","part_pm25_pm10_2" ,"part_no2_pm25_pm10_2","part_no2_pm25_2")]

data_bar<-as.data.frame(data_bar)

stat_des<-colMeans(data_bar,na.rm = T)
stat_des<-as.data.frame(stat_des)
stat_des$name<-row.names(stat_des)

sum(stat_des$stat_des)

library(writexl)
#writexl::write_xlsx(stat_des, "code econometrics and graph/part_moyenne_full_cocktail_urbain_dense_inter.xlsx")


########## plot rural





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







data2<-filter(data, data$Typologie_urbain_rural=="rural autonome peu dense"| data$Typologie_urbain_rural=="rural autonome très peu dense"|data$Typologie_urbain_rural=="rural sous faible influence d'un pôle"|data$Typologie_urbain_rural=="rural sous forte influence d'un pôle")



data$somme_cocktail<-data$part_no2_pm25_pm10_o3+data$part_no2_pm10_2+data$part_pm10_o3_2+data$part_no2_pm10_o3_2+data$part_no2_o3_2+data$part_pm25_o3_2+data$part_o3_2+data$part_no2_2+data$part_pm10_2+data$part_pm25_2+data$part_pm25_no2_o3_2+data$part_pm10_pm25_o3_2+data$part_pm25_pm10_2+data$part_no2_pm25_pm10_2+data$part_no2_pm25_2
table(data$somme_cocktail)


data_bar<-data2[,c("part_no2_pm25_pm10_o3","part_no2_pm10_2","part_pm10_o3_2" ,"part_no2_pm10_o3_2","part_no2_o3_2","part_pm25_o3_2","part_o3_2", "part_no2_2","part_pm10_2","part_pm25_2"  ,"part_pm25_no2_o3_2","part_pm10_pm25_o3_2","part_pm25_pm10_2" ,"part_no2_pm25_pm10_2","part_no2_pm25_2")]

data_bar<-as.data.frame(data_bar)

stat_des<-colMeans(data_bar,na.rm = T)
stat_des<-as.data.frame(stat_des)
stat_des$name<-row.names(stat_des)

sum(stat_des$stat_des)

library(writexl)
#writexl::write_xlsx(stat_des, "code econometrics and graph/part_moyenne_full_cocktail_rural.xlsx")


