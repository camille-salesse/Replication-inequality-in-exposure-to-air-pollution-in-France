

setwd("D:/code and data inequality in exposure to air pollution")

####### no2 daily ###########


ConcCommunales_2009_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2009_NO2_daymean.csv", header=FALSE)
ConcCommunales_2010_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2010_NO2_daymean.csv", header=FALSE)
ConcCommunales_2011_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2011_NO2_daymean.csv", header=FALSE)
ConcCommunales_2012_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2012_NO2_daymean.csv", header=FALSE)
ConcCommunales_2013_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2013_NO2_daymean.csv", header=FALSE)
ConcCommunales_2014_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2014_NO2_daymean.csv", header=FALSE)
ConcCommunales_2015_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2015_NO2_daymean.csv", header=FALSE)
ConcCommunales_2016_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2016_NO2_daymean.csv", header=FALSE)
ConcCommunales_2017_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2017_NO2_daymean.csv", header=FALSE)
ConcCommunales_2018_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2018_NO2_daymean.csv", header=FALSE)
ConcCommunales_2019_NO2_daymean <- read.csv("data air pollution/ConcCommunales_2019_NO2_daymean.csv", header=FALSE)




#WHO guideline thresold : NO2 2021
#10 microg/m3 annual mean
#25 microg/m3 24-hour mean



com_2009<-ConcCommunales_2009_NO2_daymean
com_2009[3:35230,4:368][com_2009[3:35230,4:368] <= 25]<-0
com_2009[3:35230,4:368][com_2009[3:35230,4:368]>25]<-1
com_2009$nombre_jours_au_dessus_seuils<-rowSums(com_2009[,4:368])
com_2009$annee<-2009



com_2010<-ConcCommunales_2010_NO2_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 25]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>25]<-1
com_2010$nombre_jours_au_dessus_seuils<-rowSums(com_2010[,4:368])
com_2010$annee<-2010


com_2011<-ConcCommunales_2011_NO2_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 25]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>25]<-1
com_2011$nombre_jours_au_dessus_seuils<-rowSums(com_2011[,4:368])
com_2011$annee<-2011


com_2012<-ConcCommunales_2012_NO2_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 25]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>25]<-1
com_2012$nombre_jours_au_dessus_seuils<-rowSums(com_2012[,4:368])
com_2012$annee<-2012


com_2013<-ConcCommunales_2013_NO2_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 25]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>25]<-1
com_2013$nombre_jours_au_dessus_seuils<-rowSums(com_2013[,4:368])
com_2013$annee<-2013


com_2014<-ConcCommunales_2014_NO2_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 25]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>25]<-1
com_2014$nombre_jours_au_dessus_seuils<-rowSums(com_2014[,4:368])
com_2014$annee<-2014


com_2015<-ConcCommunales_2015_NO2_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 25]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>25]<-1
com_2015$nombre_jours_au_dessus_seuils<-rowSums(com_2015[,4:368])
com_2015$annee<-2015


com_2016<-ConcCommunales_2016_NO2_daymean
com_2016[3:35230,4:368][com_2016[3:35230,4:368] <= 25]<-0
com_2016[3:35230,4:368][com_2016[3:35230,4:368]>25]<-1
com_2016$nombre_jours_au_dessus_seuils<-rowSums(com_2016[,4:368])
com_2016$annee<-2016


com_2017<-ConcCommunales_2017_NO2_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 25]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>25]<-1
com_2017$nombre_jours_au_dessus_seuils<-rowSums(com_2017[,4:368])
com_2017$annee<-2017


com_2018<-ConcCommunales_2018_NO2_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 25]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>25]<-1
com_2018$nombre_jours_au_dessus_seuils<-rowSums(com_2018[,4:368])
com_2018$annee<-2018


com_2019<-ConcCommunales_2019_NO2_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 25]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>25]<-1
com_2019$nombre_jours_au_dessus_seuils<-rowSums(com_2019[,4:368])
com_2019$annee<-2019

library(dplyr)
donee_com_nbr_jour_seuil<-com_2019
donee_com_nbr_jour_seuil<-donee_com_nbr_jour_seuil[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]

com_2009<-com_2009[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2010<-com_2010[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2011<-com_2011[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2012<-com_2012[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2013<-com_2013[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2014<-com_2014[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2015<-com_2015[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2016<-com_2016[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2017<-com_2017[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2018<-com_2018[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]


donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2009)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2010)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2011)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2012)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2013)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2014)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2015)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2016)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2017)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2018)

donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils[donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils>365]<-NA
names(donee_com_nbr_jour_seuil)[names(donee_com_nbr_jour_seuil)=="nombre_jours_au_dessus_seuils"]<-"nombre_jours_au_dessus_seuils_no2"

#write.csv(donee_com_nbr_jour_seuil,"code air pollution/donnee_seuil_jour_no2")



####### pm10 daily #####

ConcCommunales_2009_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2009_PM10_daymean.csv", header=FALSE)
ConcCommunales_2010_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2010_PM10_daymean.csv", header=FALSE)
ConcCommunales_2011_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2011_PM10_daymean.csv", header=FALSE)
ConcCommunales_2012_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2012_PM10_daymean.csv", header=FALSE)
ConcCommunales_2013_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2013_PM10_daymean.csv", header=FALSE)
ConcCommunales_2014_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2014_PM10_daymean.csv", header=FALSE)
ConcCommunales_2015_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2015_PM10_daymean.csv", header=FALSE)
ConcCommunales_2016_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2016_PM10_daymean.csv", header=FALSE)
ConcCommunales_2017_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2017_PM10_daymean.csv", header=FALSE)
ConcCommunales_2018_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2018_PM10_daymean.csv", header=FALSE)
ConcCommunales_2019_PM10_daymean <- read.csv("data air pollution/ConcCommunales_2019_PM10_daymean.csv", header=FALSE)



#WHO guideline thresold : pm10 2021
#Coarse particulate matter (PM10)
#15 micro g/m3 annual mean
#45 micr g/m3 24-hour mean



com_2009<-ConcCommunales_2009_PM10_daymean
com_2009[3:35230,4:368][com_2009[3:35230,4:368] <= 45]<-0
com_2009[3:35230,4:368][com_2009[3:35230,4:368]>45]<-1
com_2009$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2009[,4:368])
com_2009$annee<-2009



com_2010<-ConcCommunales_2010_PM10_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 45]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>45]<-1
com_2010$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2010[,4:368])
com_2010$annee<-2010


com_2011<-ConcCommunales_2011_PM10_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 45]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>45]<-1
com_2011$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2011[,4:368])
com_2011$annee<-2011


com_2012<-ConcCommunales_2012_PM10_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 45]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>45]<-1
com_2012$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2012[,4:368])
com_2012$annee<-2012


com_2013<-ConcCommunales_2013_PM10_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 45]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>45]<-1
com_2013$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2013[,4:368])
com_2013$annee<-2013


com_2014<-ConcCommunales_2014_PM10_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 45]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>45]<-1
com_2014$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2014[,4:368])
com_2014$annee<-2014


com_2015<-ConcCommunales_2015_PM10_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 45]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>45]<-1
com_2015$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2015[,4:368])
com_2015$annee<-2015


com_2016<-ConcCommunales_2016_PM10_daymean
com_2016[3:35230,4:368][com_2016[3:35230,4:368] <= 45]<-0
com_2016[3:35230,4:368][com_2016[3:35230,4:368]>45]<-1
com_2016$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2016[,4:368])
com_2016$annee<-2016


com_2017<-ConcCommunales_2017_PM10_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 45]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>45]<-1
com_2017$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2017[,4:368])
com_2017$annee<-2017


com_2018<-ConcCommunales_2018_PM10_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 45]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>45]<-1
com_2018$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2018[,4:368])
com_2018$annee<-2018


com_2019<-ConcCommunales_2019_PM10_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 45]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>45]<-1
com_2019$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2019[,4:368])
com_2019$annee<-2019

library(dplyr)
donee_com_nbr_jour_seuil<-com_2019
donee_com_nbr_jour_seuil<-donee_com_nbr_jour_seuil[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]

com_2009<-com_2009[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2010<-com_2010[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2011<-com_2011[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2012<-com_2012[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2013<-com_2013[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2014<-com_2014[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2015<-com_2015[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2016<-com_2016[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2017<-com_2017[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]
com_2018<-com_2018[,c("V1","V2","V3","nombre_jours_au_dessus_seuils_pm10","annee")]


donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2009)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2010)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2011)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2012)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2013)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2014)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2015)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2016)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2017)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2018)

donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils_pm10[donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils_pm10>365]<-NA

#write.csv(donee_com_nbr_jour_seuil,"code air pollution/donnee_seuil_jour_pm10")



#######  pm2.5 daily #####

ConcCommunales_2009_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2009_PM25_daymean.csv", header=FALSE)
ConcCommunales_2010_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2010_PM25_daymean.csv", header=FALSE)
ConcCommunales_2011_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2011_PM25_daymean.csv", header=FALSE)
ConcCommunales_2012_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2012_PM25_daymean.csv", header=FALSE)
ConcCommunales_2013_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2013_PM25_daymean.csv", header=FALSE)
ConcCommunales_2014_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2014_PM25_daymean.csv", header=FALSE)
ConcCommunales_2015_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2015_PM25_daymean.csv", header=FALSE)
ConcCommunales_2016_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2016_PM25_daymean.csv", header=FALSE)
ConcCommunales_2017_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2017_PM25_daymean.csv", header=FALSE)
ConcCommunales_2018_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2018_PM25_daymean.csv", header=FALSE)
ConcCommunales_2019_PM25_daymean <- read.csv("data air pollution/ConcCommunales_2019_PM25_daymean.csv", header=FALSE)


#WHO guideline thresold : PM2.5 2021
#5 MICg/m3 annual mean
#15 MICg/m3 24-hour mean


com_2009<-ConcCommunales_2009_PM25_daymean
com_2009[3:35230,4:368][com_2009[3:35230,4:368] <= 15]<-0
com_2009[3:35230,4:368][com_2009[3:35230,4:368]>15]<-1
com_2009$nombre_jours_au_dessus_seuils<-rowSums(com_2009[,4:368])
com_2009$annee<-2009



com_2010<-ConcCommunales_2010_PM25_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 15]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>15]<-1
com_2010$nombre_jours_au_dessus_seuils<-rowSums(com_2010[,4:368])
com_2010$annee<-2010


com_2011<-ConcCommunales_2011_PM25_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 15]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>15]<-1
com_2011$nombre_jours_au_dessus_seuils<-rowSums(com_2011[,4:368])
com_2011$annee<-2011


com_2012<-ConcCommunales_2012_PM25_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 15]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>15]<-1
com_2012$nombre_jours_au_dessus_seuils<-rowSums(com_2012[,4:368])
com_2012$annee<-2012


com_2013<-ConcCommunales_2013_PM25_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 15]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>15]<-1
com_2013$nombre_jours_au_dessus_seuils<-rowSums(com_2013[,4:368])
com_2013$annee<-2013


com_2014<-ConcCommunales_2014_PM25_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 15]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>15]<-1
com_2014$nombre_jours_au_dessus_seuils<-rowSums(com_2014[,4:368])
com_2014$annee<-2014


com_2015<-ConcCommunales_2015_PM25_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 15]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>15]<-1
com_2015$nombre_jours_au_dessus_seuils<-rowSums(com_2015[,4:368])
com_2015$annee<-2015


com_2016<-ConcCommunales_2016_PM25_daymean
com_2016[3:35230,4:368][com_2016[3:35230,4:368] <= 15]<-0
com_2016[3:35230,4:368][com_2016[3:35230,4:368]>15]<-1
com_2016$nombre_jours_au_dessus_seuils<-rowSums(com_2016[,4:368])
com_2016$annee<-2016


com_2017<-ConcCommunales_2017_PM25_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 15]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>15]<-1
com_2017$nombre_jours_au_dessus_seuils<-rowSums(com_2017[,4:368])
com_2017$annee<-2017


com_2018<-ConcCommunales_2018_PM25_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 15]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>15]<-1
com_2018$nombre_jours_au_dessus_seuils<-rowSums(com_2018[,4:368])
com_2018$annee<-2018


com_2019<-ConcCommunales_2019_PM25_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 15]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>15]<-1
com_2019$nombre_jours_au_dessus_seuils<-rowSums(com_2019[,4:368])
com_2019$annee<-2019

library(dplyr)
donee_com_nbr_jour_seuil<-com_2019
donee_com_nbr_jour_seuil<-donee_com_nbr_jour_seuil[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]

com_2009<-com_2009[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2010<-com_2010[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2011<-com_2011[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2012<-com_2012[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2013<-com_2013[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2014<-com_2014[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2015<-com_2015[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2016<-com_2016[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2017<-com_2017[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2018<-com_2018[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]


donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2009)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2010)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2011)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2012)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2013)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2014)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2015)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2016)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2017)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2018)

donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils[donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils>365]<-NA
names(donee_com_nbr_jour_seuil)[names(donee_com_nbr_jour_seuil)=="nombre_jours_au_dessus_seuils"]<-"nombre_jours_au_dessus_seuils_pm25"

#write.csv(donee_com_nbr_jour_seuil,"code air pollution/donnee_seuil_jour_pm25")



###### o3 daily ####

ConcCommunales_2009_O3_daymean <- read.csv("data air pollution/ConcCommunales_2009_O3_daymean.csv", header=FALSE)
ConcCommunales_2010_O3_daymean <- read.csv("data air pollution/ConcCommunales_2010_O3_daymean.csv", header=FALSE)
ConcCommunales_2011_O3_daymean <- read.csv("data air pollution/ConcCommunales_2011_O3_daymean.csv", header=FALSE)
ConcCommunales_2012_O3_daymean <- read.csv("data air pollution/ConcCommunales_2012_O3_daymean.csv", header=FALSE)
ConcCommunales_2013_O3_daymean <- read.csv("data air pollution/ConcCommunales_2013_O3_daymean.csv", header=FALSE)
ConcCommunales_2014_O3_daymean <- read.csv("data air pollution/ConcCommunales_2014_O3_daymean.csv", header=FALSE)
ConcCommunales_2015_O3_daymean <- read.csv("data air pollution/ConcCommunales_2015_O3_daymean.csv", header=FALSE)
ConcCommunales_2016_O3_daymean <- read.csv("data air pollution/ConcCommunales_2016_O3_daymean.csv", header=FALSE)
ConcCommunales_2017_O3_daymean <- read.csv("data air pollution/ConcCommunales_2017_O3_daymean.csv", header=FALSE)
ConcCommunales_2018_O3_daymean <- read.csv("data air pollution/ConcCommunales_2018_O3_daymean.csv", header=FALSE)
ConcCommunales_2019_O3_daymean <- read.csv("data air pollution/ConcCommunales_2019_O3_daymean.csv", header=FALSE)


#WHO guideline thresold : O3 2021
#take the daily maximum of the 8-hour rolling average (take the highest of the 8-hour rolling averages)
#we take the 24-hour average which must not exceed 100 

com_2009<-ConcCommunales_2009_O3_daymean
com_2009[3:35230,4:368][com_2009[3:35230,4:368] <= 100 ]<-0
com_2009[3:35230,4:368][com_2009[3:35230,4:368]>100]<-1
com_2009$nombre_jours_au_dessus_seuils<-rowSums(com_2009[,4:368])
com_2009$annee<-2009



com_2010<-ConcCommunales_2010_O3_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 100 ]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>100]<-1
com_2010$nombre_jours_au_dessus_seuils<-rowSums(com_2010[,4:368])
com_2010$annee<-2010


com_2011<-ConcCommunales_2011_O3_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 100 ]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>100]<-1
com_2011$nombre_jours_au_dessus_seuils<-rowSums(com_2011[,4:368])
com_2011$annee<-2011


com_2012<-ConcCommunales_2012_O3_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 100 ]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>100]<-1
com_2012$nombre_jours_au_dessus_seuils<-rowSums(com_2012[,4:368])
com_2012$annee<-2012


com_2013<-ConcCommunales_2013_O3_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 100 ]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>100]<-1
com_2013$nombre_jours_au_dessus_seuils<-rowSums(com_2013[,4:368])
com_2013$annee<-2013


com_2014<-ConcCommunales_2014_O3_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 100 ]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>100]<-1
com_2014$nombre_jours_au_dessus_seuils<-rowSums(com_2014[,4:368])
com_2014$annee<-2014


com_2015<-ConcCommunales_2015_O3_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 100 ]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>100]<-1
com_2015$nombre_jours_au_dessus_seuils<-rowSums(com_2015[,4:368])
com_2015$annee<-2015


com_2016<-ConcCommunales_2016_O3_daymean
com_2016[3:35289,4:368][com_2016[3:35289,4:368] <= 100 ]<-0
com_2016[3:35289,4:368][com_2016[3:35289,4:368]>100]<-1
com_2016$nombre_jours_au_dessus_seuils<-rowSums(com_2016[,4:368])
com_2016$annee<-2016


com_2017<-ConcCommunales_2017_O3_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 100 ]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>100]<-1
com_2017$nombre_jours_au_dessus_seuils<-rowSums(com_2017[,4:368])
com_2017$annee<-2017


com_2018<-ConcCommunales_2018_O3_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 100 ]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>100]<-1
com_2018$nombre_jours_au_dessus_seuils<-rowSums(com_2018[,4:368])
com_2018$annee<-2018


com_2019<-ConcCommunales_2019_O3_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 100 ]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>100]<-1
com_2019$nombre_jours_au_dessus_seuils<-rowSums(com_2019[,4:368])
com_2019$annee<-2019

library(dplyr)
donee_com_nbr_jour_seuil<-com_2019
donee_com_nbr_jour_seuil<-donee_com_nbr_jour_seuil[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]

com_2009<-com_2009[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2010<-com_2010[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2011<-com_2011[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2012<-com_2012[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2013<-com_2013[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2014<-com_2014[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2015<-com_2015[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2016<-com_2016[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2017<-com_2017[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]
com_2018<-com_2018[,c("V1","V2","V3","nombre_jours_au_dessus_seuils","annee")]


donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2009)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2010)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2011)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2012)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2013)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2014)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2015)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2016)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2017)
donee_com_nbr_jour_seuil<-rbind(donee_com_nbr_jour_seuil,com_2018)

donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils[donee_com_nbr_jour_seuil$nombre_jours_au_dessus_seuils>365]<-NA
names(donee_com_nbr_jour_seuil)[names(donee_com_nbr_jour_seuil)=="nombre_jours_au_dessus_seuils"]<-"nombre_jours_au_dessus_seuils_O3"

#write.csv(donee_com_nbr_jour_seuil,"code air pollution/donnee_seuil_jour_O3")



