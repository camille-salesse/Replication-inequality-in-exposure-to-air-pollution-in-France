

setwd("D:/code and data inequality in exposure to air pollution")


#######base no2 daily ###########


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
#com_2009$nombre_jours_au_dessus_seuils<-rowSums(com_2009[,4:368])
#com_2009$annee<-2009
com_2009<-com_2009[-1,]
com_2009<-com_2009[-1,]

names(com_2009) <- sub('V', 'V_', names(com_2009))
com_2009<-reshape(com_2009, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2009), sep = "_")
names(com_2009)[names(com_2009)=="V"]<-"no2"
com_2009$annee<-2009

com_2009_no2<-com_2009




com_2010<-ConcCommunales_2010_NO2_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 25]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>25]<-1
#com_2010$nombre_jours_au_dessus_seuils<-rowSums(com_2010[,4:368])
#com_2010$annee<-2010
com_2010<-com_2010[-1,]
com_2010<-com_2010[-1,]

names(com_2010) <- sub('V', 'V_', names(com_2010))
com_2010<-reshape(com_2010, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2010), sep = "_")
names(com_2010)[names(com_2010)=="V"]<-"no2"
com_2010$annee<-2010

com_2010_no2<-com_2010


com_2011<-ConcCommunales_2011_NO2_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 25]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>25]<-1
#com_2011$nombre_jours_au_dessus_seuils<-rowSums(com_2011[,4:368])
#com_2011$annee<-2011
com_2011<-com_2011[-1,]
com_2011<-com_2011[-1,]

names(com_2011) <- sub('V', 'V_', names(com_2011))
com_2011<-reshape(com_2011, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2011), sep = "_")
names(com_2011)[names(com_2011)=="V"]<-"no2"
com_2011$annee<-2011

com_2011_no2<-com_2011



com_2012<-ConcCommunales_2012_NO2_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 25]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>25]<-1
#com_2012$nombre_jours_au_dessus_seuils<-rowSums(com_2012[,4:368])
#com_2012$annee<-2012
com_2012<-com_2012[-1,]
com_2012<-com_2012[-1,]

names(com_2012) <- sub('V', 'V_', names(com_2012))
com_2012<-com_2012[,-369]

com_2012<-reshape(com_2012, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2012), sep = "_")
names(com_2012)[names(com_2012)=="V"]<-"no2"
com_2012$annee<-2012

com_2012_no2<-com_2012



com_2013<-ConcCommunales_2013_NO2_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 25]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>25]<-1
#com_2013$nombre_jours_au_dessus_seuils<-rowSums(com_2013[,4:368])
#com_2013$annee<-2013
com_2013<-com_2013[-1,]
com_2013<-com_2013[-1,]

names(com_2013) <- sub('V', 'V_', names(com_2013))
com_2013<-reshape(com_2013, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2013), sep = "_")
names(com_2013)[names(com_2013)=="V"]<-"no2"
com_2013$annee<-2013

com_2013_no2<-com_2013



com_2014<-ConcCommunales_2014_NO2_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 25]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>25]<-1
#com_2014$nombre_jours_au_dessus_seuils<-rowSums(com_2014[,4:368])
#com_2014$annee<-2014
com_2014<-com_2014[-1,]
com_2014<-com_2014[-1,]

names(com_2014) <- sub('V', 'V_', names(com_2014))
com_2014<-reshape(com_2014, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2014), sep = "_")
names(com_2014)[names(com_2014)=="V"]<-"no2"
com_2014$annee<-2014

com_2014_no2<-com_2014



com_2015<-ConcCommunales_2015_NO2_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 25]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>25]<-1
#com_2015$nombre_jours_au_dessus_seuils<-rowSums(com_2015[,4:368])
#com_2015$annee<-2015
com_2015<-com_2015[-1,]
com_2015<-com_2015[-1,]

names(com_2015) <- sub('V', 'V_', names(com_2015))
com_2015<-reshape(com_2015, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2015), sep = "_")
names(com_2015)[names(com_2015)=="V"]<-"no2"
com_2015$annee<-2015

com_2015_no2<-com_2015



com_2016<-ConcCommunales_2016_NO2_daymean
com_2016[3:35230,4:368][com_2016[3:35230,4:368] <= 25]<-0
com_2016[3:35230,4:368][com_2016[3:35230,4:368]>25]<-1
#com_2016$nombre_jours_au_dessus_seuils<-rowSums(com_2016[,4:368])
#com_2016$annee<-2016
com_2016<-com_2016[-1,]
com_2016<-com_2016[-1,]

names(com_2016) <- sub('V', 'V_', names(com_2016))
com_2016<-com_2016[,-369]
com_2016<-reshape(com_2016, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2016), sep = "_")
names(com_2016)[names(com_2016)=="V"]<-"no2"
com_2016$annee<-2016

com_2016_no2<-com_2016



com_2017<-ConcCommunales_2017_NO2_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 25]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>25]<-1
#com_2017$nombre_jours_au_dessus_seuils<-rowSums(com_2017[,4:368])
#com_2017$annee<-2017
com_2017<-com_2017[-1,]
com_2017<-com_2017[-1,]

names(com_2017) <- sub('V', 'V_', names(com_2017))
com_2017<-reshape(com_2017, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2017), sep = "_")
names(com_2017)[names(com_2017)=="V"]<-"no2"
com_2017$annee<-2017

com_2017_no2<-com_2017



com_2018<-ConcCommunales_2018_NO2_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 25]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>25]<-1
#com_2018$nombre_jours_au_dessus_seuils<-rowSums(com_2018[,4:368])
#com_2018$annee<-2018
com_2018<-com_2018[-1,]
com_2018<-com_2018[-1,]

names(com_2018) <- sub('V', 'V_', names(com_2018))
com_2018<-reshape(com_2018, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2018), sep = "_")
names(com_2018)[names(com_2018)=="V"]<-"no2"
com_2018$annee<-2018

com_2018_no2<-com_2018



com_2019<-ConcCommunales_2019_NO2_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 25]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>25]<-1
#com_2019$nombre_jours_au_dessus_seuils<-rowSums(com_2019[,4:368])
#com_2019$annee<-2019
com_2019<-com_2019[-1,]
com_2019<-com_2019[-1,]

names(com_2019) <- sub('V', 'V_', names(com_2019))
com_2019<-reshape(com_2019, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2019), sep = "_")
names(com_2019)[names(com_2019)=="V"]<-"no2"
com_2019$annee<-2019

com_2019_no2<-com_2019



#######base pm10 daily #####

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
#com_2009$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2009[,4:368])
#com_2009$annee<-2009

com_2009<-com_2009[-1,]
com_2009<-com_2009[-1,]

names(com_2009) <- sub('V', 'V_', names(com_2009))
com_2009<-reshape(com_2009, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2009), sep = "_")
names(com_2009)[names(com_2009)=="V"]<-"pm10"
com_2009$annee<-2009

com_2009_pm10<-com_2009




com_2010<-ConcCommunales_2010_PM10_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 45]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>45]<-1
#com_2010$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2010[,4:368])
#com_2010$annee<-2010

com_2010<-com_2010[-1,]
com_2010<-com_2010[-1,]

names(com_2010) <- sub('V', 'V_', names(com_2010))
com_2010<-reshape(com_2010, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2010), sep = "_")
names(com_2010)[names(com_2010)=="V"]<-"pm10"
com_2010$annee<-2010

com_2010_pm10<-com_2010




com_2011<-ConcCommunales_2011_PM10_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 45]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>45]<-1
#com_2011$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2011[,4:368])
#com_2011$annee<-2011

com_2011<-com_2011[-1,]
com_2011<-com_2011[-1,]

names(com_2011) <- sub('V', 'V_', names(com_2011))
com_2011<-reshape(com_2011, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2011), sep = "_")
names(com_2011)[names(com_2011)=="V"]<-"pm10"
com_2011$annee<-2011

com_2011_pm10<-com_2011





com_2012<-ConcCommunales_2012_PM10_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 45]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>45]<-1
#com_2012$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2012[,4:368])
#com_2012$annee<-2012

com_2012<-com_2012[-1,]
com_2012<-com_2012[-1,]

names(com_2012) <- sub('V', 'V_', names(com_2012))
com_2012<-com_2012[,-369]

com_2012<-reshape(com_2012, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2012), sep = "_")
names(com_2012)[names(com_2012)=="V"]<-"pm10"
com_2012$annee<-2012

com_2012_pm10<-com_2012





com_2013<-ConcCommunales_2013_PM10_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 45]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>45]<-1
#com_2013$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2013[,4:368])
#com_2013$annee<-2013

com_2013<-com_2013[-1,]
com_2013<-com_2013[-1,]

names(com_2013) <- sub('V', 'V_', names(com_2013))
com_2013<-reshape(com_2013, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2013), sep = "_")
names(com_2013)[names(com_2013)=="V"]<-"pm10"
com_2013$annee<-2013

com_2013_pm10<-com_2013





com_2014<-ConcCommunales_2014_PM10_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 45]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>45]<-1
#com_2014$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2014[,4:368])
#com_2014$annee<-2014

com_2014<-com_2014[-1,]
com_2014<-com_2014[-1,]

names(com_2014) <- sub('V', 'V_', names(com_2014))
com_2014<-reshape(com_2014, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2014), sep = "_")
names(com_2014)[names(com_2014)=="V"]<-"pm10"
com_2014$annee<-2014

com_2014_pm10<-com_2014





com_2015<-ConcCommunales_2015_PM10_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 45]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>45]<-1
#com_2015$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2015[,4:368])
#com_2015$annee<-2015

com_2015<-com_2015[-1,]
com_2015<-com_2015[-1,]

names(com_2015) <- sub('V', 'V_', names(com_2015))
com_2015<-reshape(com_2015, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2015), sep = "_")
names(com_2015)[names(com_2015)=="V"]<-"pm10"
com_2015$annee<-2015

com_2015_pm10<-com_2015





com_2016<-ConcCommunales_2016_PM10_daymean
com_2016[3:35230,4:368][com_2016[3:35230,4:368] <= 45]<-0
com_2016[3:35230,4:368][com_2016[3:35230,4:368]>45]<-1
#com_2016$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2016[,4:368])
#com_2016$annee<-2016

com_2016<-com_2016[-1,]
com_2016<-com_2016[-1,]

names(com_2016) <- sub('V', 'V_', names(com_2016))
com_2016<-com_2016[,-369]

com_2016<-reshape(com_2016, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2016), sep = "_")
names(com_2016)[names(com_2016)=="V"]<-"pm10"
com_2016$annee<-2016

com_2016_pm10<-com_2016





com_2017<-ConcCommunales_2017_PM10_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 45]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>45]<-1
#com_2017$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2017[,4:368])
#com_2017$annee<-2017

com_2017<-com_2017[-1,]
com_2017<-com_2017[-1,]

names(com_2017) <- sub('V', 'V_', names(com_2017))
com_2017<-reshape(com_2017, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2017), sep = "_")
names(com_2017)[names(com_2017)=="V"]<-"pm10"
com_2017$annee<-2017

com_2017_pm10<-com_2017



com_2018<-ConcCommunales_2018_PM10_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 45]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>45]<-1
#com_2018$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2018[,4:368])
#com_2018$annee<-2018

com_2018<-com_2018[-1,]
com_2018<-com_2018[-1,]

names(com_2018) <- sub('V', 'V_', names(com_2018))
com_2018<-reshape(com_2018, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2018), sep = "_")
names(com_2018)[names(com_2018)=="V"]<-"pm10"
com_2018$annee<-2018

com_2018_pm10<-com_2018





com_2019<-ConcCommunales_2019_PM10_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 45]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>45]<-1
#com_2019$nombre_jours_au_dessus_seuils_pm10<-rowSums(com_2019[,4:368])
#com_2019$annee<-2019

com_2019<-com_2019[-1,]
com_2019<-com_2019[-1,]

names(com_2019) <- sub('V', 'V_', names(com_2019))
com_2019<-reshape(com_2019, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2019), sep = "_")
names(com_2019)[names(com_2019)=="V"]<-"pm10"
com_2019$annee<-2019

com_2019_pm10<-com_2019





####### base pm2.5 daily #####

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
#com_2009$nombre_jours_au_dessus_seuils<-rowSums(com_2009[,4:368])
#com_2009$annee<-2009

com_2009<-com_2009[-1,]
com_2009<-com_2009[-1,]

names(com_2009) <- sub('V', 'V_', names(com_2009))
com_2009<-reshape(com_2009, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2009), sep = "_")
names(com_2009)[names(com_2009)=="V"]<-"pm25"
com_2009$annee<-2009

com_2009_pm25<-com_2009




com_2010<-ConcCommunales_2010_PM25_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 15]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>15]<-1
#com_2010$nombre_jours_au_dessus_seuils<-rowSums(com_2010[,4:368])
#com_2010$annee<-2010

com_2010<-com_2010[-1,]
com_2010<-com_2010[-1,]

names(com_2010) <- sub('V', 'V_', names(com_2010))
com_2010<-reshape(com_2010, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2010), sep = "_")
names(com_2010)[names(com_2010)=="V"]<-"pm25"
com_2010$annee<-2010

com_2010_pm25<-com_2010




com_2011<-ConcCommunales_2011_PM25_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 15]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>15]<-1
#com_2011$nombre_jours_au_dessus_seuils<-rowSums(com_2011[,4:368])
#com_2011$annee<-2011

com_2011<-com_2011[-1,]
com_2011<-com_2011[-1,]

names(com_2011) <- sub('V', 'V_', names(com_2011))
com_2011<-reshape(com_2011, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2011), sep = "_")
names(com_2011)[names(com_2011)=="V"]<-"pm25"
com_2011$annee<-2011

com_2011_pm25<-com_2011




com_2012<-ConcCommunales_2012_PM25_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 15]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>15]<-1
#com_2012$nombre_jours_au_dessus_seuils<-rowSums(com_2012[,4:368])
#com_2012$annee<-2012

com_2012<-com_2012[-1,]
com_2012<-com_2012[-1,]

names(com_2012) <- sub('V', 'V_', names(com_2012))
com_2012<-com_2012[,-369]

com_2012<-reshape(com_2012, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2012), sep = "_")
names(com_2012)[names(com_2012)=="V"]<-"pm25"
com_2012$annee<-2012

com_2012_pm25<-com_2012




com_2013<-ConcCommunales_2013_PM25_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 15]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>15]<-1
#com_2013$nombre_jours_au_dessus_seuils<-rowSums(com_2013[,4:368])
#com_2013$annee<-2013

com_2013<-com_2013[-1,]
com_2013<-com_2013[-1,]

names(com_2013) <- sub('V', 'V_', names(com_2013))
com_2013<-reshape(com_2013, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2013), sep = "_")
names(com_2013)[names(com_2013)=="V"]<-"pm25"
com_2013$annee<-2013

com_2013_pm25<-com_2013




com_2014<-ConcCommunales_2014_PM25_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 15]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>15]<-1
#com_2014$nombre_jours_au_dessus_seuils<-rowSums(com_2014[,4:368])
#com_2014$annee<-2014

com_2014<-com_2014[-1,]
com_2014<-com_2014[-1,]

names(com_2014) <- sub('V', 'V_', names(com_2014))
com_2014<-reshape(com_2014, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2014), sep = "_")
names(com_2014)[names(com_2014)=="V"]<-"pm25"
com_2014$annee<-2014

com_2014_pm25<-com_2014




com_2015<-ConcCommunales_2015_PM25_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 15]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>15]<-1
#com_2015$nombre_jours_au_dessus_seuils<-rowSums(com_2015[,4:368])
#com_2015$annee<-2015

com_2015<-com_2015[-1,]
com_2015<-com_2015[-1,]

names(com_2015) <- sub('V', 'V_', names(com_2015))
com_2015<-reshape(com_2015, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2015), sep = "_")
names(com_2015)[names(com_2015)=="V"]<-"pm25"
com_2015$annee<-2015

com_2015_pm25<-com_2015




com_2016<-ConcCommunales_2016_PM25_daymean
com_2016[3:35230,4:368][com_2016[3:35230,4:368] <= 15]<-0
com_2016[3:35230,4:368][com_2016[3:35230,4:368]>15]<-1
#com_2016$nombre_jours_au_dessus_seuils<-rowSums(com_2016[,4:368])
#com_2016$annee<-2016

com_2016<-com_2016[-1,]
com_2016<-com_2016[-1,]

names(com_2016) <- sub('V', 'V_', names(com_2016))
com_2016<-com_2016[,-369]

com_2016<-reshape(com_2016, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2016), sep = "_")
names(com_2016)[names(com_2016)=="V"]<-"pm25"
com_2016$annee<-2016

com_2016_pm25<-com_2016





com_2017<-ConcCommunales_2017_PM25_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 15]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>15]<-1
#com_2017$nombre_jours_au_dessus_seuils<-rowSums(com_2017[,4:368])
#com_2017$annee<-2017

com_2017<-com_2017[-1,]
com_2017<-com_2017[-1,]

names(com_2017) <- sub('V', 'V_', names(com_2017))
com_2017<-reshape(com_2017, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2017), sep = "_")
names(com_2017)[names(com_2017)=="V"]<-"pm25"
com_2017$annee<-2017

com_2017_pm25<-com_2017




com_2018<-ConcCommunales_2018_PM25_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 15]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>15]<-1
#com_2018$nombre_jours_au_dessus_seuils<-rowSums(com_2018[,4:368])
#com_2018$annee<-2018

com_2018<-com_2018[-1,]
com_2018<-com_2018[-1,]

names(com_2018) <- sub('V', 'V_', names(com_2018))
com_2018<-reshape(com_2018, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2018), sep = "_")
names(com_2018)[names(com_2018)=="V"]<-"pm25"
com_2018$annee<-2018

com_2018_pm25<-com_2018



com_2019<-ConcCommunales_2019_PM25_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 15]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>15]<-1
#com_2019$nombre_jours_au_dessus_seuils<-rowSums(com_2019[,4:368])
#com_2019$annee<-2019

com_2019<-com_2019[-1,]
com_2019<-com_2019[-1,]

names(com_2019) <- sub('V', 'V_', names(com_2019))
com_2019<-reshape(com_2019, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2019), sep = "_")
names(com_2019)[names(com_2019)=="V"]<-"pm25"
com_2019$annee<-2019

com_2019_pm25<-com_2019





######base o3 daily####

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
#prendre le maximum journalier de la moyenne glissante sur 8heures (prendre la moyenne la plus grande parmi les moyennes glissantes sur 8 heures)
#la moyenne max ne doit pas depasser 100 

com_2009<-ConcCommunales_2009_O3_daymean
com_2009[3:35230,4:368][com_2009[3:35230,4:368] <= 100 ]<-0
com_2009[3:35230,4:368][com_2009[3:35230,4:368]>100]<-1
#com_2009$nombre_jours_au_dessus_seuils<-rowSums(com_2009[,4:368])
#com_2009$annee<-2009


com_2009<-com_2009[-1,]
com_2009<-com_2009[-1,]

names(com_2009) <- sub('V', 'V_', names(com_2009))
com_2009<-reshape(com_2009, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2009), sep = "_")
names(com_2009)[names(com_2009)=="V"]<-"o3"
com_2009$annee<-2009

com_2009_o3<-com_2009






com_2010<-ConcCommunales_2010_O3_daymean
com_2010[3:35230,4:368][com_2010[3:35230,4:368] <= 100 ]<-0
com_2010[3:35230,4:368][com_2010[3:35230,4:368]>100]<-1
#com_2010$nombre_jours_au_dessus_seuils<-rowSums(com_2010[,4:368])
#com_2010$annee<-2010


com_2010<-com_2010[-1,]
com_2010<-com_2010[-1,]

names(com_2010) <- sub('V', 'V_', names(com_2010))
com_2010<-reshape(com_2010, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2010), sep = "_")
names(com_2010)[names(com_2010)=="V"]<-"o3"
com_2010$annee<-2010

com_2010_o3<-com_2010




com_2011<-ConcCommunales_2011_O3_daymean
com_2011[3:35230,4:368][com_2011[3:35230,4:368] <= 100 ]<-0
com_2011[3:35230,4:368][com_2011[3:35230,4:368]>100]<-1
#com_2011$nombre_jours_au_dessus_seuils<-rowSums(com_2011[,4:368])
#com_2011$annee<-2011


com_2011<-com_2011[-1,]
com_2011<-com_2011[-1,]

names(com_2011) <- sub('V', 'V_', names(com_2011))
com_2011<-reshape(com_2011, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2011), sep = "_")
names(com_2011)[names(com_2011)=="V"]<-"o3"
com_2011$annee<-2011

com_2011_o3<-com_2011




com_2012<-ConcCommunales_2012_O3_daymean
com_2012[3:35230,4:368][com_2012[3:35230,4:368] <= 100 ]<-0
com_2012[3:35230,4:368][com_2012[3:35230,4:368]>100]<-1
#com_2012$nombre_jours_au_dessus_seuils<-rowSums(com_2012[,4:368])
#com_2012$annee<-2012


com_2012<-com_2012[-1,]
com_2012<-com_2012[-1,]

names(com_2012) <- sub('V', 'V_', names(com_2012))
com_2012<-com_2012[,-369]

com_2012<-reshape(com_2012, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2012), sep = "_")
names(com_2012)[names(com_2012)=="V"]<-"o3"
com_2012$annee<-2012

com_2012_o3<-com_2012



com_2013<-ConcCommunales_2013_O3_daymean
com_2013[3:35230,4:368][com_2013[3:35230,4:368] <= 100 ]<-0
com_2013[3:35230,4:368][com_2013[3:35230,4:368]>100]<-1
#com_2013$nombre_jours_au_dessus_seuils<-rowSums(com_2013[,4:368])
#com_2013$annee<-2013


com_2013<-com_2013[-1,]
com_2013<-com_2013[-1,]

names(com_2013) <- sub('V', 'V_', names(com_2013))
com_2013<-reshape(com_2013, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2013), sep = "_")
names(com_2013)[names(com_2013)=="V"]<-"o3"
com_2013$annee<-2013

com_2013_o3<-com_2013





com_2014<-ConcCommunales_2014_O3_daymean
com_2014[3:35230,4:368][com_2014[3:35230,4:368] <= 100 ]<-0
com_2014[3:35230,4:368][com_2014[3:35230,4:368]>100]<-1
#com_2014$nombre_jours_au_dessus_seuils<-rowSums(com_2014[,4:368])
#com_2014$annee<-2014


com_2014<-com_2014[-1,]
com_2014<-com_2014[-1,]

names(com_2014) <- sub('V', 'V_', names(com_2014))
com_2014<-reshape(com_2014, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2014), sep = "_")
names(com_2014)[names(com_2014)=="V"]<-"o3"
com_2014$annee<-2014

com_2014_o3<-com_2014




com_2015<-ConcCommunales_2015_O3_daymean
com_2015[3:35230,4:368][com_2015[3:35230,4:368] <= 100 ]<-0
com_2015[3:35230,4:368][com_2015[3:35230,4:368]>100]<-1
#com_2015$nombre_jours_au_dessus_seuils<-rowSums(com_2015[,4:368])
#com_2015$annee<-2015


com_2015<-com_2015[-1,]
com_2015<-com_2015[-1,]

names(com_2015) <- sub('V', 'V_', names(com_2015))
com_2015<-reshape(com_2015, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2015), sep = "_")
names(com_2015)[names(com_2015)=="V"]<-"o3"
com_2015$annee<-2015

com_2015_o3<-com_2015





com_2016<-ConcCommunales_2016_O3_daymean
com_2016[3:35289,4:368][com_2016[3:35289,4:368] <= 100 ]<-0
com_2016[3:35289,4:368][com_2016[3:35289,4:368]>100]<-1


com_2016<-com_2016[-1,]
com_2016<-com_2016[-1,]

names(com_2016) <- sub('V', 'V_', names(com_2016))
com_2016<-com_2016[,-369]

com_2016<-reshape(com_2016, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2016), sep = "_")
names(com_2016)[names(com_2016)=="V"]<-"o3"
com_2016$annee<-2016

com_2016_o3<-com_2016

com_2016_o3<-com_2016_o3[,c("V_1","time","o3","annee")]




com_2017<-ConcCommunales_2017_O3_daymean
com_2017[3:35230,4:368][com_2017[3:35230,4:368] <= 100 ]<-0
com_2017[3:35230,4:368][com_2017[3:35230,4:368]>100]<-1
#com_2017$nombre_jours_au_dessus_seuils<-rowSums(com_2017[,4:368])
#com_2017$annee<-2017


com_2017<-com_2017[-1,]
com_2017<-com_2017[-1,]

names(com_2017) <- sub('V', 'V_', names(com_2017))
com_2017<-reshape(com_2017, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2017), sep = "_")
names(com_2017)[names(com_2017)=="V"]<-"o3"
com_2017$annee<-2017

com_2017_o3<-com_2017





com_2018<-ConcCommunales_2018_O3_daymean
com_2018[3:35230,4:368][com_2018[3:35230,4:368] <= 100 ]<-0
com_2018[3:35230,4:368][com_2018[3:35230,4:368]>100]<-1
#com_2018$nombre_jours_au_dessus_seuils<-rowSums(com_2018[,4:368])
#com_2018$annee<-2018


com_2018<-com_2018[-1,]
com_2018<-com_2018[-1,]

names(com_2018) <- sub('V', 'V_', names(com_2018))
com_2018<-reshape(com_2018, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2018), sep = "_")
names(com_2018)[names(com_2018)=="V"]<-"o3"
com_2018$annee<-2018

com_2018_o3<-com_2018





com_2019<-ConcCommunales_2019_O3_daymean
com_2019[3:35230,4:368][com_2019[3:35230,4:368] <= 100 ]<-0
com_2019[3:35230,4:368][com_2019[3:35230,4:368]>100]<-1
#com_2019$nombre_jours_au_dessus_seuils<-rowSums(com_2019[,4:368])
#com_2019$annee<-2019


com_2019<-com_2019[-1,]
com_2019<-com_2019[-1,]

names(com_2019) <- sub('V', 'V_', names(com_2019))
com_2019<-reshape(com_2019, direction = "long", idvar="V_1", 
                  varying = 4:ncol(com_2019), sep = "_")
names(com_2019)[names(com_2019)=="V"]<-"o3"
com_2019$annee<-2019

com_2019_o3<-com_2019








rm(ConcCommunales_2009_PM10_daymean,ConcCommunales_2010_PM10_daymean,ConcCommunales_2011_PM10_daymean,ConcCommunales_2012_PM10_daymean,ConcCommunales_2013_PM10_daymean,ConcCommunales_2014_PM10_daymean,ConcCommunales_2015_PM10_daymean,ConcCommunales_2016_PM10_daymean,ConcCommunales_2017_PM10_daymean,ConcCommunales_2018_PM10_daymean,ConcCommunales_2019_PM10_daymean)
rm(ConcCommunales_2009_PM25_daymean,ConcCommunales_2010_PM25_daymean,ConcCommunales_2011_PM25_daymean,ConcCommunales_2012_PM25_daymean,ConcCommunales_2013_PM25_daymean,ConcCommunales_2014_PM25_daymean,ConcCommunales_2015_PM25_daymean,ConcCommunales_2016_PM25_daymean,ConcCommunales_2017_PM25_daymean,ConcCommunales_2018_PM25_daymean,ConcCommunales_2019_PM25_daymean)
rm(ConcCommunales_2009_O3_daymean,ConcCommunales_2010_O3_daymean,ConcCommunales_2011_O3_daymean,ConcCommunales_2012_O3_daymean,ConcCommunales_2013_O3_daymean,ConcCommunales_2014_O3_daymean,ConcCommunales_2015_O3_daymean,ConcCommunales_2016_O3_daymean,ConcCommunales_2017_O3_daymean,ConcCommunales_2018_O3_daymean,ConcCommunales_2019_O3_daymean)
rm(ConcCommunales_2009_NO2_daymean,ConcCommunales_2010_NO2_daymean,ConcCommunales_2011_NO2_daymean,ConcCommunales_2012_NO2_daymean,ConcCommunales_2013_NO2_daymean,ConcCommunales_2014_NO2_daymean,ConcCommunales_2015_NO2_daymean,ConcCommunales_2016_NO2_daymean,ConcCommunales_2017_NO2_daymean,ConcCommunales_2018_NO2_daymean,ConcCommunales_2019_NO2_daymean)

gc()









library(dplyr)

com_2009_final<-left_join(com_2009_no2,com_2009_pm25)
com_2009_final<-left_join(com_2009_final,com_2009_pm10)
com_2009_final<-left_join(com_2009_final,com_2009_o3)


com_2009_final$no2_pm25_pm10<-rowSums(com_2009_final[,c(5,7,8)])
com_2009_final$no2_pm25_pm10[com_2009_final$no2_pm25_pm10<3]<-0
com_2009_final$no2_pm25_pm10[com_2009_final$no2_pm25_pm10==3]<-1

com_2009_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2009_final,sum)




com_2009_final$pm25_pm10<-rowSums(com_2009_final[,c(7,8)])
com_2009_final$pm25_pm10[com_2009_final$pm25_pm10<2]<-0
com_2009_final$pm25_pm10[com_2009_final$pm25_pm10==2]<-1

com_2009_final_ag_2<-aggregate(pm25_pm10~V_1,com_2009_final,sum)


#8 variables, 4 looking just above the thresholds and 4 also excluding other pollutants



com_2009_final$no2_pm25<-rowSums(com_2009_final[,c(5,7)])
com_2009_final$no2_pm25[com_2009_final$no2_pm25<2]<-0
com_2009_final$no2_pm25[com_2009_final$no2_pm25==2]<-1

com_2009_final_ag_3<-aggregate(no2_pm25~V_1,com_2009_final,sum)




com_2009_final$no2_pm25_pm10_o3<-rowSums(com_2009_final[,c(5,7,8,9)])
com_2009_final$no2_pm25_pm10_o3[com_2009_final$no2_pm25_pm10_o3<4]<-0
com_2009_final$no2_pm25_pm10_o3[com_2009_final$no2_pm25_pm10_o3==4]<-1

com_2009_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2009_final,sum)




com_2009_final$pm10_no2<-rowSums(com_2009_final[,c(5,8)])
com_2009_final$pm10_no2[com_2009_final$pm10_no2<2]<-0
com_2009_final$pm10_no2[com_2009_final$pm10_no2==2]<-1

com_2009_final_ag_4<-aggregate(pm10_no2~V_1,com_2009_final,sum)






com_2009_final$pm10_o3<-rowSums(com_2009_final[,c(9,8)])
com_2009_final$pm10_o3[com_2009_final$pm10_o3<2]<-0
com_2009_final$pm10_o3[com_2009_final$pm10_o3==2]<-1

com_2009_final_ag_5<-aggregate(pm10_o3~V_1,com_2009_final,sum)







com_2009_final$pm25_o3<-rowSums(com_2009_final[,c(7,9)])
com_2009_final$pm25_o3[com_2009_final$pm25_o3<2]<-0
com_2009_final$pm25_o3[com_2009_final$pm25_o3==2]<-1

com_2009_final_ag_6<-aggregate(pm25_o3~V_1,com_2009_final,sum)






com_2009_final$no2_o3<-rowSums(com_2009_final[,c(5,9)])
com_2009_final$no2_o3[com_2009_final$no2_o3<2]<-0
com_2009_final$no2_o3[com_2009_final$no2_o3==2]<-1

com_2009_final_ag_7<-aggregate(no2_o3~V_1,com_2009_final,sum)






com_2009_final$pm10_no2_o3<-rowSums(com_2009_final[,c(8,9,5)])
com_2009_final$pm10_no2_o3[com_2009_final$pm10_no2_o3<3]<-0
com_2009_final$pm10_no2_o3[com_2009_final$pm10_no2_o3==3]<-1

com_2009_final_8<-aggregate(pm10_no2_o3~V_1,com_2009_final,sum)





com_2009_final$pm25_no2_o3<-rowSums(com_2009_final[,c(7,9,5)])
com_2009_final$pm25_no2_o3[com_2009_final$pm25_no2_o3<3]<-0
com_2009_final$pm25_no2_o3[com_2009_final$pm25_no2_o3==3]<-1

com_2009_final_9<-aggregate(pm25_no2_o3~V_1,com_2009_final,sum)






com_2009_final$pm10_pm25_o3<-rowSums(com_2009_final[,c(7,9,8)])
com_2009_final$pm10_pm25_o3[com_2009_final$pm10_pm25_o3<3]<-0
com_2009_final$pm10_pm25_o3[com_2009_final$pm10_pm25_o3==3]<-1

com_2009_final_10<-aggregate(pm10_pm25_o3~V_1,com_2009_final,sum)





####

com_2009_final$no2_pm25_pm10_2<-rowSums(com_2009_final[,c(5,7,8)])
com_2009_final$no2_pm25_pm10_2[com_2009_final$no2_pm25_pm10_2<3 | com_2009_final$o3==1]<-0
com_2009_final$no2_pm25_pm10_2[com_2009_final$no2_pm25_pm10_2==3]<-1

com_2009_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2009_final,sum)




com_2009_final$pm25_pm10_2<-rowSums(com_2009_final[,c(7,8)])
com_2009_final$pm25_pm10_2[com_2009_final$pm25_pm10_2<2 | com_2009_final$o3==1 | com_2009_final$no2==1]<-0
com_2009_final$pm25_pm10_2[com_2009_final$pm25_pm10_2==2]<-1

com_2009_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2009_final,sum)



com_2009_final$no2_pm25_2<-rowSums(com_2009_final[,c(5,7)])
com_2009_final$no2_pm25_2[com_2009_final$no2_pm25_2<2 | com_2009_final$o3==1 | com_2009_final$pm10==1]<-0
com_2009_final$no2_pm25_2[com_2009_final$no2_pm25_2==2]<-1

com_2009_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2009_final,sum)












com_2009_final$pm10_pm25_o3_2<-rowSums(com_2009_final[,c(9,7,8)])
com_2009_final$pm10_pm25_o3_2[com_2009_final$pm10_pm25_o3_2<3 | com_2009_final$no2==1]<-0
com_2009_final$pm10_pm25_o3_2[com_2009_final$pm10_pm25_o3_2==3]<-1

com_2009_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2009_final,sum)



com_2009_final$pm25_no2_o3_2<-rowSums(com_2009_final[,c(5,7,9)])
com_2009_final$pm25_no2_o3_2[com_2009_final$pm25_no2_o3_2<3 | com_2009_final$pm10==1]<-0
com_2009_final$pm25_no2_o3_2[com_2009_final$pm25_no2_o3_2==3]<-1

com_2009_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2009_final,sum)


com_2009_final$pm25_2<-com_2009_final[,c(7)]
com_2009_final$pm25_2[com_2009_final$pm25_2<1 | com_2009_final$pm10==1 |com_2009_final$no2==1|com_2009_final$o3==1]<-0
com_2009_final$pm25_2[com_2009_final$pm25_2==1]<-1


com_2009_final_ag_17<-aggregate(pm25_2~V_1,com_2009_final,sum)



com_2009_final$pm10_2<-com_2009_final[,c(8)]
com_2009_final$pm10_2[com_2009_final$pm10_2<1 | com_2009_final$pm25==1 |com_2009_final$no2==1|com_2009_final$o3==1]<-0
com_2009_final$pm10_2[com_2009_final$pm10_2==1]<-1

com_2009_final_ag_18<-aggregate(pm10_2~V_1,com_2009_final,sum)



com_2009_final$no2_2<-com_2009_final[,c(5)]
com_2009_final$no2_2[com_2009_final$no2_2<1 | com_2009_final$pm25==1 |com_2009_final$pm10==1|com_2009_final$o3==1]<-0
com_2009_final$no2_2[com_2009_final$no2_2==1]<-1

com_2009_final_ag_19<-aggregate(no2_2~V_1,com_2009_final,sum)



com_2009_final$o3_2<-com_2009_final[,c(9)]
com_2009_final$o3_2[com_2009_final$o3_2<1 | com_2009_final$pm25==1 |com_2009_final$pm10==1|com_2009_final$no2==1]<-0
com_2009_final$o3_2[com_2009_final$o3_2==1]<-1

com_2009_final_ag_20<-aggregate(o3_2~V_1,com_2009_final,sum)



com_2009_final$pm25_o3_2<-rowSums(com_2009_final[,c(9,7)])
com_2009_final$pm25_o3_2[com_2009_final$pm25_o3_2<2 | com_2009_final$no2==1| com_2009_final$pm10==1]<-0
com_2009_final$pm25_o3_2[com_2009_final$pm25_o3_2==2]<-1

com_2009_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2009_final,sum)



com_2009_final$no2_pm10_2<-rowSums(com_2009_final[,c(5,8)])
com_2009_final$no2_pm10_2[com_2009_final$no2_pm10_2<2 | com_2009_final$pm25==1| com_2009_final$o3==1]<-0
com_2009_final$no2_pm10_2[com_2009_final$no2_pm10_2==2]<-1

com_2009_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2009_final,sum)


com_2009_final$no2_o3_2<-rowSums(com_2009_final[,c(5,9)])
com_2009_final$no2_o3_2[com_2009_final$no2_o3_2<2 | com_2009_final$pm25==1| com_2009_final$pm10==1]<-0
com_2009_final$no2_o3_2[com_2009_final$no2_o3_2==2]<-1

com_2009_final_ag_23<-aggregate(no2_o3_2~V_1,com_2009_final,sum)


com_2009_final$no2_pm10_o3_2<-rowSums(com_2009_final[,c(5,8,9)])
com_2009_final$no2_pm10_o3_2[com_2009_final$no2_pm10_o3_2<3 | com_2009_final$pm25==1]<-0
com_2009_final$no2_pm10_o3_2[com_2009_final$no2_pm10_o3_2==3]<-1


com_2009_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2009_final,sum)



com_2009_final$pm10_o3_2<-rowSums(com_2009_final[,c(8,9)])
com_2009_final$pm10_o3_2[com_2009_final$pm10_o3_2<2 | com_2009_final$pm25==1| com_2009_final$no2==1]<-0
com_2009_final$pm10_o3_2[com_2009_final$pm10_o3_2==2]<-1

com_2009_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2009_final,sum)
















############





library(dplyr)

com_2010_final<-left_join(com_2010_no2,com_2010_pm25)
com_2010_final<-left_join(com_2010_final,com_2010_pm10)
com_2010_final<-left_join(com_2010_final,com_2010_o3)


com_2010_final$no2_pm25_pm10<-rowSums(com_2010_final[,c(5,7,8)])
com_2010_final$no2_pm25_pm10[com_2010_final$no2_pm25_pm10<3]<-0
com_2010_final$no2_pm25_pm10[com_2010_final$no2_pm25_pm10==3]<-1

com_2010_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2010_final,sum)




com_2010_final$pm25_pm10<-rowSums(com_2010_final[,c(7,8)])
com_2010_final$pm25_pm10[com_2010_final$pm25_pm10<2]<-0
com_2010_final$pm25_pm10[com_2010_final$pm25_pm10==2]<-1

com_2010_final_ag_2<-aggregate(pm25_pm10~V_1,com_2010_final,sum)


#


com_2010_final$no2_pm25<-rowSums(com_2010_final[,c(5,7)])
com_2010_final$no2_pm25[com_2010_final$no2_pm25<2]<-0
com_2010_final$no2_pm25[com_2010_final$no2_pm25==2]<-1

com_2010_final_ag_3<-aggregate(no2_pm25~V_1,com_2010_final,sum)




com_2010_final$no2_pm25_pm10_o3<-rowSums(com_2010_final[,c(5,7,8,9)])
com_2010_final$no2_pm25_pm10_o3[com_2010_final$no2_pm25_pm10_o3<4]<-0
com_2010_final$no2_pm25_pm10_o3[com_2010_final$no2_pm25_pm10_o3==4]<-1

com_2010_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2010_final,sum)




com_2010_final$pm10_no2<-rowSums(com_2010_final[,c(5,8)])
com_2010_final$pm10_no2[com_2010_final$pm10_no2<2]<-0
com_2010_final$pm10_no2[com_2010_final$pm10_no2==2]<-1

com_2010_final_ag_4<-aggregate(pm10_no2~V_1,com_2010_final,sum)






com_2010_final$pm10_o3<-rowSums(com_2010_final[,c(9,8)])
com_2010_final$pm10_o3[com_2010_final$pm10_o3<2]<-0
com_2010_final$pm10_o3[com_2010_final$pm10_o3==2]<-1

com_2010_final_ag_5<-aggregate(pm10_o3~V_1,com_2010_final,sum)







com_2010_final$pm25_o3<-rowSums(com_2010_final[,c(7,9)])
com_2010_final$pm25_o3[com_2010_final$pm25_o3<2]<-0
com_2010_final$pm25_o3[com_2010_final$pm25_o3==2]<-1

com_2010_final_ag_6<-aggregate(pm25_o3~V_1,com_2010_final,sum)






com_2010_final$no2_o3<-rowSums(com_2010_final[,c(5,9)])
com_2010_final$no2_o3[com_2010_final$no2_o3<2]<-0
com_2010_final$no2_o3[com_2010_final$no2_o3==2]<-1

com_2010_final_ag_7<-aggregate(no2_o3~V_1,com_2010_final,sum)






com_2010_final$pm10_no2_o3<-rowSums(com_2010_final[,c(8,9,5)])
com_2010_final$pm10_no2_o3[com_2010_final$pm10_no2_o3<3]<-0
com_2010_final$pm10_no2_o3[com_2010_final$pm10_no2_o3==3]<-1

com_2010_final_8<-aggregate(pm10_no2_o3~V_1,com_2010_final,sum)





com_2010_final$pm25_no2_o3<-rowSums(com_2010_final[,c(7,9,5)])
com_2010_final$pm25_no2_o3[com_2010_final$pm25_no2_o3<3]<-0
com_2010_final$pm25_no2_o3[com_2010_final$pm25_no2_o3==3]<-1

com_2010_final_9<-aggregate(pm25_no2_o3~V_1,com_2010_final,sum)






com_2010_final$pm10_pm25_o3<-rowSums(com_2010_final[,c(7,9,8)])
com_2010_final$pm10_pm25_o3[com_2010_final$pm10_pm25_o3<3]<-0
com_2010_final$pm10_pm25_o3[com_2010_final$pm10_pm25_o3==3]<-1

com_2010_final_10<-aggregate(pm10_pm25_o3~V_1,com_2010_final,sum)





####

com_2010_final$no2_pm25_pm10_2<-rowSums(com_2010_final[,c(5,7,8)])
com_2010_final$no2_pm25_pm10_2[com_2010_final$no2_pm25_pm10_2<3 | com_2010_final$o3==1]<-0
com_2010_final$no2_pm25_pm10_2[com_2010_final$no2_pm25_pm10_2==3]<-1

com_2010_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2010_final,sum)




com_2010_final$pm25_pm10_2<-rowSums(com_2010_final[,c(7,8)])
com_2010_final$pm25_pm10_2[com_2010_final$pm25_pm10_2<2 | com_2010_final$o3==1 | com_2010_final$no2==1]<-0
com_2010_final$pm25_pm10_2[com_2010_final$pm25_pm10_2==2]<-1

com_2010_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2010_final,sum)



com_2010_final$no2_pm25_2<-rowSums(com_2010_final[,c(5,7)])
com_2010_final$no2_pm25_2[com_2010_final$no2_pm25_2<2 | com_2010_final$o3==1 | com_2010_final$pm10==1]<-0
com_2010_final$no2_pm25_2[com_2010_final$no2_pm25_2==2]<-1

com_2010_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2010_final,sum)











com_2010_final$pm10_pm25_o3_2<-rowSums(com_2010_final[,c(9,7,8)])
com_2010_final$pm10_pm25_o3_2[com_2010_final$pm10_pm25_o3_2<3 | com_2010_final$no2==1]<-0
com_2010_final$pm10_pm25_o3_2[com_2010_final$pm10_pm25_o3_2==3]<-1

com_2010_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2010_final,sum)



com_2010_final$pm25_no2_o3_2<-rowSums(com_2010_final[,c(5,7,9)])
com_2010_final$pm25_no2_o3_2[com_2010_final$pm25_no2_o3_2<3 | com_2010_final$pm10==1]<-0
com_2010_final$pm25_no2_o3_2[com_2010_final$pm25_no2_o3_2==3]<-1

com_2010_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2010_final,sum)


com_2010_final$pm25_2<-com_2010_final[,c(7)]
com_2010_final$pm25_2[com_2010_final$pm25_2<1 | com_2010_final$pm10==1 |com_2010_final$no2==1|com_2010_final$o3==1]<-0
com_2010_final$pm25_2[com_2010_final$pm25_2==1]<-1


com_2010_final_ag_17<-aggregate(pm25_2~V_1,com_2010_final,sum)



com_2010_final$pm10_2<-com_2010_final[,c(8)]
com_2010_final$pm10_2[com_2010_final$pm10_2<1 | com_2010_final$pm25==1 |com_2010_final$no2==1|com_2010_final$o3==1]<-0
com_2010_final$pm10_2[com_2010_final$pm10_2==1]<-1

com_2010_final_ag_18<-aggregate(pm10_2~V_1,com_2010_final,sum)



com_2010_final$no2_2<-com_2010_final[,c(5)]
com_2010_final$no2_2[com_2010_final$no2_2<1 | com_2010_final$pm25==1 |com_2010_final$pm10==1|com_2010_final$o3==1]<-0
com_2010_final$no2_2[com_2010_final$no2_2==1]<-1

com_2010_final_ag_19<-aggregate(no2_2~V_1,com_2010_final,sum)



com_2010_final$o3_2<-com_2010_final[,c(9)]
com_2010_final$o3_2[com_2010_final$o3_2<1 | com_2010_final$pm25==1 |com_2010_final$pm10==1|com_2010_final$no2==1]<-0
com_2010_final$o3_2[com_2010_final$o3_2==1]<-1

com_2010_final_ag_20<-aggregate(o3_2~V_1,com_2010_final,sum)



com_2010_final$pm25_o3_2<-rowSums(com_2010_final[,c(9,7)])
com_2010_final$pm25_o3_2[com_2010_final$pm25_o3_2<2 | com_2010_final$no2==1| com_2010_final$pm10==1]<-0
com_2010_final$pm25_o3_2[com_2010_final$pm25_o3_2==2]<-1

com_2010_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2010_final,sum)



com_2010_final$no2_pm10_2<-rowSums(com_2010_final[,c(5,8)])
com_2010_final$no2_pm10_2[com_2010_final$no2_pm10_2<2 | com_2010_final$pm25==1| com_2010_final$o3==1]<-0
com_2010_final$no2_pm10_2[com_2010_final$no2_pm10_2==2]<-1

com_2010_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2010_final,sum)


com_2010_final$no2_o3_2<-rowSums(com_2010_final[,c(5,9)])
com_2010_final$no2_o3_2[com_2010_final$no2_o3_2<2 | com_2010_final$pm25==1| com_2010_final$pm10==1]<-0
com_2010_final$no2_o3_2[com_2010_final$no2_o3_2==2]<-1

com_2010_final_ag_23<-aggregate(no2_o3_2~V_1,com_2010_final,sum)


com_2010_final$no2_pm10_o3_2<-rowSums(com_2010_final[,c(5,8,9)])
com_2010_final$no2_pm10_o3_2[com_2010_final$no2_pm10_o3_2<3 | com_2010_final$pm25==1]<-0
com_2010_final$no2_pm10_o3_2[com_2010_final$no2_pm10_o3_2==3]<-1


com_2010_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2010_final,sum)



com_2010_final$pm10_o3_2<-rowSums(com_2010_final[,c(8,9)])
com_2010_final$pm10_o3_2[com_2010_final$pm10_o3_2<2 | com_2010_final$pm25==1| com_2010_final$no2==1]<-0
com_2010_final$pm10_o3_2[com_2010_final$pm10_o3_2==2]<-1

com_2010_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2010_final,sum)










###################





library(dplyr)

com_2011_final<-left_join(com_2011_no2,com_2011_pm25)
com_2011_final<-left_join(com_2011_final,com_2011_pm10)
com_2011_final<-left_join(com_2011_final,com_2011_o3)


com_2011_final$no2_pm25_pm10<-rowSums(com_2011_final[,c(5,7,8)])
com_2011_final$no2_pm25_pm10[com_2011_final$no2_pm25_pm10<3]<-0
com_2011_final$no2_pm25_pm10[com_2011_final$no2_pm25_pm10==3]<-1

com_2011_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2011_final,sum)




com_2011_final$pm25_pm10<-rowSums(com_2011_final[,c(7,8)])
com_2011_final$pm25_pm10[com_2011_final$pm25_pm10<2]<-0
com_2011_final$pm25_pm10[com_2011_final$pm25_pm10==2]<-1

com_2011_final_ag_2<-aggregate(pm25_pm10~V_1,com_2011_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2011_final$no2_pm25<-rowSums(com_2011_final[,c(5,7)])
com_2011_final$no2_pm25[com_2011_final$no2_pm25<2]<-0
com_2011_final$no2_pm25[com_2011_final$no2_pm25==2]<-1

com_2011_final_ag_3<-aggregate(no2_pm25~V_1,com_2011_final,sum)




com_2011_final$no2_pm25_pm10_o3<-rowSums(com_2011_final[,c(5,7,8,9)])
com_2011_final$no2_pm25_pm10_o3[com_2011_final$no2_pm25_pm10_o3<4]<-0
com_2011_final$no2_pm25_pm10_o3[com_2011_final$no2_pm25_pm10_o3==4]<-1

com_2011_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2011_final,sum)




com_2011_final$pm10_no2<-rowSums(com_2011_final[,c(5,8)])
com_2011_final$pm10_no2[com_2011_final$pm10_no2<2]<-0
com_2011_final$pm10_no2[com_2011_final$pm10_no2==2]<-1

com_2011_final_ag_4<-aggregate(pm10_no2~V_1,com_2011_final,sum)






com_2011_final$pm10_o3<-rowSums(com_2011_final[,c(9,8)])
com_2011_final$pm10_o3[com_2011_final$pm10_o3<2]<-0
com_2011_final$pm10_o3[com_2011_final$pm10_o3==2]<-1

com_2011_final_ag_5<-aggregate(pm10_o3~V_1,com_2011_final,sum)







com_2011_final$pm25_o3<-rowSums(com_2011_final[,c(7,9)])
com_2011_final$pm25_o3[com_2011_final$pm25_o3<2]<-0
com_2011_final$pm25_o3[com_2011_final$pm25_o3==2]<-1

com_2011_final_ag_6<-aggregate(pm25_o3~V_1,com_2011_final,sum)






com_2011_final$no2_o3<-rowSums(com_2011_final[,c(5,9)])
com_2011_final$no2_o3[com_2011_final$no2_o3<2]<-0
com_2011_final$no2_o3[com_2011_final$no2_o3==2]<-1

com_2011_final_ag_7<-aggregate(no2_o3~V_1,com_2011_final,sum)






com_2011_final$pm10_no2_o3<-rowSums(com_2011_final[,c(8,9,5)])
com_2011_final$pm10_no2_o3[com_2011_final$pm10_no2_o3<3]<-0
com_2011_final$pm10_no2_o3[com_2011_final$pm10_no2_o3==3]<-1

com_2011_final_8<-aggregate(pm10_no2_o3~V_1,com_2011_final,sum)





com_2011_final$pm25_no2_o3<-rowSums(com_2011_final[,c(7,9,5)])
com_2011_final$pm25_no2_o3[com_2011_final$pm25_no2_o3<3]<-0
com_2011_final$pm25_no2_o3[com_2011_final$pm25_no2_o3==3]<-1

com_2011_final_9<-aggregate(pm25_no2_o3~V_1,com_2011_final,sum)






com_2011_final$pm10_pm25_o3<-rowSums(com_2011_final[,c(7,9,8)])
com_2011_final$pm10_pm25_o3[com_2011_final$pm10_pm25_o3<3]<-0
com_2011_final$pm10_pm25_o3[com_2011_final$pm10_pm25_o3==3]<-1

com_2011_final_10<-aggregate(pm10_pm25_o3~V_1,com_2011_final,sum)





####

com_2011_final$no2_pm25_pm10_2<-rowSums(com_2011_final[,c(5,7,8)])
com_2011_final$no2_pm25_pm10_2[com_2011_final$no2_pm25_pm10_2<3 | com_2011_final$o3==1]<-0
com_2011_final$no2_pm25_pm10_2[com_2011_final$no2_pm25_pm10_2==3]<-1

com_2011_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2011_final,sum)




com_2011_final$pm25_pm10_2<-rowSums(com_2011_final[,c(7,8)])
com_2011_final$pm25_pm10_2[com_2011_final$pm25_pm10_2<2 | com_2011_final$o3==1 | com_2011_final$no2==1]<-0
com_2011_final$pm25_pm10_2[com_2011_final$pm25_pm10_2==2]<-1

com_2011_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2011_final,sum)



com_2011_final$no2_pm25_2<-rowSums(com_2011_final[,c(5,7)])
com_2011_final$no2_pm25_2[com_2011_final$no2_pm25_2<2 | com_2011_final$o3==1 | com_2011_final$pm10==1]<-0
com_2011_final$no2_pm25_2[com_2011_final$no2_pm25_2==2]<-1

com_2011_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2011_final,sum)










com_2011_final$pm10_pm25_o3_2<-rowSums(com_2011_final[,c(9,7,8)])
com_2011_final$pm10_pm25_o3_2[com_2011_final$pm10_pm25_o3_2<3 | com_2011_final$no2==1]<-0
com_2011_final$pm10_pm25_o3_2[com_2011_final$pm10_pm25_o3_2==3]<-1

com_2011_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2011_final,sum)



com_2011_final$pm25_no2_o3_2<-rowSums(com_2011_final[,c(5,7,9)])
com_2011_final$pm25_no2_o3_2[com_2011_final$pm25_no2_o3_2<3 | com_2011_final$pm10==1]<-0
com_2011_final$pm25_no2_o3_2[com_2011_final$pm25_no2_o3_2==3]<-1

com_2011_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2011_final,sum)


com_2011_final$pm25_2<-com_2011_final[,c(7)]
com_2011_final$pm25_2[com_2011_final$pm25_2<1 | com_2011_final$pm10==1 |com_2011_final$no2==1|com_2011_final$o3==1]<-0
com_2011_final$pm25_2[com_2011_final$pm25_2==1]<-1


com_2011_final_ag_17<-aggregate(pm25_2~V_1,com_2011_final,sum)



com_2011_final$pm10_2<-com_2011_final[,c(8)]
com_2011_final$pm10_2[com_2011_final$pm10_2<1 | com_2011_final$pm25==1 |com_2011_final$no2==1|com_2011_final$o3==1]<-0
com_2011_final$pm10_2[com_2011_final$pm10_2==1]<-1

com_2011_final_ag_18<-aggregate(pm10_2~V_1,com_2011_final,sum)



com_2011_final$no2_2<-com_2011_final[,c(5)]
com_2011_final$no2_2[com_2011_final$no2_2<1 | com_2011_final$pm25==1 |com_2011_final$pm10==1|com_2011_final$o3==1]<-0
com_2011_final$no2_2[com_2011_final$no2_2==1]<-1

com_2011_final_ag_19<-aggregate(no2_2~V_1,com_2011_final,sum)



com_2011_final$o3_2<-com_2011_final[,c(9)]
com_2011_final$o3_2[com_2011_final$o3_2<1 | com_2011_final$pm25==1 |com_2011_final$pm10==1|com_2011_final$no2==1]<-0
com_2011_final$o3_2[com_2011_final$o3_2==1]<-1

com_2011_final_ag_20<-aggregate(o3_2~V_1,com_2011_final,sum)



com_2011_final$pm25_o3_2<-rowSums(com_2011_final[,c(9,7)])
com_2011_final$pm25_o3_2[com_2011_final$pm25_o3_2<2 | com_2011_final$no2==1| com_2011_final$pm10==1]<-0
com_2011_final$pm25_o3_2[com_2011_final$pm25_o3_2==2]<-1

com_2011_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2011_final,sum)



com_2011_final$no2_pm10_2<-rowSums(com_2011_final[,c(5,8)])
com_2011_final$no2_pm10_2[com_2011_final$no2_pm10_2<2 | com_2011_final$pm25==1| com_2011_final$o3==1]<-0
com_2011_final$no2_pm10_2[com_2011_final$no2_pm10_2==2]<-1

com_2011_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2011_final,sum)


com_2011_final$no2_o3_2<-rowSums(com_2011_final[,c(5,9)])
com_2011_final$no2_o3_2[com_2011_final$no2_o3_2<2 | com_2011_final$pm25==1| com_2011_final$pm10==1]<-0
com_2011_final$no2_o3_2[com_2011_final$no2_o3_2==2]<-1

com_2011_final_ag_23<-aggregate(no2_o3_2~V_1,com_2011_final,sum)


com_2011_final$no2_pm10_o3_2<-rowSums(com_2011_final[,c(5,8,9)])
com_2011_final$no2_pm10_o3_2[com_2011_final$no2_pm10_o3_2<3 | com_2011_final$pm25==1]<-0
com_2011_final$no2_pm10_o3_2[com_2011_final$no2_pm10_o3_2==3]<-1


com_2011_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2011_final,sum)



com_2011_final$pm10_o3_2<-rowSums(com_2011_final[,c(8,9)])
com_2011_final$pm10_o3_2[com_2011_final$pm10_o3_2<2 | com_2011_final$pm25==1| com_2011_final$no2==1]<-0
com_2011_final$pm10_o3_2[com_2011_final$pm10_o3_2==2]<-1

com_2011_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2011_final,sum)







##############










library(dplyr)

com_2012_final<-left_join(com_2012_no2,com_2012_pm25)
com_2012_final<-left_join(com_2012_final,com_2012_pm10)
com_2012_final<-left_join(com_2012_final,com_2012_o3)


com_2012_final$no2_pm25_pm10<-rowSums(com_2012_final[,c(5,7,8)])
com_2012_final$no2_pm25_pm10[com_2012_final$no2_pm25_pm10<3]<-0
com_2012_final$no2_pm25_pm10[com_2012_final$no2_pm25_pm10==3]<-1

com_2012_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2012_final,sum)




com_2012_final$pm25_pm10<-rowSums(com_2012_final[,c(7,8)])
com_2012_final$pm25_pm10[com_2012_final$pm25_pm10<2]<-0
com_2012_final$pm25_pm10[com_2012_final$pm25_pm10==2]<-1

com_2012_final_ag_2<-aggregate(pm25_pm10~V_1,com_2012_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2012_final$no2_pm25<-rowSums(com_2012_final[,c(5,7)])
com_2012_final$no2_pm25[com_2012_final$no2_pm25<2]<-0
com_2012_final$no2_pm25[com_2012_final$no2_pm25==2]<-1

com_2012_final_ag_3<-aggregate(no2_pm25~V_1,com_2012_final,sum)




com_2012_final$no2_pm25_pm10_o3<-rowSums(com_2012_final[,c(5,7,8,9)])
com_2012_final$no2_pm25_pm10_o3[com_2012_final$no2_pm25_pm10_o3<4]<-0
com_2012_final$no2_pm25_pm10_o3[com_2012_final$no2_pm25_pm10_o3==4]<-1

com_2012_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2012_final,sum)




com_2012_final$pm10_no2<-rowSums(com_2012_final[,c(5,8)])
com_2012_final$pm10_no2[com_2012_final$pm10_no2<2]<-0
com_2012_final$pm10_no2[com_2012_final$pm10_no2==2]<-1

com_2012_final_ag_4<-aggregate(pm10_no2~V_1,com_2012_final,sum)






com_2012_final$pm10_o3<-rowSums(com_2012_final[,c(9,8)])
com_2012_final$pm10_o3[com_2012_final$pm10_o3<2]<-0
com_2012_final$pm10_o3[com_2012_final$pm10_o3==2]<-1

com_2012_final_ag_5<-aggregate(pm10_o3~V_1,com_2012_final,sum)







com_2012_final$pm25_o3<-rowSums(com_2012_final[,c(7,9)])
com_2012_final$pm25_o3[com_2012_final$pm25_o3<2]<-0
com_2012_final$pm25_o3[com_2012_final$pm25_o3==2]<-1

com_2012_final_ag_6<-aggregate(pm25_o3~V_1,com_2012_final,sum)






com_2012_final$no2_o3<-rowSums(com_2012_final[,c(5,9)])
com_2012_final$no2_o3[com_2012_final$no2_o3<2]<-0
com_2012_final$no2_o3[com_2012_final$no2_o3==2]<-1

com_2012_final_ag_7<-aggregate(no2_o3~V_1,com_2012_final,sum)






com_2012_final$pm10_no2_o3<-rowSums(com_2012_final[,c(8,9,5)])
com_2012_final$pm10_no2_o3[com_2012_final$pm10_no2_o3<3]<-0
com_2012_final$pm10_no2_o3[com_2012_final$pm10_no2_o3==3]<-1

com_2012_final_8<-aggregate(pm10_no2_o3~V_1,com_2012_final,sum)





com_2012_final$pm25_no2_o3<-rowSums(com_2012_final[,c(7,9,5)])
com_2012_final$pm25_no2_o3[com_2012_final$pm25_no2_o3<3]<-0
com_2012_final$pm25_no2_o3[com_2012_final$pm25_no2_o3==3]<-1

com_2012_final_9<-aggregate(pm25_no2_o3~V_1,com_2012_final,sum)






com_2012_final$pm10_pm25_o3<-rowSums(com_2012_final[,c(7,9,8)])
com_2012_final$pm10_pm25_o3[com_2012_final$pm10_pm25_o3<3]<-0
com_2012_final$pm10_pm25_o3[com_2012_final$pm10_pm25_o3==3]<-1

com_2012_final_10<-aggregate(pm10_pm25_o3~V_1,com_2012_final,sum)





####

com_2012_final$no2_pm25_pm10_2<-rowSums(com_2012_final[,c(5,7,8)])
com_2012_final$no2_pm25_pm10_2[com_2012_final$no2_pm25_pm10_2<3 | com_2012_final$o3==1]<-0
com_2012_final$no2_pm25_pm10_2[com_2012_final$no2_pm25_pm10_2==3]<-1

com_2012_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2012_final,sum)




com_2012_final$pm25_pm10_2<-rowSums(com_2012_final[,c(7,8)])
com_2012_final$pm25_pm10_2[com_2012_final$pm25_pm10_2<2 | com_2012_final$o3==1 | com_2012_final$no2==1]<-0
com_2012_final$pm25_pm10_2[com_2012_final$pm25_pm10_2==2]<-1

com_2012_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2012_final,sum)



com_2012_final$no2_pm25_2<-rowSums(com_2012_final[,c(5,7)])
com_2012_final$no2_pm25_2[com_2012_final$no2_pm25_2<2 | com_2012_final$o3==1 | com_2012_final$pm10==1]<-0
com_2012_final$no2_pm25_2[com_2012_final$no2_pm25_2==2]<-1

com_2012_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2012_final,sum)








com_2012_final$pm10_pm25_o3_2<-rowSums(com_2012_final[,c(9,7,8)])
com_2012_final$pm10_pm25_o3_2[com_2012_final$pm10_pm25_o3_2<3 | com_2012_final$no2==1]<-0
com_2012_final$pm10_pm25_o3_2[com_2012_final$pm10_pm25_o3_2==3]<-1

com_2012_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2012_final,sum)



com_2012_final$pm25_no2_o3_2<-rowSums(com_2012_final[,c(5,7,9)])
com_2012_final$pm25_no2_o3_2[com_2012_final$pm25_no2_o3_2<3 | com_2012_final$pm10==1]<-0
com_2012_final$pm25_no2_o3_2[com_2012_final$pm25_no2_o3_2==3]<-1

com_2012_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2012_final,sum)


com_2012_final$pm25_2<-com_2012_final[,c(7)]
com_2012_final$pm25_2[com_2012_final$pm25_2<1 | com_2012_final$pm10==1 |com_2012_final$no2==1|com_2012_final$o3==1]<-0
com_2012_final$pm25_2[com_2012_final$pm25_2==1]<-1


com_2012_final_ag_17<-aggregate(pm25_2~V_1,com_2012_final,sum)



com_2012_final$pm10_2<-com_2012_final[,c(8)]
com_2012_final$pm10_2[com_2012_final$pm10_2<1 | com_2012_final$pm25==1 |com_2012_final$no2==1|com_2012_final$o3==1]<-0
com_2012_final$pm10_2[com_2012_final$pm10_2==1]<-1

com_2012_final_ag_18<-aggregate(pm10_2~V_1,com_2012_final,sum)



com_2012_final$no2_2<-com_2012_final[,c(5)]
com_2012_final$no2_2[com_2012_final$no2_2<1 | com_2012_final$pm25==1 |com_2012_final$pm10==1|com_2012_final$o3==1]<-0
com_2012_final$no2_2[com_2012_final$no2_2==1]<-1

com_2012_final_ag_19<-aggregate(no2_2~V_1,com_2012_final,sum)



com_2012_final$o3_2<-com_2012_final[,c(9)]
com_2012_final$o3_2[com_2012_final$o3_2<1 | com_2012_final$pm25==1 |com_2012_final$pm10==1|com_2012_final$no2==1]<-0
com_2012_final$o3_2[com_2012_final$o3_2==1]<-1

com_2012_final_ag_20<-aggregate(o3_2~V_1,com_2012_final,sum)



com_2012_final$pm25_o3_2<-rowSums(com_2012_final[,c(9,7)])
com_2012_final$pm25_o3_2[com_2012_final$pm25_o3_2<2 | com_2012_final$no2==1| com_2012_final$pm10==1]<-0
com_2012_final$pm25_o3_2[com_2012_final$pm25_o3_2==2]<-1

com_2012_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2012_final,sum)



com_2012_final$no2_pm10_2<-rowSums(com_2012_final[,c(5,8)])
com_2012_final$no2_pm10_2[com_2012_final$no2_pm10_2<2 | com_2012_final$pm25==1| com_2012_final$o3==1]<-0
com_2012_final$no2_pm10_2[com_2012_final$no2_pm10_2==2]<-1

com_2012_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2012_final,sum)


com_2012_final$no2_o3_2<-rowSums(com_2012_final[,c(5,9)])
com_2012_final$no2_o3_2[com_2012_final$no2_o3_2<2 | com_2012_final$pm25==1| com_2012_final$pm10==1]<-0
com_2012_final$no2_o3_2[com_2012_final$no2_o3_2==2]<-1

com_2012_final_ag_23<-aggregate(no2_o3_2~V_1,com_2012_final,sum)


com_2012_final$no2_pm10_o3_2<-rowSums(com_2012_final[,c(5,8,9)])
com_2012_final$no2_pm10_o3_2[com_2012_final$no2_pm10_o3_2<3 | com_2012_final$pm25==1]<-0
com_2012_final$no2_pm10_o3_2[com_2012_final$no2_pm10_o3_2==3]<-1


com_2012_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2012_final,sum)



com_2012_final$pm10_o3_2<-rowSums(com_2012_final[,c(8,9)])
com_2012_final$pm10_o3_2[com_2012_final$pm10_o3_2<2 | com_2012_final$pm25==1| com_2012_final$no2==1]<-0
com_2012_final$pm10_o3_2[com_2012_final$pm10_o3_2==2]<-1

com_2012_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2012_final,sum)






#############








library(dplyr)

com_2013_final<-left_join(com_2013_no2,com_2013_pm25)
com_2013_final<-left_join(com_2013_final,com_2013_pm10)
com_2013_final<-left_join(com_2013_final,com_2013_o3)


com_2013_final$no2_pm25_pm10<-rowSums(com_2013_final[,c(5,7,8)])
com_2013_final$no2_pm25_pm10[com_2013_final$no2_pm25_pm10<3]<-0
com_2013_final$no2_pm25_pm10[com_2013_final$no2_pm25_pm10==3]<-1

com_2013_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2013_final,sum)




com_2013_final$pm25_pm10<-rowSums(com_2013_final[,c(7,8)])
com_2013_final$pm25_pm10[com_2013_final$pm25_pm10<2]<-0
com_2013_final$pm25_pm10[com_2013_final$pm25_pm10==2]<-1

com_2013_final_ag_2<-aggregate(pm25_pm10~V_1,com_2013_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2013_final$no2_pm25<-rowSums(com_2013_final[,c(5,7)])
com_2013_final$no2_pm25[com_2013_final$no2_pm25<2]<-0
com_2013_final$no2_pm25[com_2013_final$no2_pm25==2]<-1

com_2013_final_ag_3<-aggregate(no2_pm25~V_1,com_2013_final,sum)




com_2013_final$no2_pm25_pm10_o3<-rowSums(com_2013_final[,c(5,7,8,9)])
com_2013_final$no2_pm25_pm10_o3[com_2013_final$no2_pm25_pm10_o3<4]<-0
com_2013_final$no2_pm25_pm10_o3[com_2013_final$no2_pm25_pm10_o3==4]<-1

com_2013_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2013_final,sum)




com_2013_final$pm10_no2<-rowSums(com_2013_final[,c(5,8)])
com_2013_final$pm10_no2[com_2013_final$pm10_no2<2]<-0
com_2013_final$pm10_no2[com_2013_final$pm10_no2==2]<-1

com_2013_final_ag_4<-aggregate(pm10_no2~V_1,com_2013_final,sum)






com_2013_final$pm10_o3<-rowSums(com_2013_final[,c(9,8)])
com_2013_final$pm10_o3[com_2013_final$pm10_o3<2]<-0
com_2013_final$pm10_o3[com_2013_final$pm10_o3==2]<-1

com_2013_final_ag_5<-aggregate(pm10_o3~V_1,com_2013_final,sum)







com_2013_final$pm25_o3<-rowSums(com_2013_final[,c(7,9)])
com_2013_final$pm25_o3[com_2013_final$pm25_o3<2]<-0
com_2013_final$pm25_o3[com_2013_final$pm25_o3==2]<-1

com_2013_final_ag_6<-aggregate(pm25_o3~V_1,com_2013_final,sum)






com_2013_final$no2_o3<-rowSums(com_2013_final[,c(5,9)])
com_2013_final$no2_o3[com_2013_final$no2_o3<2]<-0
com_2013_final$no2_o3[com_2013_final$no2_o3==2]<-1

com_2013_final_ag_7<-aggregate(no2_o3~V_1,com_2013_final,sum)






com_2013_final$pm10_no2_o3<-rowSums(com_2013_final[,c(8,9,5)])
com_2013_final$pm10_no2_o3[com_2013_final$pm10_no2_o3<3]<-0
com_2013_final$pm10_no2_o3[com_2013_final$pm10_no2_o3==3]<-1

com_2013_final_8<-aggregate(pm10_no2_o3~V_1,com_2013_final,sum)





com_2013_final$pm25_no2_o3<-rowSums(com_2013_final[,c(7,9,5)])
com_2013_final$pm25_no2_o3[com_2013_final$pm25_no2_o3<3]<-0
com_2013_final$pm25_no2_o3[com_2013_final$pm25_no2_o3==3]<-1

com_2013_final_9<-aggregate(pm25_no2_o3~V_1,com_2013_final,sum)






com_2013_final$pm10_pm25_o3<-rowSums(com_2013_final[,c(7,9,8)])
com_2013_final$pm10_pm25_o3[com_2013_final$pm10_pm25_o3<3]<-0
com_2013_final$pm10_pm25_o3[com_2013_final$pm10_pm25_o3==3]<-1

com_2013_final_10<-aggregate(pm10_pm25_o3~V_1,com_2013_final,sum)





####

com_2013_final$no2_pm25_pm10_2<-rowSums(com_2013_final[,c(5,7,8)])
com_2013_final$no2_pm25_pm10_2[com_2013_final$no2_pm25_pm10_2<3 | com_2013_final$o3==1]<-0
com_2013_final$no2_pm25_pm10_2[com_2013_final$no2_pm25_pm10_2==3]<-1

com_2013_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2013_final,sum)




com_2013_final$pm25_pm10_2<-rowSums(com_2013_final[,c(7,8)])
com_2013_final$pm25_pm10_2[com_2013_final$pm25_pm10_2<2 | com_2013_final$o3==1 | com_2013_final$no2==1]<-0
com_2013_final$pm25_pm10_2[com_2013_final$pm25_pm10_2==2]<-1

com_2013_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2013_final,sum)



com_2013_final$no2_pm25_2<-rowSums(com_2013_final[,c(5,7)])
com_2013_final$no2_pm25_2[com_2013_final$no2_pm25_2<2 | com_2013_final$o3==1 | com_2013_final$pm10==1]<-0
com_2013_final$no2_pm25_2[com_2013_final$no2_pm25_2==2]<-1

com_2013_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2013_final,sum)







com_2013_final$pm10_pm25_o3_2<-rowSums(com_2013_final[,c(9,7,8)])
com_2013_final$pm10_pm25_o3_2[com_2013_final$pm10_pm25_o3_2<3 | com_2013_final$no2==1]<-0
com_2013_final$pm10_pm25_o3_2[com_2013_final$pm10_pm25_o3_2==3]<-1

com_2013_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2013_final,sum)



com_2013_final$pm25_no2_o3_2<-rowSums(com_2013_final[,c(5,7,9)])
com_2013_final$pm25_no2_o3_2[com_2013_final$pm25_no2_o3_2<3 | com_2013_final$pm10==1]<-0
com_2013_final$pm25_no2_o3_2[com_2013_final$pm25_no2_o3_2==3]<-1

com_2013_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2013_final,sum)


com_2013_final$pm25_2<-com_2013_final[,c(7)]
com_2013_final$pm25_2[com_2013_final$pm25_2<1 | com_2013_final$pm10==1 |com_2013_final$no2==1|com_2013_final$o3==1]<-0
com_2013_final$pm25_2[com_2013_final$pm25_2==1]<-1


com_2013_final_ag_17<-aggregate(pm25_2~V_1,com_2013_final,sum)



com_2013_final$pm10_2<-com_2013_final[,c(8)]
com_2013_final$pm10_2[com_2013_final$pm10_2<1 | com_2013_final$pm25==1 |com_2013_final$no2==1|com_2013_final$o3==1]<-0
com_2013_final$pm10_2[com_2013_final$pm10_2==1]<-1

com_2013_final_ag_18<-aggregate(pm10_2~V_1,com_2013_final,sum)



com_2013_final$no2_2<-com_2013_final[,c(5)]
com_2013_final$no2_2[com_2013_final$no2_2<1 | com_2013_final$pm25==1 |com_2013_final$pm10==1|com_2013_final$o3==1]<-0
com_2013_final$no2_2[com_2013_final$no2_2==1]<-1

com_2013_final_ag_19<-aggregate(no2_2~V_1,com_2013_final,sum)



com_2013_final$o3_2<-com_2013_final[,c(9)]
com_2013_final$o3_2[com_2013_final$o3_2<1 | com_2013_final$pm25==1 |com_2013_final$pm10==1|com_2013_final$no2==1]<-0
com_2013_final$o3_2[com_2013_final$o3_2==1]<-1

com_2013_final_ag_20<-aggregate(o3_2~V_1,com_2013_final,sum)



com_2013_final$pm25_o3_2<-rowSums(com_2013_final[,c(9,7)])
com_2013_final$pm25_o3_2[com_2013_final$pm25_o3_2<2 | com_2013_final$no2==1| com_2013_final$pm10==1]<-0
com_2013_final$pm25_o3_2[com_2013_final$pm25_o3_2==2]<-1

com_2013_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2013_final,sum)



com_2013_final$no2_pm10_2<-rowSums(com_2013_final[,c(5,8)])
com_2013_final$no2_pm10_2[com_2013_final$no2_pm10_2<2 | com_2013_final$pm25==1| com_2013_final$o3==1]<-0
com_2013_final$no2_pm10_2[com_2013_final$no2_pm10_2==2]<-1

com_2013_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2013_final,sum)


com_2013_final$no2_o3_2<-rowSums(com_2013_final[,c(5,9)])
com_2013_final$no2_o3_2[com_2013_final$no2_o3_2<2 | com_2013_final$pm25==1| com_2013_final$pm10==1]<-0
com_2013_final$no2_o3_2[com_2013_final$no2_o3_2==2]<-1

com_2013_final_ag_23<-aggregate(no2_o3_2~V_1,com_2013_final,sum)


com_2013_final$no2_pm10_o3_2<-rowSums(com_2013_final[,c(5,8,9)])
com_2013_final$no2_pm10_o3_2[com_2013_final$no2_pm10_o3_2<3 | com_2013_final$pm25==1]<-0
com_2013_final$no2_pm10_o3_2[com_2013_final$no2_pm10_o3_2==3]<-1


com_2013_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2013_final,sum)



com_2013_final$pm10_o3_2<-rowSums(com_2013_final[,c(8,9)])
com_2013_final$pm10_o3_2[com_2013_final$pm10_o3_2<2 | com_2013_final$pm25==1| com_2013_final$no2==1]<-0
com_2013_final$pm10_o3_2[com_2013_final$pm10_o3_2==2]<-1

com_2013_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2013_final,sum)





############




rm(com_2009_no2,com_2009_pm25,com_2009_pm10,com_2009_o3)
rm(com_2010_no2,com_2010_pm25,com_2010_pm10,com_2010_o3)
rm(com_2011_no2,com_2011_pm25,com_2011_pm10,com_2011_o3)
rm(com_2012_no2,com_2012_pm25,com_2012_pm10,com_2012_o3)
rm(com_2013_no2,com_2013_pm25,com_2013_pm10,com_2013_o3)

gc()









library(dplyr)

com_2014_final<-left_join(com_2014_no2,com_2014_pm25)
com_2014_final<-left_join(com_2014_final,com_2014_pm10)
com_2014_final<-left_join(com_2014_final,com_2014_o3)


com_2014_final$no2_pm25_pm10<-rowSums(com_2014_final[,c(5,7,8)])
com_2014_final$no2_pm25_pm10[com_2014_final$no2_pm25_pm10<3]<-0
com_2014_final$no2_pm25_pm10[com_2014_final$no2_pm25_pm10==3]<-1

com_2014_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2014_final,sum)




com_2014_final$pm25_pm10<-rowSums(com_2014_final[,c(7,8)])
com_2014_final$pm25_pm10[com_2014_final$pm25_pm10<2]<-0
com_2014_final$pm25_pm10[com_2014_final$pm25_pm10==2]<-1

com_2014_final_ag_2<-aggregate(pm25_pm10~V_1,com_2014_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2014_final$no2_pm25<-rowSums(com_2014_final[,c(5,7)])
com_2014_final$no2_pm25[com_2014_final$no2_pm25<2]<-0
com_2014_final$no2_pm25[com_2014_final$no2_pm25==2]<-1

com_2014_final_ag_3<-aggregate(no2_pm25~V_1,com_2014_final,sum)




com_2014_final$no2_pm25_pm10_o3<-rowSums(com_2014_final[,c(5,7,8,9)])
com_2014_final$no2_pm25_pm10_o3[com_2014_final$no2_pm25_pm10_o3<4]<-0
com_2014_final$no2_pm25_pm10_o3[com_2014_final$no2_pm25_pm10_o3==4]<-1

com_2014_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2014_final,sum)




com_2014_final$pm10_no2<-rowSums(com_2014_final[,c(5,8)])
com_2014_final$pm10_no2[com_2014_final$pm10_no2<2]<-0
com_2014_final$pm10_no2[com_2014_final$pm10_no2==2]<-1

com_2014_final_ag_4<-aggregate(pm10_no2~V_1,com_2014_final,sum)






com_2014_final$pm10_o3<-rowSums(com_2014_final[,c(9,8)])
com_2014_final$pm10_o3[com_2014_final$pm10_o3<2]<-0
com_2014_final$pm10_o3[com_2014_final$pm10_o3==2]<-1

com_2014_final_ag_5<-aggregate(pm10_o3~V_1,com_2014_final,sum)







com_2014_final$pm25_o3<-rowSums(com_2014_final[,c(7,9)])
com_2014_final$pm25_o3[com_2014_final$pm25_o3<2]<-0
com_2014_final$pm25_o3[com_2014_final$pm25_o3==2]<-1

com_2014_final_ag_6<-aggregate(pm25_o3~V_1,com_2014_final,sum)






com_2014_final$no2_o3<-rowSums(com_2014_final[,c(5,9)])
com_2014_final$no2_o3[com_2014_final$no2_o3<2]<-0
com_2014_final$no2_o3[com_2014_final$no2_o3==2]<-1

com_2014_final_ag_7<-aggregate(no2_o3~V_1,com_2014_final,sum)






com_2014_final$pm10_no2_o3<-rowSums(com_2014_final[,c(8,9,5)])
com_2014_final$pm10_no2_o3[com_2014_final$pm10_no2_o3<3]<-0
com_2014_final$pm10_no2_o3[com_2014_final$pm10_no2_o3==3]<-1

com_2014_final_8<-aggregate(pm10_no2_o3~V_1,com_2014_final,sum)





com_2014_final$pm25_no2_o3<-rowSums(com_2014_final[,c(7,9,5)])
com_2014_final$pm25_no2_o3[com_2014_final$pm25_no2_o3<3]<-0
com_2014_final$pm25_no2_o3[com_2014_final$pm25_no2_o3==3]<-1

com_2014_final_9<-aggregate(pm25_no2_o3~V_1,com_2014_final,sum)






com_2014_final$pm10_pm25_o3<-rowSums(com_2014_final[,c(7,9,8)])
com_2014_final$pm10_pm25_o3[com_2014_final$pm10_pm25_o3<3]<-0
com_2014_final$pm10_pm25_o3[com_2014_final$pm10_pm25_o3==3]<-1

com_2014_final_10<-aggregate(pm10_pm25_o3~V_1,com_2014_final,sum)





####

com_2014_final$no2_pm25_pm10_2<-rowSums(com_2014_final[,c(5,7,8)])
com_2014_final$no2_pm25_pm10_2[com_2014_final$no2_pm25_pm10_2<3 | com_2014_final$o3==1]<-0
com_2014_final$no2_pm25_pm10_2[com_2014_final$no2_pm25_pm10_2==3]<-1

com_2014_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2014_final,sum)




com_2014_final$pm25_pm10_2<-rowSums(com_2014_final[,c(7,8)])
com_2014_final$pm25_pm10_2[com_2014_final$pm25_pm10_2<2 | com_2014_final$o3==1 | com_2014_final$no2==1]<-0
com_2014_final$pm25_pm10_2[com_2014_final$pm25_pm10_2==2]<-1

com_2014_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2014_final,sum)



com_2014_final$no2_pm25_2<-rowSums(com_2014_final[,c(5,7)])
com_2014_final$no2_pm25_2[com_2014_final$no2_pm25_2<2 | com_2014_final$o3==1 | com_2014_final$pm10==1]<-0
com_2014_final$no2_pm25_2[com_2014_final$no2_pm25_2==2]<-1

com_2014_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2014_final,sum)









com_2014_final$pm10_pm25_o3_2<-rowSums(com_2014_final[,c(9,7,8)])
com_2014_final$pm10_pm25_o3_2[com_2014_final$pm10_pm25_o3_2<3 | com_2014_final$no2==1]<-0
com_2014_final$pm10_pm25_o3_2[com_2014_final$pm10_pm25_o3_2==3]<-1

com_2014_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2014_final,sum)



com_2014_final$pm25_no2_o3_2<-rowSums(com_2014_final[,c(5,7,9)])
com_2014_final$pm25_no2_o3_2[com_2014_final$pm25_no2_o3_2<3 | com_2014_final$pm10==1]<-0
com_2014_final$pm25_no2_o3_2[com_2014_final$pm25_no2_o3_2==3]<-1

com_2014_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2014_final,sum)


com_2014_final$pm25_2<-com_2014_final[,c(7)]
com_2014_final$pm25_2[com_2014_final$pm25_2<1 | com_2014_final$pm10==1 |com_2014_final$no2==1|com_2014_final$o3==1]<-0
com_2014_final$pm25_2[com_2014_final$pm25_2==1]<-1


com_2014_final_ag_17<-aggregate(pm25_2~V_1,com_2014_final,sum)



com_2014_final$pm10_2<-com_2014_final[,c(8)]
com_2014_final$pm10_2[com_2014_final$pm10_2<1 | com_2014_final$pm25==1 |com_2014_final$no2==1|com_2014_final$o3==1]<-0
com_2014_final$pm10_2[com_2014_final$pm10_2==1]<-1

com_2014_final_ag_18<-aggregate(pm10_2~V_1,com_2014_final,sum)



com_2014_final$no2_2<-com_2014_final[,c(5)]
com_2014_final$no2_2[com_2014_final$no2_2<1 | com_2014_final$pm25==1 |com_2014_final$pm10==1|com_2014_final$o3==1]<-0
com_2014_final$no2_2[com_2014_final$no2_2==1]<-1

com_2014_final_ag_19<-aggregate(no2_2~V_1,com_2014_final,sum)



com_2014_final$o3_2<-com_2014_final[,c(9)]
com_2014_final$o3_2[com_2014_final$o3_2<1 | com_2014_final$pm25==1 |com_2014_final$pm10==1|com_2014_final$no2==1]<-0
com_2014_final$o3_2[com_2014_final$o3_2==1]<-1

com_2014_final_ag_20<-aggregate(o3_2~V_1,com_2014_final,sum)



com_2014_final$pm25_o3_2<-rowSums(com_2014_final[,c(9,7)])
com_2014_final$pm25_o3_2[com_2014_final$pm25_o3_2<2 | com_2014_final$no2==1| com_2014_final$pm10==1]<-0
com_2014_final$pm25_o3_2[com_2014_final$pm25_o3_2==2]<-1

com_2014_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2014_final,sum)



com_2014_final$no2_pm10_2<-rowSums(com_2014_final[,c(5,8)])
com_2014_final$no2_pm10_2[com_2014_final$no2_pm10_2<2 | com_2014_final$pm25==1| com_2014_final$o3==1]<-0
com_2014_final$no2_pm10_2[com_2014_final$no2_pm10_2==2]<-1

com_2014_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2014_final,sum)


com_2014_final$no2_o3_2<-rowSums(com_2014_final[,c(5,9)])
com_2014_final$no2_o3_2[com_2014_final$no2_o3_2<2 | com_2014_final$pm25==1| com_2014_final$pm10==1]<-0
com_2014_final$no2_o3_2[com_2014_final$no2_o3_2==2]<-1

com_2014_final_ag_23<-aggregate(no2_o3_2~V_1,com_2014_final,sum)


com_2014_final$no2_pm10_o3_2<-rowSums(com_2014_final[,c(5,8,9)])
com_2014_final$no2_pm10_o3_2[com_2014_final$no2_pm10_o3_2<3 | com_2014_final$pm25==1]<-0
com_2014_final$no2_pm10_o3_2[com_2014_final$no2_pm10_o3_2==3]<-1


com_2014_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2014_final,sum)



com_2014_final$pm10_o3_2<-rowSums(com_2014_final[,c(8,9)])
com_2014_final$pm10_o3_2[com_2014_final$pm10_o3_2<2 | com_2014_final$pm25==1| com_2014_final$no2==1]<-0
com_2014_final$pm10_o3_2[com_2014_final$pm10_o3_2==2]<-1

com_2014_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2014_final,sum)








#############








library(dplyr)

com_2015_final<-left_join(com_2015_no2,com_2015_pm25)
com_2015_final<-left_join(com_2015_final,com_2015_pm10)
com_2015_final<-left_join(com_2015_final,com_2015_o3)


com_2015_final$no2_pm25_pm10<-rowSums(com_2015_final[,c(5,7,8)])
com_2015_final$no2_pm25_pm10[com_2015_final$no2_pm25_pm10<3]<-0
com_2015_final$no2_pm25_pm10[com_2015_final$no2_pm25_pm10==3]<-1

com_2015_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2015_final,sum)




com_2015_final$pm25_pm10<-rowSums(com_2015_final[,c(7,8)])
com_2015_final$pm25_pm10[com_2015_final$pm25_pm10<2]<-0
com_2015_final$pm25_pm10[com_2015_final$pm25_pm10==2]<-1

com_2015_final_ag_2<-aggregate(pm25_pm10~V_1,com_2015_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2015_final$no2_pm25<-rowSums(com_2015_final[,c(5,7)])
com_2015_final$no2_pm25[com_2015_final$no2_pm25<2]<-0
com_2015_final$no2_pm25[com_2015_final$no2_pm25==2]<-1

com_2015_final_ag_3<-aggregate(no2_pm25~V_1,com_2015_final,sum)




com_2015_final$no2_pm25_pm10_o3<-rowSums(com_2015_final[,c(5,7,8,9)])
com_2015_final$no2_pm25_pm10_o3[com_2015_final$no2_pm25_pm10_o3<4]<-0
com_2015_final$no2_pm25_pm10_o3[com_2015_final$no2_pm25_pm10_o3==4]<-1

com_2015_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2015_final,sum)




com_2015_final$pm10_no2<-rowSums(com_2015_final[,c(5,8)])
com_2015_final$pm10_no2[com_2015_final$pm10_no2<2]<-0
com_2015_final$pm10_no2[com_2015_final$pm10_no2==2]<-1

com_2015_final_ag_4<-aggregate(pm10_no2~V_1,com_2015_final,sum)






com_2015_final$pm10_o3<-rowSums(com_2015_final[,c(9,8)])
com_2015_final$pm10_o3[com_2015_final$pm10_o3<2]<-0
com_2015_final$pm10_o3[com_2015_final$pm10_o3==2]<-1

com_2015_final_ag_5<-aggregate(pm10_o3~V_1,com_2015_final,sum)







com_2015_final$pm25_o3<-rowSums(com_2015_final[,c(7,9)])
com_2015_final$pm25_o3[com_2015_final$pm25_o3<2]<-0
com_2015_final$pm25_o3[com_2015_final$pm25_o3==2]<-1

com_2015_final_ag_6<-aggregate(pm25_o3~V_1,com_2015_final,sum)






com_2015_final$no2_o3<-rowSums(com_2015_final[,c(5,9)])
com_2015_final$no2_o3[com_2015_final$no2_o3<2]<-0
com_2015_final$no2_o3[com_2015_final$no2_o3==2]<-1

com_2015_final_ag_7<-aggregate(no2_o3~V_1,com_2015_final,sum)






com_2015_final$pm10_no2_o3<-rowSums(com_2015_final[,c(8,9,5)])
com_2015_final$pm10_no2_o3[com_2015_final$pm10_no2_o3<3]<-0
com_2015_final$pm10_no2_o3[com_2015_final$pm10_no2_o3==3]<-1

com_2015_final_8<-aggregate(pm10_no2_o3~V_1,com_2015_final,sum)





com_2015_final$pm25_no2_o3<-rowSums(com_2015_final[,c(7,9,5)])
com_2015_final$pm25_no2_o3[com_2015_final$pm25_no2_o3<3]<-0
com_2015_final$pm25_no2_o3[com_2015_final$pm25_no2_o3==3]<-1

com_2015_final_9<-aggregate(pm25_no2_o3~V_1,com_2015_final,sum)






com_2015_final$pm10_pm25_o3<-rowSums(com_2015_final[,c(7,9,8)])
com_2015_final$pm10_pm25_o3[com_2015_final$pm10_pm25_o3<3]<-0
com_2015_final$pm10_pm25_o3[com_2015_final$pm10_pm25_o3==3]<-1

com_2015_final_10<-aggregate(pm10_pm25_o3~V_1,com_2015_final,sum)





####

com_2015_final$no2_pm25_pm10_2<-rowSums(com_2015_final[,c(5,7,8)])
com_2015_final$no2_pm25_pm10_2[com_2015_final$no2_pm25_pm10_2<3 | com_2015_final$o3==1]<-0
com_2015_final$no2_pm25_pm10_2[com_2015_final$no2_pm25_pm10_2==3]<-1

com_2015_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2015_final,sum)




com_2015_final$pm25_pm10_2<-rowSums(com_2015_final[,c(7,8)])
com_2015_final$pm25_pm10_2[com_2015_final$pm25_pm10_2<2 | com_2015_final$o3==1 | com_2015_final$no2==1]<-0
com_2015_final$pm25_pm10_2[com_2015_final$pm25_pm10_2==2]<-1

com_2015_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2015_final,sum)



com_2015_final$no2_pm25_2<-rowSums(com_2015_final[,c(5,7)])
com_2015_final$no2_pm25_2[com_2015_final$no2_pm25_2<2 | com_2015_final$o3==1 | com_2015_final$pm10==1]<-0
com_2015_final$no2_pm25_2[com_2015_final$no2_pm25_2==2]<-1

com_2015_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2015_final,sum)






com_2015_final$pm10_pm25_o3_2<-rowSums(com_2015_final[,c(9,7,8)])
com_2015_final$pm10_pm25_o3_2[com_2015_final$pm10_pm25_o3_2<3 | com_2015_final$no2==1]<-0
com_2015_final$pm10_pm25_o3_2[com_2015_final$pm10_pm25_o3_2==3]<-1

com_2015_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2015_final,sum)



com_2015_final$pm25_no2_o3_2<-rowSums(com_2015_final[,c(5,7,9)])
com_2015_final$pm25_no2_o3_2[com_2015_final$pm25_no2_o3_2<3 | com_2015_final$pm10==1]<-0
com_2015_final$pm25_no2_o3_2[com_2015_final$pm25_no2_o3_2==3]<-1

com_2015_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2015_final,sum)


com_2015_final$pm25_2<-com_2015_final[,c(7)]
com_2015_final$pm25_2[com_2015_final$pm25_2<1 | com_2015_final$pm10==1 |com_2015_final$no2==1|com_2015_final$o3==1]<-0
com_2015_final$pm25_2[com_2015_final$pm25_2==1]<-1


com_2015_final_ag_17<-aggregate(pm25_2~V_1,com_2015_final,sum)



com_2015_final$pm10_2<-com_2015_final[,c(8)]
com_2015_final$pm10_2[com_2015_final$pm10_2<1 | com_2015_final$pm25==1 |com_2015_final$no2==1|com_2015_final$o3==1]<-0
com_2015_final$pm10_2[com_2015_final$pm10_2==1]<-1

com_2015_final_ag_18<-aggregate(pm10_2~V_1,com_2015_final,sum)



com_2015_final$no2_2<-com_2015_final[,c(5)]
com_2015_final$no2_2[com_2015_final$no2_2<1 | com_2015_final$pm25==1 |com_2015_final$pm10==1|com_2015_final$o3==1]<-0
com_2015_final$no2_2[com_2015_final$no2_2==1]<-1

com_2015_final_ag_19<-aggregate(no2_2~V_1,com_2015_final,sum)



com_2015_final$o3_2<-com_2015_final[,c(9)]
com_2015_final$o3_2[com_2015_final$o3_2<1 | com_2015_final$pm25==1 |com_2015_final$pm10==1|com_2015_final$no2==1]<-0
com_2015_final$o3_2[com_2015_final$o3_2==1]<-1

com_2015_final_ag_20<-aggregate(o3_2~V_1,com_2015_final,sum)



com_2015_final$pm25_o3_2<-rowSums(com_2015_final[,c(9,7)])
com_2015_final$pm25_o3_2[com_2015_final$pm25_o3_2<2 | com_2015_final$no2==1| com_2015_final$pm10==1]<-0
com_2015_final$pm25_o3_2[com_2015_final$pm25_o3_2==2]<-1

com_2015_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2015_final,sum)



com_2015_final$no2_pm10_2<-rowSums(com_2015_final[,c(5,8)])
com_2015_final$no2_pm10_2[com_2015_final$no2_pm10_2<2 | com_2015_final$pm25==1| com_2015_final$o3==1]<-0
com_2015_final$no2_pm10_2[com_2015_final$no2_pm10_2==2]<-1

com_2015_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2015_final,sum)


com_2015_final$no2_o3_2<-rowSums(com_2015_final[,c(5,9)])
com_2015_final$no2_o3_2[com_2015_final$no2_o3_2<2 | com_2015_final$pm25==1| com_2015_final$pm10==1]<-0
com_2015_final$no2_o3_2[com_2015_final$no2_o3_2==2]<-1

com_2015_final_ag_23<-aggregate(no2_o3_2~V_1,com_2015_final,sum)


com_2015_final$no2_pm10_o3_2<-rowSums(com_2015_final[,c(5,8,9)])
com_2015_final$no2_pm10_o3_2[com_2015_final$no2_pm10_o3_2<3 | com_2015_final$pm25==1]<-0
com_2015_final$no2_pm10_o3_2[com_2015_final$no2_pm10_o3_2==3]<-1


com_2015_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2015_final,sum)



com_2015_final$pm10_o3_2<-rowSums(com_2015_final[,c(8,9)])
com_2015_final$pm10_o3_2[com_2015_final$pm10_o3_2<2 | com_2015_final$pm25==1| com_2015_final$no2==1]<-0
com_2015_final$pm10_o3_2[com_2015_final$pm10_o3_2==2]<-1

com_2015_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2015_final,sum)




###############


rm(com_2014_no2,com_2014_pm25,com_2014_pm10,com_2014_o3)
rm(com_2015_no2,com_2015_pm25,com_2015_pm10,com_2015_o3)
gc()









library(dplyr)

com_2016_final<-left_join(com_2016_no2,com_2016_pm25)
com_2016_final<-left_join(com_2016_final,com_2016_pm10)
com_2016_final<-left_join(com_2016_final,com_2016_o3)


com_2016_final$no2_pm25_pm10<-rowSums(com_2016_final[,c(5,7,8)])
com_2016_final$no2_pm25_pm10[com_2016_final$no2_pm25_pm10<3]<-0
com_2016_final$no2_pm25_pm10[com_2016_final$no2_pm25_pm10==3]<-1

com_2016_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2016_final,sum)




com_2016_final$pm25_pm10<-rowSums(com_2016_final[,c(7,8)])
com_2016_final$pm25_pm10[com_2016_final$pm25_pm10<2]<-0
com_2016_final$pm25_pm10[com_2016_final$pm25_pm10==2]<-1

com_2016_final_ag_2<-aggregate(pm25_pm10~V_1,com_2016_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2016_final$no2_pm25<-rowSums(com_2016_final[,c(5,7)])
com_2016_final$no2_pm25[com_2016_final$no2_pm25<2]<-0
com_2016_final$no2_pm25[com_2016_final$no2_pm25==2]<-1

com_2016_final_ag_3<-aggregate(no2_pm25~V_1,com_2016_final,sum)




com_2016_final$no2_pm25_pm10_o3<-rowSums(com_2016_final[,c(5,7,8,9)])
com_2016_final$no2_pm25_pm10_o3[com_2016_final$no2_pm25_pm10_o3<4]<-0
com_2016_final$no2_pm25_pm10_o3[com_2016_final$no2_pm25_pm10_o3==4]<-1

com_2016_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2016_final,sum)




com_2016_final$pm10_no2<-rowSums(com_2016_final[,c(5,8)])
com_2016_final$pm10_no2[com_2016_final$pm10_no2<2]<-0
com_2016_final$pm10_no2[com_2016_final$pm10_no2==2]<-1

com_2016_final_ag_4<-aggregate(pm10_no2~V_1,com_2016_final,sum)






com_2016_final$pm10_o3<-rowSums(com_2016_final[,c(9,8)])
com_2016_final$pm10_o3[com_2016_final$pm10_o3<2]<-0
com_2016_final$pm10_o3[com_2016_final$pm10_o3==2]<-1

com_2016_final_ag_5<-aggregate(pm10_o3~V_1,com_2016_final,sum)







com_2016_final$pm25_o3<-rowSums(com_2016_final[,c(7,9)])
com_2016_final$pm25_o3[com_2016_final$pm25_o3<2]<-0
com_2016_final$pm25_o3[com_2016_final$pm25_o3==2]<-1

com_2016_final_ag_6<-aggregate(pm25_o3~V_1,com_2016_final,sum)






com_2016_final$no2_o3<-rowSums(com_2016_final[,c(5,9)])
com_2016_final$no2_o3[com_2016_final$no2_o3<2]<-0
com_2016_final$no2_o3[com_2016_final$no2_o3==2]<-1

com_2016_final_ag_7<-aggregate(no2_o3~V_1,com_2016_final,sum)






com_2016_final$pm10_no2_o3<-rowSums(com_2016_final[,c(8,9,5)])
com_2016_final$pm10_no2_o3[com_2016_final$pm10_no2_o3<3]<-0
com_2016_final$pm10_no2_o3[com_2016_final$pm10_no2_o3==3]<-1

com_2016_final_8<-aggregate(pm10_no2_o3~V_1,com_2016_final,sum)





com_2016_final$pm25_no2_o3<-rowSums(com_2016_final[,c(7,9,5)])
com_2016_final$pm25_no2_o3[com_2016_final$pm25_no2_o3<3]<-0
com_2016_final$pm25_no2_o3[com_2016_final$pm25_no2_o3==3]<-1

com_2016_final_9<-aggregate(pm25_no2_o3~V_1,com_2016_final,sum)






com_2016_final$pm10_pm25_o3<-rowSums(com_2016_final[,c(7,9,8)])
com_2016_final$pm10_pm25_o3[com_2016_final$pm10_pm25_o3<3]<-0
com_2016_final$pm10_pm25_o3[com_2016_final$pm10_pm25_o3==3]<-1

com_2016_final_10<-aggregate(pm10_pm25_o3~V_1,com_2016_final,sum)





####

com_2016_final$no2_pm25_pm10_2<-rowSums(com_2016_final[,c(5,7,8)])
com_2016_final$no2_pm25_pm10_2[com_2016_final$no2_pm25_pm10_2<3 | com_2016_final$o3==1]<-0
com_2016_final$no2_pm25_pm10_2[com_2016_final$no2_pm25_pm10_2==3]<-1

com_2016_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2016_final,sum)




com_2016_final$pm25_pm10_2<-rowSums(com_2016_final[,c(7,8)])
com_2016_final$pm25_pm10_2[com_2016_final$pm25_pm10_2<2 | com_2016_final$o3==1 | com_2016_final$no2==1]<-0
com_2016_final$pm25_pm10_2[com_2016_final$pm25_pm10_2==2]<-1

com_2016_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2016_final,sum)



com_2016_final$no2_pm25_2<-rowSums(com_2016_final[,c(5,7)])
com_2016_final$no2_pm25_2[com_2016_final$no2_pm25_2<2 | com_2016_final$o3==1 | com_2016_final$pm10==1]<-0
com_2016_final$no2_pm25_2[com_2016_final$no2_pm25_2==2]<-1

com_2016_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2016_final,sum)





com_2016_final$pm10_pm25_o3_2<-rowSums(com_2016_final[,c(9,7,8)])
com_2016_final$pm10_pm25_o3_2[com_2016_final$pm10_pm25_o3_2<3 | com_2016_final$no2==1]<-0
com_2016_final$pm10_pm25_o3_2[com_2016_final$pm10_pm25_o3_2==3]<-1

com_2016_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2016_final,sum)



com_2016_final$pm25_no2_o3_2<-rowSums(com_2016_final[,c(5,7,9)])
com_2016_final$pm25_no2_o3_2[com_2016_final$pm25_no2_o3_2<3 | com_2016_final$pm10==1]<-0
com_2016_final$pm25_no2_o3_2[com_2016_final$pm25_no2_o3_2==3]<-1

com_2016_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2016_final,sum)


com_2016_final$pm25_2<-com_2016_final[,c(7)]
com_2016_final$pm25_2[com_2016_final$pm25_2<1 | com_2016_final$pm10==1 |com_2016_final$no2==1|com_2016_final$o3==1]<-0
com_2016_final$pm25_2[com_2016_final$pm25_2==1]<-1


com_2016_final_ag_17<-aggregate(pm25_2~V_1,com_2016_final,sum)



com_2016_final$pm10_2<-com_2016_final[,c(8)]
com_2016_final$pm10_2[com_2016_final$pm10_2<1 | com_2016_final$pm25==1 |com_2016_final$no2==1|com_2016_final$o3==1]<-0
com_2016_final$pm10_2[com_2016_final$pm10_2==1]<-1

com_2016_final_ag_18<-aggregate(pm10_2~V_1,com_2016_final,sum)



com_2016_final$no2_2<-com_2016_final[,c(5)]
com_2016_final$no2_2[com_2016_final$no2_2<1 | com_2016_final$pm25==1 |com_2016_final$pm10==1|com_2016_final$o3==1]<-0
com_2016_final$no2_2[com_2016_final$no2_2==1]<-1

com_2016_final_ag_19<-aggregate(no2_2~V_1,com_2016_final,sum)



com_2016_final$o3_2<-com_2016_final[,c(9)]
com_2016_final$o3_2[com_2016_final$o3_2<1 | com_2016_final$pm25==1 |com_2016_final$pm10==1|com_2016_final$no2==1]<-0
com_2016_final$o3_2[com_2016_final$o3_2==1]<-1

com_2016_final_ag_20<-aggregate(o3_2~V_1,com_2016_final,sum)



com_2016_final$pm25_o3_2<-rowSums(com_2016_final[,c(9,7)])
com_2016_final$pm25_o3_2[com_2016_final$pm25_o3_2<2 | com_2016_final$no2==1| com_2016_final$pm10==1]<-0
com_2016_final$pm25_o3_2[com_2016_final$pm25_o3_2==2]<-1

com_2016_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2016_final,sum)



com_2016_final$no2_pm10_2<-rowSums(com_2016_final[,c(5,8)])
com_2016_final$no2_pm10_2[com_2016_final$no2_pm10_2<2 | com_2016_final$pm25==1| com_2016_final$o3==1]<-0
com_2016_final$no2_pm10_2[com_2016_final$no2_pm10_2==2]<-1

com_2016_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2016_final,sum)


com_2016_final$no2_o3_2<-rowSums(com_2016_final[,c(5,9)])
com_2016_final$no2_o3_2[com_2016_final$no2_o3_2<2 | com_2016_final$pm25==1| com_2016_final$pm10==1]<-0
com_2016_final$no2_o3_2[com_2016_final$no2_o3_2==2]<-1

com_2016_final_ag_23<-aggregate(no2_o3_2~V_1,com_2016_final,sum)


com_2016_final$no2_pm10_o3_2<-rowSums(com_2016_final[,c(5,8,9)])
com_2016_final$no2_pm10_o3_2[com_2016_final$no2_pm10_o3_2<3 | com_2016_final$pm25==1]<-0
com_2016_final$no2_pm10_o3_2[com_2016_final$no2_pm10_o3_2==3]<-1


com_2016_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2016_final,sum)



com_2016_final$pm10_o3_2<-rowSums(com_2016_final[,c(8,9)])
com_2016_final$pm10_o3_2[com_2016_final$pm10_o3_2<2 | com_2016_final$pm25==1| com_2016_final$no2==1]<-0
com_2016_final$pm10_o3_2[com_2016_final$pm10_o3_2==2]<-1

com_2016_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2016_final,sum)




##########








library(dplyr)

com_2017_final<-left_join(com_2017_no2,com_2017_pm25)
com_2017_final<-left_join(com_2017_final,com_2017_pm10)
com_2017_final<-left_join(com_2017_final,com_2017_o3)


com_2017_final$no2_pm25_pm10<-rowSums(com_2017_final[,c(5,7,8)])
com_2017_final$no2_pm25_pm10[com_2017_final$no2_pm25_pm10<3]<-0
com_2017_final$no2_pm25_pm10[com_2017_final$no2_pm25_pm10==3]<-1

com_2017_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2017_final,sum)




com_2017_final$pm25_pm10<-rowSums(com_2017_final[,c(7,8)])
com_2017_final$pm25_pm10[com_2017_final$pm25_pm10<2]<-0
com_2017_final$pm25_pm10[com_2017_final$pm25_pm10==2]<-1

com_2017_final_ag_2<-aggregate(pm25_pm10~V_1,com_2017_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2017_final$no2_pm25<-rowSums(com_2017_final[,c(5,7)])
com_2017_final$no2_pm25[com_2017_final$no2_pm25<2]<-0
com_2017_final$no2_pm25[com_2017_final$no2_pm25==2]<-1

com_2017_final_ag_3<-aggregate(no2_pm25~V_1,com_2017_final,sum)




com_2017_final$no2_pm25_pm10_o3<-rowSums(com_2017_final[,c(5,7,8,9)])
com_2017_final$no2_pm25_pm10_o3[com_2017_final$no2_pm25_pm10_o3<4]<-0
com_2017_final$no2_pm25_pm10_o3[com_2017_final$no2_pm25_pm10_o3==4]<-1

com_2017_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2017_final,sum)




com_2017_final$pm10_no2<-rowSums(com_2017_final[,c(5,8)])
com_2017_final$pm10_no2[com_2017_final$pm10_no2<2]<-0
com_2017_final$pm10_no2[com_2017_final$pm10_no2==2]<-1

com_2017_final_ag_4<-aggregate(pm10_no2~V_1,com_2017_final,sum)






com_2017_final$pm10_o3<-rowSums(com_2017_final[,c(9,8)])
com_2017_final$pm10_o3[com_2017_final$pm10_o3<2]<-0
com_2017_final$pm10_o3[com_2017_final$pm10_o3==2]<-1

com_2017_final_ag_5<-aggregate(pm10_o3~V_1,com_2017_final,sum)







com_2017_final$pm25_o3<-rowSums(com_2017_final[,c(7,9)])
com_2017_final$pm25_o3[com_2017_final$pm25_o3<2]<-0
com_2017_final$pm25_o3[com_2017_final$pm25_o3==2]<-1

com_2017_final_ag_6<-aggregate(pm25_o3~V_1,com_2017_final,sum)






com_2017_final$no2_o3<-rowSums(com_2017_final[,c(5,9)])
com_2017_final$no2_o3[com_2017_final$no2_o3<2]<-0
com_2017_final$no2_o3[com_2017_final$no2_o3==2]<-1

com_2017_final_ag_7<-aggregate(no2_o3~V_1,com_2017_final,sum)






com_2017_final$pm10_no2_o3<-rowSums(com_2017_final[,c(8,9,5)])
com_2017_final$pm10_no2_o3[com_2017_final$pm10_no2_o3<3]<-0
com_2017_final$pm10_no2_o3[com_2017_final$pm10_no2_o3==3]<-1

com_2017_final_8<-aggregate(pm10_no2_o3~V_1,com_2017_final,sum)





com_2017_final$pm25_no2_o3<-rowSums(com_2017_final[,c(7,9,5)])
com_2017_final$pm25_no2_o3[com_2017_final$pm25_no2_o3<3]<-0
com_2017_final$pm25_no2_o3[com_2017_final$pm25_no2_o3==3]<-1

com_2017_final_9<-aggregate(pm25_no2_o3~V_1,com_2017_final,sum)






com_2017_final$pm10_pm25_o3<-rowSums(com_2017_final[,c(7,9,8)])
com_2017_final$pm10_pm25_o3[com_2017_final$pm10_pm25_o3<3]<-0
com_2017_final$pm10_pm25_o3[com_2017_final$pm10_pm25_o3==3]<-1

com_2017_final_10<-aggregate(pm10_pm25_o3~V_1,com_2017_final,sum)





####

com_2017_final$no2_pm25_pm10_2<-rowSums(com_2017_final[,c(5,7,8)])
com_2017_final$no2_pm25_pm10_2[com_2017_final$no2_pm25_pm10_2<3 | com_2017_final$o3==1]<-0
com_2017_final$no2_pm25_pm10_2[com_2017_final$no2_pm25_pm10_2==3]<-1

com_2017_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2017_final,sum)




com_2017_final$pm25_pm10_2<-rowSums(com_2017_final[,c(7,8)])
com_2017_final$pm25_pm10_2[com_2017_final$pm25_pm10_2<2 | com_2017_final$o3==1 | com_2017_final$no2==1]<-0
com_2017_final$pm25_pm10_2[com_2017_final$pm25_pm10_2==2]<-1

com_2017_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2017_final,sum)



com_2017_final$no2_pm25_2<-rowSums(com_2017_final[,c(5,7)])
com_2017_final$no2_pm25_2[com_2017_final$no2_pm25_2<2 | com_2017_final$o3==1 | com_2017_final$pm10==1]<-0
com_2017_final$no2_pm25_2[com_2017_final$no2_pm25_2==2]<-1

com_2017_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2017_final,sum)








com_2017_final$pm10_pm25_o3_2<-rowSums(com_2017_final[,c(9,7,8)])
com_2017_final$pm10_pm25_o3_2[com_2017_final$pm10_pm25_o3_2<3 | com_2017_final$no2==1]<-0
com_2017_final$pm10_pm25_o3_2[com_2017_final$pm10_pm25_o3_2==3]<-1

com_2017_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2017_final,sum)



com_2017_final$pm25_no2_o3_2<-rowSums(com_2017_final[,c(5,7,9)])
com_2017_final$pm25_no2_o3_2[com_2017_final$pm25_no2_o3_2<3 | com_2017_final$pm10==1]<-0
com_2017_final$pm25_no2_o3_2[com_2017_final$pm25_no2_o3_2==3]<-1

com_2017_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2017_final,sum)


com_2017_final$pm25_2<-com_2017_final[,c(7)]
com_2017_final$pm25_2[com_2017_final$pm25_2<1 | com_2017_final$pm10==1 |com_2017_final$no2==1|com_2017_final$o3==1]<-0
com_2017_final$pm25_2[com_2017_final$pm25_2==1]<-1


com_2017_final_ag_17<-aggregate(pm25_2~V_1,com_2017_final,sum)



com_2017_final$pm10_2<-com_2017_final[,c(8)]
com_2017_final$pm10_2[com_2017_final$pm10_2<1 | com_2017_final$pm25==1 |com_2017_final$no2==1|com_2017_final$o3==1]<-0
com_2017_final$pm10_2[com_2017_final$pm10_2==1]<-1

com_2017_final_ag_18<-aggregate(pm10_2~V_1,com_2017_final,sum)



com_2017_final$no2_2<-com_2017_final[,c(5)]
com_2017_final$no2_2[com_2017_final$no2_2<1 | com_2017_final$pm25==1 |com_2017_final$pm10==1|com_2017_final$o3==1]<-0
com_2017_final$no2_2[com_2017_final$no2_2==1]<-1

com_2017_final_ag_19<-aggregate(no2_2~V_1,com_2017_final,sum)



com_2017_final$o3_2<-com_2017_final[,c(9)]
com_2017_final$o3_2[com_2017_final$o3_2<1 | com_2017_final$pm25==1 |com_2017_final$pm10==1|com_2017_final$no2==1]<-0
com_2017_final$o3_2[com_2017_final$o3_2==1]<-1

com_2017_final_ag_20<-aggregate(o3_2~V_1,com_2017_final,sum)



com_2017_final$pm25_o3_2<-rowSums(com_2017_final[,c(9,7)])
com_2017_final$pm25_o3_2[com_2017_final$pm25_o3_2<2 | com_2017_final$no2==1| com_2017_final$pm10==1]<-0
com_2017_final$pm25_o3_2[com_2017_final$pm25_o3_2==2]<-1

com_2017_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2017_final,sum)



com_2017_final$no2_pm10_2<-rowSums(com_2017_final[,c(5,8)])
com_2017_final$no2_pm10_2[com_2017_final$no2_pm10_2<2 | com_2017_final$pm25==1| com_2017_final$o3==1]<-0
com_2017_final$no2_pm10_2[com_2017_final$no2_pm10_2==2]<-1

com_2017_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2017_final,sum)


com_2017_final$no2_o3_2<-rowSums(com_2017_final[,c(5,9)])
com_2017_final$no2_o3_2[com_2017_final$no2_o3_2<2 | com_2017_final$pm25==1| com_2017_final$pm10==1]<-0
com_2017_final$no2_o3_2[com_2017_final$no2_o3_2==2]<-1

com_2017_final_ag_23<-aggregate(no2_o3_2~V_1,com_2017_final,sum)


com_2017_final$no2_pm10_o3_2<-rowSums(com_2017_final[,c(5,8,9)])
com_2017_final$no2_pm10_o3_2[com_2017_final$no2_pm10_o3_2<3 | com_2017_final$pm25==1]<-0
com_2017_final$no2_pm10_o3_2[com_2017_final$no2_pm10_o3_2==3]<-1


com_2017_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2017_final,sum)



com_2017_final$pm10_o3_2<-rowSums(com_2017_final[,c(8,9)])
com_2017_final$pm10_o3_2[com_2017_final$pm10_o3_2<2 | com_2017_final$pm25==1| com_2017_final$no2==1]<-0
com_2017_final$pm10_o3_2[com_2017_final$pm10_o3_2==2]<-1

com_2017_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2017_final,sum)




############








library(dplyr)

com_2018_final<-left_join(com_2018_no2,com_2018_pm25)
com_2018_final<-left_join(com_2018_final,com_2018_pm10)
com_2018_final<-left_join(com_2018_final,com_2018_o3)


com_2018_final$no2_pm25_pm10<-rowSums(com_2018_final[,c(5,7,8)])
com_2018_final$no2_pm25_pm10[com_2018_final$no2_pm25_pm10<3]<-0
com_2018_final$no2_pm25_pm10[com_2018_final$no2_pm25_pm10==3]<-1

com_2018_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2018_final,sum)




com_2018_final$pm25_pm10<-rowSums(com_2018_final[,c(7,8)])
com_2018_final$pm25_pm10[com_2018_final$pm25_pm10<2]<-0
com_2018_final$pm25_pm10[com_2018_final$pm25_pm10==2]<-1

com_2018_final_ag_2<-aggregate(pm25_pm10~V_1,com_2018_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2018_final$no2_pm25<-rowSums(com_2018_final[,c(5,7)])
com_2018_final$no2_pm25[com_2018_final$no2_pm25<2]<-0
com_2018_final$no2_pm25[com_2018_final$no2_pm25==2]<-1

com_2018_final_ag_3<-aggregate(no2_pm25~V_1,com_2018_final,sum)




com_2018_final$no2_pm25_pm10_o3<-rowSums(com_2018_final[,c(5,7,8,9)])
com_2018_final$no2_pm25_pm10_o3[com_2018_final$no2_pm25_pm10_o3<4]<-0
com_2018_final$no2_pm25_pm10_o3[com_2018_final$no2_pm25_pm10_o3==4]<-1

com_2018_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2018_final,sum)




com_2018_final$pm10_no2<-rowSums(com_2018_final[,c(5,8)])
com_2018_final$pm10_no2[com_2018_final$pm10_no2<2]<-0
com_2018_final$pm10_no2[com_2018_final$pm10_no2==2]<-1

com_2018_final_ag_4<-aggregate(pm10_no2~V_1,com_2018_final,sum)






com_2018_final$pm10_o3<-rowSums(com_2018_final[,c(9,8)])
com_2018_final$pm10_o3[com_2018_final$pm10_o3<2]<-0
com_2018_final$pm10_o3[com_2018_final$pm10_o3==2]<-1

com_2018_final_ag_5<-aggregate(pm10_o3~V_1,com_2018_final,sum)







com_2018_final$pm25_o3<-rowSums(com_2018_final[,c(7,9)])
com_2018_final$pm25_o3[com_2018_final$pm25_o3<2]<-0
com_2018_final$pm25_o3[com_2018_final$pm25_o3==2]<-1

com_2018_final_ag_6<-aggregate(pm25_o3~V_1,com_2018_final,sum)






com_2018_final$no2_o3<-rowSums(com_2018_final[,c(5,9)])
com_2018_final$no2_o3[com_2018_final$no2_o3<2]<-0
com_2018_final$no2_o3[com_2018_final$no2_o3==2]<-1

com_2018_final_ag_7<-aggregate(no2_o3~V_1,com_2018_final,sum)






com_2018_final$pm10_no2_o3<-rowSums(com_2018_final[,c(8,9,5)])
com_2018_final$pm10_no2_o3[com_2018_final$pm10_no2_o3<3]<-0
com_2018_final$pm10_no2_o3[com_2018_final$pm10_no2_o3==3]<-1

com_2018_final_8<-aggregate(pm10_no2_o3~V_1,com_2018_final,sum)





com_2018_final$pm25_no2_o3<-rowSums(com_2018_final[,c(7,9,5)])
com_2018_final$pm25_no2_o3[com_2018_final$pm25_no2_o3<3]<-0
com_2018_final$pm25_no2_o3[com_2018_final$pm25_no2_o3==3]<-1

com_2018_final_9<-aggregate(pm25_no2_o3~V_1,com_2018_final,sum)






com_2018_final$pm10_pm25_o3<-rowSums(com_2018_final[,c(7,9,8)])
com_2018_final$pm10_pm25_o3[com_2018_final$pm10_pm25_o3<3]<-0
com_2018_final$pm10_pm25_o3[com_2018_final$pm10_pm25_o3==3]<-1

com_2018_final_10<-aggregate(pm10_pm25_o3~V_1,com_2018_final,sum)





####

com_2018_final$no2_pm25_pm10_2<-rowSums(com_2018_final[,c(5,7,8)])
com_2018_final$no2_pm25_pm10_2[com_2018_final$no2_pm25_pm10_2<3 | com_2018_final$o3==1]<-0
com_2018_final$no2_pm25_pm10_2[com_2018_final$no2_pm25_pm10_2==3]<-1

com_2018_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2018_final,sum)




com_2018_final$pm25_pm10_2<-rowSums(com_2018_final[,c(7,8)])
com_2018_final$pm25_pm10_2[com_2018_final$pm25_pm10_2<2 | com_2018_final$o3==1 | com_2018_final$no2==1]<-0
com_2018_final$pm25_pm10_2[com_2018_final$pm25_pm10_2==2]<-1

com_2018_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2018_final,sum)



com_2018_final$no2_pm25_2<-rowSums(com_2018_final[,c(5,7)])
com_2018_final$no2_pm25_2[com_2018_final$no2_pm25_2<2 | com_2018_final$o3==1 | com_2018_final$pm10==1]<-0
com_2018_final$no2_pm25_2[com_2018_final$no2_pm25_2==2]<-1

com_2018_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2018_final,sum)






com_2018_final$pm10_pm25_o3_2<-rowSums(com_2018_final[,c(9,7,8)])
com_2018_final$pm10_pm25_o3_2[com_2018_final$pm10_pm25_o3_2<3 | com_2018_final$no2==1]<-0
com_2018_final$pm10_pm25_o3_2[com_2018_final$pm10_pm25_o3_2==3]<-1

com_2018_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2018_final,sum)



com_2018_final$pm25_no2_o3_2<-rowSums(com_2018_final[,c(5,7,9)])
com_2018_final$pm25_no2_o3_2[com_2018_final$pm25_no2_o3_2<3 | com_2018_final$pm10==1]<-0
com_2018_final$pm25_no2_o3_2[com_2018_final$pm25_no2_o3_2==3]<-1

com_2018_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2018_final,sum)


com_2018_final$pm25_2<-com_2018_final[,c(7)]
com_2018_final$pm25_2[com_2018_final$pm25_2<1 | com_2018_final$pm10==1 |com_2018_final$no2==1|com_2018_final$o3==1]<-0
com_2018_final$pm25_2[com_2018_final$pm25_2==1]<-1


com_2018_final_ag_17<-aggregate(pm25_2~V_1,com_2018_final,sum)



com_2018_final$pm10_2<-com_2018_final[,c(8)]
com_2018_final$pm10_2[com_2018_final$pm10_2<1 | com_2018_final$pm25==1 |com_2018_final$no2==1|com_2018_final$o3==1]<-0
com_2018_final$pm10_2[com_2018_final$pm10_2==1]<-1

com_2018_final_ag_18<-aggregate(pm10_2~V_1,com_2018_final,sum)



com_2018_final$no2_2<-com_2018_final[,c(5)]
com_2018_final$no2_2[com_2018_final$no2_2<1 | com_2018_final$pm25==1 |com_2018_final$pm10==1|com_2018_final$o3==1]<-0
com_2018_final$no2_2[com_2018_final$no2_2==1]<-1

com_2018_final_ag_19<-aggregate(no2_2~V_1,com_2018_final,sum)



com_2018_final$o3_2<-com_2018_final[,c(9)]
com_2018_final$o3_2[com_2018_final$o3_2<1 | com_2018_final$pm25==1 |com_2018_final$pm10==1|com_2018_final$no2==1]<-0
com_2018_final$o3_2[com_2018_final$o3_2==1]<-1

com_2018_final_ag_20<-aggregate(o3_2~V_1,com_2018_final,sum)



com_2018_final$pm25_o3_2<-rowSums(com_2018_final[,c(9,7)])
com_2018_final$pm25_o3_2[com_2018_final$pm25_o3_2<2 | com_2018_final$no2==1| com_2018_final$pm10==1]<-0
com_2018_final$pm25_o3_2[com_2018_final$pm25_o3_2==2]<-1

com_2018_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2018_final,sum)



com_2018_final$no2_pm10_2<-rowSums(com_2018_final[,c(5,8)])
com_2018_final$no2_pm10_2[com_2018_final$no2_pm10_2<2 | com_2018_final$pm25==1| com_2018_final$o3==1]<-0
com_2018_final$no2_pm10_2[com_2018_final$no2_pm10_2==2]<-1

com_2018_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2018_final,sum)


com_2018_final$no2_o3_2<-rowSums(com_2018_final[,c(5,9)])
com_2018_final$no2_o3_2[com_2018_final$no2_o3_2<2 | com_2018_final$pm25==1| com_2018_final$pm10==1]<-0
com_2018_final$no2_o3_2[com_2018_final$no2_o3_2==2]<-1

com_2018_final_ag_23<-aggregate(no2_o3_2~V_1,com_2018_final,sum)


com_2018_final$no2_pm10_o3_2<-rowSums(com_2018_final[,c(5,8,9)])
com_2018_final$no2_pm10_o3_2[com_2018_final$no2_pm10_o3_2<3 | com_2018_final$pm25==1]<-0
com_2018_final$no2_pm10_o3_2[com_2018_final$no2_pm10_o3_2==3]<-1


com_2018_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2018_final,sum)



com_2018_final$pm10_o3_2<-rowSums(com_2018_final[,c(8,9)])
com_2018_final$pm10_o3_2[com_2018_final$pm10_o3_2<2 | com_2018_final$pm25==1| com_2018_final$no2==1]<-0
com_2018_final$pm10_o3_2[com_2018_final$pm10_o3_2==2]<-1

com_2018_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2018_final,sum)





###############


rm(com_2016_no2,com_2016_pm25,com_2016_pm10,com_2016_o3)
rm(com_2017_no2,com_2017_pm25,com_2017_pm10,com_2017_o3)
rm(com_2018_no2,com_2018_pm25,com_2018_pm10,com_2018_o3)

gc()







library(dplyr)

com_2019_final<-left_join(com_2019_no2,com_2019_pm25)
com_2019_final<-left_join(com_2019_final,com_2019_pm10)
com_2019_final<-left_join(com_2019_final,com_2019_o3)


com_2019_final$no2_pm25_pm10<-rowSums(com_2019_final[,c(5,7,8)])
com_2019_final$no2_pm25_pm10[com_2019_final$no2_pm25_pm10<3]<-0
com_2019_final$no2_pm25_pm10[com_2019_final$no2_pm25_pm10==3]<-1

com_2019_final_ag<-aggregate(no2_pm25_pm10~V_1,com_2019_final,sum)




com_2019_final$pm25_pm10<-rowSums(com_2019_final[,c(7,8)])
com_2019_final$pm25_pm10[com_2019_final$pm25_pm10<2]<-0
com_2019_final$pm25_pm10[com_2019_final$pm25_pm10==2]<-1

com_2019_final_ag_2<-aggregate(pm25_pm10~V_1,com_2019_final,sum)


#8 variables, 4 qui regardent juste au dessus des seuils et 4 prenant aussi l'exclusion des autres polluants



com_2019_final$no2_pm25<-rowSums(com_2019_final[,c(5,7)])
com_2019_final$no2_pm25[com_2019_final$no2_pm25<2]<-0
com_2019_final$no2_pm25[com_2019_final$no2_pm25==2]<-1

com_2019_final_ag_3<-aggregate(no2_pm25~V_1,com_2019_final,sum)




com_2019_final$no2_pm25_pm10_o3<-rowSums(com_2019_final[,c(5,7,8,9)])
com_2019_final$no2_pm25_pm10_o3[com_2019_final$no2_pm25_pm10_o3<4]<-0
com_2019_final$no2_pm25_pm10_o3[com_2019_final$no2_pm25_pm10_o3==4]<-1

com_2019_final_ag_p<-aggregate(no2_pm25_pm10_o3~V_1,com_2019_final,sum)




com_2019_final$pm10_no2<-rowSums(com_2019_final[,c(5,8)])
com_2019_final$pm10_no2[com_2019_final$pm10_no2<2]<-0
com_2019_final$pm10_no2[com_2019_final$pm10_no2==2]<-1

com_2019_final_ag_4<-aggregate(pm10_no2~V_1,com_2019_final,sum)






com_2019_final$pm10_o3<-rowSums(com_2019_final[,c(9,8)])
com_2019_final$pm10_o3[com_2019_final$pm10_o3<2]<-0
com_2019_final$pm10_o3[com_2019_final$pm10_o3==2]<-1

com_2019_final_ag_5<-aggregate(pm10_o3~V_1,com_2019_final,sum)







com_2019_final$pm25_o3<-rowSums(com_2019_final[,c(7,9)])
com_2019_final$pm25_o3[com_2019_final$pm25_o3<2]<-0
com_2019_final$pm25_o3[com_2019_final$pm25_o3==2]<-1

com_2019_final_ag_6<-aggregate(pm25_o3~V_1,com_2019_final,sum)






com_2019_final$no2_o3<-rowSums(com_2019_final[,c(5,9)])
com_2019_final$no2_o3[com_2019_final$no2_o3<2]<-0
com_2019_final$no2_o3[com_2019_final$no2_o3==2]<-1

com_2019_final_ag_7<-aggregate(no2_o3~V_1,com_2019_final,sum)






com_2019_final$pm10_no2_o3<-rowSums(com_2019_final[,c(8,9,5)])
com_2019_final$pm10_no2_o3[com_2019_final$pm10_no2_o3<3]<-0
com_2019_final$pm10_no2_o3[com_2019_final$pm10_no2_o3==3]<-1

com_2019_final_8<-aggregate(pm10_no2_o3~V_1,com_2019_final,sum)





com_2019_final$pm25_no2_o3<-rowSums(com_2019_final[,c(7,9,5)])
com_2019_final$pm25_no2_o3[com_2019_final$pm25_no2_o3<3]<-0
com_2019_final$pm25_no2_o3[com_2019_final$pm25_no2_o3==3]<-1

com_2019_final_9<-aggregate(pm25_no2_o3~V_1,com_2019_final,sum)






com_2019_final$pm10_pm25_o3<-rowSums(com_2019_final[,c(7,9,8)])
com_2019_final$pm10_pm25_o3[com_2019_final$pm10_pm25_o3<3]<-0
com_2019_final$pm10_pm25_o3[com_2019_final$pm10_pm25_o3==3]<-1

com_2019_final_10<-aggregate(pm10_pm25_o3~V_1,com_2019_final,sum)





####

com_2019_final$no2_pm25_pm10_2<-rowSums(com_2019_final[,c(5,7,8)])
com_2019_final$no2_pm25_pm10_2[com_2019_final$no2_pm25_pm10_2<3 | com_2019_final$o3==1]<-0
com_2019_final$no2_pm25_pm10_2[com_2019_final$no2_pm25_pm10_2==3]<-1

com_2019_final_ag_12<-aggregate(no2_pm25_pm10_2~V_1,com_2019_final,sum)




com_2019_final$pm25_pm10_2<-rowSums(com_2019_final[,c(7,8)])
com_2019_final$pm25_pm10_2[com_2019_final$pm25_pm10_2<2 | com_2019_final$o3==1 | com_2019_final$no2==1]<-0
com_2019_final$pm25_pm10_2[com_2019_final$pm25_pm10_2==2]<-1

com_2019_final_ag_13<-aggregate(pm25_pm10_2~V_1,com_2019_final,sum)



com_2019_final$no2_pm25_2<-rowSums(com_2019_final[,c(5,7)])
com_2019_final$no2_pm25_2[com_2019_final$no2_pm25_2<2 | com_2019_final$o3==1 | com_2019_final$pm10==1]<-0
com_2019_final$no2_pm25_2[com_2019_final$no2_pm25_2==2]<-1

com_2019_final_ag_14<-aggregate(no2_pm25_2~V_1,com_2019_final,sum)











com_2019_final$pm10_pm25_o3_2<-rowSums(com_2019_final[,c(9,7,8)])
com_2019_final$pm10_pm25_o3_2[com_2019_final$pm10_pm25_o3_2<3 | com_2019_final$no2==1]<-0
com_2019_final$pm10_pm25_o3_2[com_2019_final$pm10_pm25_o3_2==3]<-1

com_2019_final_ag_15<-aggregate(pm10_pm25_o3_2~V_1,com_2019_final,sum)



com_2019_final$pm25_no2_o3_2<-rowSums(com_2019_final[,c(5,7,9)])
com_2019_final$pm25_no2_o3_2[com_2019_final$pm25_no2_o3_2<3 | com_2019_final$pm10==1]<-0
com_2019_final$pm25_no2_o3_2[com_2019_final$pm25_no2_o3_2==3]<-1

com_2019_final_ag_16<-aggregate(pm25_no2_o3_2~V_1,com_2019_final,sum)


com_2019_final$pm25_2<-com_2019_final[,c(7)]
com_2019_final$pm25_2[com_2019_final$pm25_2<1 | com_2019_final$pm10==1 |com_2019_final$no2==1|com_2019_final$o3==1]<-0
com_2019_final$pm25_2[com_2019_final$pm25_2==1]<-1


com_2019_final_ag_17<-aggregate(pm25_2~V_1,com_2019_final,sum)



com_2019_final$pm10_2<-com_2019_final[,c(8)]
com_2019_final$pm10_2[com_2019_final$pm10_2<1 | com_2019_final$pm25==1 |com_2019_final$no2==1|com_2019_final$o3==1]<-0
com_2019_final$pm10_2[com_2019_final$pm10_2==1]<-1

com_2019_final_ag_18<-aggregate(pm10_2~V_1,com_2019_final,sum)



com_2019_final$no2_2<-com_2019_final[,c(5)]
com_2019_final$no2_2[com_2019_final$no2_2<1 | com_2019_final$pm25==1 |com_2019_final$pm10==1|com_2019_final$o3==1]<-0
com_2019_final$no2_2[com_2019_final$no2_2==1]<-1

com_2019_final_ag_19<-aggregate(no2_2~V_1,com_2019_final,sum)



com_2019_final$o3_2<-com_2019_final[,c(9)]
com_2019_final$o3_2[com_2019_final$o3_2<1 | com_2019_final$pm25==1 |com_2019_final$pm10==1|com_2019_final$no2==1]<-0
com_2019_final$o3_2[com_2019_final$o3_2==1]<-1

com_2019_final_ag_20<-aggregate(o3_2~V_1,com_2019_final,sum)



com_2019_final$pm25_o3_2<-rowSums(com_2019_final[,c(9,7)])
com_2019_final$pm25_o3_2[com_2019_final$pm25_o3_2<2 | com_2019_final$no2==1| com_2019_final$pm10==1]<-0
com_2019_final$pm25_o3_2[com_2019_final$pm25_o3_2==2]<-1

com_2019_final_ag_21<-aggregate(pm25_o3_2~V_1,com_2019_final,sum)



com_2019_final$no2_pm10_2<-rowSums(com_2019_final[,c(5,8)])
com_2019_final$no2_pm10_2[com_2019_final$no2_pm10_2<2 | com_2019_final$pm25==1| com_2019_final$o3==1]<-0
com_2019_final$no2_pm10_2[com_2019_final$no2_pm10_2==2]<-1

com_2019_final_ag_22<-aggregate(no2_pm10_2~V_1,com_2019_final,sum)


com_2019_final$no2_o3_2<-rowSums(com_2019_final[,c(5,9)])
com_2019_final$no2_o3_2[com_2019_final$no2_o3_2<2 | com_2019_final$pm25==1| com_2019_final$pm10==1]<-0
com_2019_final$no2_o3_2[com_2019_final$no2_o3_2==2]<-1

com_2019_final_ag_23<-aggregate(no2_o3_2~V_1,com_2019_final,sum)


com_2019_final$no2_pm10_o3_2<-rowSums(com_2019_final[,c(5,8,9)])
com_2019_final$no2_pm10_o3_2[com_2019_final$no2_pm10_o3_2<3 | com_2019_final$pm25==1]<-0
com_2019_final$no2_pm10_o3_2[com_2019_final$no2_pm10_o3_2==3]<-1


com_2019_final_ag_24<-aggregate(no2_pm10_o3_2~V_1,com_2019_final,sum)



com_2019_final$pm10_o3_2<-rowSums(com_2019_final[,c(8,9)])
com_2019_final$pm10_o3_2[com_2019_final$pm10_o3_2<2 | com_2019_final$pm25==1| com_2019_final$no2==1]<-0
com_2019_final$pm10_o3_2[com_2019_final$pm10_o3_2==2]<-1

com_2019_final_ag_25<-aggregate(pm10_o3_2~V_1,com_2019_final,sum)


















com_2009_final_ag_final<-left_join(com_2009_final_ag, com_2009_final_ag_2)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_3)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_p)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_4)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_5)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_6)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_7)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_8)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_9)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_10)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_14)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_12)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_13)

com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_15)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_16)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_17)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_18)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_19)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_20)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_21)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_23)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_24)
com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_25)

com_2009_final_ag_final<-left_join(com_2009_final_ag_final, com_2009_final_ag_22)




com_2010_final_ag_final<-left_join(com_2010_final_ag, com_2010_final_ag_2)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_3)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_p)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_4)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_5)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_6)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_7)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_8)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_9)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_10)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_14)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_12)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_13)

com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_15)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_16)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_17)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_18)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_19)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_20)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_21)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_23)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_24)
com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_25)

com_2010_final_ag_final<-left_join(com_2010_final_ag_final, com_2010_final_ag_22)





com_2011_final_ag_final<-left_join(com_2011_final_ag, com_2011_final_ag_2)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_3)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_p)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_4)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_5)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_6)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_7)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_8)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_9)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_10)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_14)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_12)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_13)

com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_15)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_16)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_17)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_18)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_19)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_20)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_21)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_23)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_24)
com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_25)

com_2011_final_ag_final<-left_join(com_2011_final_ag_final, com_2011_final_ag_22)


com_2012_final_ag_final<-left_join(com_2012_final_ag, com_2012_final_ag_2)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_3)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_p)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_4)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_5)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_6)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_7)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_8)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_9)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_10)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_14)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_12)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_13)

com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_15)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_16)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_17)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_18)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_19)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_20)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_21)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_23)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_24)
com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_25)

com_2012_final_ag_final<-left_join(com_2012_final_ag_final, com_2012_final_ag_22)



com_2013_final_ag_final<-left_join(com_2013_final_ag, com_2013_final_ag_2)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_3)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_p)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_4)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_5)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_6)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_7)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_8)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_9)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_10)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_14)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_12)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_13)

com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_15)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_16)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_17)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_18)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_19)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_20)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_21)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_23)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_24)
com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_25)

com_2013_final_ag_final<-left_join(com_2013_final_ag_final, com_2013_final_ag_22)


com_2014_final_ag_final<-left_join(com_2014_final_ag, com_2014_final_ag_2)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_3)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_p)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_4)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_5)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_6)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_7)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_8)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_9)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_10)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_14)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_12)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_13)

com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_15)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_16)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_17)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_18)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_19)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_20)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_21)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_23)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_24)
com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_25)

com_2014_final_ag_final<-left_join(com_2014_final_ag_final, com_2014_final_ag_22)




com_2015_final_ag_final<-left_join(com_2015_final_ag, com_2015_final_ag_2)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_3)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_p)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_4)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_5)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_6)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_7)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_8)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_9)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_10)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_14)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_12)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_13)

com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_15)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_16)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_17)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_18)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_19)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_20)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_21)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_23)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_24)
com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_25)


com_2015_final_ag_final<-left_join(com_2015_final_ag_final, com_2015_final_ag_22)






com_2016_final_ag_final<-left_join(com_2016_final_ag, com_2016_final_ag_2)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_3)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_p)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_4)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_5)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_6)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_7)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_8)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_9)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_10)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_14)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_12)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_13)

com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_15)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_16)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_17)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_18)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_19)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_20)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_21)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_23)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_24)
com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_25)


com_2016_final_ag_final<-left_join(com_2016_final_ag_final, com_2016_final_ag_22)





com_2017_final_ag_final<-left_join(com_2017_final_ag, com_2017_final_ag_2)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_3)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_p)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_4)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_5)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_6)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_7)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_8)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_9)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_10)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_14)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_12)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_13)

com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_15)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_16)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_17)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_18)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_19)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_20)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_21)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_23)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_24)
com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_25)


com_2017_final_ag_final<-left_join(com_2017_final_ag_final, com_2017_final_ag_22)





com_2018_final_ag_final<-left_join(com_2018_final_ag, com_2018_final_ag_2)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_3)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_p)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_4)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_5)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_6)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_7)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_8)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_9)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_10)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_14)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_12)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_13)

com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_15)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_16)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_17)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_18)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_19)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_20)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_21)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_23)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_24)
com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_25)



com_2018_final_ag_final<-left_join(com_2018_final_ag_final, com_2018_final_ag_22)



com_2019_final_ag_final<-left_join(com_2019_final_ag, com_2019_final_ag_2)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_3)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_p)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_4)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_5)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_6)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_7)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_8)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_9)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_10)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_14)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_12)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_13)

com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_15)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_16)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_17)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_18)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_19)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_20)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_21)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_23)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_24)
com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_25)

com_2019_final_ag_final<-left_join(com_2019_final_ag_final, com_2019_final_ag_22)






names(com_2009_final_ag_final)[names(com_2009_final_ag_final)=="V_1"]<-"insee"
com_2009_final_ag_final$annee<-2009



names(com_2010_final_ag_final)[names(com_2010_final_ag_final)=="V_1"]<-"insee"
com_2010_final_ag_final$annee<-2010




names(com_2011_final_ag_final)[names(com_2011_final_ag_final)=="V_1"]<-"insee"
com_2011_final_ag_final$annee<-2011




names(com_2012_final_ag_final)[names(com_2012_final_ag_final)=="V_1"]<-"insee"
com_2012_final_ag_final$annee<-2012



names(com_2013_final_ag_final)[names(com_2013_final_ag_final)=="V_1"]<-"insee"
com_2013_final_ag_final$annee<-2013




names(com_2014_final_ag_final)[names(com_2014_final_ag_final)=="V_1"]<-"insee"
com_2014_final_ag_final$annee<-2014




names(com_2015_final_ag_final)[names(com_2015_final_ag_final)=="V_1"]<-"insee"
com_2015_final_ag_final$annee<-2015



names(com_2016_final_ag_final)[names(com_2016_final_ag_final)=="V_1"]<-"insee"
com_2016_final_ag_final$annee<-2016


names(com_2017_final_ag_final)[names(com_2017_final_ag_final)=="V_1"]<-"insee"
com_2017_final_ag_final$annee<-2017



names(com_2018_final_ag_final)[names(com_2018_final_ag_final)=="V_1"]<-"insee"
com_2018_final_ag_final$annee<-2018




names(com_2019_final_ag_final)[names(com_2019_final_ag_final)=="V_1"]<-"insee"
com_2019_final_ag_final$annee<-2019





com_2009_2019_final_ag_final<-rbind(com_2009_final_ag_final,com_2010_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2011_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2012_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2013_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2014_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2015_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2016_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2017_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2018_final_ag_final)
com_2009_2019_final_ag_final<-rbind(com_2009_2019_final_ag_final,com_2019_final_ag_final)







#write.csv(com_2009_2019_final_ag_final,"air pollution cocktail/com_2009_2019_final_ag_final_final_2_2")





