
#over the period several municipalities merging, we build a passage table between old and new municipalities ("communes_nouvelles").


setwd("D:/code and data inequality in exposure to air pollution")


library(dplyr)
library(writexl)
library(readxl)

communes_nouvelles_2015_2 <- read_excel("merge of municipality/communes_nouvelles_2015_2.xlsx")

communes_nouvelles_2016_2 <- read_excel("merge of municipality/communes_nouvelles_2016_2.xlsx")

communes_nouvelles_2017_2 <- read_excel("merge of municipality/communes_nouvelles_2017_2.xlsx")

communes_nouvelles_2018_2 <- read_excel("merge of municipality/communes_nouvelles_2018_2.xlsx")

communes_nouvelles_2014_2013_main <- read_excel("merge of municipality/communes_nouvelles_2014_2013_main.xlsx")
communes_nouvelles_2014_2013_main$DepComN<-as.character(communes_nouvelles_2014_2013_main$DepComN)
communes_nouvelles_2014_2013_main$DepComA<-as.character(communes_nouvelles_2014_2013_main$DepComA)


data_fusion<-rbind(communes_nouvelles_2015_2,communes_nouvelles_2016_2)
data_fusion<-rbind(data_fusion,communes_nouvelles_2017_2)
data_fusion<-rbind(data_fusion,communes_nouvelles_2018_2)
data_fusion<-rbind(data_fusion,communes_nouvelles_2014_2013_main)


#data<-data_fusion[,c("DepComA","NomCA")]
#writexl::write_xlsx(data, "merge of municipality/com_sup.xlsx")


a<-table(data_fusion$DepComA)
a<-as.data.frame(a)
a<-filter(a, a$Freq>1)

names(a)[names(a)=="Var1"]<-"DepComA"
a<-left_join(a,data_fusion)
ab<-filter(a, a$DepComA!=a$DepComN) #identifies the municipality that first merged and then merged again


#municipalities with multiple mergers (creation of 1st commune from merger then recreation of commune from merger)
#49261
#49080
#49065
#49149
#solution: directly merge the municipalities that make up the mergers ("cancel" the first merger to directly create the 2nd):


data_fusion$DepComN[data_fusion$DepComN=="49065"]<-"49080"
data_fusion$DepComN[data_fusion$DepComN=="49149"]<-"49261"

data_fusion <- data_fusion %>% group_by(DepComN)

data_fusion<-data_fusion[!duplicated(data_fusion$DepComA), ]

names(data_fusion)[names(data_fusion)=="DepComA"]<-"insee"

#writexl::write_xlsx(data_fusion, "merge of municipality/data_fusion.xlsx")









