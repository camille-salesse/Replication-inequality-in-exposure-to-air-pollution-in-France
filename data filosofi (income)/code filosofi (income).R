

setwd("D:/code and data inequality in exposure to air pollution")


library(dplyr)
library(writexl)
library(readxl)
data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")

revenu_dispo_2012<- read_excel("data filosofi (income)/2012/revenu dispo 2012.xlsx")
revenu_dispo_2012<-revenu_dispo_2012[,c("insee","mediane")]

names(data_fusion)[names(data_fusion)=="DepComA"]<-"insee"
test<-left_join(data_fusion,revenu_dispo_2012)
#test_<-test[!duplicated(test$insee), ]


ag_1<-aggregate(mediane~DepComN,test,mean)

library(writexl)
writexl::write_xlsx(ag_1, "data filosofi (income)/ag_1.xlsx")

write.csv2(ag_1, "data filosofi (income)/ag_1")

revenu_dispo_2013<- read_excel("data filosofi (income)/2013/revenu dispo 2013.xlsx")
revenu_dispo_2013<-revenu_dispo_2013[,c("insee","mediane")]

test_2<-left_join(data_fusion,revenu_dispo_2013)
ag_2<-aggregate(mediane~DepComN,test_2,mean)

write.csv2(ag_2, "data filosofi (income)/ag_2")
writexl::write_xlsx(ag_2, "data filosofi (income)/ag_2.xlsx")


revenu_dispo_2014 <- read_excel("data filosofi (income)/2014/revenu dispo 2014.xlsx")
revenu_dispo_2014<-revenu_dispo_2014[,c("insee","mediane")]

test_3<-left_join(data_fusion,revenu_dispo_2014)
ag_3<-aggregate(mediane~DepComN,test_3,mean)


write.csv2(ag_3, "data filosofi (income)/ag_3")
writexl::write_xlsx(ag_3, "data filosofi (income)/ag_3.xlsx")


revenu_dispo_2015 <- read_excel("data filosofi (income)/2015/revenu dispo 2015.xlsx")
revenu_dispo_2015<-revenu_dispo_2015[,c("insee","mediane")]

test_4<-left_join(data_fusion,revenu_dispo_2015)
ag_4<-aggregate(mediane~DepComN,test_4,mean)

write.csv2(ag_4, "data filosofi (income)/ag_4")
writexl::write_xlsx(ag_4, "data filosofi (income)/ag_4.xlsx")



revenu_dispo_2016 <- read_excel("data filosofi (income)/2016/revenu dispo 2016.xlsx")
revenu_dispo_2016<-revenu_dispo_2016[,c("insee","mediane")]

test_5<-left_join(data_fusion,revenu_dispo_2016)
ag_5<-aggregate(mediane~DepComN,test_5,mean)

write.csv2(ag_5, "data filosofi (income)/ag_5")
writexl::write_xlsx(ag_5, "data filosofi (income)/ag_5.xlsx")


donne_niv_vie_2017_filosofi <- read_excel("data filosofi (income)/2017/donne niv vie 2017 filosofi.xlsx")
revenu_disponible_2017<-donne_niv_vie_2017_filosofi[,c("insee","mediane")]

test_6<-left_join(data_fusion,revenu_disponible_2017)
ag_6<-aggregate(mediane~DepComN,test_6,mean)

write.csv2(ag_6, "data filosofi (income)/ag_6")
writexl::write_xlsx(ag_6, "data filosofi (income)/ag_6.xlsx")



revenu_disponible_2018 <- read_excel("data filosofi (income)/2018/revenu disponible.xlsx")
revenu_disponible_2018<-revenu_disponible_2018[,c("insee","mediane")]

test_7<-left_join(data_fusion,revenu_disponible_2018)
ag_7<-aggregate(mediane~DepComN,test_7,mean)

write.csv2(ag_7, "data filosofi (income)/ag_7")
writexl::write_xlsx(ag_7, "data filosofi (income)/ag_7.xlsx")



#difference in observations between "ag_" due to NA




#filosofi#####


rm(list = ls())
gc()


library(readxl)
library(dplyr)

revenu_disponible_2018 <- read_excel("data filosofi (income)/2018/revenu disponible.xlsx")
revenu_disponible_2018<-revenu_disponible_2018[,c("insee","mediane")]
revenu_disponible_2018$annee<-2018


library(readxl)
ag_7 <- read_excel("data filosofi (income)/ag_7.xlsx")
names(ag_7)[names(ag_7)=="DepComN"]<-"insee"
ag_7$annee<-2018

names(ag_7)[names(ag_7)=="mediane"]<-"mediane2"
ag_71<-full_join(revenu_disponible_2018,ag_7)

ag_71$mediane3<-ifelse(is.na(ag_71$mediane2),ag_71$mediane,ag_71$mediane2)

#com_sup <- read_excel("data filosofi (income)/com_sup.xlsx")
#names(com_sup)[names(com_sup)=="DepComA"]<-"insee"
#com_sup$a<-1
#ag_71<-left_join(ag_71, com_sup)
#ag_71<-filter(ag_71, is.na(ag_71$a))






donne_niv_vie_2017_filosofi <- read_excel("data filosofi (income)/2017/donne niv vie 2017 filosofi.xlsx")
revenu_disponible_2017<-donne_niv_vie_2017_filosofi[,c("insee","mediane")]
revenu_disponible_2017$annee<-2017

ag_6 <- read_excel("data filosofi (income)/ag_6.xlsx")
names(ag_6)[names(ag_6)=="DepComN"]<-"insee"
ag_6$annee<-2017
names(ag_6)[names(ag_6)=="mediane"]<-"mediane2"
ag_61<-full_join(revenu_disponible_2017,ag_6)

ag_61$mediane3<-ifelse(is.na(ag_61$mediane2),ag_61$mediane,ag_61$mediane2)






revenu_dispo_2016 <- read_excel("data filosofi (income)/2016/revenu dispo 2016.xlsx")
revenu_dispo_2016<-revenu_dispo_2016[,c("insee","mediane")]
revenu_dispo_2016$annee<-2016

ag_5 <- read_excel("data filosofi (income)/ag_5.xlsx")
names(ag_5)[names(ag_5)=="DepComN"]<-"insee"
ag_5$annee<-2016
names(ag_5)[names(ag_5)=="mediane"]<-"mediane2"
ag_51<-full_join(revenu_dispo_2016,ag_5)

ag_51$mediane3<-ifelse(is.na(ag_51$mediane2),ag_51$mediane,ag_51$mediane2)




revenu_dispo_2015 <- read_excel("data filosofi (income)/2015/revenu dispo 2015.xlsx")
revenu_dispo_2015<-revenu_dispo_2015[,c("insee","mediane")]
revenu_dispo_2015$annee<-2015


ag_4 <- read_excel("data filosofi (income)/ag_4.xlsx")
names(ag_4)[names(ag_4)=="DepComN"]<-"insee"
ag_4$annee<-2015
names(ag_4)[names(ag_4)=="mediane"]<-"mediane2"
ag_41<-full_join(revenu_dispo_2015,ag_4)

ag_41$mediane3<-ifelse(is.na(ag_41$mediane2),ag_41$mediane,ag_41$mediane2)




revenu_dispo_2014 <- read_excel("data filosofi (income)/2014/revenu dispo 2014.xlsx")
revenu_dispo_2014<-revenu_dispo_2014[,c("insee","mediane")]
revenu_dispo_2014$annee<-2014


ag_3 <- read_excel("data filosofi (income)/ag_3.xlsx")
names(ag_3)[names(ag_3)=="DepComN"]<-"insee"
ag_3$annee<-2014
names(ag_3)[names(ag_3)=="mediane"]<-"mediane2"
ag_31<-full_join(revenu_dispo_2014,ag_3)

ag_31$mediane3<-ifelse(is.na(ag_31$mediane2),ag_31$mediane,ag_31$mediane2)





revenu_dispo_2013<- read_excel("data filosofi (income)/2013/revenu dispo 2013.xlsx")
revenu_dispo_2013<-revenu_dispo_2013[,c("insee","mediane")]
revenu_dispo_2013$annee<-2013





ag_2 <- read_excel("data filosofi (income)/ag_2.xlsx")
names(ag_2)[names(ag_2)=="DepComN"]<-"insee"
ag_2$annee<-2013
names(ag_2)[names(ag_2)=="mediane"]<-"mediane2"
ag_21<-full_join(revenu_dispo_2013,ag_2)

ag_21$mediane3<-ifelse(is.na(ag_21$mediane2),ag_21$mediane,ag_21$mediane2)

a<-table(ag_21$insee)
a<-as.data.frame(a)











revenu_dispo_2012<- read_excel("data filosofi (income)/2012/revenu dispo 2012.xlsx")
revenu_dispo_2012<-revenu_dispo_2012[,c("insee","mediane")]
revenu_dispo_2012$annee<-2012




ag_1 <- read_excel("data filosofi (income)/ag_1.xlsx")
names(ag_1)[names(ag_1)=="DepComN"]<-"insee"
ag_1$annee<-2012
names(ag_1)[names(ag_1)=="mediane"]<-"mediane2"
ag_11<-full_join(revenu_dispo_2012,ag_1)

ag_11$mediane3<-ifelse(is.na(ag_11$mediane2),ag_11$mediane,ag_11$mediane2)

a<-table(ag_11$insee)
a<-as.data.frame(a)









data_filo<-rbind(ag_11,ag_21)
data_filo<-rbind(data_filo,ag_31)
data_filo<-rbind(data_filo,ag_41)
data_filo<-rbind(data_filo,ag_51)
data_filo<-rbind(data_filo,ag_61)
data_filo<-rbind(data_filo,ag_71)

a<-table(data_filo$insee)
a<-as.data.frame(a)
b<-filter(a, a$Freq<7)
names(b)[names(b)=="Var1"]<-"insee"
data_filo<-left_join(data_filo,b)


data_filo<-filter(data_filo, is.na(data_filo$Freq))

a<-table(data_filo$insee)
a<-as.data.frame(a)


b<-table(revenu_disponible_2018$insee)
b<-as.data.frame(b)

ac<-rbind(a,b)
acd<-table(ac$Var1)
acd<-as.data.frame(acd)

#158 municipalities difference with filosofi 2018 due to NA 


data_filo<-data_filo[,c("insee","annee","mediane3")]
names(data_filo)[names(data_filo)=="mediane3"]<-"mediane"


library(writexl)
#writexl::write_xlsx(data_filo, "data filosofi (income)/base_filo_2012_2018_com.xlsx")








#test to see if the former municipalities are well gone:
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




#filosofi poverty data #####


rm(list = ls())
gc()



library(readxl)
library(dplyr)

revenu_disponible_2018 <- read_excel("data filosofi (income)/poverty data/revenu_disponible_2018.xlsx")
revenu_disponible_2018<-revenu_disponible_2018[,c("insee","D118","D918","PPSOC18","PPAT18")]
revenu_disponible_2018$annee<-2018
names(revenu_disponible_2018)[names(revenu_disponible_2018)=="D118"]<-"premier_decile"
names(revenu_disponible_2018)[names(revenu_disponible_2018)=="D918"]<-"neuvieme_decile"
names(revenu_disponible_2018)[names(revenu_disponible_2018)=="PPSOC18"]<-"part_prestation_sociale"
names(revenu_disponible_2018)[names(revenu_disponible_2018)=="PPAT18"]<-"part_revenu_patrimoine_et_autre_revenu"

taux_pauvre_2018 <- read_excel("data filosofi (income)/poverty data/taux_pauvrete_2018.xlsx")
taux_pauvre_2018<-taux_pauvre_2018[,c("insee","taux_pauvrete_seuil_60")]
taux_pauvre_2018$annee<-2018

revenu_disponible_2018<-left_join(revenu_disponible_2018, taux_pauvre_2018)



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2018)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2018.xlsx")

library(readxl)
ag_pauvrete_2018 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2018.xlsx")
names(ag_pauvrete_2018)[names(ag_pauvrete_2018)=="DepComN"]<-"insee"


ag_pauvrete_1_2018<-full_join(revenu_disponible_2018,ag_pauvrete_2018, by= c("insee"))

ag_pauvrete_1_2018$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2018$premier_decile.y),ag_pauvrete_1_2018$premier_decile.x,ag_pauvrete_1_2018$premier_decile.y)
ag_pauvrete_1_2018$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2018$neuvieme_decile.y),ag_pauvrete_1_2018$neuvieme_decile.x,ag_pauvrete_1_2018$neuvieme_decile.y)
ag_pauvrete_1_2018$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2018$part_prestation_sociale.y),ag_pauvrete_1_2018$part_prestation_sociale.x,ag_pauvrete_1_2018$part_prestation_sociale.y)
ag_pauvrete_1_2018$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2018$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2018$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2018$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2018$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2018$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2018$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2018$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2018<-ag_pauvrete_1_2018[,c(1,14:18)]

a<-table(ag_pauvrete_1_2018$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2018)[names(ag_pauvrete_1_2018)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2018)[names(ag_pauvrete_1_2018)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2018)[names(ag_pauvrete_1_2018)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2018)[names(ag_pauvrete_1_2018)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2018)[names(ag_pauvrete_1_2018)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2018$a<-rowSums(ag_pauvrete_1_2018[,c(7:13)])
#test : ok
ag_pauvrete_1_2018$annee<-2018



###########





library(readxl)
library(dplyr)

revenu_disponible_2017 <- read_excel("data filosofi (income)/poverty data/donne niv vie 2017 filosofi.xlsx")
revenu_disponible_2017<-revenu_disponible_2017[,c("insee","D117","D917","PPSOC17","PPAT17")]
revenu_disponible_2017$annee<-2017
names(revenu_disponible_2017)[names(revenu_disponible_2017)=="D117"]<-"premier_decile"
names(revenu_disponible_2017)[names(revenu_disponible_2017)=="D917"]<-"neuvieme_decile"
names(revenu_disponible_2017)[names(revenu_disponible_2017)=="PPSOC17"]<-"part_prestation_sociale"
names(revenu_disponible_2017)[names(revenu_disponible_2017)=="PPAT17"]<-"part_revenu_patrimoine_et_autre_revenu"

taux_pauvrete_2017 <- read_excel("data filosofi (income)/poverty data/taux_pauvrete_2017.xlsx")
taux_pauvrete_2017<-taux_pauvrete_2017[,c("insee","taux_pauvrete_seuil_60")]
taux_pauvrete_2017$annee<-2017

revenu_disponible_2017<-left_join(revenu_disponible_2017, taux_pauvrete_2017)



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2017)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2017.xlsx")

library(readxl)
ag_pauvrete_2017 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2017.xlsx")
names(ag_pauvrete_2017)[names(ag_pauvrete_2017)=="DepComN"]<-"insee"


ag_pauvrete_1_2017<-full_join(revenu_disponible_2017,ag_pauvrete_2017, by= c("insee"))

ag_pauvrete_1_2017$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2017$premier_decile.y),ag_pauvrete_1_2017$premier_decile.x,ag_pauvrete_1_2017$premier_decile.y)
ag_pauvrete_1_2017$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2017$neuvieme_decile.y),ag_pauvrete_1_2017$neuvieme_decile.x,ag_pauvrete_1_2017$neuvieme_decile.y)
ag_pauvrete_1_2017$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2017$part_prestation_sociale.y),ag_pauvrete_1_2017$part_prestation_sociale.x,ag_pauvrete_1_2017$part_prestation_sociale.y)
ag_pauvrete_1_2017$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2017$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2017$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2017$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2017$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2017$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2017$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2017$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2017<-ag_pauvrete_1_2017[,c(1,14:18)]

a<-table(ag_pauvrete_1_2017$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2017)[names(ag_pauvrete_1_2017)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2017)[names(ag_pauvrete_1_2017)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2017)[names(ag_pauvrete_1_2017)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2017)[names(ag_pauvrete_1_2017)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2017)[names(ag_pauvrete_1_2017)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2017$a<-rowSums(ag_pauvrete_1_2017[,c(7:13)])
#test : ok
ag_pauvrete_1_2017$annee<-2017


############







library(readxl)
library(dplyr)

revenu_disponible_2016 <- read_excel("data filosofi (income)/poverty data/revenu dispo 2016.xlsx")
revenu_disponible_2016<-revenu_disponible_2016[,c("insee","D116","D916","PPSOC16","PPAT16","TP6016")]
revenu_disponible_2016$annee<-2016
names(revenu_disponible_2016)[names(revenu_disponible_2016)=="D116"]<-"premier_decile"
names(revenu_disponible_2016)[names(revenu_disponible_2016)=="D916"]<-"neuvieme_decile"
names(revenu_disponible_2016)[names(revenu_disponible_2016)=="PPSOC16"]<-"part_prestation_sociale"
names(revenu_disponible_2016)[names(revenu_disponible_2016)=="PPAT16"]<-"part_revenu_patrimoine_et_autre_revenu"
names(revenu_disponible_2016)[names(revenu_disponible_2016)=="TP6016"]<-"taux_pauvrete_seuil_60"



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2016)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2016.xlsx")

library(readxl)
ag_pauvrete_2016 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2016.xlsx")
names(ag_pauvrete_2016)[names(ag_pauvrete_2016)=="DepComN"]<-"insee"


ag_pauvrete_1_2016<-full_join(revenu_disponible_2016,ag_pauvrete_2016, by= c("insee"))

ag_pauvrete_1_2016$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2016$premier_decile.y),ag_pauvrete_1_2016$premier_decile.x,ag_pauvrete_1_2016$premier_decile.y)
ag_pauvrete_1_2016$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2016$neuvieme_decile.y),ag_pauvrete_1_2016$neuvieme_decile.x,ag_pauvrete_1_2016$neuvieme_decile.y)
ag_pauvrete_1_2016$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2016$part_prestation_sociale.y),ag_pauvrete_1_2016$part_prestation_sociale.x,ag_pauvrete_1_2016$part_prestation_sociale.y)
ag_pauvrete_1_2016$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2016$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2016$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2016$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2016$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2016$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2016$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2016$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2016<-ag_pauvrete_1_2016[,c(1,14:18)]

a<-table(ag_pauvrete_1_2016$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2016)[names(ag_pauvrete_1_2016)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2016)[names(ag_pauvrete_1_2016)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2016)[names(ag_pauvrete_1_2016)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2016)[names(ag_pauvrete_1_2016)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2016)[names(ag_pauvrete_1_2016)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2016$a<-rowSums(ag_pauvrete_1_2016[,c(7:13)])
#test : ok
ag_pauvrete_1_2016$annee<-2016


##########








library(readxl)
library(dplyr)

revenu_disponible_2015 <- read_excel("data filosofi (income)/poverty data/revenu dispo 2015.xlsx")
revenu_disponible_2015<-revenu_disponible_2015[,c("insee","D115","D915","PPSOC15","PPAT15","TP6015")]
revenu_disponible_2015$annee<-2015
names(revenu_disponible_2015)[names(revenu_disponible_2015)=="D115"]<-"premier_decile"
names(revenu_disponible_2015)[names(revenu_disponible_2015)=="D915"]<-"neuvieme_decile"
names(revenu_disponible_2015)[names(revenu_disponible_2015)=="PPSOC15"]<-"part_prestation_sociale"
names(revenu_disponible_2015)[names(revenu_disponible_2015)=="PPAT15"]<-"part_revenu_patrimoine_et_autre_revenu"
names(revenu_disponible_2015)[names(revenu_disponible_2015)=="TP6015"]<-"taux_pauvrete_seuil_60"



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2015)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2015.xlsx")

library(readxl)
ag_pauvrete_2015 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2015.xlsx")
names(ag_pauvrete_2015)[names(ag_pauvrete_2015)=="DepComN"]<-"insee"


ag_pauvrete_1_2015<-full_join(revenu_disponible_2015,ag_pauvrete_2015, by= c("insee"))

ag_pauvrete_1_2015$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2015$premier_decile.y),ag_pauvrete_1_2015$premier_decile.x,ag_pauvrete_1_2015$premier_decile.y)
ag_pauvrete_1_2015$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2015$neuvieme_decile.y),ag_pauvrete_1_2015$neuvieme_decile.x,ag_pauvrete_1_2015$neuvieme_decile.y)
ag_pauvrete_1_2015$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2015$part_prestation_sociale.y),ag_pauvrete_1_2015$part_prestation_sociale.x,ag_pauvrete_1_2015$part_prestation_sociale.y)
ag_pauvrete_1_2015$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2015$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2015$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2015$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2015$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2015$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2015$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2015$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2015<-ag_pauvrete_1_2015[,c(1,14:18)]

a<-table(ag_pauvrete_1_2015$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2015)[names(ag_pauvrete_1_2015)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2015)[names(ag_pauvrete_1_2015)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2015)[names(ag_pauvrete_1_2015)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2015)[names(ag_pauvrete_1_2015)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2015)[names(ag_pauvrete_1_2015)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2015$a<-rowSums(ag_pauvrete_1_2015[,c(7:13)])
#test : ok
ag_pauvrete_1_2015$annee<-2015




#############



library(readxl)
library(dplyr)

revenu_disponible_2014 <- read_excel("data filosofi (income)/poverty data/revenu dispo 2014.xlsx")
revenu_disponible_2014<-revenu_disponible_2014[,c("insee","D114","D914","PPSOC14","PPAT14","TP6014")]
revenu_disponible_2014$annee<-2014
names(revenu_disponible_2014)[names(revenu_disponible_2014)=="D114"]<-"premier_decile"
names(revenu_disponible_2014)[names(revenu_disponible_2014)=="D914"]<-"neuvieme_decile"
names(revenu_disponible_2014)[names(revenu_disponible_2014)=="PPSOC14"]<-"part_prestation_sociale"
names(revenu_disponible_2014)[names(revenu_disponible_2014)=="PPAT14"]<-"part_revenu_patrimoine_et_autre_revenu"
names(revenu_disponible_2014)[names(revenu_disponible_2014)=="TP6014"]<-"taux_pauvrete_seuil_60"



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2014)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2014.xlsx")

library(readxl)
ag_pauvrete_2014 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2014.xlsx")
names(ag_pauvrete_2014)[names(ag_pauvrete_2014)=="DepComN"]<-"insee"


ag_pauvrete_1_2014<-full_join(revenu_disponible_2014,ag_pauvrete_2014, by= c("insee"))

ag_pauvrete_1_2014$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2014$premier_decile.y),ag_pauvrete_1_2014$premier_decile.x,ag_pauvrete_1_2014$premier_decile.y)
ag_pauvrete_1_2014$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2014$neuvieme_decile.y),ag_pauvrete_1_2014$neuvieme_decile.x,ag_pauvrete_1_2014$neuvieme_decile.y)
ag_pauvrete_1_2014$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2014$part_prestation_sociale.y),ag_pauvrete_1_2014$part_prestation_sociale.x,ag_pauvrete_1_2014$part_prestation_sociale.y)
ag_pauvrete_1_2014$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2014$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2014$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2014$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2014$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2014$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2014$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2014$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2014<-ag_pauvrete_1_2014[,c(1,14:18)]

a<-table(ag_pauvrete_1_2014$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2014)[names(ag_pauvrete_1_2014)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2014)[names(ag_pauvrete_1_2014)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2014)[names(ag_pauvrete_1_2014)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2014)[names(ag_pauvrete_1_2014)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2014)[names(ag_pauvrete_1_2014)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2014$a<-rowSums(ag_pauvrete_1_2014[,c(7:13)])
#test : ok
ag_pauvrete_1_2014$annee<-2014



############






library(readxl)
library(dplyr)

revenu_disponible_2013 <- read_excel("data filosofi (income)/poverty data/revenu dispo 2013 2.xlsx")
revenu_disponible_2013<-revenu_disponible_2013[,c("insee","D113","D913","PPSOC13","PPAT13","TP6013")]
revenu_disponible_2013$annee<-2013
names(revenu_disponible_2013)[names(revenu_disponible_2013)=="D113"]<-"premier_decile"
names(revenu_disponible_2013)[names(revenu_disponible_2013)=="D913"]<-"neuvieme_decile"
names(revenu_disponible_2013)[names(revenu_disponible_2013)=="PPSOC13"]<-"part_prestation_sociale"
names(revenu_disponible_2013)[names(revenu_disponible_2013)=="PPAT13"]<-"part_revenu_patrimoine_et_autre_revenu"
names(revenu_disponible_2013)[names(revenu_disponible_2013)=="TP6013"]<-"taux_pauvrete_seuil_60"



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2013)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2013.xlsx")

library(readxl)
ag_pauvrete_2013 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2013.xlsx")
names(ag_pauvrete_2013)[names(ag_pauvrete_2013)=="DepComN"]<-"insee"


ag_pauvrete_1_2013<-full_join(revenu_disponible_2013,ag_pauvrete_2013, by= c("insee"))

ag_pauvrete_1_2013$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2013$premier_decile.y),ag_pauvrete_1_2013$premier_decile.x,ag_pauvrete_1_2013$premier_decile.y)
ag_pauvrete_1_2013$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2013$neuvieme_decile.y),ag_pauvrete_1_2013$neuvieme_decile.x,ag_pauvrete_1_2013$neuvieme_decile.y)
ag_pauvrete_1_2013$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2013$part_prestation_sociale.y),ag_pauvrete_1_2013$part_prestation_sociale.x,ag_pauvrete_1_2013$part_prestation_sociale.y)
ag_pauvrete_1_2013$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2013$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2013$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2013$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2013$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2013$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2013$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2013$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2013<-ag_pauvrete_1_2013[,c(1,14:18)]

a<-table(ag_pauvrete_1_2013$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2013)[names(ag_pauvrete_1_2013)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2013)[names(ag_pauvrete_1_2013)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2013)[names(ag_pauvrete_1_2013)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2013)[names(ag_pauvrete_1_2013)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2013)[names(ag_pauvrete_1_2013)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2013$a<-rowSums(ag_pauvrete_1_2013[,c(7:13)])
#test : ok
ag_pauvrete_1_2013$annee<-2013





#############







library(readxl)
library(dplyr)

revenu_disponible_2012 <- read_excel("data filosofi (income)/poverty data/revenu dispo 2012.xlsx")
revenu_disponible_2012<-revenu_disponible_2012[,c("insee","D112","D912","PPSOC12","PPAT12","TP6012")]
revenu_disponible_2012$annee<-2012
names(revenu_disponible_2012)[names(revenu_disponible_2012)=="D112"]<-"premier_decile"
names(revenu_disponible_2012)[names(revenu_disponible_2012)=="D912"]<-"neuvieme_decile"
names(revenu_disponible_2012)[names(revenu_disponible_2012)=="PPSOC12"]<-"part_prestation_sociale"
names(revenu_disponible_2012)[names(revenu_disponible_2012)=="PPAT12"]<-"part_revenu_patrimoine_et_autre_revenu"
names(revenu_disponible_2012)[names(revenu_disponible_2012)=="TP6012"]<-"taux_pauvrete_seuil_60"



library(readxl)

data_fusion <- read_excel("merge of municipality/data_fusion.xlsx")
test<-left_join(data_fusion,revenu_disponible_2012)
test<-test[,c(1,5:10)]

ag_1<-aggregate(.~DepComN,test,mean)

library(writexl)
#writexl::write_xlsx(ag_1, "data filosofi (income)/poverty data/ag_pauvrete_2012.xlsx")

library(readxl)
ag_pauvrete_2012 <- read_excel("data filosofi (income)/poverty data/ag_pauvrete_2012.xlsx")
names(ag_pauvrete_2012)[names(ag_pauvrete_2012)=="DepComN"]<-"insee"


ag_pauvrete_1_2012<-full_join(revenu_disponible_2012,ag_pauvrete_2012, by= c("insee"))

ag_pauvrete_1_2012$premier_decile.b<-ifelse(is.na(ag_pauvrete_1_2012$premier_decile.y),ag_pauvrete_1_2012$premier_decile.x,ag_pauvrete_1_2012$premier_decile.y)
ag_pauvrete_1_2012$neuvieme_decile.b<-ifelse(is.na(ag_pauvrete_1_2012$neuvieme_decile.y),ag_pauvrete_1_2012$neuvieme_decile.x,ag_pauvrete_1_2012$neuvieme_decile.y)
ag_pauvrete_1_2012$part_prestation_sociale.b<-ifelse(is.na(ag_pauvrete_1_2012$part_prestation_sociale.y),ag_pauvrete_1_2012$part_prestation_sociale.x,ag_pauvrete_1_2012$part_prestation_sociale.y)
ag_pauvrete_1_2012$part_revenu_patrimoine_et_autre_revenu.b<-ifelse(is.na(ag_pauvrete_1_2012$part_revenu_patrimoine_et_autre_revenu.y),ag_pauvrete_1_2012$part_revenu_patrimoine_et_autre_revenu.x,ag_pauvrete_1_2012$part_revenu_patrimoine_et_autre_revenu.y)
ag_pauvrete_1_2012$taux_pauvrete_seuil_60.b<-ifelse(is.na(ag_pauvrete_1_2012$taux_pauvrete_seuil_60.y),ag_pauvrete_1_2012$taux_pauvrete_seuil_60.x,ag_pauvrete_1_2012$taux_pauvrete_seuil_60.y)


ag_pauvrete_1_2012<-ag_pauvrete_1_2012[,c(1,14:18)]

a<-table(ag_pauvrete_1_2012$insee)
a<-as.data.frame(a)


names(ag_pauvrete_1_2012)[names(ag_pauvrete_1_2012)=="premier_decile.b"]<-"premier_decile"
names(ag_pauvrete_1_2012)[names(ag_pauvrete_1_2012)=="neuvieme_decile.b"]<-"neuvieme_decile"
names(ag_pauvrete_1_2012)[names(ag_pauvrete_1_2012)=="part_prestation_sociale.b"]<-"part_prestation_sociale"
names(ag_pauvrete_1_2012)[names(ag_pauvrete_1_2012)=="part_revenu_patrimoine_et_autre_revenu.b"]<-"part_revenu_patrimoine_et_autre_revenu"
names(ag_pauvrete_1_2012)[names(ag_pauvrete_1_2012)=="taux_pauvrete_seuil_60.b"]<-"taux_pauvrete_seuil_60"

#ag_pauvrete_1_2012$a<-rowSums(ag_pauvrete_1_2012[,c(7:13)])
#test : ok
ag_pauvrete_1_2012$annee<-2012



















data_pauvrete<-rbind(ag_pauvrete_1_2012,ag_pauvrete_1_2013)
data_pauvrete<-rbind(data_pauvrete,ag_pauvrete_1_2014)
data_pauvrete<-rbind(data_pauvrete,ag_pauvrete_1_2015)
data_pauvrete<-rbind(data_pauvrete,ag_pauvrete_1_2016)
data_pauvrete<-rbind(data_pauvrete,ag_pauvrete_1_2017)
data_pauvrete<-rbind(data_pauvrete,ag_pauvrete_1_2018)


library(writexl)
#writexl::write_xlsx(data_pauvrete, "data filosofi (income)/poverty data/data_pauvrete.xlsx")





