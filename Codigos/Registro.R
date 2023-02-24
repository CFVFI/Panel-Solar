library(dplyr)
#Aqui econtraremos todos los datos correspondiente a la Corriente

###Enero 2013####
dfEN13 <- read.csv("Data/Years/2013/ENE2013.csv")


dfEN13 <- dfEN13[1:583,1:8] 
dfEN13$Fecha.de.la.Medida <- as.Date(dfEN13$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfEN13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in
     1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN13, Fecha.de.la.Medida==Dias[i]))
}

names(dfEN13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
####IENE1-IENE3###
mediaIENE <- vector("numeric", ncol(dfEN13[3:5]))
for (i in seq_along(dfEN13[3:5])) {# 2. secuencia
  mediaIENE[[i]] <- mean(ifelse(dfEN13[3:5][[i]]<=0,NA,dfEN13[3:5][[i]]),
                         na.rm = T )    # 3. cuerpo
}
maxIENE<- vector("numeric", ncol(dfEN13[3:5]))
for (i in seq_along(dfEN13[3:5])) {# 2. secuencia
  maxIENE[[i]] <- max(dfEN13[3:5][[i]] )    # 3. cuerpo
}
##14-enero a 15-enero##
###Dia 14###
media_dia_14IENE<-vector("numeric",  ncol(dfEN13[3:5]))
for (i in seq_along(Dia_14[3:5])) {
  media_dia_14IENE[[i]] <- mean(ifelse(Dia_14[3:5][[i]]<=0,NA,Dia_14[3:5][[i]]),
                                na.rm=T)
}
max_dia_14IENE <- vector("numeric",  ncol(dfEN13[3:5]))
for (i in seq_along(Dia_14[3:5])) {
  max_dia_14IENE[[i]] <- max(Dia_14[3:5][[i]])
}
###Dia 15###
media_dia_15IENE<-vector("numeric",  ncol(dfEN13[3:5]))
for (i in seq_along(Dia_15[3:5])) {
  media_dia_15IENE[[i]] <- mean(ifelse(Dia_15[3:5][[i]]<=0,NA,Dia_15[3:5][[i]]),
                                na.rm=T)
}
max_dia_15IENE <- vector("numeric",  ncol(dfEN13[3:5]))
for (i in seq_along(Dia_15[3:5])) {
  max_dia_15IENE[[i]] <- max(Dia_15[3:5][[i]])
}
###Dia 16###
media_dia_16IENE<-vector("numeric",  ncol(dfEN13[3:5]))

for (i in seq_along(Dia_16[3:5])) {
  media_dia_16IENE[[i]] <- mean(ifelse(Dia_16[3:5][[i]]<=0,NA,Dia_16[3:5][[i]]),
                                na.rm=T)
}
max_dia_16IENE <- vector("numeric",  ncol(dfEN13[3:5]))
for (i in seq_along(Dia_16[3:5])) {
  max_dia_16IENE[[i]] <- max(Dia_16[3:5][[i]])
}
medias_diasIEN13 <- rbind(media_dia_14IENE,media_dia_15IENE,media_dia_16IENE)
##View(medias_diasI)
medias_diasIEN13 <- cbind(levels(DiasF),medias_diasIEN13)
medias_diasIEN13 <- as.data.frame(medias_diasIEN13)
##View(medias_diasIEN13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN13) <- nombres
medias_diasIEN13$Fecha <- as.Date(medias_diasIEN13$Fecha)
options(digits=5)
medias_diasIEN13$I1 <- as.numeric(medias_diasIEN13$I1)
medias_diasIEN13$I2 <- as.numeric(medias_diasIEN13$I2)
medias_diasIEN13$I3 <- as.numeric(medias_diasIEN13$I3)
##View(medias_diasIEN13)
max_diasIEN13 <- rbind(max_dia_14IENE,max_dia_15IENE,max_dia_16IENE)
##View(max_diasIEN13)
max_diasIEN13 <- cbind(levels(DiasF),max_diasIEN13)
max_diasIEN13 <- as.data.frame(max_diasIEN13)
##View(max_diasIEN13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN13) <- nombres
max_diasIEN13$Fecha <- as.Date(max_diasIEN13$Fecha)
options(digits=5)
max_diasIEN13$I1 <- as.numeric(max_diasIEN13$I1)
max_diasIEN13$I2 <- as.numeric(max_diasIEN13$I2)
max_diasIEN13$I3 <- as.numeric(max_diasIEN13$I3)
rownames(medias_diasIEN13) <- NULL
##View(max_diasIEN13)



###Enero 2014####
dfEN14 <- read.csv("Data/Years/2014/ENE2014.csv")
#View(dfEN14)

dfEN14 <- dfEN14[1:1998,1:8]
dfEN14[3] <- dfEN14[3]/sqrt(3)
dfEN14[4] <- dfEN14[4]/sqrt(3)
dfEN14[5] <- dfEN14[5]/sqrt(3)

# dfFE[6] <- dfFE[6]*2
# dfFE[7] <- dfFE[7]*2
# dfFE[7] <- dfFE[7]*2



dfEN14$Fecha.de.la.Medida <- as.Date(dfEN14$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfEN14$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in
     1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN14, Fecha.de.la.Medida==Dias[i]))
}

names(dfEN14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

####IEN1-IEN3###
mediaIEN <- vector("numeric", ncol(dfEN14[6:8]))
for (i in seq_along(dfEN14[6:8])) {# 2. secuencia
  mediaIEN[[i]] <- mean(ifelse(dfEN14[6:8][[i]]<=0,NA,dfEN14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}
maxIEN<- vector("numeric", ncol(dfEN14[6:8]))
for (i in seq_along(dfEN14[6:8])) {# 2. secuencia
  maxIEN[[i]] <- max(dfEN14[6:8][[i]] )    # 3. cuerpo
}
##13-enero a 20-enero##
###Dia 13###
media_dia_13IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IEN[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T)
}
max_dia_13IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IEN[[i]] <- max(Dia_13[6:8][[i]])
}

###Dia 14###
media_dia_14IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IEN[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T)
}
max_dia_14IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IEN[[i]] <- max(Dia_14[6:8][[i]])
}

###Dia 15###
media_dia_15IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IEN[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T)
}
max_dia_15IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IEN[[i]] <- max(Dia_15[6:8][[i]])
}
###Dia 16###
media_dia_16IEN<-vector("numeric",  ncol(dfEN14[6:8]))

for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IEN[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T)
}
max_dia_16IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IEN[[i]] <- max(Dia_16[6:8][[i]])
}

###Dia 17###
media_dia_17IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IEN[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T)
}
max_dia_17IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IEN[[i]] <- max(Dia_17[6:8][[i]])
}

###Dia 18###
media_dia_18IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IEN[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T)
}
max_dia_18IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IEN[[i]] <- max(Dia_18[6:8][[i]])
}

###Dia 19###
media_dia_19IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IEN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T)
}
max_dia_19IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IEN[[i]] <- max(Dia_19[6:8][[i]])
}

###Dia 20###
media_dia_20IEN<-vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IEN[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T)
}
max_dia_20IEN <- vector("numeric",  ncol(dfEN14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IEN[[i]] <- max(Dia_20[6:8][[i]])
}



medias_diasIEN14 <- rbind(media_dia_13IEN,media_dia_14IEN,media_dia_15IEN,
                          media_dia_16IEN,media_dia_17IEN,media_dia_18IEN,
                          media_dia_19IEN,media_dia_20IEN)
##View(medias_diasI)
medias_diasIEN14 <- cbind(levels(DiasF),medias_diasIEN14)
medias_diasIEN14 <- as.data.frame(medias_diasIEN14)
##View(medias_diasIEN14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN14) <- nombres
medias_diasIEN14$Fecha <- as.Date(medias_diasIEN14$Fecha)
options(digits=5)
medias_diasIEN14$I1 <- as.numeric(medias_diasIEN14$I1)
medias_diasIEN14$I2 <- as.numeric(medias_diasIEN14$I2)
medias_diasIEN14$I3 <- as.numeric(medias_diasIEN14$I3)
##View(medias_diasIEN14)
max_diasIEN14 <- rbind(max_dia_13IEN,max_dia_14IEN,max_dia_15IEN,
                       max_dia_16IEN,max_dia_17IEN,max_dia_18IEN,
                       max_dia_19IEN,max_dia_20IEN)
##View(max_diasIEN14)
max_diasIEN14 <- cbind(levels(DiasF),max_diasIEN14)
max_diasIEN14 <- as.data.frame(max_diasIEN14)
##View(max_diasIEN14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN14) <- nombres
max_diasIEN14$Fecha <- as.Date(max_diasIEN14$Fecha)
options(digits=5)
max_diasIEN14$I1 <- as.numeric(max_diasIEN14$I1)
max_diasIEN14$I2 <- as.numeric(max_diasIEN14$I2)
max_diasIEN14$I3 <- as.numeric(max_diasIEN14$I3)
rownames(medias_diasIEN14) <- NULL
##View(max_diasIEN14)


##Enero 2015####
dfEN15 <- read.csv("Data/Years/2015/ENE2015.csv")
#View(dfEN15)
dfEN15 <- dfEN15[1:3592,c(1:8)]
dfEN15[3:8] <- sapply(dfEN15[3:8],as.numeric)

dfEN15[3] <- dfEN15[3]/sqrt(3)
dfEN15[4] <- dfEN15[4]/sqrt(3)
dfEN15[5] <- dfEN15[5]/sqrt(3)

# dfEN15[6] <- dfEN15[6]*2
# dfEN15[7] <- dfEN15[7]*2
# dfEN15[7] <- dfEN15[7]*2



dfEN15$Feha.de.la.Medda <- as.Date(dfEN15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfEN15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN15, Feha.de.la.Medda==Dias[i]))
}

names(dfEN15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIEN <- vector("numeric", ncol(dfEN15[6:8]))
for (i in seq_along(dfEN15[6:8])) {# 2. secuencia
  mediaIEN[[i]] <- mean(ifelse(dfEN15[6:8][[i]]<=0,NA,dfEN15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIEN<- vector("numeric", ncol(dfEN15[6:8]))
for (i in seq_along(dfEN15[6:8])) {# 2. secuencia
  maxIEN[[i]] <- max(dfEN15[6:8][[i]] )    # 3. cuerpo
}

##19-Enero a 31-Enero#
###Dia 19###
media_dia_19IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IEN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T)
}

max_dia_19IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IEN[[i]] <- max(Dia_19[6:8][[i]])
}
###Dia 20###
media_dia_20IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IEN[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T)
}

max_dia_20IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IEN[[i]] <- max(Dia_20[6:8][[i]])
}

###Dia 21###
media_dia_21IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IEN[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T)
}

max_dia_21IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IEN[[i]] <- max(Dia_21[6:8][[i]])
}
###Dia 22###
media_dia_22IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IEN[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T)
}

max_dia_22IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IEN[[i]] <- max(Dia_22[6:8][[i]])
}
###Dia 23###
media_dia_23IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IEN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T)
}

max_dia_23IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IEN[[i]] <- max(Dia_23[6:8][[i]])
}
###Dia 24###
media_dia_24IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IEN[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T)
}

max_dia_24IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IEN[[i]] <- max(Dia_24[6:8][[i]])
}

###Dia 25###
media_dia_25IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IEN[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T)
}

max_dia_25IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IEN[[i]] <- max(Dia_25[6:8][[i]])
}

###Dia 26###
media_dia_26IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IEN[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T)
}

max_dia_26IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IEN[[i]] <- max(Dia_26[6:8][[i]])
}

###Dia 27###
media_dia_27IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IEN[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T)
}

max_dia_27IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IEN[[i]] <- max(Dia_27[6:8][[i]])
}

###Dia 28###
media_dia_28IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IEN[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T)
}

max_dia_28IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IEN[[i]] <- max(Dia_28[6:8][[i]])
}

###Dia 29###
media_dia_29IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IEN[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T)
}

max_dia_29IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IEN[[i]] <- max(Dia_29[6:8][[i]])
}

###Dia 30###
media_dia_30IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IEN[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T)
}

max_dia_30IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IEN[[i]] <- max(Dia_30[6:8][[i]])
}

###Dia 31###
media_dia_31IEN<-vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IEN[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T)
}

max_dia_31IEN <- vector("numeric",  ncol(dfEN15[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IEN[[i]] <- max(Dia_31[6:8][[i]])
}



medias_diasIEN15 <- rbind(media_dia_19IEN,media_dia_20IEN,media_dia_21IEN,
                          media_dia_22IEN,media_dia_23IEN,media_dia_24IEN,
                          media_dia_25IEN,media_dia_26IEN,media_dia_27IEN,
                          media_dia_28IEN,media_dia_29IEN,media_dia_30IEN,
                          media_dia_31IEN)
##View(medias_diasI)
medias_diasIEN15 <- cbind(levels(DiasF),medias_diasIEN15)
medias_diasIEN15 <- as.data.frame(medias_diasIEN15)
##View(medias_diasIEN15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN15) <- nombres
medias_diasIEN15$Fecha <- as.Date(medias_diasIEN15$Fecha)
options(digits=5)
medias_diasIEN15$I1 <- as.numeric(medias_diasIEN15$I1)
medias_diasIEN15$I2 <- as.numeric(medias_diasIEN15$I2)
medias_diasIEN15$I3 <- as.numeric(medias_diasIEN15$I3)
##View(medias_diasIEN15)

max_diasIEN15 <- rbind(max_dia_19IEN,max_dia_20IEN,max_dia_21IEN,
                       max_dia_22IEN,max_dia_23IEN,max_dia_24IEN,
                       max_dia_25IEN,max_dia_26IEN,max_dia_27IEN,
                       max_dia_28IEN,max_dia_29IEN,max_dia_30IEN,
                       max_dia_31IEN)
##View(max_diasIEN15)
max_diasIEN15 <- cbind(levels(DiasF),max_diasIEN15)
max_diasIEN15 <- as.data.frame(max_diasIEN15)
##View(max_diasIEN15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN15) <- nombres
max_diasIEN15$Fecha <- as.Date(max_diasIEN15$Fecha)
options(digits=5)
max_diasIEN15$I1 <- as.numeric(max_diasIEN15$I1)
max_diasIEN15$I2 <- as.numeric(max_diasIEN15$I2)
max_diasIEN15$I3 <- as.numeric(max_diasIEN15$I3)
##View(max_diasIEN15)

##Enero 2016####
dfEN16 <- read.csv("Data/Years/2016/ENE16.csv")
#View(dfEN16)
dfEN16 <- dfEN16[1:6824,c(1:8)]
dfEN16[3:8] <- sapply(dfEN16[3:8],as.numeric)

dfEN16[3] <- dfEN16[3]/sqrt(3)
dfEN16[4] <- dfEN16[4]/sqrt(3)
dfEN16[5] <- dfEN16[5]/sqrt(3)

# dfEN16[6] <- dfEN16[6]*2
# dfEN16[7] <- dfEN16[7]*2
# dfEN16[8] <- dfEN16[8]*2



dfEN16$Feha.de.la.Medda <- as.Date(dfEN16$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfEN16$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN16, Feha.de.la.Medda==Dias[i]))
}

names(dfEN16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIEN <- vector("numeric", ncol(dfEN16[6:8]))
for (i in seq_along(dfEN16[6:8])) {# 2. secuencia
  mediaIEN[[i]] <- mean(ifelse(dfEN16[6:8][[i]]<=0,NA,dfEN16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIEN<- vector("numeric", ncol(dfEN16[6:8]))
for (i in seq_along(dfEN16[6:8])) {# 2. secuencia
  maxIEN[[i]] <- max(dfEN16[6:8][[i]] )    # 3. cuerpo
}

##22-Enero a 31-Enero#
###Dia 22###
media_dia_22IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IEN[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T)
}

max_dia_22IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IEN[[i]] <- max(Dia_22[6:8][[i]])
}

###Dia 23###
media_dia_23IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IEN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T)
}

max_dia_23IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IEN[[i]] <- max(Dia_23[6:8][[i]])
}

###Dia 24###
media_dia_24IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IEN[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T)
}

max_dia_24IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IEN[[i]] <- max(Dia_24[6:8][[i]])
}

###Dia 25###
media_dia_25IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IEN[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T)
}

max_dia_25IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IEN[[i]] <- max(Dia_25[6:8][[i]])
}

###Dia 26###
media_dia_26IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IEN[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T)
}

max_dia_26IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IEN[[i]] <- max(Dia_26[6:8][[i]])
}

###Dia 27###
media_dia_27IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IEN[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T)
}

max_dia_27IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IEN[[i]] <- max(Dia_27[6:8][[i]])
}

###Dia 28###
media_dia_28IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IEN[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T)
}

max_dia_28IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IEN[[i]] <- max(Dia_28[6:8][[i]])
}

###Dia 29###
media_dia_29IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IEN[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T)
}

max_dia_29IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IEN[[i]] <- max(Dia_29[6:8][[i]])
}

###Dia 30###
media_dia_30IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IEN[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T)
}

max_dia_30IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IEN[[i]] <- max(Dia_30[6:8][[i]])
}

###Dia 31###
media_dia_31IEN<-vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IEN[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T)
}

max_dia_31IEN <- vector("numeric",  ncol(dfEN16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IEN[[i]] <- max(Dia_31[6:8][[i]])
}




medias_diasIEN16 <- rbind(media_dia_22IEN,media_dia_23IEN,media_dia_24IEN,
                          media_dia_25IEN,media_dia_26IEN,media_dia_27IEN,
                          media_dia_28IEN,media_dia_29IEN,media_dia_30IEN,
                          media_dia_31IEN)
##View(medias_diasI)
medias_diasIEN16 <- cbind(levels(DiasF),medias_diasIEN16)
medias_diasIEN16 <- as.data.frame(medias_diasIEN16)
##View(medias_diasIEN16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN16) <- nombres
medias_diasIEN16$Fecha <- as.Date(medias_diasIEN16$Fecha)
options(digits=5)
medias_diasIEN16$I1 <- as.numeric(medias_diasIEN16$I1)
medias_diasIEN16$I2 <- as.numeric(medias_diasIEN16$I2)
medias_diasIEN16$I3 <- as.numeric(medias_diasIEN16$I3)
##View(medias_diasIEN16)

max_diasIEN16 <- rbind(max_dia_22IEN,max_dia_23IEN,max_dia_24IEN,
                       max_dia_25IEN,max_dia_26IEN,max_dia_27IEN,
                       max_dia_28IEN,max_dia_29IEN,max_dia_30IEN,
                       max_dia_31IEN)
##View(max_diasIEN16)
max_diasIEN16 <- cbind(levels(DiasF),max_diasIEN16)
max_diasIEN16 <- as.data.frame(max_diasIEN16)
##View(max_diasIEN16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN16) <- nombres
max_diasIEN16$Fecha <- as.Date(max_diasIEN16$Fecha)
options(digits=5)
max_diasIEN16$I1 <- as.numeric(max_diasIEN16$I1)
max_diasIEN16$I2 <- as.numeric(max_diasIEN16$I2)
max_diasIEN16$I3 <- as.numeric(max_diasIEN16$I3)
##View(max_diasIEN16)


##Enero 2017####
dfEN17 <- read.csv("Data/Years/2017/ENERO2017.csv")
#View(dfEN17)
dfEN17 <- dfEN17[1:1776,c(1:8)]
dfEN17[3:8] <- sapply(dfEN17[3:8],as.numeric)

dfEN17[3] <- dfEN17[3]/sqrt(3)
dfEN17[4] <- dfEN17[4]/sqrt(3)
dfEN17[5] <- dfEN17[5]/sqrt(3)

# dfEN17[6] <- dfEN17[6]*2
# dfEN17[7] <- dfEN17[7]*2
# dfEN17[8] <- dfEN17[8]*2



dfEN17$Fecha.de.la.Medida <- as.Date(dfEN17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfEN17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN17, Fecha.de.la.Medida==Dias[i]))
}

names(dfEN17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIEN <- vector("numeric", ncol(dfEN17[6:8]))
for (i in seq_along(dfEN17[6:8])) {# 2. secuencia
  mediaIEN[[i]] <- mean(ifelse(dfEN17[6:8][[i]]<=0,NA,dfEN17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIEN<- vector("numeric", ncol(dfEN17[6:8]))
for (i in seq_along(dfEN17[6:8])) {# 2. secuencia
  maxIEN[[i]] <- max(dfEN17[6:8][[i]] )    # 3. cuerpo
}

##13-Enero a 31-Enero#
###Dia 13###
media_dia_13IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IEN[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T)
}

max_dia_13IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IEN[[i]] <- max(Dia_13[6:8][[i]])
}

###Dia 14###
media_dia_14IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IEN[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T)
}

max_dia_14IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IEN[[i]] <- max(Dia_14[6:8][[i]])
}

###Dia 15###
media_dia_15IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IEN[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T)
}

max_dia_15IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IEN[[i]] <- max(Dia_15[6:8][[i]])
}

###Dia 16###
media_dia_16IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IEN[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T)
}

max_dia_16IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IEN[[i]] <- max(Dia_16[6:8][[i]])
}

###Dia 17###
media_dia_17IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IEN[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T)
}

max_dia_17IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IEN[[i]] <- max(Dia_17[6:8][[i]])
}

###Dia 18###
media_dia_18IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IEN[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T)
}

max_dia_18IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IEN[[i]] <- max(Dia_18[6:8][[i]])
}

###Dia 19###
media_dia_19IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IEN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T)
}

max_dia_19IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IEN[[i]] <- max(Dia_19[6:8][[i]])
}

###Dia 20###
media_dia_20IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IEN[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T)
}

max_dia_20IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IEN[[i]] <- max(Dia_20[6:8][[i]])
}

###Dia 21###
media_dia_21IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IEN[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T)
}

max_dia_21IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IEN[[i]] <- max(Dia_21[6:8][[i]])
}

###Dia 22###
media_dia_22IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IEN[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T)
}

max_dia_22IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IEN[[i]] <- max(Dia_22[6:8][[i]])
}

###Dia 23###
media_dia_23IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IEN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T)
}

max_dia_23IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IEN[[i]] <- max(Dia_23[6:8][[i]])
}

###Dia 24###
media_dia_24IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IEN[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T)
}

max_dia_24IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IEN[[i]] <- max(Dia_24[6:8][[i]])
}

###Dia 25###
media_dia_25IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IEN[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T)
}

max_dia_25IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IEN[[i]] <- max(Dia_25[6:8][[i]])
}

###Dia 26###
media_dia_26IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IEN[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T)
}

max_dia_26IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IEN[[i]] <- max(Dia_26[6:8][[i]])
}

###Dia 27###
media_dia_27IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IEN[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T)
}

max_dia_27IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IEN[[i]] <- max(Dia_27[6:8][[i]])
}

###Dia 28###
media_dia_28IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IEN[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T)
}

max_dia_28IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IEN[[i]] <- max(Dia_28[6:8][[i]])
}

###Dia 29###
media_dia_29IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IEN[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T)
}

max_dia_29IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IEN[[i]] <- max(Dia_29[6:8][[i]])
}

###Dia 30###
media_dia_30IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IEN[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T)
}

max_dia_30IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IEN[[i]] <- max(Dia_30[6:8][[i]])
}

###Dia 31###
media_dia_31IEN<-vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IEN[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T)
}

max_dia_31IEN <- vector("numeric",  ncol(dfEN17[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IEN[[i]] <- max(Dia_31[6:8][[i]])
}

medias_diasIEN17 <- rbind(media_dia_13IEN,
                          media_dia_14IEN,media_dia_15IEN,media_dia_16IEN,
                          media_dia_17IEN,
                          media_dia_18IEN,media_dia_19IEN,media_dia_20IEN,
                          media_dia_21IEN,media_dia_22IEN,media_dia_23IEN,
                          media_dia_24IEN,media_dia_25IEN,media_dia_26IEN,
                          media_dia_27IEN,
                          media_dia_28IEN,media_dia_29IEN,media_dia_30IEN,
                          media_dia_31IEN)
##View(medias_diasI)
medias_diasIEN17 <- cbind(levels(DiasF),medias_diasIEN17)
medias_diasIEN17 <- as.data.frame(medias_diasIEN17)
##View(medias_diasIEN17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN17) <- nombres
medias_diasIEN17$Fecha <- as.Date(medias_diasIEN17$Fecha)
options(digits=5)
medias_diasIEN17$I1 <- as.numeric(medias_diasIEN17$I1)
medias_diasIEN17$I2 <- as.numeric(medias_diasIEN17$I2)
medias_diasIEN17$I3 <- as.numeric(medias_diasIEN17$I3)
##View(medias_diasIEN17)

max_diasIEN17 <- rbind(max_dia_13IEN,
                       max_dia_14IEN,max_dia_15IEN,max_dia_16IEN,
                       max_dia_17IEN,
                       max_dia_18IEN,max_dia_19IEN,max_dia_20IEN,
                       max_dia_21IEN,max_dia_22IEN,max_dia_23IEN,
                       max_dia_24IEN,max_dia_25IEN,max_dia_26IEN,
                       max_dia_27IEN,
                       max_dia_28IEN,max_dia_29IEN,max_dia_30IEN,
                       max_dia_31IEN)
##View(max_diasIEN17)
max_diasIEN17 <- cbind(levels(DiasF),max_diasIEN17)
max_diasIEN17 <- as.data.frame(max_diasIEN17)
##View(max_diasIEN17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN17) <- nombres
max_diasIEN17$Fecha <- as.Date(max_diasIEN17$Fecha)
options(digits=5)
max_diasIEN17$I1 <- as.numeric(max_diasIEN17$I1)
max_diasIEN17$I2 <- as.numeric(max_diasIEN17$I2)
max_diasIEN17$I3 <- as.numeric(max_diasIEN17$I3)
##View(max_diasIEN17)


##Enero 2018####
dfEN18 <- read.csv("Data/Years/2018/ENE2018.csv")
#View(dfEN18)
dfEN18 <- dfEN18[1:665,c(1:8)]
dfEN18[3:8] <- sapply(dfEN18[3:8],as.numeric)

dfEN18[3] <- dfEN18[3]/sqrt(3)
dfEN18[4] <- dfEN18[4]/sqrt(3)
dfEN18[5] <- dfEN18[5]/sqrt(3)

# dfEN18[6] <- dfEN18[6]*2
# dfEN18[7] <- dfEN18[7]*2
# dfEN18[8] <- dfEN18[8]*2



dfEN18$Fecha.de.la.Medida <- as.Date(dfEN18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfEN18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN18, Fecha.de.la.Medida==Dias[i]))
}
names(dfEN18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")


mediaIEN <- vector("numeric", ncol(dfEN18[6:8]))
for (i in seq_along(dfEN18[6:8])) {# 2. secuencia
  mediaIEN[[i]] <- mean(ifelse(dfEN18[6:8][[i]]<=0,NA,dfEN18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIEN<- vector("numeric", ncol(dfEN18[6:8]))
for (i in seq_along(dfEN18[6:8])) {# 2. secuencia
  maxIEN[[i]] <- max(dfEN18[6:8][[i]] )    # 3. cuerpo
}

##16-Enero a 23-Enero#
###Dia 16###
media_dia_16IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IEN[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T)
}

max_dia_16IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IEN[[i]] <- max(Dia_16[6:8][[i]])
}

###Dia 17###
media_dia_17IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IEN[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T)
}

max_dia_17IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IEN[[i]] <- max(Dia_17[6:8][[i]])
}

###Dia 18###
media_dia_18IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IEN[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T)
}

max_dia_18IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IEN[[i]] <- max(Dia_18[6:8][[i]])
}

###Dia 19###
media_dia_19IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IEN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T)
}

max_dia_19IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IEN[[i]] <- max(Dia_19[6:8][[i]])
}

###Dia 20###
media_dia_20IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IEN[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T)
}

max_dia_20IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IEN[[i]] <- max(Dia_20[6:8][[i]])
}

###Dia 21###
media_dia_21IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IEN[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T)
}

max_dia_21IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IEN[[i]] <- max(Dia_21[6:8][[i]])
}

###Dia 22###
media_dia_22IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IEN[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T)
}

max_dia_22IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IEN[[i]] <- max(Dia_22[6:8][[i]])
}

###Dia 23###
media_dia_23IEN<-vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IEN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T)
}

max_dia_23IEN <- vector("numeric",  ncol(dfEN18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IEN[[i]] <- max(Dia_23[6:8][[i]])
}



medias_diasIEN18 <- rbind(media_dia_16IEN,
                          media_dia_17IEN,
                          media_dia_18IEN,media_dia_19IEN,media_dia_20IEN,
                          media_dia_21IEN,media_dia_22IEN,media_dia_23IEN)
##View(medias_diasI)
medias_diasIEN18 <- cbind(levels(DiasF),medias_diasIEN18)
medias_diasIEN18 <- as.data.frame(medias_diasIEN18)
##View(medias_diasIEN18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN18) <- nombres
medias_diasIEN18$Fecha <- as.Date(medias_diasIEN18$Fecha)
options(digits=5)
medias_diasIEN18$I1 <- as.numeric(medias_diasIEN18$I1)
medias_diasIEN18$I2 <- as.numeric(medias_diasIEN18$I2)
medias_diasIEN18$I3 <- as.numeric(medias_diasIEN18$I3)
##View(medias_diasIEN18)

max_diasIEN18 <- rbind(max_dia_16IEN,
                       max_dia_17IEN,
                       max_dia_18IEN,max_dia_19IEN,max_dia_20IEN,
                       max_dia_21IEN,max_dia_22IEN,max_dia_23IEN)
##View(max_diasIEN18)
max_diasIEN18 <- cbind(levels(DiasF),max_diasIEN18)
max_diasIEN18 <- as.data.frame(max_diasIEN18)
##View(max_diasIEN18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN18) <- nombres
max_diasIEN18$Fecha <- as.Date(max_diasIEN18$Fecha)
options(digits=5)
max_diasIEN18$I1 <- as.numeric(max_diasIEN18$I1)
max_diasIEN18$I2 <- as.numeric(max_diasIEN18$I2)
max_diasIEN18$I3 <- as.numeric(max_diasIEN18$I3)
##View(max_diasIEN18)























##Enero 2022####
library(dplyr)
dfEN22 <- read.csv("Data/Years/2022/Enero.csv")
#View(dfEN22)
dfEN22 <- dfEN22[1:3982,1:8] 
dfEN22[3:8] <- sapply(dfEN22[3:8],as.numeric)

dfEN22$Fecha <- as.Date(dfEN22$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfEN22$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)


for (i in
     1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN22, Fecha == Dias[i]))
}

names(dfEN22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")


####IENE1-IENE3###
mediaIEN <- vector("numeric", ncol(dfEN22[6:8]))
for (i in seq_along(dfEN22[6:8])) {# 2. secuencia
  mediaIEN[[i]] <- mean(ifelse(dfEN22[6:8][[i]]<=0,NA,dfEN22[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}


maxIENE<- vector("numeric", ncol(dfEN22[6:8]))
for (i in seq_along(dfEN22[6:8])) {# 2. secuencia
  maxIENE[[i]] <- max(dfEN22[6:8][[i]] )    # 3. cuerpo
}


###Dia 1###
media_dia_1IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IENE[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                               na.rm=T)
}
max_dia_1IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IENE[[i]] <- max(Dia_1[6:8][[i]])
}

###Dia 2###
media_dia_2IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IENE[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                               na.rm=T)
}
max_dia_2IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IENE[[i]] <- max(Dia_2[6:8][[i]])
}

###Dia 3###
media_dia_3IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IENE[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                               na.rm=T)
}
max_dia_3IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IENE[[i]] <- max(Dia_3[6:8][[i]])
}

###Dia 4###
media_dia_4IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IENE[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                               na.rm=T)
}
max_dia_4IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IENE[[i]] <- max(Dia_4[6:8][[i]])
}

###Dia 5###
media_dia_5IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IENE[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                               na.rm=T)
}
max_dia_5IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IENE[[i]] <- max(Dia_5[6:8][[i]])
}

###Dia 6###
media_dia_6IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IENE[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                               na.rm=T)
}
max_dia_6IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IENE[[i]] <- max(Dia_6[6:8][[i]])
}

###Dia 7###
media_dia_7IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IENE[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                               na.rm=T)
}
max_dia_7IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IENE[[i]] <- max(Dia_7[6:8][[i]])
}

###Dia 8###
media_dia_8IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IENE[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                               na.rm=T)
}
max_dia_8IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IENE[[i]] <- max(Dia_8[6:8][[i]])
}

###Dia 9###
media_dia_9IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IENE[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                               na.rm=T)
}
max_dia_9IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IENE[[i]] <- max(Dia_9[6:8][[i]])
}

###Dia 10###
media_dia_10IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IENE[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                                na.rm=T)
}
max_dia_10IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IENE[[i]] <- max(Dia_10[6:8][[i]])
}

###Dia 11###
media_dia_11IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IENE[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                                na.rm=T)
}
max_dia_11IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IENE[[i]] <- max(Dia_11[6:8][[i]])
}

###Dia 12###
media_dia_12IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IENE[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                                na.rm=T)
}
max_dia_12IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IENE[[i]] <- max(Dia_12[6:8][[i]])
}

###Dia 13###
media_dia_13IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IENE[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                                na.rm=T)
}
max_dia_13IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IENE[[i]] <- max(Dia_13[6:8][[i]])
}

###Dia 14###
media_dia_14IENE<-vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IENE[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                                na.rm=T)
}
max_dia_14IENE <- vector("numeric",  ncol(dfEN22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IENE[[i]] <- max(Dia_14[6:8][[i]])
}

medias_diasIEN22 <- rbind(media_dia_1IENE,media_dia_2IENE,media_dia_3IENE,
                          media_dia_4IENE,media_dia_5IENE,media_dia_6IENE,
                          media_dia_7IENE,media_dia_8IENE,media_dia_9IENE,
                          media_dia_10IENE,media_dia_11IENE,media_dia_12IENE,
                          media_dia_13IENE,media_dia_14IENE)
##View(medias_diasIEN22)
medias_diasIEN22 <- cbind(levels(DiasF),medias_diasIEN22)
medias_diasIEN22 <- as.data.frame(medias_diasIEN22)
##View(medias_diasIEN22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIEN22) <- nombres
medias_diasIEN22$Fecha <- as.Date(medias_diasIEN22$Fecha)
options(digits=5)
medias_diasIEN22$I1 <- as.numeric(medias_diasIEN22$I1)
medias_diasIEN22$I2 <- as.numeric(medias_diasIEN22$I2)
medias_diasIEN22$I3 <- as.numeric(medias_diasIEN22$I3)
rownames(medias_diasIEN22) <- NULL

##View(medias_diasIEN22)

max_diasIEN22 <- rbind(max_dia_1IENE,max_dia_2IENE,max_dia_3IENE,
                       max_dia_4IENE,max_dia_5IENE,max_dia_6IENE,
                       max_dia_7IENE,max_dia_8IENE,max_dia_9IENE,
                       max_dia_10IENE,max_dia_11IENE,max_dia_12IENE,
                       max_dia_13IENE,max_dia_14IENE)
##View(max_diasIEN22)
max_diasIEN22 <- cbind(levels(DiasF),max_diasIEN22)
max_diasIEN22 <- as.data.frame(max_diasIEN22)
##View(max_diasIEN22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIEN22) <- nombres
max_diasIEN22$Fecha <- as.Date(max_diasIEN22$Fecha)
options(digits=5)
max_diasIEN22$I1 <- as.numeric(max_diasIEN22$I1)
max_diasIEN22$I2 <- as.numeric(max_diasIEN22$I2)
max_diasIEN22$I3 <- as.numeric(max_diasIEN22$I3)
rownames(max_diasIEN22) <- NULL
##View(max_diasIEN22)


#####
Enero <- rbind(medias_diasIEN13,medias_diasIEN14,medias_diasIEN15,
               medias_diasIEN16,medias_diasIEN17,medias_diasIEN18,
               medias_diasIEN22)
EneroM <- rbind(max_diasIEN13,max_diasIEN14,max_diasIEN15,
               max_diasIEN16,max_diasIEN17,max_diasIEN18,
               max_diasIEN22)

dfEN <- rbind(dfEN13,dfEN14,dfEN15,dfEN16,dfEN17,dfEN18,dfEN22)
##Febrero 2013####
dfFE13 <- read.csv("Data/Years/2013/FEB013.csv")
#View(dfFE13)
dfFE13 <- dfFE13[1:2015,1:8] 
dfFE13[3:8] <- sapply(dfFE13[3:8],as.numeric)  

dfFE13[3] <- dfFE13[3]/sqrt(3)
dfFE13[4] <- dfFE13[4]/sqrt(3)
dfFE13[5] <- dfFE13[5]/sqrt(3)

dfFE13[6] <- dfFE13[6]*2
dfFE13[7] <- dfFE13[7]*2
dfFE13[7] <- dfFE13[7]*2



dfFE13$Fecha.de.la.Medida <- as.Date(dfFE13$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfFE13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE13, Fecha.de.la.Medida==Dias[i]))
}

names(dfFE13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIFE <- vector("numeric", ncol(dfFE13[6:8]))
for (i in seq_along(dfFE13[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE13[6:8][[i]]<=0,NA,dfFE13[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE13[6:8]))
for (i in seq_along(dfFE13[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE13[6:8][[i]] )    # 3. cuerpo
}

##19-enero a 26-enero##
###Dia 19###
media_dia_19IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IFE[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IFE[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IFE[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T)
}

max_dia_20IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IFE[[i]] <- max(Dia_20[6:8][[i]]) 
}
###Dia 21###
media_dia_21IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IFE[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IFE[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IFE[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IFE[[i]] <- max(Dia_22[6:8][[i]]) 
}
###Dia 23###
media_dia_23IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IFE[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IFE[[i]] <- max(Dia_23[6:8][[i]]) 
}
###Dia 24###
media_dia_24IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IFE[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IFE[[i]] <- max(Dia_24[6:8][[i]]) 
}
###Dia 25###
media_dia_25IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IFE[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IFE[[i]] <- max(Dia_25[6:8][[i]]) 
}
###Dia 26###
media_dia_26IFE<-vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IFE[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IFE <- vector("numeric",  ncol(dfFE13[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IFE[[i]] <- max(Dia_26[6:8][[i]]) 
}

medias_diasIFE13 <- rbind(media_dia_19IFE,media_dia_20IFE,media_dia_21IFE,
                        media_dia_22IFE,media_dia_23IFE,media_dia_24IFE,
                        media_dia_25IFE,media_dia_26IFE)
##View(medias_diasI)
medias_diasIFE13 <- cbind(levels(DiasF),medias_diasIFE13)
medias_diasIFE13 <- as.data.frame(medias_diasIFE13)
##View(medias_diasIFE13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE13) <- nombres
medias_diasIFE13$Fecha <- as.Date(medias_diasIFE13$Fecha)
options(digits=5)
medias_diasIFE13$I1 <- as.numeric(medias_diasIFE13$I1)
medias_diasIFE13$I2 <- as.numeric(medias_diasIFE13$I2)
medias_diasIFE13$I3 <- as.numeric(medias_diasIFE13$I3)
##View(medias_diasIFE13)

max_diasIFE13 <- rbind(max_dia_19IFE,max_dia_20IFE,max_dia_21IFE,
                     max_dia_22IFE,max_dia_23IFE,max_dia_24IFE,
                     max_dia_25IFE,max_dia_26IFE)
##View(max_diasIFE13)
max_diasIFE13 <- cbind(levels(DiasF),max_diasIFE13)
max_diasIFE13 <- as.data.frame(max_diasIFE13)
##View(max_diasIFE13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE13) <- nombres
max_diasIFE13$Fecha <- as.Date(max_diasIFE13$Fecha)
options(digits=5)
max_diasIFE13$I1 <- as.numeric(max_diasIFE13$I1)
max_diasIFE13$I2 <- as.numeric(max_diasIFE13$I2)
max_diasIFE13$I3 <- as.numeric(max_diasIFE13$I3)
##View(max_diasIFE13)




##Febrero 2014####
dfFE14 <- read.csv("Data/Years/2014/FEB2014.csv")
#View(dfFE14)
dfFE14 <- dfFE14[1:2298,c(1:8)] 
dfFE14[3:8] <- sapply(dfFE14[3:8],as.numeric)  

dfFE14[3] <- dfFE14[3]/sqrt(3)
dfFE14[4] <- dfFE14[4]/sqrt(3)
dfFE14[5] <- dfFE14[5]/sqrt(3)

# dfFE14[6] <- dfFE14[6]*2
# dfFE14[7] <- dfFE14[7]*2
# dfFE14[7] <- dfFE14[7]*2



dfFE14$Fecha.de.la.Medida <- as.Date(dfFE14$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfFE14$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE14, Fecha.de.la.Medida==Dias[i]))
}

names(dfFE14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
mediaIFE <- vector("numeric", ncol(dfFE14[6:8]))
for (i in seq_along(dfFE14[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE14[6:8][[i]]<=0,NA,dfFE14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE14[6:8]))
for (i in seq_along(dfFE14[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE14[6:8][[i]] )    # 3. cuerpo
}

##5-enero a 13-enero##
###Dia 5###
media_dia_5IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IFE[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IFE[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IFE[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IFE[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IFE[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IFE[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IFE[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IFE[[i]] <- max(Dia_8[6:8][[i]]) 
}
###Dia 9###
media_dia_9IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IFE[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IFE[[i]] <- max(Dia_9[6:8][[i]]) 
}
###Dia 10###
media_dia_10IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IFE[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IFE[[i]] <- max(Dia_10[6:8][[i]]) 
}
###Dia 11###
media_dia_11IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IFE[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IFE[[i]] <- max(Dia_11[6:8][[i]]) 
}
###Dia 12###
media_dia_12IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IFE[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IFE[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IFE<-vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IFE[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IFE <- vector("numeric",  ncol(dfFE14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IFE[[i]] <- max(Dia_13[6:8][[i]]) 
}



medias_diasIFE14 <- rbind(media_dia_5IFE,media_dia_6IFE,media_dia_7IFE,
                        media_dia_8IFE,media_dia_9IFE,media_dia_10IFE,
                        media_dia_11IFE,media_dia_12IFE,media_dia_13IFE)
##View(medias_diasI)
medias_diasIFE14 <- cbind(levels(DiasF),medias_diasIFE14)
medias_diasIFE14 <- as.data.frame(medias_diasIFE14)
##View(medias_diasIFE14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE14) <- nombres
medias_diasIFE14$Fecha <- as.Date(medias_diasIFE14$Fecha)
options(digits=5)
medias_diasIFE14$I1 <- as.numeric(medias_diasIFE14$I1)
medias_diasIFE14$I2 <- as.numeric(medias_diasIFE14$I2)
medias_diasIFE14$I3 <- as.numeric(medias_diasIFE14$I3)
##View(medias_diasIFE14)

max_diasIFE14 <- rbind(max_dia_5IFE,max_dia_6IFE,max_dia_7IFE,
                     max_dia_8IFE,max_dia_9IFE,max_dia_10IFE,
                     max_dia_11IFE,max_dia_12IFE,max_dia_13IFE)
##View(max_diasIFE14)
max_diasIFE14 <- cbind(levels(DiasF),max_diasIFE14)
max_diasIFE14 <- as.data.frame(max_diasIFE14)
##View(max_diasIFE14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE14) <- nombres
max_diasIFE14$Fecha <- as.Date(max_diasIFE14$Fecha)
options(digits=5)
max_diasIFE14$I1 <- as.numeric(max_diasIFE14$I1)
max_diasIFE14$I2 <- as.numeric(max_diasIFE14$I2)
max_diasIFE14$I3 <- as.numeric(max_diasIFE14$I3)
##View(max_diasIFE14)




##Febrero 2015####
dfFE15 <- read.csv("Data/Years/2015/FEB2015.csv")
#View(dfFE15)
dfFE15 <- dfFE15[1:4719,c(1:8)] 
dfFE15[3:8] <- sapply(dfFE15[3:8],as.numeric)  

dfFE15[3] <- dfFE15[3]/sqrt(3)
dfFE15[4] <- dfFE15[4]/sqrt(3)
dfFE15[5] <- dfFE15[5]/sqrt(3)

# dfFE15[6] <- dfFE15[6]*2
# dfFE15[7] <- dfFE15[7]*2
# dfFE15[7] <- dfFE15[7]*2



dfFE15$Feha.de.la.Medda <- as.Date(dfFE15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfFE15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE15, Feha.de.la.Medda==Dias[i]))
}

names(dfFE15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
mediaIFE <- vector("numeric", ncol(dfFE15[6:8]))
for (i in seq_along(dfFE15[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE15[6:8][[i]]<=0,NA,dfFE15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE15[6:8]))
for (i in seq_along(dfFE15[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE15[6:8][[i]] )    # 3. cuerpo
}

##4-Febrero a 20-Febrero#
###Dia 4###
media_dia_4IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IFE[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IFE[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IFE[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IFE[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IFE[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IFE[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IFE[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IFE[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IFE[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IFE[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IFE[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IFE[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IFE[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IFE[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IFE[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IFE[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IFE[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IFE[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IFE[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IFE[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IFE[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IFE[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IFE[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IFE[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IFE[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IFE[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IFE[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IFE[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IFE[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IFE[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IFE[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IFE[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20IFE<-vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IFE[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IFE <- vector("numeric",  ncol(dfFE15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IFE[[i]] <- max(Dia_20[6:8][[i]]) 
}



medias_diasIFE15 <- rbind(media_dia_4IFE,
                        media_dia_5IFE,media_dia_6IFE,media_dia_7IFE,
                        media_dia_8IFE,media_dia_9IFE,media_dia_10IFE,
                        media_dia_11IFE,media_dia_12IFE,media_dia_13IFE,
                        media_dia_14IFE,media_dia_15IFE,media_dia_16IFE,
                        media_dia_17IFE,media_dia_18IFE,media_dia_19IFE,
                        media_dia_20IFE)
##View(medias_diasI)
medias_diasIFE15 <- cbind(levels(DiasF),medias_diasIFE15)
medias_diasIFE15 <- as.data.frame(medias_diasIFE15)
##View(medias_diasIFE15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE15) <- nombres
medias_diasIFE15$Fecha <- as.Date(medias_diasIFE15$Fecha)
options(digits=5)
medias_diasIFE15$I1 <- as.numeric(medias_diasIFE15$I1)
medias_diasIFE15$I2 <- as.numeric(medias_diasIFE15$I2)
medias_diasIFE15$I3 <- as.numeric(medias_diasIFE15$I3)
##View(medias_diasIFE15)

max_diasIFE15 <- rbind(max_dia_4IFE,
                     max_dia_5IFE,max_dia_6IFE,max_dia_7IFE,
                     max_dia_8IFE,max_dia_9IFE,max_dia_10IFE,
                     max_dia_11IFE,max_dia_12IFE,max_dia_13IFE,
                     max_dia_14IFE,max_dia_15IFE,max_dia_16IFE,
                     max_dia_17IFE,max_dia_18IFE,max_dia_19IFE,
                     max_dia_20IFE)
##View(max_diasIFE15)
max_diasIFE15 <- cbind(levels(DiasF),max_diasIFE15)
max_diasIFE15 <- as.data.frame(max_diasIFE15)
##View(max_diasIFE15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE15) <- nombres
max_diasIFE15$Fecha <- as.Date(max_diasIFE15$Fecha)
options(digits=5)
max_diasIFE15$I1 <- as.numeric(max_diasIFE15$I1)
max_diasIFE15$I2 <- as.numeric(max_diasIFE15$I2)
max_diasIFE15$I3 <- as.numeric(max_diasIFE15$I3)
##View(max_diasIFE15)



##Febrero 2016####
dfFE16 <- read.csv("Data/Years/2016/FEB16.csv")
#View(dfFE16)
dfFE16 <- dfFE16[1:8314,c(1:8)] 
dfFE16[3:8] <- sapply(dfFE16[3:8],as.numeric)  

dfFE16[3] <- dfFE16[3]/sqrt(3)
dfFE16[4] <- dfFE16[4]/sqrt(3)
dfFE16[5] <- dfFE16[5]/sqrt(3)

dfFE16[6] <- dfFE16[6]*2
dfFE16[7] <- dfFE16[7]*2
dfFE16[8] <- dfFE16[8]*2



dfFE16$Feha.de.la.Medda <- as.Date(dfFE16$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfFE16$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE16, Feha.de.la.Medda==Dias[i]))
}

names(dfFE16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
mediaIFE <- vector("numeric", ncol(dfFE16[6:8]))
for (i in seq_along(dfFE16[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE16[6:8][[i]]<=0,NA,dfFE16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE16[6:8]))
for (i in seq_along(dfFE16[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE16[6:8][[i]] )    # 3. cuerpo
}

##5-Febrero a 29-Febrero#
###Dia 5###
media_dia_5IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IFE[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IFE[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IFE[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IFE[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IFE[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IFE[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IFE[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IFE[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IFE[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IFE[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IFE[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IFE[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IFE[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IFE[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IFE[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IFE[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IFE[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IFE[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IFE[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IFE[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IFE[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IFE[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IFE[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IFE[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IFE[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IFE[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IFE[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IFE[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IFE[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IFE[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IFE[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IFE[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IFE[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IFE[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IFE[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IFE[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IFE[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IFE[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IFE[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IFE[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IFE[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IFE[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IFE[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IFE[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IFE[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IFE[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IFE[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IFE[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IFE<-vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IFE[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IFE <- vector("numeric",  ncol(dfFE16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IFE[[i]] <- max(Dia_29[6:8][[i]]) 
}




medias_diasIFE16 <- rbind(media_dia_5IFE,media_dia_6IFE,media_dia_7IFE,
                        media_dia_8IFE,media_dia_9IFE,media_dia_10IFE,
                        media_dia_11IFE,media_dia_12IFE,media_dia_13IFE,
                        media_dia_14IFE,media_dia_15IFE,media_dia_16IFE,
                        media_dia_17IFE,
                        media_dia_18IFE,media_dia_19IFE,media_dia_20IFE,
                        media_dia_21IFE,media_dia_22IFE,media_dia_23IFE,
                        media_dia_24IFE,media_dia_25IFE,media_dia_26IFE,
                        media_dia_27IFE,
                        media_dia_28IFE,media_dia_29IFE)
##View(medias_diasI)
medias_diasIFE16 <- cbind(levels(DiasF),medias_diasIFE16)
medias_diasIFE16 <- as.data.frame(medias_diasIFE16)
##View(medias_diasIFE16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE16) <- nombres
medias_diasIFE16$Fecha <- as.Date(medias_diasIFE16$Fecha)
options(digits=5)
medias_diasIFE16$I1 <- as.numeric(medias_diasIFE16$I1)
medias_diasIFE16$I2 <- as.numeric(medias_diasIFE16$I2)
medias_diasIFE16$I3 <- as.numeric(medias_diasIFE16$I3)
##View(medias_diasIFE16)

max_diasIFE16 <- rbind(max_dia_5IFE,max_dia_6IFE,max_dia_7IFE,
                     max_dia_8IFE,max_dia_9IFE,max_dia_10IFE,
                     max_dia_11IFE,max_dia_12IFE,max_dia_13IFE,
                     max_dia_14IFE,max_dia_15IFE,max_dia_16IFE,
                     max_dia_17IFE,
                     max_dia_18IFE,max_dia_19IFE,max_dia_20IFE,
                     max_dia_21IFE,max_dia_22IFE,max_dia_23IFE,
                     max_dia_24IFE,max_dia_25IFE,max_dia_26IFE,
                     max_dia_27IFE,
                     max_dia_28IFE,max_dia_29IFE)
##View(max_diasIFE16)
max_diasIFE16 <- cbind(levels(DiasF),max_diasIFE16)
max_diasIFE16 <- as.data.frame(max_diasIFE16)
##View(max_diasIFE16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE16) <- nombres
max_diasIFE16$Fecha <- as.Date(max_diasIFE16$Fecha)
options(digits=5)
max_diasIFE16$I1 <- as.numeric(max_diasIFE16$I1)
max_diasIFE16$I2 <- as.numeric(max_diasIFE16$I2)
max_diasIFE16$I3 <- as.numeric(max_diasIFE16$I3)
##View(max_diasIFE16)




##Febrero 2017####
dfFE17 <- read.csv("Data/Years/2017/FEB2017.csv")
#View(dfFE17)
dfFE17 <- dfFE17[1:1871,c(1:8)] 
dfFE17[3:8] <- sapply(dfFE17[3:8],as.numeric)  

dfFE17[3] <- dfFE17[3]/sqrt(3)
dfFE17[4] <- dfFE17[4]/sqrt(3)
dfFE17[5] <- dfFE17[5]/sqrt(3)

# dfFE17[6] <- dfFE17[6]*2
# dfFE17[7] <- dfFE17[7]*2
# dfFE17[8] <- dfFE17[8]*2



dfFE17$Fecha.de.la.Medida <- as.Date(dfFE17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfFE17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE17, Fecha.de.la.Medida==Dias[i]))
}

names(dfFE17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
mediaIFE <- vector("numeric", ncol(dfFE17[6:8]))
for (i in seq_along(dfFE17[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE17[6:8][[i]]<=0,NA,dfFE17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE17[6:8]))
for (i in seq_along(dfFE17[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE17[6:8][[i]] )    # 3. cuerpo
}

##9-Febrero a 28-Febrero#
###Dia 9###
media_dia_9IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IFE[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IFE[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IFE[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IFE[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IFE[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IFE[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IFE[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IFE[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IFE[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IFE[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IFE[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IFE[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IFE[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IFE[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IFE[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IFE[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IFE[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IFE[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IFE[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IFE[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IFE[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IFE[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IFE[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IFE[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IFE[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IFE[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IFE[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IFE[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IFE[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IFE[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IFE[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IFE[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IFE[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IFE[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IFE[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IFE[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IFE[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IFE[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IFE<-vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IFE[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IFE <- vector("numeric",  ncol(dfFE17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IFE[[i]] <- max(Dia_28[6:8][[i]]) 
}


medias_diasIFE17 <- rbind(media_dia_9IFE,media_dia_10IFE,
                        media_dia_11IFE,media_dia_12IFE,media_dia_13IFE,
                        media_dia_14IFE,media_dia_15IFE,media_dia_16IFE,
                        media_dia_17IFE,
                        media_dia_18IFE,media_dia_19IFE,media_dia_20IFE,
                        media_dia_21IFE,media_dia_22IFE,media_dia_23IFE,
                        media_dia_24IFE,media_dia_25IFE,media_dia_26IFE,
                        media_dia_27IFE,
                        media_dia_28IFE)
##View(medias_diasI)
medias_diasIFE17 <- cbind(levels(DiasF),medias_diasIFE17)
medias_diasIFE17 <- as.data.frame(medias_diasIFE17)
##View(medias_diasIFE17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE17) <- nombres
medias_diasIFE17$Fecha <- as.Date(medias_diasIFE17$Fecha)
options(digits=5)
medias_diasIFE17$I1 <- as.numeric(medias_diasIFE17$I1)
medias_diasIFE17$I2 <- as.numeric(medias_diasIFE17$I2)
medias_diasIFE17$I3 <- as.numeric(medias_diasIFE17$I3)
##View(medias_diasIFE17)

max_diasIFE17 <- rbind(max_dia_9IFE,max_dia_10IFE,
                     max_dia_11IFE,max_dia_12IFE,max_dia_13IFE,
                     max_dia_14IFE,max_dia_15IFE,max_dia_16IFE,
                     max_dia_17IFE,
                     max_dia_18IFE,max_dia_19IFE,max_dia_20IFE,
                     max_dia_21IFE,max_dia_22IFE,max_dia_23IFE,
                     max_dia_24IFE,max_dia_25IFE,max_dia_26IFE,
                     max_dia_27IFE,
                     max_dia_28IFE)
##View(max_diasIFE17)
max_diasIFE17 <- cbind(levels(DiasF),max_diasIFE17)
max_diasIFE17 <- as.data.frame(max_diasIFE17)
##View(max_diasIFE17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE17) <- nombres
max_diasIFE17$Fecha <- as.Date(max_diasIFE17$Fecha)
options(digits=5)
max_diasIFE17$I1 <- as.numeric(max_diasIFE17$I1)
max_diasIFE17$I2 <- as.numeric(max_diasIFE17$I2)
max_diasIFE17$I3 <- as.numeric(max_diasIFE17$I3)
##View(max_diasIFE17)




##Febrero 2018####
dfFE18 <- read.csv("Data/Years/2018/FEB2018.csv")
#View(dfFE18)
dfFE18 <- dfFE18[1:453,c(1:8)] 
dfFE18[3:8] <- sapply(dfFE18[3:8],as.numeric)  

dfFE18[3] <- dfFE18[3]/sqrt(3)
dfFE18[4] <- dfFE18[4]/sqrt(3)
dfFE18[5] <- dfFE18[5]/sqrt(3)

dfFE18[6] <- dfFE18[6]*2
dfFE18[7] <- dfFE18[7]*2
dfFE18[8] <- dfFE18[8]*2



dfFE18$Fecha.de.la.Medida <- as.Date(dfFE18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfFE18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE18, Fecha.de.la.Medida==Dias[i]))
}

names(dfFE18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
mediaIFE <- vector("numeric", ncol(dfFE18[6:8]))
for (i in seq_along(dfFE18[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE18[6:8][[i]]<=0,NA,dfFE18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE18[6:8]))
for (i in seq_along(dfFE18[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE18[6:8][[i]] )    # 3. cuerpo
}

##19-Febrero a 34-Febrero#

###Dia 19###
media_dia_19IFE<-vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IFE[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IFE <- vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IFE[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IFE<-vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IFE[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IFE <- vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IFE[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IFE<-vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IFE[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IFE <- vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IFE[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IFE<-vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IFE[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IFE <- vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IFE[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IFE<-vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IFE[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IFE <- vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IFE[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IFE<-vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IFE[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IFE <- vector("numeric",  ncol(dfFE18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IFE[[i]] <- max(Dia_24[6:8][[i]]) 
}




medias_diasIFE18 <- rbind(media_dia_19IFE,media_dia_20IFE,
                        media_dia_21IFE,media_dia_22IFE,media_dia_23IFE,
                        media_dia_24IFE)
##View(medias_diasI)
medias_diasIFE18 <- cbind(levels(DiasF),medias_diasIFE18)
medias_diasIFE18 <- as.data.frame(medias_diasIFE18)
##View(medias_diasIFE18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE18) <- nombres
medias_diasIFE18$Fecha <- as.Date(medias_diasIFE18$Fecha)
options(digits=5)
medias_diasIFE18$I1 <- as.numeric(medias_diasIFE18$I1)
medias_diasIFE18$I2 <- as.numeric(medias_diasIFE18$I2)
medias_diasIFE18$I3 <- as.numeric(medias_diasIFE18$I3)
##View(medias_diasIFE18)

max_diasIFE18 <- rbind(max_dia_19IFE,max_dia_20IFE,
                     max_dia_21IFE,max_dia_22IFE,max_dia_23IFE,
                     max_dia_24IFE)
##View(max_diasIFE18)
max_diasIFE18 <- cbind(levels(DiasF),max_diasIFE18)
max_diasIFE18 <- as.data.frame(max_diasIFE18)
##View(max_diasIFE18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE18) <- nombres
max_diasIFE18$Fecha <- as.Date(max_diasIFE18$Fecha)
options(digits=5)
max_diasIFE18$I1 <- as.numeric(max_diasIFE18$I1)
max_diasIFE18$I2 <- as.numeric(max_diasIFE18$I2)
max_diasIFE18$I3 <- as.numeric(max_diasIFE18$I3)
##View(max_diasIFE18)



##Febrero 2022####
dfFE22 <- read.csv("Data/Years/2022/Febrero.csv")
#View(dfFE22)

dfFE22 <- dfFE22[1:1883,1:8] 
dfFE22[3:8] <- sapply(dfFE22[3:8],as.numeric)  


dfFE22$Fecha <- as.Date(dfFE22$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfFE22$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE22, Fecha==Dias[i]))
}

names(dfFE22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIFE <- vector("numeric", ncol(dfFE22[6:8]))
for (i in seq_along(dfFE22[6:8])) {# 2. secuencia
  mediaIFE[[i]] <- mean(ifelse(dfFE22[6:8][[i]]<=0,NA,dfFE22[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIFE<- vector("numeric", ncol(dfFE22[6:8]))
for (i in seq_along(dfFE22[6:8])) {# 2. secuencia
  maxIFE[[i]] <- max(dfFE22[6:8][[i]] )    # 3. cuerpo
}


###Dia 22###
media_dia_22IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IFE[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IFE[[i]] <- max(Dia_22[6:8][[i]]) 
}
###Dia 23###
media_dia_23IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IFE[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IFE[[i]] <- max(Dia_23[6:8][[i]]) 
}
###Dia 24###
media_dia_24IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IFE[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IFE[[i]] <- max(Dia_24[6:8][[i]]) 
}
###Dia 25###
media_dia_25IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IFE[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IFE[[i]] <- max(Dia_25[6:8][[i]]) 
}
###Dia 26###
media_dia_26IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IFE[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IFE[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IFE[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IFE[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IFE<-vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IFE[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IFE <- vector("numeric",  ncol(dfFE22[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IFE[[i]] <- max(Dia_28[6:8][[i]]) 
}

medias_diasIFE22 <- rbind(media_dia_22IFE,media_dia_23IFE,media_dia_24IFE,
                          media_dia_25IFE,media_dia_26IFE,media_dia_27IFE,
                          media_dia_28IFE)
##View(medias_diasI)
medias_diasIFE22 <- cbind(levels(DiasF),medias_diasIFE22)
medias_diasIFE22 <- as.data.frame(medias_diasIFE22)
##View(medias_diasIFE22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIFE22) <- nombres
medias_diasIFE22$Fecha <- as.Date(medias_diasIFE22$Fecha)
options(digits=5)
medias_diasIFE22$I1 <- as.numeric(medias_diasIFE22$I1)
medias_diasIFE22$I2 <- as.numeric(medias_diasIFE22$I2)
medias_diasIFE22$I3 <- as.numeric(medias_diasIFE22$I3)
##View(medias_diasIFE22)

max_diasIFE22 <- rbind(max_dia_22IFE,max_dia_23IFE,max_dia_24IFE,
                       max_dia_25IFE,max_dia_26IFE,max_dia_27IFE,
                       max_dia_28IFE)
##View(max_diasIFE22)
max_diasIFE22 <- cbind(levels(DiasF),max_diasIFE22)
max_diasIFE22 <- as.data.frame(max_diasIFE22)
##View(max_diasIFE22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIFE22) <- nombres
max_diasIFE22$Fecha <- as.Date(max_diasIFE22$Fecha)
options(digits=5)
max_diasIFE22$I1 <- as.numeric(max_diasIFE22$I1)
max_diasIFE22$I2 <- as.numeric(max_diasIFE22$I2)
max_diasIFE22$I3 <- as.numeric(max_diasIFE22$I3)
#####
Febrero <- rbind(medias_diasIFE13,medias_diasIFE14,medias_diasIFE15,
                 medias_diasIFE16,medias_diasIFE17,medias_diasIFE18,
                 medias_diasIFE22)
FebreroM <- rbind(max_diasIFE13,max_diasIFE14,max_diasIFE15,
                 max_diasIFE16,max_diasIFE17,max_diasIFE18,
                 max_diasIFE22)

dfFE <- rbind(dfFE13,dfFE14,dfFE15,dfFE16,dfFE17,dfFE18,dfFE22)


##Marzo 2014####
dfMA14 <- read.csv("Data/Years/2014/MARZ2014.csv")
#View(dfMA14)
dfMA14 <- dfMA14[1:2015,c(1:8)] 
dfMA14[3:8] <- sapply(dfMA14[3:8],as.numeric)  

dfMA14[3] <- dfMA14[3]/sqrt(3)
dfMA14[4] <- dfMA14[4]/sqrt(3)
dfMA14[5] <- dfMA14[5]/sqrt(3)

# dfMA14[6] <- dfMA14[6]*2
# dfMA14[7] <- dfMA14[7]*2
# dfMA14[7] <- dfMA14[7]*2



dfMA14$Fecha.de.la.Medida <- as.Date(dfMA14$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMA14$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMA14, Fecha.de.la.Medida==Dias[i]))
}

names(dfMA14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")
mediaIMA <- vector("numeric", ncol(dfMA14[6:8]))
for (i in seq_along(dfMA14[6:8])) {# 2. secuencia
  mediaIMA[[i]] <- mean(ifelse(dfMA14[6:8][[i]]<=0,NA,dfMA14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMA<- vector("numeric", ncol(dfMA14[6:8]))
for (i in seq_along(dfMA14[6:8])) {# 2. secuencia
  maxIMA[[i]] <- max(dfMA14[6:8][[i]] )    # 3. cuerpo
}

##4-enero a 13-enero##
###Dia 4###
media_dia_4IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IMA[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IMA[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IMA[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IMA[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IMA[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IMA[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IMA[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IMA[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IMA[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IMA[[i]] <- max(Dia_8[6:8][[i]]) 
}
###Dia 9###
media_dia_9IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IMA[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IMA[[i]] <- max(Dia_9[6:8][[i]]) 
}
###Dia 10###
media_dia_10IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IMA[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IMA[[i]] <- max(Dia_10[6:8][[i]]) 
}
###Dia 11###
media_dia_11IMA<-vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IMA[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IMA <- vector("numeric",  ncol(dfMA14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IMA[[i]] <- max(Dia_11[6:8][[i]]) 
}



medias_diasIMA14 <- rbind(media_dia_4IMA,media_dia_5IMA,media_dia_6IMA,
                        media_dia_7IMA,media_dia_8IMA,media_dia_9IMA,
                        media_dia_10IMA,media_dia_11IMA)
##View(medias_diasI)
medias_diasIMA14 <- cbind(levels(DiasF),medias_diasIMA14)
medias_diasIMA14 <- as.data.frame(medias_diasIMA14)
##View(medias_diasIMA14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMA14) <- nombres
medias_diasIMA14$Fecha <- as.Date(medias_diasIMA14$Fecha)
options(digits=5)
medias_diasIMA14$I1 <- as.numeric(medias_diasIMA14$I1)
medias_diasIMA14$I2 <- as.numeric(medias_diasIMA14$I2)
medias_diasIMA14$I3 <- as.numeric(medias_diasIMA14$I3)
##View(medias_diasIMA14)

max_diasIMA14 <- rbind(max_dia_4IMA,max_dia_5IMA,max_dia_6IMA,
                     max_dia_7IMA, max_dia_8IMA,max_dia_9IMA,
                     max_dia_10IMA,max_dia_11IMA)
##View(max_diasIMA14)
max_diasIMA14 <- cbind(levels(DiasF),max_diasIMA14)
max_diasIMA14 <- as.data.frame(max_diasIMA14)
##View(max_diasIMA14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMA14) <- nombres
max_diasIMA14$Fecha <- as.Date(max_diasIMA14$Fecha)
options(digits=5)
max_diasIMA14$I1 <- as.numeric(max_diasIMA14$I1)
max_diasIMA14$I2 <- as.numeric(max_diasIMA14$I2)
max_diasIMA14$I3 <- as.numeric(max_diasIMA14$I3)
##View(max_diasIMA14)


##Marzo 2015####
dfMA15 <- read.csv("Data/Years/2015/MARZO2015.csv")
#View(dfMA15)
dfMA15 <- dfMA15[1:4269,c(1:8)] 
dfMA15[3:8] <- sapply(dfMA15[3:8],as.numeric)  

dfMA15[3] <- dfMA15[3]/sqrt(3)
dfMA15[4] <- dfMA15[4]/sqrt(3)
dfMA15[5] <- dfMA15[5]/sqrt(3)

dfMA15[6] <- dfMA15[6]*2
dfMA15[7] <- dfMA15[7]*2
dfMA15[8] <- dfMA15[8]*2



dfMA15$Feha.de.la.Medda <- as.Date(dfMA15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfMA15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMA15, Feha.de.la.Medda==Dias[i]))
}

names(dfMA15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMA <- vector("numeric", ncol(dfMA15[6:8]))
for (i in seq_along(dfMA15[6:8])) {# 2. secuencia
  mediaIMA[[i]] <- mean(ifelse(dfMA15[6:8][[i]]<=0,NA,dfMA15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMA<- vector("numeric", ncol(dfMA15[6:8]))
for (i in seq_along(dfMA15[6:8])) {# 2. secuencia
  maxIMA[[i]] <- max(dfMA15[6:8][[i]] )    # 3. cuerpo
}

##5-Marzo a 20-Marzo#
###Dia 5###
media_dia_5IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IMA[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IMA[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IMA[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IMA[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IMA[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IMA[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IMA[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IMA[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IMA[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IMA[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IMA[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IMA[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IMA[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IMA[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IMA[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IMA[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IMA[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IMA[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IMA[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IMA[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMA[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMA[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMA[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMA[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMA[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMA[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMA[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMA[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMA[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMA[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMA<-vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMA[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMA <- vector("numeric",  ncol(dfMA15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMA[[i]] <- max(Dia_20[6:8][[i]]) 
}

medias_diasIMA15 <- rbind(media_dia_5IMA,media_dia_6IMA,media_dia_7IMA,
                        media_dia_8IMA,media_dia_9IMA,media_dia_10IMA,
                        media_dia_11IMA,media_dia_12IMA,media_dia_13IMA,
                        media_dia_14IMA,media_dia_15IMA,media_dia_16IMA,
                        media_dia_17IMA,media_dia_18IMA,media_dia_19IMA,
                        media_dia_20IMA)
##View(medias_diasI)
medias_diasIMA15 <- cbind(levels(DiasF),medias_diasIMA15)
medias_diasIMA15 <- as.data.frame(medias_diasIMA15)
##View(medias_diasIMA15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMA15) <- nombres
medias_diasIMA15$Fecha <- as.Date(medias_diasIMA15$Fecha)
options(digits=5)
medias_diasIMA15$I1 <- as.numeric(medias_diasIMA15$I1)
medias_diasIMA15$I2 <- as.numeric(medias_diasIMA15$I2)
medias_diasIMA15$I3 <- as.numeric(medias_diasIMA15$I3)
##View(medias_diasIMA15)

max_diasIMA15 <- rbind(max_dia_5IMA,max_dia_6IMA,max_dia_7IMA,
                     max_dia_8IMA,max_dia_9IMA,max_dia_10IMA,
                     max_dia_11IMA,max_dia_12IMA,max_dia_13IMA,
                     max_dia_14IMA,max_dia_15IMA,max_dia_16IMA,
                     max_dia_17IMA,max_dia_18IMA,max_dia_19IMA,
                     max_dia_20IMA)
##View(max_diasIMA15)
max_diasIMA15 <- cbind(levels(DiasF),max_diasIMA15)
max_diasIMA15 <- as.data.frame(max_diasIMA15)
##View(max_diasIMA15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMA15) <- nombres
max_diasIMA15$Fecha <- as.Date(max_diasIMA15$Fecha)
options(digits=5)
max_diasIMA15$I1 <- as.numeric(max_diasIMA15$I1)
max_diasIMA15$I2 <- as.numeric(max_diasIMA15$I2)
max_diasIMA15$I3 <- as.numeric(max_diasIMA15$I3)
##View(max_diasIMA15)




##Marzo 2016####
dfMA16 <- read.csv("Data/Years/2016/MARZO2016.csv")
#View(dfMA16)
dfMA16 <- dfMA16[1:1482,c(1:8)] 
dfMA16[3:8] <- sapply(dfMA16[3:8],as.numeric)  

dfMA16[3] <- dfMA16[3]/sqrt(3)
dfMA16[4] <- dfMA16[4]/sqrt(3)
dfMA16[5] <- dfMA16[5]/sqrt(3)

dfMA16[6] <- dfMA16[6]*2
dfMA16[7] <- dfMA16[7]*2
dfMA16[8] <- dfMA16[8]*2



dfMA16$Fecha.de.la.Medida <- as.Date(dfMA16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMA16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMA16, Fecha.de.la.Medida==Dias[i]))
}

names(dfMA16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMA <- vector("numeric", ncol(dfMA16[6:8]))
for (i in seq_along(dfMA16[6:8])) {# 2. secuencia
  mediaIMA[[i]] <- mean(ifelse(dfMA16[6:8][[i]]<=0,NA,dfMA16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMA<- vector("numeric", ncol(dfMA16[6:8]))
for (i in seq_along(dfMA16[6:8])) {# 2. secuencia
  maxIMA[[i]] <- max(dfMA16[6:8][[i]] )    # 3. cuerpo
}

##16-Marzo a 31-Marzo#
###Dia 16###
media_dia_16IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMA[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMA[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMA[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMA[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMA[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMA[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMA[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMA[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMA[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMA[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMA[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMA[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IMA[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IMA[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMA[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMA[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IMA[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IMA[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IMA[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IMA[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IMA[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IMA[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IMA[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IMA[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IMA[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IMA[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IMA[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IMA[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IMA[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IMA[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IMA<-vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IMA[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IMA <- vector("numeric",  ncol(dfMA16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IMA[[i]] <- max(Dia_31[6:8][[i]]) 
}

medias_diasIMA16 <- rbind(media_dia_16IMA,
                        media_dia_17IMA,
                        media_dia_18IMA,media_dia_19IMA,media_dia_20IMA,
                        media_dia_21IMA,media_dia_22IMA,media_dia_23IMA,
                        media_dia_24IMA,media_dia_25IMA,media_dia_26IMA,
                        media_dia_27IMA,
                        media_dia_28IMA,media_dia_29IMA,media_dia_30IMA,
                        media_dia_31IMA)
##View(medias_diasI)
medias_diasIMA16 <- cbind(levels(DiasF),medias_diasIMA16)
medias_diasIMA16 <- as.data.frame(medias_diasIMA16)
##View(medias_diasIMA16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMA16) <- nombres
medias_diasIMA16$Fecha <- as.Date(medias_diasIMA16$Fecha)
options(digits=5)
medias_diasIMA16$I1 <- as.numeric(medias_diasIMA16$I1)
medias_diasIMA16$I2 <- as.numeric(medias_diasIMA16$I2)
medias_diasIMA16$I3 <- as.numeric(medias_diasIMA16$I3)
##View(medias_diasIMA16)

max_diasIMA16 <- rbind(max_dia_16IMA,
                     max_dia_17IMA,
                     max_dia_18IMA,max_dia_19IMA,max_dia_20IMA,
                     max_dia_21IMA,max_dia_22IMA,max_dia_23IMA,
                     max_dia_24IMA,max_dia_25IMA,max_dia_26IMA,
                     max_dia_27IMA,
                     max_dia_28IMA,max_dia_29IMA,max_dia_30IMA,
                     max_dia_31IMA)
##View(max_diasIMA16)
max_diasIMA16 <- cbind(levels(DiasF),max_diasIMA16)
max_diasIMA16 <- as.data.frame(max_diasIMA16)
##View(max_diasIMA16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMA16) <- nombres
max_diasIMA16$Fecha <- as.Date(max_diasIMA16$Fecha)
options(digits=5)
max_diasIMA16$I1 <- as.numeric(max_diasIMA16$I1)
max_diasIMA16$I2 <- as.numeric(max_diasIMA16$I2)
max_diasIMA16$I3 <- as.numeric(max_diasIMA16$I3)
##View(max_diasIMA16)





##Marzo 2017####
dfMA17 <- read.csv("Data/Years/2017/MAR2017.csv")
#View(dfMA17)
dfMA17 <- dfMA17[1:1581,c(1:8)] 
dfMA17[3:8] <- sapply(dfMA17[3:8],as.numeric)  

dfMA17[3] <- dfMA17[3]/sqrt(3)
dfMA17[4] <- dfMA17[4]/sqrt(3)
dfMA17[5] <- dfMA17[5]/sqrt(3)

# dfMA17[6] <- dfMA17[6]*2
# dfMA17[7] <- dfMA17[7]*2
# dfMA17[8] <- dfMA17[8]*2



dfMA17$Fecha.de.la.Medida <- as.Date(dfMA17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMA17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMA17, Fecha.de.la.Medida==Dias[i]))
}

names(dfMA17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMA <- vector("numeric", ncol(dfMA17[6:8]))
for (i in seq_along(dfMA17[6:8])) {# 2. secuencia
  mediaIMA[[i]] <- mean(ifelse(dfMA17[6:8][[i]]<=0,NA,dfMA17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMA<- vector("numeric", ncol(dfMA17[6:8]))
for (i in seq_along(dfMA17[6:8])) {# 2. secuencia
  maxIMA[[i]] <- max(dfMA17[6:8][[i]] )    # 3. cuerpo
}

##1-Marzo a 30-Marzo#
###Dia 15###
media_dia_15IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMA[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMA[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMA[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMA[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMA[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMA[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMA[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMA[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMA[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMA[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMA[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMA[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMA[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMA[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IMA[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IMA[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMA[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMA[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IMA[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IMA[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IMA[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IMA[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IMA[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IMA[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IMA[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IMA[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IMA[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IMA[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IMA[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IMA[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IMA[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IMA[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IMA<-vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IMA[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IMA <- vector("numeric",  ncol(dfMA17[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IMA[[i]] <- max(Dia_31[6:8][[i]]) 
}

medias_diasIMA17 <- rbind(media_dia_15IMA,media_dia_16IMA,
                        media_dia_17IMA,
                        media_dia_18IMA,media_dia_19IMA,media_dia_20IMA,
                        media_dia_21IMA,media_dia_22IMA,media_dia_23IMA,
                        media_dia_24IMA,media_dia_25IMA,media_dia_26IMA,
                        media_dia_27IMA,
                        media_dia_28IMA,media_dia_29IMA,media_dia_30IMA,
                        media_dia_31IMA)
##View(medias_diasI)
medias_diasIMA17 <- cbind(levels(DiasF),medias_diasIMA17)
medias_diasIMA17 <- as.data.frame(medias_diasIMA17)
##View(medias_diasIMA17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMA17) <- nombres
medias_diasIMA17$Fecha <- as.Date(medias_diasIMA17$Fecha)
options(digits=5)
medias_diasIMA17$I1 <- as.numeric(medias_diasIMA17$I1)
medias_diasIMA17$I2 <- as.numeric(medias_diasIMA17$I2)
medias_diasIMA17$I3 <- as.numeric(medias_diasIMA17$I3)
##View(medias_diasIMA17)

max_diasIMA17 <- rbind(max_dia_15IMA,max_dia_16IMA,
                     max_dia_17IMA,
                     max_dia_18IMA,max_dia_19IMA,max_dia_20IMA,
                     max_dia_21IMA,max_dia_22IMA,max_dia_23IMA,
                     max_dia_24IMA,max_dia_25IMA,max_dia_26IMA,
                     max_dia_27IMA,
                     max_dia_28IMA,max_dia_29IMA,max_dia_30IMA,
                     max_dia_31IMA)
##View(max_diasIMA17)
max_diasIMA17 <- cbind(levels(DiasF),max_diasIMA17)
max_diasIMA17 <- as.data.frame(max_diasIMA17)
##View(max_diasIMA17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMA17) <- nombres
max_diasIMA17$Fecha <- as.Date(max_diasIMA17$Fecha)
options(digits=5)
max_diasIMA17$I1 <- as.numeric(max_diasIMA17$I1)
max_diasIMA17$I2 <- as.numeric(max_diasIMA17$I2)
max_diasIMA17$I3 <- as.numeric(max_diasIMA17$I3)
##View(max_diasIMA17)




##Marzo 2018####
dfMA18 <- read.csv("Data/Years/2018/MARZ2018.csv")
#View(dfMA18)
dfMA18 <- dfMA18[1:669,c(1:8)] 
dfMA18[3:8] <- sapply(dfMA18[3:8],as.numeric)  

dfMA18[3] <- dfMA18[3]/sqrt(3)
dfMA18[4] <- dfMA18[4]/sqrt(3)
dfMA18[5] <- dfMA18[5]/sqrt(3)
# 
# dfMA18[6] <- dfMA18[6]*2
# dfMA18[7] <- dfMA18[7]*2
# dfMA18[8] <- dfMA18[8]*2



dfMA18$Fecha.de.la.Medida <- as.Date(dfMA18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMA18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMA18, Fecha.de.la.Medida==Dias[i]))
}

names(dfMA18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMA <- vector("numeric", ncol(dfMA18[6:8]))
for (i in seq_along(dfMA18[6:8])) {# 2. secuencia
  mediaIMA[[i]] <- mean(ifelse(dfMA18[6:8][[i]]<=0,NA,dfMA18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMA<- vector("numeric", ncol(dfMA18[6:8]))
for (i in seq_along(dfMA18[6:8])) {# 2. secuencia
  maxIMA[[i]] <- max(dfMA18[6:8][[i]] )    # 3. cuerpo
}

##16-Marzo a 23-Marzo#

###Dia 16###
media_dia_16IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMA[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMA[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMA[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMA[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMA[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMA[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMA[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMA[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMA[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMA[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMA[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMA[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IMA[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IMA[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IMA<-vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMA[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMA <- vector("numeric",  ncol(dfMA18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMA[[i]] <- max(Dia_23[6:8][[i]]) 
}




medias_diasIMA18 <- rbind(media_dia_16IMA,
                        media_dia_17IMA,
                        media_dia_18IMA,media_dia_19IMA,media_dia_20IMA,
                        media_dia_21IMA,media_dia_22IMA,media_dia_23IMA)
##View(medias_diasI)
medias_diasIMA18 <- cbind(levels(DiasF),medias_diasIMA18)
medias_diasIMA18 <- as.data.frame(medias_diasIMA18)
##View(medias_diasIMA18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMA18) <- nombres
medias_diasIMA18$Fecha <- as.Date(medias_diasIMA18$Fecha)
options(digits=5)
medias_diasIMA18$I1 <- as.numeric(medias_diasIMA18$I1)
medias_diasIMA18$I2 <- as.numeric(medias_diasIMA18$I2)
medias_diasIMA18$I3 <- as.numeric(medias_diasIMA18$I3)
##View(medias_diasIMA18)

max_diasIMA18 <- rbind(max_dia_16IMA,
                     max_dia_17IMA,
                     max_dia_18IMA,max_dia_19IMA,max_dia_20IMA,
                     max_dia_21IMA,max_dia_22IMA,max_dia_23IMA)
##View(max_diasIMA18)
max_diasIMA18 <- cbind(levels(DiasF),max_diasIMA18)
max_diasIMA18 <- as.data.frame(max_diasIMA18)
##View(max_diasIMA18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMA18) <- nombres
max_diasIMA18$Fecha <- as.Date(max_diasIMA18$Fecha)
options(digits=5)
max_diasIMA18$I1 <- as.numeric(max_diasIMA18$I1)
max_diasIMA18$I2 <- as.numeric(max_diasIMA18$I2)
max_diasIMA18$I3 <- as.numeric(max_diasIMA18$I3)
##View(max_diasIMA18)




##Marzo 2022####
dfMA22 <- read.csv("Data/Years/2022/Marzo.csv")
#View(dfMA22)
dfMA22 <- dfMA22[1:4977,1:8] 
dfMA22[3:8] <- sapply(dfMA22[3:8],as.numeric)  

dfMA22$Fecha <- as.Date(dfMA22$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfMA22$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)
for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMA22, Fecha==Dias[i]))
}

names(dfMA22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMA <- vector("numeric", ncol(dfMA22[6:8]))
for (i in seq_along(dfMA22[6:8])) {# 2. secuencia
  mediaIMA[[i]] <- mean(ifelse(dfMA22[6:8][[i]]<=0,NA,dfMA22[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMA<- vector("numeric", ncol(dfMA22[6:8]))
for (i in seq_along(dfMA22[6:8])) {# 2. secuencia
  maxIMA[[i]] <- max(dfMA22[6:8][[i]] )    # 3. cuerpo
}

##01-marzo/11-marzo y 23-marzo/30-marzo

###Dia 1###
media_dia_1IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IMA[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IMA[[i]] <- max(Dia_1[6:8][[i]]) 
}

###Dia 2###
media_dia_2IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IMA[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}
max_dia_2IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IMA[[i]] <- max(Dia_2[6:8][[i]],na.rm = T) 
}



###Dia 3###
media_dia_3IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IMA[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IMA[[i]] <- max(Dia_3[6:8][[i]]) 
}

apply(Dia_2,2,max)
###Dia 4###
media_dia_4IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IMA[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IMA[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IMA[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IMA[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IMA[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IMA[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IMA[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IMA[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IMA[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IMA[[i]] <- max(Dia_8[6:8][[i]]) 
}
###Dia 9###
media_dia_9IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IMA[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IMA[[i]] <- max(Dia_9[6:8][[i]]) 
}
###Dia 10###
media_dia_10IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IMA[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IMA[[i]] <- max(Dia_10[6:8][[i]]) 
}
###Dia 11###
media_dia_11IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IMA[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IMA[[i]] <- max(Dia_11[6:8][[i]],na.rm = T) 
}


###Dia 12###
media_dia_12IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IMA[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IMA[[i]] <- max(Dia_12[6:8][[i]],na.rm = T) 
}

###Dia 13###
media_dia_13IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IMA[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IMA[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IMA[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IMA[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMA[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMA[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMA[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMA[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMA[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMA[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMA[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMA[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMA<-vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMA[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMA <- vector("numeric",  ncol(dfMA22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMA[[i]] <- max(Dia_19[6:8][[i]]) 
}


medias_diasIMA22 <- rbind(media_dia_1IMA,media_dia_2IMA,media_dia_3IMA,
                          media_dia_4IMA,media_dia_5IMA,media_dia_6IMA,
                          media_dia_7IMA,media_dia_8IMA,media_dia_9IMA,
                          media_dia_10IMA,media_dia_11IMA,media_dia_12IMA,
                          media_dia_13IMA,media_dia_14IMA,media_dia_15IMA,
                          media_dia_16IMA,media_dia_17IMA,media_dia_18IMA,
                          media_dia_19IMA)
##View(medias_diasIMA22)
medias_diasIMA22 <- cbind(levels(DiasF),medias_diasIMA22)
medias_diasIMA22 <- as.data.frame(medias_diasIMA22)
##View(medias_diasIMA22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMA22) <- nombres
medias_diasIMA22$Fecha <- as.Date(medias_diasIMA22$Fecha)
options(digits=5)
medias_diasIMA22$I1 <- as.numeric(medias_diasIMA22$I1)
medias_diasIMA22$I2 <- as.numeric(medias_diasIMA22$I2)
medias_diasIMA22$I3 <- as.numeric(medias_diasIMA22$I3)
##View(medias_diasIMA22)

max_diasIMA22 <- rbind(max_dia_1IMA,max_dia_2IMA,max_dia_3IMA,
                       max_dia_4IMA,max_dia_5IMA,max_dia_6IMA,
                       max_dia_7IMA,max_dia_8IMA,max_dia_9IMA,
                       max_dia_10IMA,max_dia_11IMA,max_dia_12IMA,
                       max_dia_13IMA,max_dia_14IMA,max_dia_15IMA,
                       max_dia_16IMA,max_dia_17IMA,max_dia_18IMA,
                       max_dia_19IMA)
##View(max_diasIMA22)
max_diasIMA22 <- cbind(levels(DiasF),max_diasIMA22)
max_diasIMA22 <- as.data.frame(max_diasIMA22)
##View(max_diasIMA22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMA22) <- nombres
max_diasIMA22$Fecha <- as.Date(max_diasIMA22$Fecha)
options(digits=5)
max_diasIMA22$I1 <- as.numeric(max_diasIMA22$I1)
max_diasIMA22$I2 <- as.numeric(max_diasIMA22$I2)
max_diasIMA22$I3 <- as.numeric(max_diasIMA22$I3)
##View(max_diasIMA17)


######
Marzo <- rbind(medias_diasIMA14,medias_diasIMA15,
                 medias_diasIMA16,medias_diasIMA17,
               medias_diasIMA18,medias_diasIMA22)
MarzoM <- rbind(max_diasIMA14,max_diasIMA15,
               max_diasIMA16,max_diasIMA17,
               max_diasIMA18,max_diasIMA22)
dfMA <- rbind(dfMA14,dfMA15,dfMA16,dfMA17,dfMA18,dfMA22)



##Abril 2014####
dfAB14 <- read.csv("Data/Years/2014/ABR2014.csv")
#View(dfAB14)
dfAB14 <- dfAB14[1:2060,c(1:8)] 
dfAB14[3:8] <- sapply(dfAB14[3:8],as.numeric)  

dfAB14[3] <- dfAB14[3]/sqrt(3)
dfAB14[4] <- dfAB14[4]/sqrt(3)
dfAB14[5] <- dfAB14[5]/sqrt(3)

# dfAB14[6] <- dfAB14[6]*2
# dfAB14[7] <- dfAB14[7]*2
# dfAB14[7] <- dfAB14[7]*2



dfAB14$Feha.de.la.Medda <- as.Date(dfAB14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfAB14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB14, Feha.de.la.Medda==Dias[i]))
}

names(dfAB14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAB <- vector("numeric", ncol(dfAB14[6:8]))
for (i in seq_along(dfAB14[6:8])) {# 2. secuencia
  mediaIAB[[i]] <- mean(ifelse(dfAB14[6:8][[i]]<=0,NA,dfAB14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAB<- vector("numeric", ncol(dfAB14[6:8]))
for (i in seq_along(dfAB14[6:8])) {# 2. secuencia
  maxIAB[[i]] <- max(dfAB14[6:8][[i]] )    # 3. cuerpo
}

##1-abril a 8-abril#
###Dia 1###
media_dia_1IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IAB[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IAB[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IAB[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IAB[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IAB[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IAB[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IAB[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IAB[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IAB[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IAB[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IAB[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IAB[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IAB[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IAB[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IAB<-vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAB[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAB <- vector("numeric",  ncol(dfAB14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAB[[i]] <- max(Dia_8[6:8][[i]]) 
}


medias_diasIAB14 <- rbind(media_dia_1IAB,media_dia_2IAB,media_dia_3IAB,
                        media_dia_4IAB,media_dia_5IAB,media_dia_6IAB,
                        media_dia_7IAB,media_dia_8IAB)
##View(medias_diasI)
medias_diasIAB14 <- cbind(levels(DiasF),medias_diasIAB14)
medias_diasIAB14 <- as.data.frame(medias_diasIAB14)
##View(medias_diasIAB14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAB14) <- nombres
medias_diasIAB14$Fecha <- as.Date(medias_diasIAB14$Fecha)
options(digits=5)
medias_diasIAB14$I1 <- as.numeric(medias_diasIAB14$I1)
medias_diasIAB14$I2 <- as.numeric(medias_diasIAB14$I2)
medias_diasIAB14$I3 <- as.numeric(medias_diasIAB14$I3)
##View(medias_diasIAB14)

max_diasIAB14 <- rbind(max_dia_1IAB,max_dia_2IAB,max_dia_3IAB,
                     max_dia_4IAB,max_dia_5IAB,max_dia_6IAB,
                     max_dia_7IAB,max_dia_8IAB)
##View(max_diasIAB14)
max_diasIAB14 <- cbind(levels(DiasF),max_diasIAB14)
max_diasIAB14 <- as.data.frame(max_diasIAB14)
##View(max_diasIAB14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAB14) <- nombres
max_diasIAB14$Fecha <- as.Date(max_diasIAB14$Fecha)
options(digits=5)
max_diasIAB14$I1 <- as.numeric(max_diasIAB14$I1)
max_diasIAB14$I2 <- as.numeric(max_diasIAB14$I2)
max_diasIAB14$I3 <- as.numeric(max_diasIAB14$I3)
##View(max_diasIAB14)







##Abril 2015####
dfAB15 <- read.csv("Data/Years/2015/ABRIL2015.csv")
#View(dfAB15)
dfAB15 <- dfAB15[1:2589,c(1:8)] 
dfAB15[3:8] <- sapply(dfAB15[3:8],as.numeric)  

dfAB15[3] <- dfAB15[3]/sqrt(3)
dfAB15[4] <- dfAB15[4]/sqrt(3)
dfAB15[5] <- dfAB15[5]/sqrt(3)

# dfAB15[6] <- dfAB15[6]*2
# dfAB15[7] <- dfAB15[7]*2
# dfAB15[7] <- dfAB15[7]*2



dfAB15$Feha.de.la.Medda <- as.Date(dfAB15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfAB15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)
Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB15, Feha.de.la.Medda==Dias[i]))
}

names(dfAB15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAB <- vector("numeric", ncol(dfAB15[6:8]))
for (i in seq_along(dfAB15[6:8])) {# 2. secuencia
  mediaIAB[[i]] <- mean(ifelse(dfAB15[6:8][[i]]<=0,NA,dfAB15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAB<- vector("numeric", ncol(dfAB15[6:8]))
for (i in seq_along(dfAB15[6:8])) {# 2. secuencia
  maxIAB[[i]] <- max(dfAB15[6:8][[i]] )    # 3. cuerpo
}

##9-Abril a 18-Abril#
###Dia 9###
media_dia_9IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAB[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAB[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAB[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAB[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAB[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAB[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IAB[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IAB[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IAB[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IAB[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IAB[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IAB[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IAB[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IAB[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IAB[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IAB[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAB[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAB[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IAB<-vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAB[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAB <- vector("numeric",  ncol(dfAB15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAB[[i]] <- max(Dia_18[6:8][[i]]) 
}



medias_diasIAB15 <- rbind(media_dia_9IAB,media_dia_10IAB,
                        media_dia_11IAB,media_dia_12IAB,media_dia_13IAB,
                        media_dia_14IAB,media_dia_15IAB,media_dia_16IAB,
                        media_dia_17IAB,media_dia_18IAB)
##View(medias_diasI)
medias_diasIAB15 <- cbind(levels(DiasF),medias_diasIAB15)
medias_diasIAB15 <- as.data.frame(medias_diasIAB15)
##View(medias_diasIAB15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAB15) <- nombres
medias_diasIAB15$Fecha <- as.Date(medias_diasIAB15$Fecha)
options(digits=5)
medias_diasIAB15$I1 <- as.numeric(medias_diasIAB15$I1)
medias_diasIAB15$I2 <- as.numeric(medias_diasIAB15$I2)
medias_diasIAB15$I3 <- as.numeric(medias_diasIAB15$I3)
##View(medias_diasIAB15)

max_diasIAB15 <- rbind(max_dia_9IAB,max_dia_10IAB,
                     max_dia_11IAB,max_dia_12IAB,max_dia_13IAB,
                     max_dia_14IAB,max_dia_15IAB,max_dia_16IAB,
                     max_dia_17IAB,max_dia_18IAB)
##View(max_diasIAB15)
max_diasIAB15 <- cbind(levels(DiasF),max_diasIAB15)
max_diasIAB15 <- as.data.frame(max_diasIAB15)
##View(max_diasIAB15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAB15) <- nombres
max_diasIAB15$Fecha <- as.Date(max_diasIAB15$Fecha)
options(digits=5)
max_diasIAB15$I1 <- as.numeric(max_diasIAB15$I1)
max_diasIAB15$I2 <- as.numeric(max_diasIAB15$I2)
max_diasIAB15$I3 <- as.numeric(max_diasIAB15$I3)
##View(max_diasIAB15)














##Abril 2016####
dfAB16 <- read.csv("Data/Years/2016/ABRIL2016.csv")
#View(dfAB16)
dfAB16 <- dfAB16[1:2154,c(1:8)] 
dfAB16[3:8] <- sapply(dfAB16[3:8],as.numeric)  

dfAB16[3] <- dfAB16[3]/sqrt(3)
dfAB16[4] <- dfAB16[4]/sqrt(3)
dfAB16[5] <- dfAB16[5]/sqrt(3)

dfAB16[6] <- dfAB16[6]*2
dfAB16[7] <- dfAB16[7]*2
dfAB16[8] <- dfAB16[8]*2



dfAB16$Fecha.de.la.Medida <- as.Date(dfAB16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAB16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)
for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB16, Fecha.de.la.Medida==Dias[i]))
}

names(dfAB16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAB <- vector("numeric", ncol(dfAB16[6:8]))
for (i in seq_along(dfAB16[6:8])) {# 2. secuencia
  mediaIAB[[i]] <- mean(ifelse(dfAB16[6:8][[i]]<=0,NA,dfAB16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAB<- vector("numeric", ncol(dfAB16[6:8]))
for (i in seq_along(dfAB16[6:8])) {# 2. secuencia
  maxIAB[[i]] <- max(dfAB16[6:8][[i]] )    # 3. cuerpo
}

##1-Abril a 30-Abril#
###Dia 8###
media_dia_8IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAB[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAB[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAB[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAB[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAB[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAB[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAB[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAB[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IAB[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IAB[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IAB[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IAB[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IAB[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IAB[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IAB[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IAB[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IAB[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IAB[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAB[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAB[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAB[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAB[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IAB[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IAB[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAB[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAB[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAB[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAB[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAB[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAB[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAB[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAB[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAB[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAB[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IAB[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IAB[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IAB[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IAB[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAB[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAB[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IAB[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IAB[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IAB[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IAB[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IAB<-vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IAB[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IAB <- vector("numeric",  ncol(dfAB16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IAB[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasIAB16 <- rbind(media_dia_8IAB,media_dia_9IAB,media_dia_10IAB,
                        media_dia_11IAB,media_dia_12IAB,media_dia_13IAB,
                        media_dia_14IAB,media_dia_15IAB,media_dia_16IAB,
                        media_dia_17IAB,
                        media_dia_18IAB,media_dia_19IAB,media_dia_20IAB,
                        media_dia_21IAB,media_dia_22IAB,media_dia_23IAB,
                        media_dia_24IAB,media_dia_25IAB,media_dia_26IAB,
                        media_dia_27IAB,
                        media_dia_28IAB,media_dia_29IAB,media_dia_30IAB)
##View(medias_diasI)
medias_diasIAB16 <- cbind(levels(DiasF),medias_diasIAB16)
medias_diasIAB16 <- as.data.frame(medias_diasIAB16)
##View(medias_diasIAB16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAB16) <- nombres
medias_diasIAB16$Fecha <- as.Date(medias_diasIAB16$Fecha)
options(digits=5)
medias_diasIAB16$I1 <- as.numeric(medias_diasIAB16$I1)
medias_diasIAB16$I2 <- as.numeric(medias_diasIAB16$I2)
medias_diasIAB16$I3 <- as.numeric(medias_diasIAB16$I3)
##View(medias_diasIAB16)

max_diasIAB16 <- rbind(max_dia_8IAB,max_dia_9IAB,max_dia_10IAB,
                     max_dia_11IAB,max_dia_12IAB,max_dia_13IAB,
                     max_dia_14IAB,max_dia_15IAB,max_dia_16IAB,
                     max_dia_17IAB,
                     max_dia_18IAB,max_dia_19IAB,max_dia_20IAB,
                     max_dia_21IAB,max_dia_22IAB,max_dia_23IAB,
                     max_dia_24IAB,max_dia_25IAB,max_dia_26IAB,
                     max_dia_27IAB,
                     max_dia_28IAB,max_dia_29IAB,max_dia_30IAB)
##View(max_diasIAB16)
max_diasIAB16 <- cbind(levels(DiasF),max_diasIAB16)
max_diasIAB16 <- as.data.frame(max_diasIAB16)
##View(max_diasIAB16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAB16) <- nombres
max_diasIAB16$Fecha <- as.Date(max_diasIAB16$Fecha)
options(digits=5)
max_diasIAB16$I1 <- as.numeric(max_diasIAB16$I1)
max_diasIAB16$I2 <- as.numeric(max_diasIAB16$I2)
max_diasIAB16$I3 <- as.numeric(max_diasIAB16$I3)
##View(max_diasIAB16)




##Abril 2017####
dfAB17 <- read.csv("Data/Years/2017/ABR2017.csv")
#View(dfAB17)
dfAB17 <- dfAB17[1:1099,c(1:8)] 
dfAB17[3:8] <- sapply(dfAB17[3:8],as.numeric)  

dfAB17[3] <- dfAB17[3]/sqrt(3)
dfAB17[4] <- dfAB17[4]/sqrt(3)
dfAB17[5] <- dfAB17[5]/sqrt(3)

# dfAB17[6] <- dfAB17[6]*2
# dfAB17[7] <- dfAB17[7]*2
# dfAB17[8] <- dfAB17[8]*2



dfAB17$Fecha.de.la.Medida <- as.Date(dfAB17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAB17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB17, Fecha.de.la.Medida==Dias[i]))
}

names(dfAB17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAB <- vector("numeric", ncol(dfAB17[6:8]))
for (i in seq_along(dfAB17[6:8])) {# 2. secuencia
  mediaIAB[[i]] <- mean(ifelse(dfAB17[6:8][[i]]<=0,NA,dfAB17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAB<- vector("numeric", ncol(dfAB17[6:8]))
for (i in seq_along(dfAB17[6:8])) {# 2. secuencia
  maxIAB[[i]] <- max(dfAB17[6:8][[i]] )    # 3. cuerpo
}

##19-Abril a 30-Abril#
###Dia 19###
media_dia_19IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IAB[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IAB[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAB[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAB[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAB[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAB[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAB[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAB[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAB[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAB[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAB[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAB[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IAB[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IAB[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IAB[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IAB[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAB[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAB[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IAB[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IAB[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IAB[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IAB[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IAB<-vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IAB[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IAB <- vector("numeric",  ncol(dfAB17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IAB[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasIAB17 <- rbind(media_dia_19IAB,media_dia_20IAB,
                        media_dia_21IAB,media_dia_22IAB,media_dia_23IAB,
                        media_dia_24IAB,media_dia_25IAB,media_dia_26IAB,
                        media_dia_27IAB,
                        media_dia_28IAB,media_dia_29IAB,media_dia_30IAB)
##View(medias_diasI)
medias_diasIAB17 <- cbind(levels(DiasF),medias_diasIAB17)
medias_diasIAB17 <- as.data.frame(medias_diasIAB17)
##View(medias_diasIAB17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAB17) <- nombres
medias_diasIAB17$Fecha <- as.Date(medias_diasIAB17$Fecha)
options(digits=5)
medias_diasIAB17$I1 <- as.numeric(medias_diasIAB17$I1)
medias_diasIAB17$I2 <- as.numeric(medias_diasIAB17$I2)
medias_diasIAB17$I3 <- as.numeric(medias_diasIAB17$I3)
##View(medias_diasIAB17)

max_diasIAB17 <- rbind(max_dia_19IAB,max_dia_20IAB,
                     max_dia_21IAB,max_dia_22IAB,max_dia_23IAB,
                     max_dia_24IAB,max_dia_25IAB,max_dia_26IAB,
                     max_dia_27IAB,
                     max_dia_28IAB,max_dia_29IAB,max_dia_30IAB)
##View(max_diasIAB17)
max_diasIAB17 <- cbind(levels(DiasF),max_diasIAB17)
max_diasIAB17 <- as.data.frame(max_diasIAB17)
##View(max_diasIAB17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAB17) <- nombres
max_diasIAB17$Fecha <- as.Date(max_diasIAB17$Fecha)
options(digits=5)
max_diasIAB17$I1 <- as.numeric(max_diasIAB17$I1)
max_diasIAB17$I2 <- as.numeric(max_diasIAB17$I2)
max_diasIAB17$I3 <- as.numeric(max_diasIAB17$I3)
##View(max_diasIAB17)




##Abril 2018####
dfAB18 <- read.csv("Data/Years/2018/ABR2018.csv")
#View(dfAB18)
dfAB18 <- dfAB18[1:670,c(1:8)] 
dfAB18[3:8] <- sapply(dfAB18[3:8],as.numeric)  

dfAB18[3] <- dfAB18[3]/sqrt(3)
dfAB18[4] <- dfAB18[4]/sqrt(3)
dfAB18[5] <- dfAB18[5]/sqrt(3)

dfAB18[6] <- dfAB18[6]*2
dfAB18[7] <- dfAB18[7]*2
dfAB18[8] <- dfAB18[8]*2



dfAB18$Fecha.de.la.Medida <- as.Date(dfAB18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAB18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB18, Fecha.de.la.Medida==Dias[i]))
}

names(dfAB18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAB <- vector("numeric", ncol(dfAB18[6:8]))
for (i in seq_along(dfAB18[6:8])) {# 2. secuencia
  mediaIAB[[i]] <- mean(ifelse(dfAB18[6:8][[i]]<=0,NA,dfAB18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAB<- vector("numeric", ncol(dfAB18[6:8]))
for (i in seq_along(dfAB18[6:8])) {# 2. secuencia
  maxIAB[[i]] <- max(dfAB18[6:8][[i]] )    # 3. cuerpo
}

##17-Abril a 24-Abril#

###Dia 17###
media_dia_17IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAB[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAB[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAB[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAB[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IAB[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IAB[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAB[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAB[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAB[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAB[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAB[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAB[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAB[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAB[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IAB<-vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAB[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAB <- vector("numeric",  ncol(dfAB18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAB[[i]] <- max(Dia_24[6:8][[i]]) 
}


medias_diasIAB18 <- rbind(
  media_dia_17IAB,
  media_dia_18IAB,media_dia_19IAB,media_dia_20IAB,
  media_dia_21IAB,media_dia_22IAB,media_dia_23IAB,
  media_dia_24IAB)
##View(medias_diasI)
medias_diasIAB18 <- cbind(levels(DiasF),medias_diasIAB18)
medias_diasIAB18 <- as.data.frame(medias_diasIAB18)
##View(medias_diasIAB18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAB18) <- nombres
medias_diasIAB18$Fecha <- as.Date(medias_diasIAB18$Fecha)
options(digits=5)
medias_diasIAB18$I1 <- as.numeric(medias_diasIAB18$I1)
medias_diasIAB18$I2 <- as.numeric(medias_diasIAB18$I2)
medias_diasIAB18$I3 <- as.numeric(medias_diasIAB18$I3)
##View(medias_diasIAB18)

max_diasIAB18 <- rbind(max_dia_17IAB,
                     max_dia_18IAB,max_dia_19IAB,max_dia_20IAB,
                     max_dia_21IAB,max_dia_22IAB,max_dia_23IAB,
                     max_dia_24IAB)
##View(max_diasIAB18)
max_diasIAB18 <- cbind(levels(DiasF),max_diasIAB18)
max_diasIAB18 <- as.data.frame(max_diasIAB18)
##View(max_diasIAB18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAB18) <- nombres
max_diasIAB18$Fecha <- as.Date(max_diasIAB18$Fecha)
options(digits=5)
max_diasIAB18$I1 <- as.numeric(max_diasIAB18$I1)
max_diasIAB18$I2 <- as.numeric(max_diasIAB18$I2)
max_diasIAB18$I3 <- as.numeric(max_diasIAB18$I3)
##View(max_diasIAB18)


##Abril 2022####
dfAB22 <- read.csv("Data/Years/2022/Abril.csv")
#View(dfAB22)
dfAB22 <- dfAB22[1:6932,1:8] 
dfAB22[3:8] <- sapply(dfAB22[3:8],as.numeric)  

dfAB22$Fecha <- as.Date(dfAB22$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfAB22$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB22, Fecha==Dias[i]))
}
names(dfAB22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAB <- vector("numeric", ncol(dfAB22[6:8]))
for (i in seq_along(dfAB22[6:8])) {# 2. secuencia
  mediaIAB[[i]] <- mean(ifelse(dfAB22[6:8][[i]]<=0,NA,dfAB22[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAB<- vector("numeric", ncol(dfAB22[6:8]))
for (i in seq_along(dfAB22[6:8])) {# 2. secuencia
  maxIAB[[i]] <- max(dfAB22[6:8][[i]] )    # 3. cuerpo
}

###abril 4 / abril 30

###Dia 4###
media_dia_4IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IAB[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IAB[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IAB[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IAB[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IAB[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IAB[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IAB[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IAB[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAB[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAB[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAB[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAB[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAB[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAB[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAB[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAB[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IAB[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IAB[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IAB[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IAB[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IAB[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IAB[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IAB[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IAB[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IAB[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IAB[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAB[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAB[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAB[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAB[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IAB[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IAB[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAB[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAB[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAB[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAB[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAB[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAB[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAB[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAB[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAB[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAB[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IAB[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IAB[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IAB[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IAB[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAB[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAB[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IAB[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IAB[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IAB<-vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IAB[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IAB <- vector("numeric",  ncol(dfAB22[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IAB[[i]] <- max(Dia_29[6:8][[i]]) 
}



medias_diasIAB22 <- rbind(media_dia_4IAB,media_dia_5IAB,media_dia_6IAB,
                          media_dia_7IAB,media_dia_8IAB,media_dia_9IAB,
                          media_dia_10IAB,media_dia_11IAB,media_dia_12IAB,
                          media_dia_13IAB,media_dia_14IAB,media_dia_15IAB,
                          media_dia_16IAB,media_dia_17IAB,media_dia_18IAB,
                          media_dia_19IAB,media_dia_20IAB,media_dia_21IAB,
                          media_dia_22IAB,media_dia_23IAB,media_dia_24IAB,
                          media_dia_25IAB,media_dia_26IAB,media_dia_27IAB,
                          media_dia_28IAB,media_dia_29IAB)
##View(medias_diasI)
medias_diasIAB22 <- cbind(levels(DiasF),medias_diasIAB22)
medias_diasIAB22 <- as.data.frame(medias_diasIAB22)
##View(medias_diasIAB22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAB22) <- nombres
medias_diasIAB22$Fecha <- as.Date(medias_diasIAB22$Fecha)
options(digits=5)
medias_diasIAB22$I1 <- as.numeric(medias_diasIAB22$I1)
medias_diasIAB22$I2 <- as.numeric(medias_diasIAB22$I2)
medias_diasIAB22$I3 <- as.numeric(medias_diasIAB22$I3)
##View(medias_diasIAB22)

max_diasIAB22 <- rbind(max_dia_4IAB,max_dia_5IAB,max_dia_6IAB,
                       max_dia_7IAB,max_dia_8IAB,max_dia_9IAB,
                       max_dia_10IAB,max_dia_11IAB,max_dia_12IAB,
                       max_dia_13IAB,max_dia_14IAB,max_dia_15IAB,
                       max_dia_16IAB,max_dia_17IAB,max_dia_18IAB,
                       max_dia_19IAB,max_dia_20IAB,max_dia_21IAB,
                       max_dia_22IAB,max_dia_23IAB,max_dia_24IAB,
                       max_dia_25IAB,max_dia_26IAB,max_dia_27IAB,
                       max_dia_28IAB,max_dia_29IAB)
##View(max_diasIAB22)
max_diasIAB22 <- cbind(levels(DiasF),max_diasIAB22)
max_diasIAB22 <- as.data.frame(max_diasIAB22)
##View(max_diasIAB22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAB22) <- nombres
max_diasIAB22$Fecha <- as.Date(max_diasIAB22$Fecha)
options(digits=5)
max_diasIAB22$I1 <- as.numeric(max_diasIAB22$I1)
max_diasIAB22$I2 <- as.numeric(max_diasIAB22$I2)
max_diasIAB22$I3 <- as.numeric(max_diasIAB22$I3)
##View(max_diasIAB22)
#####
Abril <- rbind(medias_diasIAB14,medias_diasIAB15,
               medias_diasIAB16,medias_diasIAB17,medias_diasIAB18,
               medias_diasIAB22)
AbrilM <- rbind(max_diasIAB14,max_diasIAB15,
               max_diasIAB16,max_diasIAB17,max_diasIAB18,
               max_diasIAB22)
dfAB <- rbind(dfAB14,dfAB15,dfAB16,dfAB17,dfAB18,dfAB22)



##Mayo 2014####
dfMY14 <- read.csv("Data/Years/2014/MAY2014.csv")
#View(dfMY14)
dfMY14 <- dfMY14[1:4052,c(1:8)] 
dfMY14[3:8] <- sapply(dfMY14[3:8],as.numeric)  

dfMY14[3] <- dfMY14[3]/sqrt(3)
dfMY14[4] <- dfMY14[4]/sqrt(3)
dfMY14[5] <- dfMY14[5]/sqrt(3)

# dfMY14[6] <- dfMY14[6]*2
# dfMY14[7] <- dfMY14[7]*2
# dfMY14[7] <- dfMY14[7]*2



dfMY14$Feha.de.la.Medda <- as.Date(dfMY14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfMY14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY14, Feha.de.la.Medda==Dias[i]))
}

names(dfMY14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMY <- vector("numeric", ncol(dfMY14[6:8]))
for (i in seq_along(dfMY14[6:8])) {# 2. secuencia
  mediaIMY[[i]] <- mean(ifelse(dfMY14[6:8][[i]]<=0,NA,dfMY14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMY<- vector("numeric", ncol(dfMY14[6:8]))
for (i in seq_along(dfMY14[6:8])) {# 2. secuencia
  maxIMY[[i]] <- max(dfMY14[6:8][[i]] )    # 3. cuerpo
}

##1-mayo a 16-mayo#
###Dia 2###
media_dia_2IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IMY[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IMY[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IMY[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IMY[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IMY[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IMY[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IMY[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IMY[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IMY[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IMY[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IMY[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IMY[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IMY[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IMY[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IMY[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IMY[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IMY[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IMY[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IMY[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IMY[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IMY[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IMY[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IMY[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IMY[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IMY[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IMY[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMY[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMY[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMY<-vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMY[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMY <- vector("numeric",  ncol(dfMY14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMY[[i]] <- max(Dia_16[6:8][[i]]) 
}


medias_diasIMY14 <- rbind(media_dia_2IMY,media_dia_3IMY,media_dia_4IMY,
                        media_dia_5IMY,media_dia_6IMY,media_dia_7IMY,
                        media_dia_8IMY,media_dia_9IMY,media_dia_10IMY,
                        media_dia_11IMY,media_dia_12IMY,media_dia_13IMY,
                        media_dia_14IMY,media_dia_15IMY,media_dia_16IMY)
##View(medias_diasI)
medias_diasIMY14 <- cbind(levels(DiasF),medias_diasIMY14)
medias_diasIMY14 <- as.data.frame(medias_diasIMY14)
##View(medias_diasIMY14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMY14) <- nombres
medias_diasIMY14$Fecha <- as.Date(medias_diasIMY14$Fecha)
options(digits=5)
medias_diasIMY14$I1 <- as.numeric(medias_diasIMY14$I1)
medias_diasIMY14$I2 <- as.numeric(medias_diasIMY14$I2)
medias_diasIMY14$I3 <- as.numeric(medias_diasIMY14$I3)
##View(medias_diasIMY14)

max_diasIMY14 <- rbind(max_dia_2IMY,max_dia_3IMY,max_dia_4IMY,
                     max_dia_5IMY,max_dia_6IMY,max_dia_7IMY,
                     max_dia_8IMY,max_dia_9IMY,max_dia_10IMY,
                     max_dia_11IMY,max_dia_12IMY,max_dia_13IMY,
                     max_dia_14IMY,max_dia_15IMY,max_dia_16IMY)
##View(max_diasIMY14)
max_diasIMY14 <- cbind(levels(DiasF),max_diasIMY14)
max_diasIMY14 <- as.data.frame(max_diasIMY14)
##View(max_diasIMY14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMY14) <- nombres
max_diasIMY14$Fecha <- as.Date(max_diasIMY14$Fecha)
options(digits=5)
max_diasIMY14$I1 <- as.numeric(max_diasIMY14$I1)
max_diasIMY14$I2 <- as.numeric(max_diasIMY14$I2)
max_diasIMY14$I3 <- as.numeric(max_diasIMY14$I3)
##View(max_diasIMY14)







##Mayo 2015####
dfMY15 <- read.csv("Data/Years/2015/MAYO2015.csv")
#View(dfMY15)
dfMY15 <- dfMY15[1:5025,c(1:8)] 
dfMY15[3:8] <- sapply(dfMY15[3:8],as.numeric)  

dfMY15[3] <- dfMY15[3]/sqrt(3)
dfMY15[4] <- dfMY15[4]/sqrt(3)
dfMY15[5] <- dfMY15[5]/sqrt(3)

dfMY15[6] <- dfMY15[6]*2
dfMY15[7] <- dfMY15[7]*2
dfMY15[8] <- dfMY15[8]*2



dfMY15$Feha.de.la.Medda <- as.Date(dfMY15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfMY15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY15, Feha.de.la.Medda==Dias[i]))
}

names(dfMY15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMY <- vector("numeric", ncol(dfMY15[6:8]))
for (i in seq_along(dfMY15[6:8])) {# 2. secuencia
  mediaIMY[[i]] <- mean(ifelse(dfMY15[6:8][[i]]<=0,NA,dfMY15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMY<- vector("numeric", ncol(dfMY15[6:8]))
for (i in seq_along(dfMY15[6:8])) {# 2. secuencia
  maxIMY[[i]] <- max(dfMY15[6:8][[i]] )    # 3. cuerpo
}

##14-mayo a 31-mayo#
###Dia 14###
media_dia_14IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IMY[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IMY[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMY[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMY[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMY[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMY[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMY[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMY[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMY[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMY[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMY[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMY[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMY[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMY[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMY[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMY[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IMY[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IMY[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMY[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMY[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IMY[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IMY[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IMY[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IMY[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IMY[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IMY[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IMY[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IMY[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IMY[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IMY[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IMY[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IMY[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IMY[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IMY[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IMY<-vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IMY[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IMY <- vector("numeric",  ncol(dfMY15[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IMY[[i]] <- max(Dia_31[6:8][[i]]) 
}




medias_diasIMY15 <- rbind(media_dia_14IMY,media_dia_15IMY,media_dia_16IMY,
                        media_dia_17IMY,media_dia_18IMY,media_dia_19IMY,
                        media_dia_20IMY,media_dia_21IMY,
                        media_dia_22IMY,media_dia_23IMY,media_dia_24IMY,
                        media_dia_25IMY,media_dia_26IMY,media_dia_27IMY,
                        media_dia_28IMY,media_dia_29IMY,media_dia_30IMY,
                        media_dia_31IMY)
##View(medias_diasI)
medias_diasIMY15 <- cbind(levels(DiasF),medias_diasIMY15)
medias_diasIMY15 <- as.data.frame(medias_diasIMY15)
##View(medias_diasIMY15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMY15) <- nombres
medias_diasIMY15$Fecha <- as.Date(medias_diasIMY15$Fecha)
options(digits=5)
medias_diasIMY15$I1 <- as.numeric(medias_diasIMY15$I1)
medias_diasIMY15$I2 <- as.numeric(medias_diasIMY15$I2)
medias_diasIMY15$I3 <- as.numeric(medias_diasIMY15$I3)
##View(medias_diasIMY15)

max_diasIMY15 <- rbind(max_dia_14IMY,max_dia_15IMY,max_dia_16IMY,
                     max_dia_17IMY,max_dia_18IMY,max_dia_19IMY,
                     max_dia_20IMY,max_dia_21IMY,
                     max_dia_22IMY,max_dia_23IMY,max_dia_24IMY,
                     max_dia_25IMY,max_dia_26IMY,max_dia_27IMY,
                     max_dia_28IMY,max_dia_29IMY,max_dia_30IMY,
                     max_dia_31IMY)
##View(max_diasIMY15)
max_diasIMY15 <- cbind(levels(DiasF),max_diasIMY15)
max_diasIMY15 <- as.data.frame(max_diasIMY15)
##View(max_diasIMY15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMY15) <- nombres
max_diasIMY15$Fecha <- as.Date(max_diasIMY15$Fecha)
options(digits=5)
max_diasIMY15$I1 <- as.numeric(max_diasIMY15$I1)
max_diasIMY15$I2 <- as.numeric(max_diasIMY15$I2)
max_diasIMY15$I3 <- as.numeric(max_diasIMY15$I3)
##View(max_diasIMY15)






##Mayo 2016####
dfMY16 <- read.csv("Data/Years/2016/MAYO2016.csv")
#View(dfMY16)
dfMY16 <- dfMY16[1:1758,c(1:8)] 
dfMY16[3:8] <- sapply(dfMY16[3:8],as.numeric)  

dfMY16[3] <- dfMY16[3]/sqrt(3)
dfMY16[4] <- dfMY16[4]/sqrt(3)
dfMY16[5] <- dfMY16[5]/sqrt(3)

dfMY16[6] <- dfMY16[6]*2
dfMY16[7] <- dfMY16[7]*2
dfMY16[8] <- dfMY16[8]*2



dfMY16$Fecha.de.la.Medida <- as.Date(dfMY16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMY16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)




for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY16, Fecha.de.la.Medida==Dias[i]))
}

names(dfMY16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMY <- vector("numeric", ncol(dfMY16[6:8]))
for (i in seq_along(dfMY16[6:8])) {# 2. secuencia
  mediaIMY[[i]] <- mean(ifelse(dfMY16[6:8][[i]]<=0,NA,dfMY16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMY<- vector("numeric", ncol(dfMY16[6:8]))
for (i in seq_along(dfMY16[6:8])) {# 2. secuencia
  maxIMY[[i]] <- max(dfMY16[6:8][[i]] )    # 3. cuerpo
}

##13-Mayo a 31-Mayo#
###Dia 13###
media_dia_13IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IMY[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IMY[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IMY[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IMY[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMY[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMY[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMY[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMY[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMY[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMY[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMY[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMY[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMY[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMY[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMY[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMY[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMY[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMY[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IMY[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IMY[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMY[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMY[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IMY[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IMY[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IMY[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IMY[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IMY[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IMY[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IMY[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IMY[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IMY[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IMY[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IMY[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IMY[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IMY[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IMY[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IMY<-vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IMY[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IMY <- vector("numeric",  ncol(dfMY16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IMY[[i]] <- max(Dia_31[6:8][[i]]) 
}



medias_diasIMY16 <- rbind(media_dia_13IMY,
                        media_dia_14IMY,media_dia_15IMY,media_dia_16IMY,
                        media_dia_17IMY,
                        media_dia_18IMY,media_dia_19IMY,media_dia_20IMY,
                        media_dia_21IMY,media_dia_22IMY,media_dia_23IMY,
                        media_dia_24IMY,media_dia_25IMY,media_dia_26IMY,
                        media_dia_27IMY,
                        media_dia_28IMY,media_dia_29IMY,media_dia_30IMY,
                        media_dia_31IMY)
##View(medias_diasI)
medias_diasIMY16 <- cbind(levels(DiasF),medias_diasIMY16)
medias_diasIMY16 <- as.data.frame(medias_diasIMY16)
##View(medias_diasIMY16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMY16) <- nombres
medias_diasIMY16$Fecha <- as.Date(medias_diasIMY16$Fecha)
options(digits=5)
medias_diasIMY16$I1 <- as.numeric(medias_diasIMY16$I1)
medias_diasIMY16$I2 <- as.numeric(medias_diasIMY16$I2)
medias_diasIMY16$I3 <- as.numeric(medias_diasIMY16$I3)
##View(medias_diasIMY16)

max_diasIMY16 <- rbind(max_dia_13IMY,
                     max_dia_14IMY,max_dia_15IMY,max_dia_16IMY,
                     max_dia_17IMY,
                     max_dia_18IMY,max_dia_19IMY,max_dia_20IMY,
                     max_dia_21IMY,max_dia_22IMY,max_dia_23IMY,
                     max_dia_24IMY,max_dia_25IMY,max_dia_26IMY,
                     max_dia_27IMY,
                     max_dia_28IMY,max_dia_29IMY,max_dia_30IMY,
                     max_dia_31IMY)
##View(max_diasIMY16)
max_diasIMY16 <- cbind(levels(DiasF),max_diasIMY16)
max_diasIMY16 <- as.data.frame(max_diasIMY16)
##View(max_diasIMY16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMY16) <- nombres
max_diasIMY16$Fecha <- as.Date(max_diasIMY16$Fecha)
options(digits=5)
max_diasIMY16$I1 <- as.numeric(max_diasIMY16$I1)
max_diasIMY16$I2 <- as.numeric(max_diasIMY16$I2)
max_diasIMY16$I3 <- as.numeric(max_diasIMY16$I3)
##View(max_diasIMY16)




##Mayo 2017####
dfMY17 <- read.csv("Data/Years/2017/MAY2017.csv")
#View(dfMY17)
dfMY17 <- dfMY17[1:1483,c(1:8)] 
dfMY17[3:8] <- sapply(dfMY17[3:8],as.numeric)  

dfMY17[3] <- dfMY17[3]/sqrt(3)
dfMY17[4] <- dfMY17[4]/sqrt(3)
dfMY17[5] <- dfMY17[5]/sqrt(3)
# 
# dfMY17[6] <- dfMY17[6]*2
# dfMY17[7] <- dfMY17[7]*2
# dfMY17[8] <- dfMY17[8]*2



dfMY17$Fecha.de.la.Medida <- as.Date(dfMY17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMY17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY17, Fecha.de.la.Medida==Dias[i]))
}

names(dfMY17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMY <- vector("numeric", ncol(dfMY17[6:8]))
for (i in seq_along(dfMY17[6:8])) {# 2. secuencia
  mediaIMY[[i]] <- mean(ifelse(dfMY17[6:8][[i]]<=0,NA,dfMY17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMY<- vector("numeric", ncol(dfMY17[6:8]))
for (i in seq_along(dfMY17[6:8])) {# 2. secuencia
  maxIMY[[i]] <- max(dfMY17[6:8][[i]] )    # 3. cuerpo
}

##16-Mayo a 31-Mayo#
###Dia 16###
media_dia_16IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMY[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMY[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMY[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMY[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMY[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMY[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMY[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMY[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMY[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMY[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMY[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMY[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IMY[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IMY[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMY[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMY[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IMY[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IMY[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IMY[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IMY[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IMY[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IMY[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IMY[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IMY[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IMY[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IMY[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IMY[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IMY[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IMY[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IMY[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IMY<-vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IMY[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IMY <- vector("numeric",  ncol(dfMY17[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IMY[[i]] <- max(Dia_31[6:8][[i]]) 
}



medias_diasIMY17 <- rbind(media_dia_16IMY,
                        media_dia_17IMY,
                        media_dia_18IMY,media_dia_19IMY,media_dia_20IMY,
                        media_dia_21IMY,media_dia_22IMY,media_dia_23IMY,
                        media_dia_24IMY,media_dia_25IMY,media_dia_26IMY,
                        media_dia_27IMY,
                        media_dia_28IMY,media_dia_29IMY,media_dia_30IMY,
                        media_dia_31IMY)
##View(medias_diasI)
medias_diasIMY17 <- cbind(levels(DiasF),medias_diasIMY17)
medias_diasIMY17 <- as.data.frame(medias_diasIMY17)
##View(medias_diasIMY17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMY17) <- nombres
medias_diasIMY17$Fecha <- as.Date(medias_diasIMY17$Fecha)
options(digits=5)
medias_diasIMY17$I1 <- as.numeric(medias_diasIMY17$I1)
medias_diasIMY17$I2 <- as.numeric(medias_diasIMY17$I2)
medias_diasIMY17$I3 <- as.numeric(medias_diasIMY17$I3)
##View(medias_diasIMY17)

max_diasIMY17 <- rbind(max_dia_16IMY,
                     max_dia_17IMY,
                     max_dia_18IMY,max_dia_19IMY,max_dia_20IMY,
                     max_dia_21IMY,max_dia_22IMY,max_dia_23IMY,
                     max_dia_24IMY,max_dia_25IMY,max_dia_26IMY,
                     max_dia_27IMY,
                     max_dia_28IMY,max_dia_29IMY,max_dia_30IMY,
                     max_dia_31IMY)
##View(max_diasIMY17)
max_diasIMY17 <- cbind(levels(DiasF),max_diasIMY17)
max_diasIMY17 <- as.data.frame(max_diasIMY17)
##View(max_diasIMY17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMY17) <- nombres
max_diasIMY17$Fecha <- as.Date(max_diasIMY17$Fecha)
options(digits=5)
max_diasIMY17$I1 <- as.numeric(max_diasIMY17$I1)
max_diasIMY17$I2 <- as.numeric(max_diasIMY17$I2)
max_diasIMY17$I3 <- as.numeric(max_diasIMY17$I3)
##View(max_diasIMY17)




##Mayo 2018####
dfMY18 <- read.csv("Data/Years/2018/MAY2018.csv")
#View(dfMY18)
dfMY18 <- dfMY18[1:767,c(1:8)] 
dfMY18[3:8] <- sapply(dfMY18[3:8],as.numeric)  

dfMY18[3] <- dfMY18[3]/sqrt(3)
dfMY18[4] <- dfMY18[4]/sqrt(3)
dfMY18[5] <- dfMY18[5]/sqrt(3)

# dfMY18[6] <- dfMY18[6]*2
# dfMY18[7] <- dfMY18[7]*2
# dfMY18[8] <- dfMY18[8]*2



dfMY18$Fecha.de.la.Medida <- as.Date(dfMY18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfMY18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY18, Fecha.de.la.Medida==Dias[i]))
}

names(dfMY18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMY <- vector("numeric", ncol(dfMY18[6:8]))
for (i in seq_along(dfMY18[6:8])) {# 2. secuencia
  mediaIMY[[i]] <- mean(ifelse(dfMY18[6:8][[i]]<=0,NA,dfMY18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMY<- vector("numeric", ncol(dfMY18[6:8]))
for (i in seq_along(dfMY18[6:8])) {# 2. secuencia
  maxIMY[[i]] <- max(dfMY18[6:8][[i]] )    # 3. cuerpo
}

##23-Mayo a 31-Mayo#
###Dia 23###
media_dia_23IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IMY[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IMY[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IMY[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IMY[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IMY[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IMY[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IMY[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IMY[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IMY[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IMY[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IMY[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IMY[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IMY[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IMY[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IMY[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IMY[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IMY<-vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IMY[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IMY <- vector("numeric",  ncol(dfMY18[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IMY[[i]] <- max(Dia_31[6:8][[i]]) 
}

medias_diasIMY18 <- rbind(media_dia_23IMY,
                        media_dia_24IMY,media_dia_25IMY,media_dia_26IMY,
                        media_dia_27IMY,
                        media_dia_28IMY,media_dia_29IMY,media_dia_30IMY,
                        media_dia_31IMY)
##View(medias_diasI)
medias_diasIMY18 <- cbind(levels(DiasF),medias_diasIMY18)
medias_diasIMY18 <- as.data.frame(medias_diasIMY18)
##View(medias_diasIMY18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMY18) <- nombres
medias_diasIMY18$Fecha <- as.Date(medias_diasIMY18$Fecha)
options(digits=5)
medias_diasIMY18$I1 <- as.numeric(medias_diasIMY18$I1)
medias_diasIMY18$I2 <- as.numeric(medias_diasIMY18$I2)
medias_diasIMY18$I3 <- as.numeric(medias_diasIMY18$I3)
##View(medias_diasIMY18)

max_diasIMY18 <- rbind(max_dia_23IMY,
                     max_dia_24IMY,max_dia_25IMY,max_dia_26IMY,
                     max_dia_27IMY,
                     max_dia_28IMY,max_dia_29IMY,max_dia_30IMY,
                     max_dia_31IMY)
##View(max_diasIMY18)
max_diasIMY18 <- cbind(levels(DiasF),max_diasIMY18)
max_diasIMY18 <- as.data.frame(max_diasIMY18)
##View(max_diasIMY18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMY18) <- nombres
max_diasIMY18$Fecha <- as.Date(max_diasIMY18$Fecha)
options(digits=5)
max_diasIMY18$I1 <- as.numeric(max_diasIMY18$I1)
max_diasIMY18$I2 <- as.numeric(max_diasIMY18$I2)
max_diasIMY18$I3 <- as.numeric(max_diasIMY18$I3)
##View(max_diasIMY18)



##Mayo 2022####
dfMY22 <- read.csv("Data/Years/2022/Mayo.csv")
#View(dfMY22)
dfMY22 <- dfMY22[1:5449,1:8] 
dfMY22[3:8] <- sapply(dfMY22[3:8],as.numeric)  

dfMY22$Fecha <- as.Date(dfMY22$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfMY22$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY22, Fecha==Dias[i]))
}

names(dfMY22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIMY <- vector("numeric", ncol(dfMY22[6:8]))
for (i in seq_along(dfMY22[6:8])) {# 2. secuencia
  mediaIMY[[i]] <- mean(ifelse(dfMY22[6:8][[i]]<=0,NA,dfMY22[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIMY<- vector("numeric", ncol(dfMY22[6:8]))
for (i in seq_along(dfMY22[6:8])) {# 2. secuencia
  maxIMY[[i]] <- max(dfMY22[6:8][[i]] )    # 3. cuerpo
}

##1-mayo a 16-mayo#
###Dia 2###
media_dia_1IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IMY[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IMY[[i]] <- max(Dia_1[6:8][[i]]) 
}

###Dia 2###
media_dia_2IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IMY[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IMY[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IMY[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IMY[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IMY[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IMY[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IMY[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IMY[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IMY[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IMY[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IMY[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IMY[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IMY[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IMY[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IMY[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IMY[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IMY[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IMY[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IMY[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IMY[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IMY[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IMY[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IMY[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IMY[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IMY[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IMY[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IMY[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IMY[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IMY[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IMY[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IMY[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IMY[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IMY[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IMY[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IMY[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IMY[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IMY[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IMY[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IMY<-vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IMY[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IMY <- vector("numeric",  ncol(dfMY22[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IMY[[i]] <- max(Dia_21[6:8][[i]]) 
}


medias_diasIMY22 <- rbind(media_dia_1IMY,
                          media_dia_2IMY,media_dia_3IMY,media_dia_4IMY,
                          media_dia_5IMY,media_dia_6IMY,media_dia_7IMY,
                          media_dia_8IMY,media_dia_9IMY,media_dia_10IMY,
                          media_dia_11IMY,media_dia_12IMY,media_dia_13IMY,
                          media_dia_14IMY,media_dia_15IMY,media_dia_16IMY,
                          media_dia_17IMY,media_dia_18IMY,media_dia_19IMY,
                          media_dia_20IMY,media_dia_21IMY
)
##View(medias_diasI)
medias_diasIMY22 <- cbind(levels(DiasF),medias_diasIMY22)
medias_diasIMY22 <- as.data.frame(medias_diasIMY22)
##View(medias_diasIMY22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIMY22) <- nombres
medias_diasIMY22$Fecha <- as.Date(medias_diasIMY22$Fecha)
options(digits=5)
medias_diasIMY22$I1 <- as.numeric(medias_diasIMY22$I1)
medias_diasIMY22$I2 <- as.numeric(medias_diasIMY22$I2)
medias_diasIMY22$I3 <- as.numeric(medias_diasIMY22$I3)
##View(medias_diasIMY22)

max_diasIMY22 <- rbind(max_dia_1IMY,
                       max_dia_2IMY,max_dia_3IMY,max_dia_4IMY,
                       max_dia_5IMY,max_dia_6IMY,max_dia_7IMY,
                       max_dia_8IMY,max_dia_9IMY,max_dia_10IMY,
                       max_dia_11IMY,max_dia_12IMY,max_dia_13IMY,
                       max_dia_14IMY,max_dia_15IMY,max_dia_16IMY,
                       max_dia_17IMY,max_dia_18IMY,max_dia_19IMY,
                       max_dia_20IMY,max_dia_21IMY)
##View(max_diasIMY22)
max_diasIMY22 <- cbind(levels(DiasF),max_diasIMY22)
max_diasIMY22 <- as.data.frame(max_diasIMY22)
##View(max_diasIMY22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIMY22) <- nombres
max_diasIMY22$Fecha <- as.Date(max_diasIMY22$Fecha)
options(digits=5)
max_diasIMY22$I1 <- as.numeric(max_diasIMY22$I1)
max_diasIMY22$I2 <- as.numeric(max_diasIMY22$I2)
max_diasIMY22$I3 <- as.numeric(max_diasIMY22$I3)
##View(max_diasIMY22)

#####
Mayo <- rbind(medias_diasIMY14,medias_diasIMY15,
               medias_diasIMY16,medias_diasIMY17,medias_diasIMY18,
              medias_diasIMY22)
MayoM <- rbind(max_diasIMY14,max_diasIMY15,
              max_diasIMY16,max_diasIMY17,max_diasIMY18,
              max_diasIMY22)
dfMY <- rbind(dfMY14,dfMY15,dfMY16,dfMY17,dfMY18,dfMY22)


##Junio 2013####

dfJN13 <- read.csv("Data/Years/2013/JUn2013.csv")
#View(dfAG)
dfJN13 <- dfJN13[1:2016,1:8]
dfJN13[3:8] <- sapply(dfJN13[3:8],as.numeric)  

dfJN13[3] <- dfJN13[3]/sqrt(3)
dfJN13[4] <- dfJN13[4]/sqrt(3)
dfJN13[5] <- dfJN13[5]/sqrt(3)

dfJN13$Fecha.de.la.Medida <- as.Date(dfJN13$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfJN13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJN13, Fecha.de.la.Medida==Dias[i]))
}

names(dfJN13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJN <- vector("numeric", ncol(dfJN13[6:8]))
for (i in seq_along(dfJN13[6:8])) {# 2. secuencia
  mediaIJN[[i]] <- mean(ifelse(dfJN13[6:8][[i]]<=0,NA,dfJN13[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJN<- vector("numeric", ncol(dfJN13[6:8]))
for (i in seq_along(dfJN13[6:8])) {# 2. secuencia
  maxIJN[[i]] <- max(dfJN13[6:8][[i]] )    # 3. cuerpo
}

##12-enero a 19-enero##
###Dia 22###
media_dia_12IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IJN[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T)
}

max_dia_12IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IJN[[i]] <- max(Dia_12[6:8][[i]]) 
}
###Dia 13###
media_dia_13IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IJN[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IJN[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IJN[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IJN[[i]] <- max(Dia_14[6:8][[i]]) 
}
###Dia 15###
media_dia_15IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IJN[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IJN[[i]] <- max(Dia_15[6:8][[i]]) 
}
###Dia 16###
media_dia_16IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IJN[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IJN[[i]] <- max(Dia_16[6:8][[i]]) 
}
###Dia 17###
media_dia_17IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IJN[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IJN[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IJN[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IJN[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IJN<-vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IJN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IJN <- vector("numeric",  ncol(dfJN13[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IJN[[i]] <- max(Dia_19[6:8][[i]]) 
}

medias_diasIJN13 <- rbind(media_dia_12IJN,media_dia_13IJN,media_dia_14IJN,
                        media_dia_15IJN,media_dia_16IJN,media_dia_17IJN,
                        media_dia_18IJN,media_dia_19IJN)
##View(medias_diasI)
medias_diasIJN13 <- cbind(levels(DiasF),medias_diasIJN13)
medias_diasIJN13 <- as.data.frame(medias_diasIJN13)
##View(medias_diasIJN13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN13) <- nombres
medias_diasIJN13$Fecha <- as.Date(medias_diasIJN13$Fecha)
options(digits=5)
medias_diasIJN13$I1 <- as.numeric(medias_diasIJN13$I1)
medias_diasIJN13$I2 <- as.numeric(medias_diasIJN13$I2)
medias_diasIJN13$I3 <- as.numeric(medias_diasIJN13$I3)
##View(medias_diasIJN13)

max_diasIJN13 <- rbind(max_dia_12IJN,max_dia_13IJN,max_dia_14IJN,
                     max_dia_15IJN,max_dia_16IJN,max_dia_17IJN,
                     max_dia_18IJN,max_dia_19IJN)
##View(max_diasIJN13)
max_diasIJN13 <- cbind(levels(DiasF),max_diasIJN13)
max_diasIJN13 <- as.data.frame(max_diasIJN13)
##View(max_diasIJN13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN13) <- nombres
max_diasIJN13$Fecha <- as.Date(max_diasIJN13$Fecha)
options(digits=5)
max_diasIJN13$I1 <- as.numeric(max_diasIJN13$I1)
max_diasIJN13$I2 <- as.numeric(max_diasIJN13$I2)
max_diasIJN13$I3 <- as.numeric(max_diasIJN13$I3)
##View(max_diasIJN13)






##Junio 2014####
dfJN14 <- read.csv("Data/Years/2014/JUN2014.csv")
#View(dfJN14)
dfJN14 <- dfJN14[1:1728,c(1:8)] 
dfJN14[3:8] <- sapply(dfJN14[3:8],as.numeric)  

dfJN14[3] <- dfJN14[3]/sqrt(3)
dfJN14[4] <- dfJN14[4]/sqrt(3)
dfJN14[5] <- dfJN14[5]/sqrt(3)

# dfJN14[6] <- dfJN14[6]*2
# dfJN14[7] <- dfJN14[7]*2
# dfJN14[7] <- dfJN14[7]*2



dfJN14$Fecha.de.la.Medda <- as.Date(dfJN14$Fecha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfJN14$Fecha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJN14, Fecha.de.la.Medda==Dias[i]))
}

names(dfJN14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJN <- vector("numeric", ncol(dfJN14[6:8]))
for (i in seq_along(dfJN14[6:8])) {# 2. secuencia
  mediaIJN[[i]] <- mean(ifelse(dfJN14[6:8][[i]]<=0,NA,dfJN14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJN<- vector("numeric", ncol(dfJN14[6:8]))
for (i in seq_along(dfJN14[6:8])) {# 2. secuencia
  maxIJN[[i]] <- max(dfJN14[6:8][[i]] )    # 3. cuerpo
}

##10-junio a 15-junio#
###Dia 10###
media_dia_10IJN<-vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IJN[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IJN <- vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IJN[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IJN<-vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IJN[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IJN <- vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IJN[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IJN<-vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IJN[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IJN <- vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IJN[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IJN<-vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IJN[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IJN <- vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IJN[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IJN<-vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IJN[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IJN <- vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IJN[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IJN<-vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IJN[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IJN <- vector("numeric",  ncol(dfJN14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IJN[[i]] <- max(Dia_15[6:8][[i]]) 
}



medias_diasIJN14 <- rbind(media_dia_10IJN,media_dia_11IJN,media_dia_12IJN,
                        media_dia_13IJN,media_dia_14IJN,media_dia_15IJN)
##View(medias_diasI)
medias_diasIJN14 <- cbind(levels(DiasF),medias_diasIJN14)
medias_diasIJN14 <- as.data.frame(medias_diasIJN14)
##View(medias_diasIJN14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN14) <- nombres
medias_diasIJN14$Fecha <- as.Date(medias_diasIJN14$Fecha)
options(digits=5)
medias_diasIJN14$I1 <- as.numeric(medias_diasIJN14$I1)
medias_diasIJN14$I2 <- as.numeric(medias_diasIJN14$I2)
medias_diasIJN14$I3 <- as.numeric(medias_diasIJN14$I3)
##View(medias_diasIJN14)

max_diasIJN14 <- rbind(max_dia_10IJN,max_dia_11IJN,max_dia_12IJN,
                     max_dia_13IJN,max_dia_14IJN,max_dia_15IJN)
##View(max_diasI)
max_diasIJN14 <- cbind(levels(DiasF),max_diasIJN14)
max_diasIJN14 <- as.data.frame(max_diasIJN14)
##View(max_diasIJN14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN14) <- nombres
max_diasIJN14$Fecha <- as.Date(max_diasIJN14$Fecha)
options(digits=5)
max_diasIJN14$I1 <- as.numeric(max_diasIJN14$I1)
max_diasIJN14$I2 <- as.numeric(max_diasIJN14$I2)
max_diasIJN14$I3 <- as.numeric(max_diasIJN14$I3)
##View(max_diasIJN14)



##Junio 2015####
dfJN15 <- read.csv("Data/Years/2015/JUNIO2015.csv")
#View(dfJN15)
dfJN15 <- dfJN15[1:1028,c(1:8)] 
dfJN15[3:8] <- sapply(dfJN15[3:8],as.numeric)  

dfJN15[3] <- dfJN15[3]/sqrt(3)
dfJN15[4] <- dfJN15[4]/sqrt(3)
dfJN15[5] <- dfJN15[5]/sqrt(3)

# dfJN15[6] <- dfJN15[6]*2
# dfJN15[7] <- dfJN15[7]*2
# dfJN15[7] <- dfJN15[7]*2



dfJN15$Feha.de.la.Medda <- as.Date(dfJN15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfJN15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJN15, Feha.de.la.Medda==Dias[i]))
}


names(dfJN15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJN <- vector("numeric", ncol(dfJN15[6:8]))
for (i in seq_along(dfJN15[6:8])) {# 2. secuencia
  mediaIJN[[i]] <- mean(ifelse(dfJN15[6:8][[i]]<=0,NA,dfJN15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJN<- vector("numeric", ncol(dfJN15[6:8]))
for (i in seq_along(dfJN15[6:8])) {# 2. secuencia
  maxIJN[[i]] <- max(dfJN15[6:8][[i]] )    # 3. cuerpo
}

##9-Junio a 13-Junio#
###Dia 9###
media_dia_9IJN<-vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IJN[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IJN <- vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IJN[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IJN<-vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IJN[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IJN <- vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IJN[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IJN<-vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IJN[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IJN <- vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IJN[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IJN<-vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IJN[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IJN <- vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IJN[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IJN<-vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IJN[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IJN <- vector("numeric",  ncol(dfJN15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IJN[[i]] <- max(Dia_13[6:8][[i]]) 
}

medias_diasIJN15 <- rbind(media_dia_9IJN,media_dia_10IJN,
                        media_dia_11IJN,media_dia_12IJN,media_dia_13IJN
)
##View(medias_diasI)
medias_diasIJN15 <- cbind(levels(DiasF),medias_diasIJN15)
medias_diasIJN15 <- as.data.frame(medias_diasIJN15)
##View(medias_diasIJN15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN15) <- nombres
medias_diasIJN15$Fecha <- as.Date(medias_diasIJN15$Fecha)
options(digits=5)
medias_diasIJN15$I1 <- as.numeric(medias_diasIJN15$I1)
medias_diasIJN15$I2 <- as.numeric(medias_diasIJN15$I2)
medias_diasIJN15$I3 <- as.numeric(medias_diasIJN15$I3)
##View(medias_diasIJN15)

max_diasIJN15 <- rbind(max_dia_9IJN,max_dia_10IJN,
                     max_dia_11IJN,max_dia_12IJN,max_dia_13IJN)
##View(max_diasIJN15)
max_diasIJN15 <- cbind(levels(DiasF),max_diasIJN15)
max_diasIJN15 <- as.data.frame(max_diasIJN15)
##View(max_diasIJN15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN15) <- nombres
max_diasIJN15$Fecha <- as.Date(max_diasIJN15$Fecha)
options(digits=5)
max_diasIJN15$I1 <- as.numeric(max_diasIJN15$I1)
max_diasIJN15$I2 <- as.numeric(max_diasIJN15$I2)
max_diasIJN15$I3 <- as.numeric(max_diasIJN15$I3)
##View(max_diasIJN15)
##Junio 2016####
dfJN16 <- read.csv("Data/Years/2016/JUNIO2016.csv")
#View(dfJN16)
dfJN16 <- dfJN16[1:2383,c(1:8)] 
dfJN16[3:8] <- sapply(dfJN16[3:8],as.numeric)  

dfJN16[3] <- dfJN16[3]/sqrt(3)
dfJN16[4] <- dfJN16[4]/sqrt(3)
dfJN16[5] <- dfJN16[5]/sqrt(3)

dfJN16[6] <- dfJN16[6]*2
dfJN16[7] <- dfJN16[7]*2
dfJN16[8] <- dfJN16[8]*2



dfJN16$Fecha.de.la.Medida <- as.Date(dfJN16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfJN16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJN16, Fecha.de.la.Medida==Dias[i]))
}

names(dfJN16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJN <- vector("numeric", ncol(dfJN16[6:8]))
for (i in seq_along(dfJN16[6:8])) {# 2. secuencia
  mediaIJN[[i]] <- mean(ifelse(dfJN16[6:8][[i]]<=0,NA,dfJN16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJN<- vector("numeric", ncol(dfJN16[6:8]))
for (i in seq_along(dfJN16[6:8])) {# 2. secuencia
  maxIJN[[i]] <- max(dfJN16[6:8][[i]] )    # 3. cuerpo
}

##1-Junio a 30-Junio#
###Dia 1###
media_dia_1IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IJN[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IJN[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IJN[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IJN[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IJN[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IJN[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IJN[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IJN[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IJN[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IJN[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IJN[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IJN[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IJN[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IJN[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IJN[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IJN[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IJN[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IJN[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IJN[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IJN[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IJN[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IJN[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IJN[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IJN[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IJN[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IJN[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IJN[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IJN[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IJN[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IJN[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IJN[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IJN[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IJN[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IJN[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IJN[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IJN[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IJN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IJN[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IJN[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IJN[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IJN[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IJN[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IJN[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IJN[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IJN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IJN[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IJN[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IJN[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IJN[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IJN[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IJN<-vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IJN[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IJN <- vector("numeric",  ncol(dfJN16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IJN[[i]] <- max(Dia_26[6:8][[i]]) 
}


medias_diasIJN16 <- rbind(media_dia_1IJN,
                        media_dia_2IJN,media_dia_3IJN,media_dia_4IJN,
                        media_dia_5IJN,media_dia_6IJN,media_dia_7IJN,
                        media_dia_8IJN,media_dia_9IJN,media_dia_10IJN,
                        media_dia_11IJN,media_dia_12IJN,media_dia_13IJN,
                        media_dia_14IJN,media_dia_15IJN,media_dia_16IJN,
                        media_dia_17IJN,
                        media_dia_18IJN,media_dia_19IJN,media_dia_20IJN,
                        media_dia_21IJN,media_dia_22IJN,media_dia_23IJN,
                        media_dia_24IJN,media_dia_25IJN,media_dia_26IJN)
##View(medias_diasI)
medias_diasIJN16 <- cbind(levels(DiasF),medias_diasIJN16)
medias_diasIJN16 <- as.data.frame(medias_diasIJN16)
##View(medias_diasIJN16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN16) <- nombres
medias_diasIJN16$Fecha <- as.Date(medias_diasIJN16$Fecha)
options(digits=5)
medias_diasIJN16$I1 <- as.numeric(medias_diasIJN16$I1)
medias_diasIJN16$I2 <- as.numeric(medias_diasIJN16$I2)
medias_diasIJN16$I3 <- as.numeric(medias_diasIJN16$I3)
##View(medias_diasIJN16)

max_diasIJN16 <- rbind(max_dia_1IJN,
                     max_dia_2IJN,max_dia_3IJN,max_dia_4IJN,
                     max_dia_5IJN,max_dia_6IJN,max_dia_7IJN,
                     max_dia_8IJN,max_dia_9IJN,max_dia_10IJN,
                     max_dia_11IJN,max_dia_12IJN,max_dia_13IJN,
                     max_dia_14IJN,max_dia_15IJN,max_dia_16IJN,
                     max_dia_17IJN,
                     max_dia_18IJN,max_dia_19IJN,max_dia_20IJN,
                     max_dia_21IJN,max_dia_22IJN,max_dia_23IJN,
                     max_dia_24IJN,max_dia_25IJN,max_dia_26IJN)
##View(max_diasIJN16)
max_diasIJN16 <- cbind(levels(DiasF),max_diasIJN16)
max_diasIJN16 <- as.data.frame(max_diasIJN16)
##View(max_diasIJN16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN16) <- nombres
max_diasIJN16$Fecha <- as.Date(max_diasIJN16$Fecha)
options(digits=5)
max_diasIJN16$I1 <- as.numeric(max_diasIJN16$I1)
max_diasIJN16$I2 <- as.numeric(max_diasIJN16$I2)
max_diasIJN16$I3 <- as.numeric(max_diasIJN16$I3)
##View(max_diasIJN16)




##Junio 2017####
dfJN17 <- read.csv("Data/Years/2017/JUN017.csv")
#View(dfJN17)
dfJN17 <- dfJN17[1:768,c(1:8)] 
dfJN17[3:8] <- sapply(dfJN17[3:8],as.numeric)  

dfJN17[3] <- dfJN17[3]/sqrt(3)
dfJN17[4] <- dfJN17[4]/sqrt(3)
dfJN17[5] <- dfJN17[5]/sqrt(3)

dfJN17[6] <- dfJN17[6]*2
dfJN17[7] <- dfJN17[7]*2
dfJN17[8] <- dfJN17[8]*2



dfJN17$Fecha.de.la.Medida <- as.Date(dfJN17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfJN17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJN17, Fecha.de.la.Medida==Dias[i]))
}

names(dfJN17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJN <- vector("numeric", ncol(dfJN17[6:8]))
for (i in seq_along(dfJN17[6:8])) {# 2. secuencia
  mediaIJN[[i]] <- mean(ifelse(dfJN17[6:8][[i]]<=0,NA,dfJN17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJN<- vector("numeric", ncol(dfJN17[6:8]))
for (i in seq_along(dfJN17[6:8])) {# 2. secuencia
  maxIJN[[i]] <- max(dfJN17[6:8][[i]] )    # 3. cuerpo
}

##23-Junio a 30-Junio#
###Dia 23###
media_dia_23IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IJN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IJN[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IJN[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IJN[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IJN[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IJN[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IJN[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IJN[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IJN[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IJN[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IJN[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IJN[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IJN[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IJN[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IJN<-vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IJN[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IJN <- vector("numeric",  ncol(dfJN17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IJN[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasIJN17 <- rbind(media_dia_23IJN,
                        media_dia_24IJN,media_dia_25IJN,media_dia_26IJN,
                        media_dia_27IJN,
                        media_dia_28IJN,media_dia_29IJN,media_dia_30IJN)
##View(medias_diasI)
medias_diasIJN17 <- cbind(levels(DiasF),medias_diasIJN17)
medias_diasIJN17 <- as.data.frame(medias_diasIJN17)
##View(medias_diasIJN17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN17) <- nombres
medias_diasIJN17$Fecha <- as.Date(medias_diasIJN17$Fecha)
options(digits=5)
medias_diasIJN17$I1 <- as.numeric(medias_diasIJN17$I1)
medias_diasIJN17$I2 <- as.numeric(medias_diasIJN17$I2)
medias_diasIJN17$I3 <- as.numeric(medias_diasIJN17$I3)
##View(medias_diasIJN17)

max_diasIJN17 <- rbind(max_dia_23IJN,
                     max_dia_24IJN,max_dia_25IJN,max_dia_26IJN,
                     max_dia_27IJN,
                     max_dia_28IJN,max_dia_29IJN,max_dia_30IJN)
##View(max_diasIJN17)
max_diasIJN17 <- cbind(levels(DiasF),max_diasIJN17)
max_diasIJN17 <- as.data.frame(max_diasIJN17)
##View(max_diasIJN17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN17) <- nombres
max_diasIJN17$Fecha <- as.Date(max_diasIJN17$Fecha)
options(digits=5)
max_diasIJN17$I1 <- as.numeric(max_diasIJN17$I1)
max_diasIJN17$I2 <- as.numeric(max_diasIJN17$I2)
max_diasIJN17$I3 <- as.numeric(max_diasIJN17$I3)
##View(max_diasIJN17)




##Junio 2018####
dfJN18 <- read.csv("Data/Years/2018/JUN2018.csv")
#View(dfJN18)
dfJN18 <- dfJN18[1:698,c(1:8)] 
dfJN18[3:8] <- sapply(dfJN18[3:8],as.numeric)  

dfJN18[3] <- dfJN18[3]/sqrt(3)
dfJN18[4] <- dfJN18[4]/sqrt(3)
dfJN18[5] <- dfJN18[5]/sqrt(3)

# dfJN18[6] <- dfJN18[6]*2
# dfJN18[7] <- dfJN18[7]*2
# dfJN18[8] <- dfJN18[8]*2



dfJN18$Fecha.de.la.Medida <- as.Date(dfJN18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfJN18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJN18, Fecha.de.la.Medida==Dias[i]))
}

names(dfJN18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJN <- vector("numeric", ncol(dfJN18[6:8]))
for (i in seq_along(dfJN18[6:8])) {# 2. secuencia
  mediaIJN[[i]] <- mean(ifelse(dfJN18[6:8][[i]]<=0,NA,dfJN18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJN<- vector("numeric", ncol(dfJN18[6:8]))
for (i in seq_along(dfJN18[6:8])) {# 2. secuencia
  maxIJN[[i]] <- max(dfJN18[6:8][[i]] )    # 3. cuerpo
}

##19-Junio a 26-Junio#
###Dia 19###
media_dia_19IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IJN[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IJN[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IJN[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IJN[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IJN[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IJN[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IJN[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IJN[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IJN[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IJN[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IJN[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IJN[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IJN[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IJN[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IJN<-vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IJN[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IJN <- vector("numeric",  ncol(dfJN18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IJN[[i]] <- max(Dia_26[6:8][[i]]) 
}


medias_diasIJN18 <- rbind(media_dia_19IJN,media_dia_20IJN,
                        media_dia_21IJN,media_dia_22IJN,media_dia_23IJN,
                        media_dia_24IJN,media_dia_25IJN,media_dia_26IJN)
##View(medias_diasI)
medias_diasIJN18 <- cbind(levels(DiasF),medias_diasIJN18)
medias_diasIJN18 <- as.data.frame(medias_diasIJN18)
##View(medias_diasIJN18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN18) <- nombres
medias_diasIJN18$Fecha <- as.Date(medias_diasIJN18$Fecha)
options(digits=5)
medias_diasIJN18$I1 <- as.numeric(medias_diasIJN18$I1)
medias_diasIJN18$I2 <- as.numeric(medias_diasIJN18$I2)
medias_diasIJN18$I3 <- as.numeric(medias_diasIJN18$I3)
##View(medias_diasIJN18)

max_diasIJN18 <- rbind(max_dia_19IJN,max_dia_20IJN,
                     max_dia_21IJN,max_dia_22IJN,max_dia_23IJN,
                     max_dia_24IJN,max_dia_25IJN,max_dia_26IJN)
##View(max_diasIJN18)
max_diasIJN18 <- cbind(levels(DiasF),max_diasIJN18)
max_diasIJN18 <- as.data.frame(max_diasIJN18)
##View(max_diasIJN18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN18) <- nombres
max_diasIJN18$Fecha <- as.Date(max_diasIJN18$Fecha)
options(digits=5)
max_diasIJN18$I1 <- as.numeric(max_diasIJN18$I1)
max_diasIJN18$I2 <- as.numeric(max_diasIJN18$I2)
max_diasIJN18$I3 <- as.numeric(max_diasIJN18$I3)
##View(max_diasIJN18)



##Junio 2022####
dfJN22 <- read.csv("Data/Years/2022/Junio.csv")
##View(dfJN22)
#str(dfJN22)

dfJN22 <- dfJN22[1:8328,1:8]

dfJN22$Fecha <- as.Date(dfJN22$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfJN22$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)

dfJN22[3:8] <-sapply(dfJN22[3:8],as.numeric)


for (i in 
     min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq(1:length(Dias)))))
  assign(Days[i],filter(dfJN22, Fecha==Dias[i]))
}




####I1-I3###
mediaI <- vector("numeric", ncol(dfJN22[6:8]))


for (i in seq_along(dfJN22[6:8])) {# 2. secuencia
  mediaI[[i]] <- mean(ifelse(dfJN22[6:8][[i]]<=0,NA,dfJN22[6:8][[i]]),
                      na.rm = T )    # 3. cuerpo
}


maxI<- vector("numeric", ncol(dfJN22[6:8]))
for (i in seq_along(dfJN22[6:8])) {# 2. secuencia
  maxI[[i]] <- max(dfJN22[6:8][[i]] )    # 3. cuerpo
}

names(dfJN22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")


##01-junio a 10-junio###
###Dia 1###
media_dia_1I<-vector("numeric",  ncol(dfJN22[6:8]))
media_dia_1I


for (i in seq_along(Dia_1[6:8])) {
  media_dia_1I[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                            na.rm=T) 
}


max_dia_1I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1I[[i]] <- max(Dia_1[6:8][[i]]) 
}

max_dia_1I


###Dia 2###
media_dia_2I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_2[6:8])) {
  media_dia_2I[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                            na.rm=T)
}

max_dia_2I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2I[[i]] <- max(Dia_2[6:8][[i]]) 
}


###Dia 3###
media_dia_3I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_3[6:8])) {
  media_dia_3I[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                            na.rm=T) 
}

max_dia_3I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3I[[i]] <- max(Dia_3[6:8][[i]]) 
}


###Dia 4###
media_dia_4I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_4[6:8])) {
  media_dia_4I[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                            na.rm=T) 
}

max_dia_4I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4I[[i]] <- max(Dia_4[6:8][[i]]) 
}



###Dia 5###

media_dia_5I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_5[6:8])) {
  media_dia_5I[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                            na.rm=T) 
}

max_dia_5I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5I[[i]] <- max(Dia_5[6:8][[i]]) 
}



###Dia 6###

media_dia_6I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_6[6:8])) {
  media_dia_6I[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                            na.rm=T) 
}

max_dia_6I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6I[[i]] <- max(Dia_6[6:8][[i]]) 
}


###Dia 7###

media_dia_7I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_7[6:8])) {
  media_dia_7I[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                            na.rm=T) 
}

max_dia_7I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7I[[i]] <- max(Dia_7[6:8][[i]]) 
}


###Dia 8 ###

media_dia_8I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_8[6:8])) {
  media_dia_8I[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                            na.rm=T) 
}

max_dia_8I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8I[[i]] <- max(Dia_8[6:8][[i]]) 
}


###Dia 9 ###

media_dia_9I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_9[6:8])) {
  media_dia_9I[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                            na.rm=T) 
}

max_dia_9I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9I[[i]] <- max(Dia_9[6:8][[i]]) 
}



###Dia 10 ###
media_dia_10I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_10[6:8])) {
  media_dia_10I[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                             na.rm=T) 
}

max_dia_10I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10I[[i]] <- max(Dia_10[6:8][[i]]) 
}





##11-junio a 20-junio###
###Dia 11###
media_dia_11I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_11[6:8])) {
  media_dia_11I[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                             na.rm=T) 
}

max_dia_11I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11I[[i]] <- max(Dia_11[6:8][[i]]) 
}



###Dia 12###
media_dia_12I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_12[6:8])) {
  media_dia_12I[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                             na.rm=T) 
}

max_dia_12I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12I[[i]] <- max(Dia_12[6:8][[i]]) 
}


###Dia 13###
media_dia_13I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_13[6:8])) {
  media_dia_13I[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                             na.rm=T) 
}

max_dia_13I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13I[[i]] <- max(Dia_13[6:8][[i]]) 
}



###Dia 14###
media_dia_14I<-vector("numeric",  ncol(dfJN22[6:8]))


for (i in seq_along(Dia_14[6:8])) {
  media_dia_14I[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                             na.rm=T) 
}

max_dia_14I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14I[[i]] <- max(Dia_14[6:8][[i]]) 
}


###Dia 15###
media_dia_15I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_15[6:8])) {
  media_dia_15I[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                             na.rm=T) 
}

max_dia_15I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15I[[i]] <- max(Dia_15[6:8][[i]]) 
}


###Dia 16###
media_dia_16I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_16[6:8])) {
  media_dia_16I[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                             na.rm=T) 
}

max_dia_16I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16I[[i]] <- max(Dia_16[6:8][[i]]) 
}


###Dia 17###
media_dia_17I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_17[6:8])) {
  media_dia_17I[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                             na.rm=T) 
}

max_dia_17I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17I[[i]] <- max(Dia_17[6:8][[i]]) 
}


###Dia 18 ###
media_dia_18I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_18[6:8])) {
  media_dia_18I[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                             na.rm=T) 
}

max_dia_18I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18I[[i]] <- max(Dia_18[6:8][[i]]) 
}


###Dia 19 ###
media_dia_19I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_19[6:8])) {
  media_dia_19I[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                             na.rm=T) 
}

max_dia_19I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19I[[i]] <- max(Dia_19[6:8][[i]]) 
}


###Dia 20 ###
media_dia_20I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_20[6:8])) {
  media_dia_20I[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                             na.rm=T) 
}

max_dia_20I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20I[[i]] <- max(Dia_20[6:8][[i]]) 
}


##21-junio a 30-junio###
###Dia 21###
media_dia_21I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_21[6:8])) {
  media_dia_21I[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                             na.rm=T) 
}

max_dia_21I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21I[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_22[6:8])) {
  media_dia_22I[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                             na.rm=T) 
}

max_dia_22I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22I[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_23[6:8])) {
  media_dia_23I[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                             na.rm=T) 
}

max_dia_23I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23I[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia24###
media_dia_24I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_24[6:8])) {
  media_dia_24I[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                             na.rm=T) 
}

max_dia_24I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24I[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_25[6:8])) {
  media_dia_25I[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                             na.rm=T) 
}

max_dia_25I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25I[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_26[6:8])) {
  media_dia_26I[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                             na.rm=T) 
}

max_dia_26I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26I[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_27[6:8])) {
  media_dia_27I[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                             na.rm=T) 
}

max_dia_27I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27I[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28 ###
media_dia_28I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_28[6:8])) {
  media_dia_28I[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                             na.rm=T) 
}

max_dia_28I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28I[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29 ###
media_dia_29I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_29[6:8])) {
  media_dia_29I[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                             na.rm=T) 
}

max_dia_29I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29I[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30 ###
media_dia_30I<-vector("numeric",  ncol(dfJN22[6:8]))

for (i in seq_along(Dia_30[6:8])) {
  media_dia_30I[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                             na.rm=T) 
}

max_dia_30I <- vector("numeric",  ncol(dfJN22[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30I[[i]] <- max(Dia_30[6:8][[i]]) 
}

medias_diasIJN22 <- rbind(media_dia_1I,media_dia_2I,media_dia_3I,media_dia_4I,
                      media_dia_5I,media_dia_6I,media_dia_7I,media_dia_8I,
                      media_dia_9I,media_dia_10I,media_dia_11I,media_dia_12I,
                      media_dia_13I,media_dia_14I,media_dia_15I,media_dia_16I,
                      media_dia_17I,media_dia_18I,media_dia_19I,media_dia_20I,
                      media_dia_21I,media_dia_22I,media_dia_23I,media_dia_24I,
                      media_dia_25I,media_dia_26I,media_dia_27I,media_dia_28I,
                      media_dia_29I,media_dia_30I)


##View(medias_diasIJN22)


medias_diasIJN22 <- cbind(levels(DiasF),medias_diasIJN22)
medias_diasIJN22 <- as.data.frame(medias_diasIJN22)
##View(medias_diasIJN22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJN22) <- nombres
medias_diasIJN22$Fecha <- as.Date(medias_diasIJN22$Fecha)
options(digits=5)
medias_diasIJN22$I1 <- as.numeric(medias_diasIJN22$I1)
medias_diasIJN22$I2 <- as.numeric(medias_diasIJN22$I2)
medias_diasIJN22$I3 <- as.numeric(medias_diasIJN22$I3)
##View(medias_diasI)

max_diasIJN22 <- rbind(max_dia_1I,max_dia_2I,max_dia_3I,max_dia_4I,
                   max_dia_5I,max_dia_6I,max_dia_7I,max_dia_8I,
                   max_dia_9I,max_dia_10I,max_dia_11I,max_dia_12I,
                   max_dia_13I,max_dia_14I,max_dia_15I,max_dia_16I,
                   max_dia_17I,max_dia_18I,max_dia_19I,max_dia_20I,
                   max_dia_21I,max_dia_22I,max_dia_23I,max_dia_24I,
                   max_dia_25I,max_dia_26I,max_dia_27I,max_dia_28I,
                   max_dia_29I,max_dia_30I)


##View(max_diasIJN22)


max_diasIJN22 <- cbind(levels(DiasF),max_diasIJN22)
max_diasIJN22 <- as.data.frame(max_diasIJN22)
##View(max_diasIJN22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJN22) <- nombres
max_diasIJN22$Fecha <- as.Date(max_diasIJN22$Fecha)
options(digits=5)
max_diasIJN22$I1 <- as.numeric(max_diasIJN22$I1)
max_diasIJN22$I2 <- as.numeric(max_diasIJN22$I2)
max_diasIJN22$I3 <- as.numeric(max_diasIJN22$I3)
##View(max_diasIJN22)

######
Junio <- rbind(medias_diasIJN13,medias_diasIJN14,medias_diasIJN15,
              medias_diasIJN16,medias_diasIJN17,medias_diasIJN18,
              medias_diasIJN22)
JunioM <- rbind(max_diasIJN13,max_diasIJN14,max_diasIJN15,
               max_diasIJN16,max_diasIJN17,max_diasIJN18,
               max_diasIJN22)
dfJN <- rbind(dfJN13,dfJN14,dfJN15,dfJN16,dfJN17,dfJN18,dfJN22)


##Julio 2014####
dfJL14 <- read.csv("Data/Years/2014/JULIO2014.csv")
#View(dfJL14)
dfJL14 <- dfJL14[120:860,c(1:8)] 
dfJL14[3:8] <- sapply(dfJL14[3:8],as.numeric)  

dfJL14[3] <- dfJL14[3]/sqrt(3)
dfJL14[4] <- dfJL14[4]/sqrt(3)
dfJL14[5] <- dfJL14[5]/sqrt(3)

# dfJL14[6] <- dfJL14[6]*2
# dfJL14[7] <- dfJL14[7]*2
# dfJL14[7] <- dfJL14[7]*2



dfJL14$Feha.de.la.Medda <- as.Date(dfJL14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfJL14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJL14, Feha.de.la.Medda==Dias[i]))
}
names(dfJL14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")


mediaIJL <- vector("numeric", ncol(dfJL14[6:8]))
for (i in seq_along(dfJL14[6:8])) {# 2. secuencia
  mediaIJL[[i]] <- mean(ifelse(dfJL14[6:8][[i]]<=0,NA,dfJL14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJL<- vector("numeric", ncol(dfJL14[6:8]))
for (i in seq_along(dfJL14[6:8])) {# 2. secuencia
  maxIJL[[i]] <- max(dfJL14[6:8][[i]] )    # 3. cuerpo
}

##1-julio a 3-julio#
###Dia 1###
media_dia_1IJL<-vector("numeric",  ncol(dfJL14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IJL[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IJL <- vector("numeric",  ncol(dfJL14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IJL[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IJL<-vector("numeric",  ncol(dfJL14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IJL[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IJL <- vector("numeric",  ncol(dfJL14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IJL[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IJL<-vector("numeric",  ncol(dfJL14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IJL[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IJL <- vector("numeric",  ncol(dfJL14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IJL[[i]] <- max(Dia_3[6:8][[i]]) 
}


medias_diasIJL14 <- rbind(media_dia_1IJL,media_dia_2IJL,media_dia_3IJL)
##View(medias_diasI)
medias_diasIJL14 <- cbind(levels(DiasF),medias_diasIJL14)
medias_diasIJL14 <- as.data.frame(medias_diasIJL14)
##View(medias_diasIJL14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJL14) <- nombres
medias_diasIJL14$Fecha <- as.Date(medias_diasIJL14$Fecha)
options(digits=5)
medias_diasIJL14$I1 <- as.numeric(medias_diasIJL14$I1)
medias_diasIJL14$I2 <- as.numeric(medias_diasIJL14$I2)
medias_diasIJL14$I3 <- as.numeric(medias_diasIJL14$I3)
##View(medias_diasIJL14)

max_diasIJL14 <- rbind(max_dia_1IJL,max_dia_2IJL,max_dia_3IJL)
##View(max_diasIJL14)
max_diasIJL14 <- cbind(levels(DiasF),max_diasIJL14)
max_diasIJL14 <- as.data.frame(max_diasIJL14)
##View(max_diasIJL14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJL14) <- nombres
max_diasIJL14$Fecha <- as.Date(max_diasIJL14$Fecha)
options(digits=5)
max_diasIJL14$I1 <- as.numeric(max_diasIJL14$I1)
max_diasIJL14$I2 <- as.numeric(max_diasIJL14$I2)
max_diasIJL14$I3 <- as.numeric(max_diasIJL14$I3)
##View(max_diasIJL14)






##Julio 2017####
dfJL17 <- read.csv("Data/Years/2017/JUL2017.csv")
#View(dfJL17)
dfJL17 <- dfJL17[1:768,c(1:8)] 
dfJL17[3:8] <- sapply(dfJL17[3:8],as.numeric)  

dfJL17[3] <- dfJL17[3]/sqrt(3)
dfJL17[4] <- dfJL17[4]/sqrt(3)
dfJL17[5] <- dfJL17[5]/sqrt(3)

dfJL17[6] <- dfJL17[6]*2
dfJL17[7] <- dfJL17[7]*2
dfJL17[8] <- dfJL17[8]*2



dfJL17$Fecha.de.la.Medida <- as.Date(dfJL17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfJL17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJL17, Fecha.de.la.Medida==Dias[i]))
}

names(dfJL17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJL <- vector("numeric", ncol(dfJL17[6:8]))
for (i in seq_along(dfJL17[6:8])) {# 2. secuencia
  mediaIJL[[i]] <- mean(ifelse(dfJL17[6:8][[i]]<=0,NA,dfJL17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJL<- vector("numeric", ncol(dfJL17[6:8]))
for (i in seq_along(dfJL17[6:8])) {# 2. secuencia
  maxIJL[[i]] <- max(dfJL17[6:8][[i]] )    # 3. cuerpo
}

##14-Julio a 21-Julio#
###Dia 14###
media_dia_14IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IJL[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IJL[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IJL[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IJL[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IJL[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IJL[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IJL[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IJL[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IJL[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IJL[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IJL[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IJL[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IJL[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IJL[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IJL<-vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IJL[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IJL <- vector("numeric",  ncol(dfJL17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IJL[[i]] <- max(Dia_21[6:8][[i]]) 
}



medias_diasIJL17 <- rbind(media_dia_14IJL,media_dia_15IJL,media_dia_16IJL,
                        media_dia_17IJL,
                        media_dia_18IJL,media_dia_19IJL,media_dia_20IJL,
                        media_dia_21IJL)
##View(medias_diasI)
medias_diasIJL17 <- cbind(levels(DiasF),medias_diasIJL17)
medias_diasIJL17 <- as.data.frame(medias_diasIJL17)
##View(medias_diasIJL17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJL17) <- nombres
medias_diasIJL17$Fecha <- as.Date(medias_diasIJL17$Fecha)
options(digits=5)
medias_diasIJL17$I1 <- as.numeric(medias_diasIJL17$I1)
medias_diasIJL17$I2 <- as.numeric(medias_diasIJL17$I2)
medias_diasIJL17$I3 <- as.numeric(medias_diasIJL17$I3)
##View(medias_diasIJL17)

max_diasIJL17 <- rbind(max_dia_14IJL,max_dia_15IJL,max_dia_16IJL,
                     max_dia_17IJL,
                     max_dia_18IJL,max_dia_19IJL,max_dia_20IJL,
                     max_dia_21IJL)
##View(max_diasIJL17)
max_diasIJL17 <- cbind(levels(DiasF),max_diasIJL17)
max_diasIJL17 <- as.data.frame(max_diasIJL17)
##View(max_diasIJL17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJL17) <- nombres
max_diasIJL17$Fecha <- as.Date(max_diasIJL17$Fecha)
options(digits=5)
max_diasIJL17$I1 <- as.numeric(max_diasIJL17$I1)
max_diasIJL17$I2 <- as.numeric(max_diasIJL17$I2)
max_diasIJL17$I3 <- as.numeric(max_diasIJL17$I3)
##View(max_diasIJL17)




##Julio 2018####
dfJL18 <- read.csv("Data/Years/2018/JUL2018.csv")
#View(dfJL18)
dfJL18 <- dfJL18[1:698,c(1:8)] 
dfJL18[3:8] <- sapply(dfJL18[3:8],as.numeric)  

dfJL18[3] <- dfJL18[3]/sqrt(3)
dfJL18[4] <- dfJL18[4]/sqrt(3)
dfJL18[5] <- dfJL18[5]/sqrt(3)
# 
# dfJL18[6] <- dfJL18[6]*2
# dfJL18[7] <- dfJL18[7]*2
# dfJL18[8] <- dfJL18[8]*2



dfJL18$Fecha.de.la.Medida <- as.Date(dfJL18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfJL18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfJL18, Fecha.de.la.Medida==Dias[i]))
}

names(dfJL18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIJL <- vector("numeric", ncol(dfJL18[6:8]))
for (i in seq_along(dfJL18[6:8])) {# 2. secuencia
  mediaIJL[[i]] <- mean(ifelse(dfJL18[6:8][[i]]<=0,NA,dfJL18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIJL<- vector("numeric", ncol(dfJL18[6:8]))
for (i in seq_along(dfJL18[6:8])) {# 2. secuencia
  maxIJL[[i]] <- max(dfJL18[6:8][[i]] )    # 3. cuerpo
}

##23-Julio a 30-Julio#
###Dia 23###
media_dia_23IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IJL[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IJL[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IJL[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IJL[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IJL[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IJL[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IJL[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IJL[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IJL[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IJL[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IJL[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IJL[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IJL[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IJL[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IJL<-vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IJL[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IJL <- vector("numeric",  ncol(dfJL18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IJL[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasIJL18 <- rbind(media_dia_23IJL,
                        media_dia_24IJL,media_dia_25IJL,media_dia_26IJL,
                        media_dia_27IJL,
                        media_dia_28IJL,media_dia_29IJL,media_dia_30IJL)
##View(medias_diasI)
medias_diasIJL18 <- cbind(levels(DiasF),medias_diasIJL18)
medias_diasIJL18 <- as.data.frame(medias_diasIJL18)
##View(medias_diasIJL18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJL18) <- nombres
medias_diasIJL18$Fecha <- as.Date(medias_diasIJL18$Fecha)
options(digits=5)
medias_diasIJL18$I1 <- as.numeric(medias_diasIJL18$I1)
medias_diasIJL18$I2 <- as.numeric(medias_diasIJL18$I2)
medias_diasIJL18$I3 <- as.numeric(medias_diasIJL18$I3)
##View(medias_diasIJL18)

max_diasIJL18 <- rbind(max_dia_23IJL,
                     max_dia_24IJL,max_dia_25IJL,max_dia_26IJL,
                     max_dia_27IJL,
                     max_dia_28IJL,max_dia_29IJL,max_dia_30IJL)
##View(max_diasIJL18)
max_diasIJL18 <- cbind(levels(DiasF),max_diasIJL18)
max_diasIJL18 <- as.data.frame(max_diasIJL18)
##View(max_diasIJL18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJL18) <- nombres
max_diasIJL18$Fecha <- as.Date(max_diasIJL18$Fecha)
options(digits=5)
max_diasIJL18$I1 <- as.numeric(max_diasIJL18$I1)
max_diasIJL18$I2 <- as.numeric(max_diasIJL18$I2)
max_diasIJL18$I3 <- as.numeric(max_diasIJL18$I3)
##View(max_diasIJL18)



## Julio 2022####
dfJL22 <- read.csv("Data/Years/2022/Julio.csv")
##View(dfJL22)
#str(dfJL22)

dfJL22 <- dfJL22[1:8569,1:8]

dfJL22$Fecha <- as.Date(dfJL22$Fecha,format="%d/%m/%Y")
DiasFJL <- as.factor(dfJL22$Fecha)
DiasJL<- levels(DiasFJL)
date <- as.Date(DiasJL)


dfJL22[3:8] <-sapply(dfJL22[3:8],as.numeric)

for (i in 
     min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysJL <- gsub("$","JL",gsub("^","Dia_",as.character( seq(1:length(DiasJL)))))
  assign(DaysJL[i],filter(dfJL22, Fecha==DiasJL[i]))
}
names(dfJL22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

####I1-I3###
mediaIJL <- vector("numeric", ncol(dfJL22[6:8]))


for (i in seq_along(dfJL22[6:8])) {# 2. secuencia
  mediaIJL[[i]] <- mean(dfJL22[6:8][[i]])    # 3. cuerpo
}



maxIJL <- vector("numeric", ncol(dfJL22[6:8]))

for (i in seq_along(dfJL22[6:8])) {# 2. secuencia
  maxIJL[[i]] <- max(dfJL22[6:8][[i]])     # 3. cuerpo
}




##01-junio a 10-junio###
###Dia 1###
media_dia_1IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_1JL[6:8])) {
  media_dia_1IJL[[i]] <- mean(ifelse(Dia_1JL[6:8][[i]]<=0,NA,Dia_1JL[6:8][[i]]),
                              na.rm = T)
}

max_dia_1IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_1JL[6:8])) {
  max_dia_1IJL[[i]] <- max(Dia_1JL[6:8][[i]])
}



###Dia 2###
media_dia_2IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_2JL[6:8])) {
  media_dia_2IJL[[i]] <- mean(ifelse(Dia_2JL[6:8][[i]]<=0,NA,Dia_2JL[6:8][[i]]),
                              na.rm = T) 
}


max_dia_2IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_2JL[6:8])) {
  max_dia_2IJL[[i]] <- max(Dia_2JL[6:8][[i]])
}



###Dia 3###
media_dia_3IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_3JL[6:8])) {
  media_dia_3IJL[[i]] <- mean(ifelse(Dia_3JL[6:8][[i]]<=0,NA,Dia_3JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_3IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_3JL[6:8])) {
  max_dia_3IJL[[i]] <- max(Dia_3JL[6:8][[i]])
}



###Dia 4###
media_dia_4IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_4JL[6:8])) {
  media_dia_4IJL[[i]] <- mean(ifelse(Dia_4JL[6:8][[i]]<=0,NA,Dia_4JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_4IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_4JL[6:8])) {
  max_dia_4IJL[[i]] <- max(Dia_4JL[6:8][[i]])
}


###Dia 5###

media_dia_5IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_5JL[6:8])) {
  media_dia_5IJL[[i]] <- mean(ifelse(Dia_5JL[6:8][[i]]<=0,NA,Dia_5JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_5IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_5JL[6:8])) {
  max_dia_5IJL[[i]] <- max(Dia_5JL[6:8][[i]])
}



###Dia 6###

media_dia_6IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_6JL[6:8])) {
  media_dia_6IJL[[i]] <- mean(ifelse(Dia_6JL[6:8][[i]]<=0,NA,Dia_6JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_6IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_6JL[6:8])) {
  max_dia_6IJL[[i]] <- max(Dia_6JL[6:8][[i]])
}



###Dia 7###

media_dia_7IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_7JL[6:8])) {
  media_dia_7IJL[[i]] <- mean(ifelse(Dia_7JL[6:8][[i]]<=0,NA,Dia_7JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_7IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_7JL[6:8])) {
  max_dia_7IJL[[i]] <- max(Dia_7JL[6:8][[i]])
}



###Dia 8 ###

media_dia_8IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_8JL[6:8])) {
  media_dia_8IJL[[i]] <- mean(ifelse(Dia_8JL[6:8][[i]]<=0,NA,Dia_8JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_8IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_8JL[6:8])) {
  max_dia_8IJL[[i]] <- max(Dia_8JL[6:8][[i]])
}



###Dia 9 ###

media_dia_9IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_9JL[6:8])) {
  media_dia_9IJL[[i]] <- mean(ifelse(Dia_9JL[6:8][[i]]<=0,NA,Dia_9JL[6:8][[i]]),
                              na.rm = T) 
}

max_dia_9IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_9JL[6:8])) {
  max_dia_9IJL[[i]] <- max(Dia_9JL[6:8][[i]])
}



###Dia 10 ###
media_dia_10IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_10JL[6:8])) {
  media_dia_10IJL[[i]] <- mean(ifelse(Dia_10JL[6:8][[i]]<=0,NA,Dia_10JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_10IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_10JL[6:8])) {
  max_dia_10IJL[[i]] <- max(Dia_10JL[6:8][[i]])
}






##11-junio a 20-junio###
###Dia 11###
media_dia_11IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_11JL[6:8])) {
  media_dia_11IJL[[i]] <- mean(ifelse(Dia_11JL[6:8][[i]]<=0,NA,Dia_11JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_11IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_11JL[6:8])) {
  max_dia_11IJL[[i]] <- max(Dia_11JL[6:8][[i]])
}

###Dia 12###
media_dia_12IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_12JL[6:8])) {
  media_dia_12IJL[[i]] <- mean(ifelse(Dia_12JL[6:8][[i]]<=0,NA,Dia_12JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_12IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_12JL[6:8])) {
  max_dia_12IJL[[i]] <- max(Dia_12JL[6:8][[i]])
}

###Dia 13###
media_dia_13IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_13JL[6:8])) {
  media_dia_13IJL[[i]] <- mean(ifelse(Dia_13JL[6:8][[i]]<=0,NA,Dia_13JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_13IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_13JL[6:8])) {
  max_dia_13IJL[[i]] <- max(Dia_13JL[6:8][[i]])
}


###Dia 14###
media_dia_14IJL<-vector("numeric",  ncol(dfJL22[6:8]))


for (i in seq_along(Dia_14JL[6:8])) {
  media_dia_14IJL[[i]] <- mean(ifelse(Dia_14JL[6:8][[i]]<=0,NA,Dia_14JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_14IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_14JL[6:8])) {
  max_dia_14IJL[[i]] <- max(Dia_14JL[6:8][[i]])
}

###Dia 15###
media_dia_15IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_15JL[6:8])) {
  media_dia_15IJL[[i]] <- mean(ifelse(Dia_15JL[6:8][[i]]<=0,NA,Dia_15JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_15IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_15JL[6:8])) {
  max_dia_15IJL[[i]] <- max(Dia_15JL[6:8][[i]])
}

###Dia 16###
media_dia_16IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_16JL[6:8])) {
  media_dia_16IJL[[i]] <- mean(ifelse(Dia_16JL[6:8][[i]]<=0,NA,Dia_16JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_16IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_16JL[6:8])) {
  max_dia_16IJL[[i]] <- max(Dia_16JL[6:8][[i]])
}

###Dia 17###
media_dia_17IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_17JL[6:8])) {
  media_dia_17IJL[[i]] <- mean(ifelse(Dia_17JL[6:8][[i]]<=0,NA,Dia_17JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_17IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_17JL[6:8])) {
  max_dia_17IJL[[i]] <- max(Dia_17JL[6:8][[i]])
}

###Dia 18 ###
media_dia_18IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_18JL[6:8])) {
  media_dia_18IJL[[i]] <- mean(ifelse(Dia_18JL[6:8][[i]]<=0,NA,Dia_18JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_18IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_18JL[6:8])) {
  max_dia_18IJL[[i]] <- max(Dia_18JL[6:8][[i]])
}

###Dia 19 ###
media_dia_19IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_19JL[6:8])) {
  media_dia_19IJL[[i]] <- mean(ifelse(Dia_19JL[6:8][[i]]<=0,NA,Dia_19JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_19IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_19JL[6:8])) {
  max_dia_19IJL[[i]] <- max(Dia_19JL[6:8][[i]])
}

###Dia 20 ###
media_dia_20IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_20JL[6:8])) {
  media_dia_20IJL[[i]] <- mean(ifelse(Dia_20JL[6:8][[i]]<=0,NA,Dia_20JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_20IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_20JL[6:8])) {
  max_dia_20IJL[[i]] <- max(Dia_20JL[6:8][[i]])
}

##21-junio a 31-junio###
###Dia 21###
media_dia_21IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_21JL[6:8])) {
  media_dia_21IJL[[i]] <- mean(ifelse(Dia_21JL[6:8][[i]]<=0,NA,Dia_21JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_21IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_21JL[6:8])) {
  max_dia_21IJL[[i]] <- max(Dia_21JL[6:8][[i]])
}

###Dia 22###
media_dia_22IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_22JL[6:8])) {
  media_dia_22IJL <- mean(ifelse(Dia_22JL[6:8][[i]]<=0,NA,Dia_22JL[6:8][[i]]),
                          na.rm = T) 
}

max_dia_22IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_22JL[6:8])) {
  max_dia_22IJL[[i]] <- max(Dia_22JL[6:8][[i]])
}


###Dia 23###
media_dia_23IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_23JL[6:8])) {
  media_dia_23IJL[[i]] <- mean(ifelse(Dia_23JL[6:8][[i]]<=0,NA,Dia_23JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_23IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_23JL[6:8])) {
  max_dia_23IJL[[i]] <- max(Dia_23JL[6:8][[i]])
}


###Dia24###
media_dia_24IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_24JL[6:8])) {
  media_dia_24IJL[[i]] <- mean(ifelse(Dia_24JL[6:8][[i]]<=0,NA,Dia_24JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_24IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_24JL[6:8])) {
  max_dia_24IJL[[i]] <- max(Dia_24JL[6:8][[i]])
}


###Dia 25###
media_dia_25IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_25JL[6:8])) {
  media_dia_25IJL[[i]] <- mean(ifelse(Dia_25JL[6:8][[i]]<=0,NA,Dia_25JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_25IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_25JL[6:8])) {
  max_dia_25IJL[[i]] <- max(Dia_25JL[6:8][[i]])
}


###Dia 26###
media_dia_26IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_26JL[6:8])) {
  media_dia_26IJL[[i]] <- mean(ifelse(Dia_26JL[6:8][[i]]<=0,NA,Dia_26JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_26IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_26JL[6:8])) {
  max_dia_26IJL[[i]] <- max(Dia_26JL[6:8][[i]])
}


###Dia 27###
media_dia_27IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_27JL[6:8])) {
  media_dia_27IJL[[i]] <- mean(ifelse(Dia_27JL[6:8][[i]]<=0,NA,Dia_27JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_27IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_27JL[6:8])) {
  max_dia_27IJL[[i]] <- max(Dia_27JL[6:8][[i]])
}


###Dia 28 ###
media_dia_28IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_28JL[6:8])) {
  media_dia_28IJL[[i]] <- mean(ifelse(Dia_28JL[6:8][[i]]<=0,NA,Dia_28JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_28IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_28JL[6:8])) {
  max_dia_28IJL[[i]] <- max(Dia_28JL[6:8][[i]])
}


###Dia 29 ###
media_dia_29IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_29JL[6:8])) {
  media_dia_29IJL[[i]] <- mean(ifelse(Dia_29JL[6:8][[i]]<=0,NA,Dia_29JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_29IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_29JL[6:8])) {
  max_dia_29IJL[[i]] <- max(Dia_29JL[6:8][[i]])
}


###Dia 30 ###
media_dia_30IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_30JL[6:8])) {
  media_dia_30IJL[[i]] <- mean(ifelse(Dia_30JL[6:8][[i]]<=0,NA,Dia_30JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_30IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_30JL[6:8])) {
  max_dia_30IJL[[i]] <- max(Dia_30JL[6:8][[i]])
}


###Dia 31 ###
media_dia_31IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_31JL[6:8])) {
  media_dia_31IJL[[i]] <- mean(ifelse(Dia_31JL[6:8][[i]]<=0,NA,Dia_31JL[6:8][[i]]),
                               na.rm = T) 
}

max_dia_31IJL<-vector("numeric",  ncol(dfJL22[6:8]))

for (i in seq_along(Dia_31JL[6:8])) {
  max_dia_31IJL[[i]] <- max(Dia_31JL[6:8][[i]])
}


medias_diasIJL22 <- rbind(media_dia_1IJL,media_dia_2IJL,media_dia_3IJL,media_dia_4IJL,
                          media_dia_5IJL,media_dia_6IJL,media_dia_7IJL,media_dia_8IJL,
                          media_dia_9IJL,media_dia_10IJL,media_dia_11IJL,media_dia_12IJL,
                          media_dia_13IJL,media_dia_14IJL,media_dia_15IJL,media_dia_16IJL,
                          media_dia_17IJL,media_dia_18IJL,media_dia_19IJL,media_dia_20IJL,
                          media_dia_21IJL,media_dia_22IJL,media_dia_23IJL,media_dia_24IJL,
                          media_dia_25IJL,media_dia_26IJL,media_dia_27IJL,media_dia_28IJL,
                          media_dia_29IJL,media_dia_30IJL,media_dia_31IJL)


medias_diasIJL22 <- cbind(levels(DiasFJL),medias_diasIJL22)
medias_diasIJL22 <- as.data.frame(medias_diasIJL22)
#View(medias_diasIJL22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIJL22) <- nombres
medias_diasIJL22$Fecha <- as.Date(medias_diasIJL22$Fecha)
options(digits=5)
medias_diasIJL22$I1 <- as.numeric(medias_diasIJL22$I1)
medias_diasIJL22$I2 <- as.numeric(medias_diasIJL22$I2)
medias_diasIJL22$I3 <- as.numeric(medias_diasIJL22$I3)
#View(medias_diasIJL22)


max_diasIJL22 <- rbind(max_dia_1IJL,max_dia_2IJL,max_dia_3IJL,max_dia_4IJL,
                       max_dia_5IJL,max_dia_6IJL,max_dia_7IJL,max_dia_8IJL,
                       max_dia_9IJL,max_dia_10IJL,max_dia_11IJL,max_dia_12IJL,
                       max_dia_13IJL,max_dia_14IJL,max_dia_15IJL,max_dia_16IJL,
                       max_dia_17IJL,max_dia_18IJL,max_dia_19IJL,max_dia_20IJL,
                       max_dia_21IJL,max_dia_22IJL,max_dia_23IJL,max_dia_24IJL,
                       max_dia_25IJL,max_dia_26IJL,max_dia_27IJL,max_dia_28IJL,
                       max_dia_29IJL,max_dia_30IJL,max_dia_31IJL)

max_diasIJL22 <- cbind(levels(DiasFJL),max_diasIJL22)
max_diasIJL22 <- as.data.frame(max_diasIJL22)
#View(max_diasIJL22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIJL22) <- nombres
max_diasIJL22$Fecha <- as.Date(max_diasIJL22$Fecha)
options(digits=5)
max_diasIJL22$I1 <- as.numeric(max_diasIJL22$I1)
max_diasIJL22$I2 <- as.numeric(max_diasIJL22$I2)
max_diasIJL22$I3 <- as.numeric(max_diasIJL22$I3)
#View(max_diasIJL22)
#####
Julio <- rbind(medias_diasIJL14,medias_diasIJL17,medias_diasIJL18,
               medias_diasIJL22)
JulioM <- rbind(max_diasIJL14,max_diasIJL17,max_diasIJL18,max_diasIJL22)
dfJL <- rbind(dfJL14,dfJL17,dfJL18,dfJL22)


##Agosto 2013####
dfAG13 <- read.csv("Data/Years/2013/Agosto2013.csv")
#View(dfAG13)
dfAG13 <- dfAG13[1:2016,1:8]
dfAG13[3:8] <- sapply(dfAG13[3:8],as.numeric)  

dfAG13[3] <- dfAG13[3]/sqrt(3)
dfAG13[4] <- dfAG13[4]/sqrt(3)
dfAG13[5] <- dfAG13[5]/sqrt(3)

dfAG13[6] <- dfAG13[6]*2
dfAG13[7] <- dfAG13[7]*2
dfAG13[8] <- dfAG13[7]*2


dfAG13$Fecha.de.la.Medida <- as.Date(dfAG13$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAG13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAG13, Fecha.de.la.Medida==Dias[i]))
}

names(dfAG13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAG <- vector("numeric", ncol(dfAG13[6:8]))
for (i in seq_along(dfAG13[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(ifelse(dfAG13[6:8][[i]]<=0,NA,dfAG13[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAG<- vector("numeric", ncol(dfAG13[6:8]))
for (i in seq_along(dfAG13[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG13[6:8][[i]] )    # 3. cuerpo
}

##20-agosto a 27-agosto##
###Dia 20###
media_dia_20IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAG[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T)
}

max_dia_20IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAG[[i]] <- max(Dia_20[6:8][[i]]) 
}
###Dia 21###
media_dia_21IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAG[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAG[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAG[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAG[[i]] <- max(Dia_22[6:8][[i]]) 
}
###Dia 23###
media_dia_23IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAG[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAG[[i]] <- max(Dia_23[6:8][[i]]) 
}
###Dia 24###
media_dia_24IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAG[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAG[[i]] <- max(Dia_24[6:8][[i]]) 
}
###Dia 25###
media_dia_25IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IAG[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IAG[[i]] <- max(Dia_25[6:8][[i]]) 
}
###Dia 26###
media_dia_26IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IAG[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IAG[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IAG<-vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAG[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAG <- vector("numeric",  ncol(dfAG13[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAG[[i]] <- max(Dia_27[6:8][[i]]) 
}

medias_diasIAG13 <- rbind(media_dia_20IAG,media_dia_21IAG,media_dia_22IAG,
                        media_dia_23IAG,media_dia_24IAG,media_dia_25IAG,
                        media_dia_26IAG,media_dia_27IAG)
##View(medias_diasI)
medias_diasIAG13 <- cbind(levels(DiasF),medias_diasIAG13)
medias_diasIAG13 <- as.data.frame(medias_diasIAG13)
##View(medias_diasIAG13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG13) <- nombres
medias_diasIAG13$Fecha <- as.Date(medias_diasIAG13$Fecha)
options(digits=5)
medias_diasIAG13$I1 <- as.numeric(medias_diasIAG13$I1)
medias_diasIAG13$I2 <- as.numeric(medias_diasIAG13$I2)
medias_diasIAG13$I3 <- as.numeric(medias_diasIAG13$I3)
##View(medias_diasIAG13)

max_diasIAG13 <- rbind(max_dia_20IAG,max_dia_21IAG,max_dia_22IAG,
                     max_dia_23IAG,max_dia_24IAG,max_dia_25IAG,
                     max_dia_26IAG,max_dia_27IAG)
##View(max_diasIAG13)
max_diasIAG13 <- cbind(levels(DiasF),max_diasIAG13)
max_diasIAG13 <- as.data.frame(max_diasIAG13)
##View(max_diasIAG13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG13) <- nombres
max_diasIAG13$Fecha <- as.Date(max_diasIAG13$Fecha)
options(digits=5)
max_diasIAG13$I1 <- as.numeric(max_diasIAG13$I1)
max_diasIAG13$I2 <- as.numeric(max_diasIAG13$I2)
max_diasIAG13$I3 <- as.numeric(max_diasIAG13$I3)
##View(max_diasIAG13)



##Agosto 2014####
dfAG14 <- read.csv("Data/Years/2014/AGO2014.csv")
#View(dfAG14)
dfAG14 <- dfAG14[1:4864,c(1:8)] 
dfAG14[3:8] <- sapply(dfAG14[3:8],as.numeric)  

dfAG14[3] <- dfAG14[3]/sqrt(3)
dfAG14[4] <- dfAG14[4]/sqrt(3)
dfAG14[5] <- dfAG14[5]/sqrt(3)

# dfAG14[6] <- dfAG14[6]*2
# dfAG14[7] <- dfAG14[7]*2
# dfAG14[7] <- dfAG14[7]*2



dfAG14$Feha.de.la.Medda <- as.Date(dfAG14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfAG14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAG14, Feha.de.la.Medda==Dias[i]))
}

names(dfAG14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAG <- vector("numeric", ncol(dfAG14[6:8]))
for (i in seq_along(dfAG14[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(ifelse(dfAG14[6:8][[i]]<=0,NA,dfAG14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAG<- vector("numeric", ncol(dfAG14[6:8]))
for (i in seq_along(dfAG14[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG14[6:8][[i]] )    # 3. cuerpo
}

##1-agosto a 18-agosto#
###Dia 1###
media_dia_1IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IAG[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IAG[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IAG[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IAG[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IAG[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IAG[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IAG[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IAG[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IAG[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IAG[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IAG[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IAG[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IAG[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IAG[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAG[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAG[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAG[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAG[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAG[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAG[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAG[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAG[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IAG[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IAG[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IAG[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IAG[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IAG[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IAG[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IAG[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IAG[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IAG[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IAG[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAG[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAG[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IAG<-vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAG[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAG <- vector("numeric",  ncol(dfAG14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAG[[i]] <- max(Dia_18[6:8][[i]]) 
}

medias_diasIAG14 <- rbind(media_dia_1IAG,media_dia_2IAG,media_dia_3IAG,
                        media_dia_4IAG,media_dia_5IAG,media_dia_6IAG,
                        media_dia_7IAG,media_dia_8IAG,media_dia_9IAG,
                        media_dia_10IAG,media_dia_11IAG,media_dia_12IAG,
                        media_dia_13IAG,media_dia_14IAG,media_dia_15IAG,
                        media_dia_16IAG,media_dia_17IAG,media_dia_18IAG)
##View(medias_diasI)
medias_diasIAG14 <- cbind(levels(DiasF),medias_diasIAG14)
medias_diasIAG14 <- as.data.frame(medias_diasIAG14)
##View(medias_diasIAG14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG14) <- nombres
medias_diasIAG14$Fecha <- as.Date(medias_diasIAG14$Fecha)
options(digits=5)
medias_diasIAG14$I1 <- as.numeric(medias_diasIAG14$I1)
medias_diasIAG14$I2 <- as.numeric(medias_diasIAG14$I2)
medias_diasIAG14$I3 <- as.numeric(medias_diasIAG14$I3)
##View(medias_diasIAG14)

max_diasIAG14 <- rbind(max_dia_1IAG,max_dia_2IAG,max_dia_3IAG,
                     max_dia_4IAG,max_dia_5IAG,max_dia_6IAG,
                     max_dia_7IAG,max_dia_8IAG,max_dia_9IAG,
                     max_dia_10IAG,max_dia_11IAG,max_dia_12IAG,
                     max_dia_13IAG,max_dia_14IAG,max_dia_15IAG,
                     max_dia_16IAG,max_dia_17IAG,max_dia_18IAG)
##View(max_diasIAG14)
max_diasIAG14 <- cbind(levels(DiasF),max_diasIAG14)
max_diasIAG14 <- as.data.frame(max_diasIAG14)
##View(max_diasIAG14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG14) <- nombres
max_diasIAG14$Fecha <- as.Date(max_diasIAG14$Fecha)
options(digits=5)
max_diasIAG14$I1 <- as.numeric(max_diasIAG14$I1)
max_diasIAG14$I2 <- as.numeric(max_diasIAG14$I2)
max_diasIAG14$I3 <- as.numeric(max_diasIAG14$I3)
##View(max_diasIAG14)




##Agosto 2015####
dfAG15 <- read.csv("Data/Years/2015/AGOSTO2015.csv")
#View(dfAG15)
dfAG15 <- dfAG15[1:7713,c(1:8)] 
dfAG15[3:8] <- sapply(dfAG15[3:8],as.numeric)  

dfAG15[3] <- dfAG15[3]/sqrt(3)
dfAG15[4] <- dfAG15[4]/sqrt(3)
dfAG15[5] <- dfAG15[5]/sqrt(3)

dfAG15[6] <- dfAG15[6]*2
dfAG15[7] <- dfAG15[7]*2
dfAG15[8] <- dfAG15[8]*2



dfAG15$Feha.de.la.Medda <- as.Date(dfAG15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfAG15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAG15, Feha.de.la.Medda==Dias[i]))
}

names(dfAG15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAG <- vector("numeric", ncol(dfAG15[6:8]))
for (i in seq_along(dfAG15[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(ifelse(dfAG15[6:8][[i]]<=0,NA,dfAG15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAG<- vector("numeric", ncol(dfAG15[6:8]))
for (i in seq_along(dfAG15[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG15[6:8][[i]] )    # 3. cuerpo
}

##4-Agosto a 31-Agosto#
###Dia 4###
media_dia_4IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IAG[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IAG[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IAG[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IAG[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IAG[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IAG[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IAG[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IAG[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAG[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAG[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAG[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAG[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAG[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAG[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAG[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAG[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IAG[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IAG[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IAG[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IAG[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IAG[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IAG[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IAG[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IAG[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IAG[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IAG[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAG[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAG[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAG[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAG[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IAG[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IAG[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAG[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAG[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAG[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAG[[i]] <- max(Dia_21[6:8][[i]]) 
}
###Dia 22###
media_dia_22IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAG[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAG[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAG[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAG[[i]] <- max(Dia_23[6:8][[i]]) 
}
###Dia 24###
media_dia_24IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAG[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAG[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 255###
media_dia_25IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IAG[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IAG[[i]] <- max(Dia_25[6:8][[i]]) 
}
###Dia 26###
media_dia_26IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IAG[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IAG[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAG[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAG[[i]] <- max(Dia_27[6:8][[i]]) 
}
###Dia 28###
media_dia_28IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IAG[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IAG[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IAG[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IAG[[i]] <- max(Dia_29[6:8][[i]]) 
}
###Dia 30###
media_dia_30IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IAG[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IAG[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IAG<-vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IAG[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IAG <- vector("numeric",  ncol(dfAG15[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IAG[[i]] <- max(Dia_31[6:8][[i]]) 
}



medias_diasIAG15 <- rbind(media_dia_4IAG,media_dia_5IAG,media_dia_6IAG,
                        media_dia_7IAG,media_dia_8IAG,media_dia_9IAG,
                        media_dia_10IAG,media_dia_11IAG,media_dia_12IAG,
                        media_dia_13IAG,media_dia_14IAG,media_dia_15IAG,
                        media_dia_16IAG,media_dia_17IAG,media_dia_18IAG,
                        media_dia_19IAG,media_dia_20IAG,media_dia_21IAG,
                        media_dia_22IAG,media_dia_23IAG, media_dia_24IAG,
                        media_dia_25IAG,media_dia_26IAG,media_dia_27IAG,
                        media_dia_28IAG,media_dia_29IAG,media_dia_30IAG,
                        media_dia_31IAG)
##View(medias_diasI)
medias_diasIAG15 <- cbind(levels(DiasF),medias_diasIAG15)
medias_diasIAG15 <- as.data.frame(medias_diasIAG15)
##View(medias_diasIAG15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG15) <- nombres
medias_diasIAG15$Fecha <- as.Date(medias_diasIAG15$Fecha)
options(digits=5)
medias_diasIAG15$I1 <- as.numeric(medias_diasIAG15$I1)
medias_diasIAG15$I2 <- as.numeric(medias_diasIAG15$I2)
medias_diasIAG15$I3 <- as.numeric(medias_diasIAG15$I3)
##View(medias_diasIAG15)

max_diasIAG15 <- rbind(max_dia_4IAG,max_dia_5IAG,max_dia_6IAG,
                     max_dia_7IAG,max_dia_8IAG,max_dia_9IAG,
                     max_dia_10IAG,max_dia_11IAG,max_dia_12IAG,
                     max_dia_13IAG,max_dia_14IAG,max_dia_15IAG,
                     max_dia_16IAG,max_dia_17IAG,max_dia_18IAG,
                     max_dia_19IAG,max_dia_20IAG,max_dia_21IAG,
                     max_dia_22IAG,max_dia_23IAG, max_dia_24IAG,
                     max_dia_25IAG,max_dia_26IAG,max_dia_27IAG,
                     max_dia_28IAG,max_dia_29IAG,max_dia_30IAG,
                     max_dia_31IAG)
##View(max_diasIAG15)
max_diasIAG15 <- cbind(levels(DiasF),max_diasIAG15)
max_diasIAG15 <- as.data.frame(max_diasIAG15)
##View(max_diasIAG15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG15) <- nombres
max_diasIAG15$Fecha <- as.Date(max_diasIAG15$Fecha)
options(digits=5)
max_diasIAG15$I1 <- as.numeric(max_diasIAG15$I1)
max_diasIAG15$I2 <- as.numeric(max_diasIAG15$I2)
max_diasIAG15$I3 <- as.numeric(max_diasIAG15$I3)
##View(max_diasIAG15)




##Agosto 2016####
dfAG16 <- read.csv("Data/Years/2016/agosto2016.csv")
#View(dfAG16)
dfAG16 <- dfAG16[1:2715,c(1:8)] 
dfAG16[3:8] <- sapply(dfAG16[3:8],as.numeric)  

dfAG16[3] <- dfAG16[3]/sqrt(3)
dfAG16[4] <- dfAG16[4]/sqrt(3)
dfAG16[5] <- dfAG16[5]/sqrt(3)

dfAG16[6] <- dfAG16[6]*2
dfAG16[7] <- dfAG16[7]*2
dfAG16[8] <- dfAG16[8]*2



dfAG16$Fecha.de.la.Medida <- as.Date(dfAG16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAG16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAG16, Fecha.de.la.Medida==Dias[i]))
}

names(dfAG16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAG <- vector("numeric", ncol(dfAG16[6:8]))
for (i in seq_along(dfAG16[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(ifelse(dfAG16[6:8][[i]]<=0,NA,dfAG16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAG<- vector("numeric", ncol(dfAG16[6:8]))
for (i in seq_along(dfAG16[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG16[6:8][[i]] )    # 3. cuerpo
}

##3-Agosto a 31-Agosto#
###Dia 3###
media_dia_3IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IAG[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IAG[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IAG[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IAG[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IAG[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IAG[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IAG[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IAG[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IAG[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IAG[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAG[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAG[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAG[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAG[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAG[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAG[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAG[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAG[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IAG[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IAG[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IAG[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IAG[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IAG[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IAG[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IAG[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IAG[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IAG[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IAG[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IAG[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IAG[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IAG[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IAG[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IAG[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IAG[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IAG[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IAG[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IAG[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IAG[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IAG[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IAG[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IAG[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IAG[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IAG[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IAG[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IAG[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IAG[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IAG[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IAG[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAG[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAG[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IAG[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IAG[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IAG[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IAG[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IAG[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IAG[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IAG<-vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IAG[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IAG <- vector("numeric",  ncol(dfAG16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IAG[[i]] <- max(Dia_31[6:8][[i]]) 
}


medias_diasIAG16 <- rbind(media_dia_3IAG,media_dia_4IAG,
                        media_dia_5IAG,media_dia_6IAG,media_dia_7IAG,
                        media_dia_8IAG,media_dia_9IAG,media_dia_10IAG,
                        media_dia_11IAG,media_dia_12IAG,media_dia_13IAG,
                        media_dia_14IAG,media_dia_15IAG,media_dia_16IAG,
                        media_dia_17IAG,
                        media_dia_18IAG,media_dia_19IAG,media_dia_20IAG,
                        media_dia_21IAG,media_dia_22IAG,media_dia_23IAG,
                        media_dia_24IAG,media_dia_25IAG,media_dia_26IAG,
                        media_dia_27IAG,
                        media_dia_28IAG,media_dia_29IAG,media_dia_30IAG,
                        media_dia_31IAG)
##View(medias_diasI)
medias_diasIAG16 <- cbind(levels(DiasF),medias_diasIAG16)
medias_diasIAG16 <- as.data.frame(medias_diasIAG16)
##View(medias_diasIAG16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG16) <- nombres
medias_diasIAG16$Fecha <- as.Date(medias_diasIAG16$Fecha)
options(digits=5)
medias_diasIAG16$I1 <- as.numeric(medias_diasIAG16$I1)
medias_diasIAG16$I2 <- as.numeric(medias_diasIAG16$I2)
medias_diasIAG16$I3 <- as.numeric(medias_diasIAG16$I3)
##View(medias_diasIAG16)

max_diasIAG16 <- rbind(max_dia_3IAG,max_dia_4IAG,
                     max_dia_5IAG,max_dia_6IAG,max_dia_7IAG,
                     max_dia_8IAG,max_dia_9IAG,max_dia_10IAG,
                     max_dia_11IAG,max_dia_12IAG,max_dia_13IAG,
                     max_dia_14IAG,max_dia_15IAG,max_dia_16IAG,
                     max_dia_17IAG,
                     max_dia_18IAG,max_dia_19IAG,max_dia_20IAG,
                     max_dia_21IAG,max_dia_22IAG,max_dia_23IAG,
                     max_dia_24IAG,max_dia_25IAG,max_dia_26IAG,
                     max_dia_27IAG,
                     max_dia_28IAG,max_dia_29IAG,max_dia_30IAG,
                     max_dia_31IAG)
##View(max_diasIAG16)
max_diasIAG16 <- cbind(levels(DiasF),max_diasIAG16)
max_diasIAG16 <- as.data.frame(max_diasIAG16)
##View(max_diasIAG16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG16) <- nombres
max_diasIAG16$Fecha <- as.Date(max_diasIAG16$Fecha)
options(digits=5)
max_diasIAG16$I1 <- as.numeric(max_diasIAG16$I1)
max_diasIAG16$I2 <- as.numeric(max_diasIAG16$I2)
max_diasIAG16$I3 <- as.numeric(max_diasIAG16$I3)
##View(max_diasIAG16)




##Agosto 2017####
dfAG17 <- read.csv("Data/Years/2017/AGO2017.csv")
#View(dfAG17)
dfAG17 <- dfAG17[1:768,c(1:8)] 
dfAG17[3:8] <- sapply(dfAG17[3:8],as.numeric)  

dfAG17[3] <- dfAG17[3]/sqrt(3)
dfAG17[4] <- dfAG17[4]/sqrt(3)
dfAG17[5] <- dfAG17[5]/sqrt(3)

dfAG17[6] <- dfAG17[6]*2
dfAG17[7] <- dfAG17[7]*2
dfAG17[8] <- dfAG17[8]*2



dfAG17$Fecha.de.la.Medida <- as.Date(dfAG17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAG17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAG17, Fecha.de.la.Medida==Dias[i]))
}

names(dfAG17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAG <- vector("numeric", ncol(dfAG17[6:8]))
for (i in seq_along(dfAG17[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(ifelse(dfAG17[6:8][[i]]<=0,NA,dfAG17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAG<- vector("numeric", ncol(dfAG17[6:8]))
for (i in seq_along(dfAG17[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG17[6:8][[i]] )    # 3. cuerpo
}

##4-Agosto a 11-Agosto#

###Dia 4###
media_dia_4IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IAG[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IAG[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IAG[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IAG[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IAG[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IAG[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IAG[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IAG[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IAG[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IAG[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IAG[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IAG[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IAG[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IAG[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IAG<-vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IAG[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IAG <- vector("numeric",  ncol(dfAG17[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IAG[[i]] <- max(Dia_11[6:8][[i]]) 
}


medias_diasIAG17 <- rbind(media_dia_4IAG,
                        media_dia_5IAG,media_dia_6IAG,media_dia_7IAG,
                        media_dia_8IAG,media_dia_9IAG,media_dia_10IAG,
                        media_dia_11IAG)
##View(medias_diasI)
medias_diasIAG17 <- cbind(levels(DiasF),medias_diasIAG17)
medias_diasIAG17 <- as.data.frame(medias_diasIAG17)
##View(medias_diasIAG17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG17) <- nombres
medias_diasIAG17$Fecha <- as.Date(medias_diasIAG17$Fecha)
options(digits=5)
medias_diasIAG17$I1 <- as.numeric(medias_diasIAG17$I1)
medias_diasIAG17$I2 <- as.numeric(medias_diasIAG17$I2)
medias_diasIAG17$I3 <- as.numeric(medias_diasIAG17$I3)
##View(medias_diasIAG17)

max_diasIAG17 <- rbind(max_dia_4IAG,
                     max_dia_5IAG,max_dia_6IAG,max_dia_7IAG,
                     max_dia_8IAG,max_dia_9IAG,max_dia_10IAG,
                     max_dia_11IAG)
##View(max_diasIAG17)
max_diasIAG17 <- cbind(levels(DiasF),max_diasIAG17)
max_diasIAG17 <- as.data.frame(max_diasIAG17)
##View(max_diasIAG17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG17) <- nombres
max_diasIAG17$Fecha <- as.Date(max_diasIAG17$Fecha)
options(digits=5)
max_diasIAG17$I1 <- as.numeric(max_diasIAG17$I1)
max_diasIAG17$I2 <- as.numeric(max_diasIAG17$I2)
max_diasIAG17$I3 <- as.numeric(max_diasIAG17$I3)
##View(max_diasIAG17)




##Agosto 2018####
dfAG18 <- read.csv("Data/Years/2018/AGO2018.csv")
#View(dfAG18)
dfAG18 <- dfAG18[1:389,c(1:8)] 
dfAG18[3:8] <- sapply(dfAG18[3:8],as.numeric)  

dfAG18[3] <- dfAG18[3]/sqrt(3)
dfAG18[4] <- dfAG18[4]/sqrt(3)
dfAG18[5] <- dfAG18[5]/sqrt(3)
# 
# dfAG18[6] <- dfAG18[6]*2
# dfAG18[7] <- dfAG18[7]*2
# dfAG18[8] <- dfAG18[8]*2
# 


dfAG18$Fecha.de.la.Medida <- as.Date(dfAG18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfAG18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAG18, Fecha.de.la.Medida==Dias[i]))
}

names(dfAG18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIAG <- vector("numeric", ncol(dfAG18[6:8]))
for (i in seq_along(dfAG18[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(ifelse(dfAG18[6:8][[i]]<=0,NA,dfAG18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIAG<- vector("numeric", ncol(dfAG18[6:8]))
for (i in seq_along(dfAG18[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG18[6:8][[i]] )    # 3. cuerpo
}

##27-Agosto a 31-Agosto#
###Dia 27###
media_dia_27IAG<-vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IAG[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IAG <- vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IAG[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IAG<-vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IAG[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IAG <- vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IAG[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IAG<-vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IAG[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IAG <- vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IAG[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IAG<-vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IAG[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IAG <- vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IAG[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia A31###
media_dia_31IAG<-vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IAG[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IAG <- vector("numeric",  ncol(dfAG18[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IAG[[i]] <- max(Dia_31[6:8][[i]]) 
}



medias_diasIAG18 <- rbind(
  media_dia_27IAG,
  media_dia_28IAG,media_dia_29IAG,media_dia_30IAG,
  media_dia_31IAG)
##View(medias_diasI)
medias_diasIAG18 <- cbind(levels(DiasF),medias_diasIAG18)
medias_diasIAG18 <- as.data.frame(medias_diasIAG18)
##View(medias_diasIAG18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG18) <- nombres
medias_diasIAG18$Fecha <- as.Date(medias_diasIAG18$Fecha)
options(digits=5)
medias_diasIAG18$I1 <- as.numeric(medias_diasIAG18$I1)
medias_diasIAG18$I2 <- as.numeric(medias_diasIAG18$I2)
medias_diasIAG18$I3 <- as.numeric(medias_diasIAG18$I3)
##View(medias_diasIAG18)

max_diasIAG18 <- rbind(max_dia_27IAG,
                     max_dia_28IAG,max_dia_29IAG,max_dia_30IAG,
                     max_dia_31IAG)
##View(max_diasIAG18)
max_diasIAG18 <- cbind(levels(DiasF),max_diasIAG18)
max_diasIAG18 <- as.data.frame(max_diasIAG18)
##View(max_diasIAG18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG18) <- nombres
max_diasIAG18$Fecha <- as.Date(max_diasIAG18$Fecha)
options(digits=5)
max_diasIAG18$I1 <- as.numeric(max_diasIAG18$I1)
max_diasIAG18$I2 <- as.numeric(max_diasIAG18$I2)
max_diasIAG18$I3 <- as.numeric(max_diasIAG18$I3)
##View(max_diasIAG18)


## Agosto 2022####
library(dplyr)
dfAG22 <- read.csv("Data/Years/2022/Agosto.csv")
##View(dfAG22)
#str(dfAG22)
dfAG22 <- dfAG22[1:8724,1:8]


dfAG22$Fecha <- as.Date(dfAG22$Fecha,format="%d/%m/%Y")
DiasFAG <- as.factor(dfAG22$Fecha)
DiasAG<- levels(DiasFAG)
date <- as.Date(DiasAG)
dfAG22[3:8] <-sapply(dfAG22[3:8],as.numeric)

for (i in min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysAG <- gsub("$","AG",gsub("^","Dia_",as.character( seq(1:length(DiasAG)))))
  assign(DaysAG[i],filter(dfAG22, Fecha==DiasAG[i]))
}

names(dfAG22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

####I1-I3###
mediaIAG <- vector("numeric", ncol(dfAG22[6:8]))


for (i in seq_along(dfAG22[6:8])) {# 2. secuencia
  mediaIAG[[i]] <- mean(dfAG22[6:8][[i]])    # 3. cuerpo
}

mediaIAG

maxIAG <- vector("numeric", ncol(dfAG22[6:8]))

for (i in seq_along(dfAG22[6:8])) {# 2. secuencia
  maxIAG[[i]] <- max(dfAG22[6:8][[i]])     # 3. cuerpo
}

maxIAG



##01-agosto a 10-agosto###
###Dia 1###
media_dia_1IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_1AG[6:8])) {
  media_dia_1IAG[[i]] <- mean(ifelse(Dia_1AG[6:8][[i]]<=0,NA,Dia_1AG[6:8][[i]]),
                              na.rm = T)
}

max_dia_1IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_1AG[6:8])) {
  max_dia_1IAG[[i]] <- max(Dia_1AG[6:8][[i]])
}



###Dia 2###
media_dia_2IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_2AG[6:8])) {
  media_dia_2IAG[[i]] <- mean(ifelse(Dia_2AG[6:8][[i]]<=0,NA,Dia_2AG[6:8][[i]]),
                              na.rm = T) 
}


max_dia_2IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_2AG[6:8])) {
  max_dia_2IAG[[i]] <- max(Dia_2AG[6:8][[i]])
}



###Dia 3###
media_dia_3IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_3AG[6:8])) {
  media_dia_3IAG[[i]] <- mean(ifelse(Dia_3AG[6:8][[i]]<=0,NA,Dia_3AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_3IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_3AG[6:8])) {
  max_dia_3IAG[[i]] <- max(Dia_3AG[6:8][[i]])
}



###Dia 4###
media_dia_4IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_4AG[6:8])) {
  media_dia_4IAG[[i]] <- mean(ifelse(Dia_4AG[6:8][[i]]<=0,NA,Dia_4AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_4IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_4AG[6:8])) {
  max_dia_4IAG[[i]] <- max(Dia_4AG[6:8][[i]])
}


###Dia 5###

media_dia_5IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_5AG[6:8])) {
  media_dia_5IAG[[i]] <- mean(ifelse(Dia_5AG[6:8][[i]]<=0,NA,Dia_5AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_5IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_5AG[6:8])) {
  max_dia_5IAG[[i]] <- max(Dia_5AG[6:8][[i]])
}



###Dia 6###

media_dia_6IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_6AG[6:8])) {
  media_dia_6IAG[[i]] <- mean(ifelse(Dia_6AG[6:8][[i]]<=0,NA,Dia_6AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_6IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_6AG[6:8])) {
  max_dia_6IAG[[i]] <- max(Dia_6AG[6:8][[i]])
}



###Dia 7###

media_dia_7IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_7AG[6:8])) {
  media_dia_7IAG[[i]] <- mean(ifelse(Dia_7AG[6:8][[i]]<=0,NA,Dia_7AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_7IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_7AG[6:8])) {
  max_dia_7IAG[[i]] <- max(Dia_7AG[6:8][[i]])
}



###Dia 8 ###

media_dia_8IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_8AG[6:8])) {
  media_dia_8IAG[[i]] <- mean(ifelse(Dia_8AG[6:8][[i]]<=0,NA,Dia_8AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_8IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_8AG[6:8])) {
  max_dia_8IAG[[i]] <- max(Dia_8AG[6:8][[i]])
}



###Dia 9 ###

media_dia_9IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_9AG[6:8])) {
  media_dia_9IAG[[i]] <- mean(ifelse(Dia_9AG[6:8][[i]]<=0,NA,Dia_9AG[6:8][[i]]),
                              na.rm = T) 
}

max_dia_9IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_9AG[6:8])) {
  max_dia_9IAG[[i]] <- max(Dia_9AG[6:8][[i]])
}



###Dia 10 ###
media_dia_10IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_10AG[6:8])) {
  media_dia_10IAG[[i]] <- mean(ifelse(Dia_10AG[6:8][[i]]<=0,NA,Dia_10AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_10IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_10AG[6:8])) {
  max_dia_10IAG[[i]] <- max(Dia_10AG[6:8][[i]])
}






##11-agosto a 20-agosto###
###Dia 11###
media_dia_11IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_11AG[6:8])) {
  media_dia_11IAG[[i]] <- mean(ifelse(Dia_11AG[6:8][[i]]<=0,NA,Dia_11AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_11IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_11AG[6:8])) {
  max_dia_11IAG[[i]] <- max(Dia_11AG[6:8][[i]])
}

###Dia 12###
media_dia_12IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_12AG[6:8])) {
  media_dia_12IAG[[i]] <- mean(ifelse(Dia_12AG[6:8][[i]]<=0,NA,Dia_12AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_12IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_12AG[6:8])) {
  max_dia_12IAG[[i]] <- max(Dia_12AG[6:8][[i]])
}

###Dia 13###
media_dia_13IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_13AG[6:8])) {
  media_dia_13IAG[[i]] <- mean(ifelse(Dia_13AG[6:8][[i]]<=0,NA,Dia_13AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_13IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_13AG[6:8])) {
  max_dia_13IAG[[i]] <- max(Dia_13AG[6:8][[i]])
}


###Dia 14###
media_dia_14IAG<-vector("numeric",  ncol(dfAG22[6:8]))


for (i in seq_along(Dia_14AG[6:8])) {
  media_dia_14IAG[[i]] <- mean(ifelse(Dia_14AG[6:8][[i]]<=0,NA,Dia_14AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_14IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_14AG[6:8])) {
  max_dia_14IAG[[i]] <- max(Dia_14AG[6:8][[i]])
}

###Dia 15###
media_dia_15IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_15AG[6:8])) {
  media_dia_15IAG[[i]] <- mean(ifelse(Dia_15AG[6:8][[i]]<=0,NA,Dia_15AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_15IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_15AG[6:8])) {
  max_dia_15IAG[[i]] <- max(Dia_15AG[6:8][[i]])
}

###Dia 16###
media_dia_16IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_16AG[6:8])) {
  media_dia_16IAG[[i]] <- mean(ifelse(Dia_16AG[6:8][[i]]<=0,NA,Dia_16AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_16IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_16AG[6:8])) {
  max_dia_16IAG[[i]] <- max(Dia_16AG[6:8][[i]])
}

###Dia 17###
media_dia_17IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_17AG[6:8])) {
  media_dia_17IAG[[i]] <- mean(ifelse(Dia_17AG[6:8][[i]]<=0,NA,Dia_17AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_17IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_17AG[6:8])) {
  max_dia_17IAG[[i]] <- max(Dia_17AG[6:8][[i]])
}

###Dia 18 ###
media_dia_18IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_18AG[6:8])) {
  media_dia_18IAG[[i]] <- mean(ifelse(Dia_18AG[6:8][[i]]<=0,NA,Dia_18AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_18IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_18AG[6:8])) {
  max_dia_18IAG[[i]] <- max(Dia_18AG[6:8][[i]])
}

###Dia 19 ###
media_dia_19IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_19AG[6:8])) {
  media_dia_19IAG[[i]] <- mean(ifelse(Dia_19AG[6:8][[i]]<=0,NA,Dia_19AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_19IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_19AG[6:8])) {
  max_dia_19IAG[[i]] <- max(Dia_19AG[6:8][[i]])
}

###Dia 20 ###
media_dia_20IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_20AG[6:8])) {
  media_dia_20IAG[[i]] <- mean(ifelse(Dia_20AG[6:8][[i]]<=0,NA,Dia_20AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_20IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_20AG[6:8])) {
  max_dia_20IAG[[i]] <- max(Dia_20AG[6:8][[i]])
}

##21-agosto a 31-agosto###
###Dia 21###
media_dia_21IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_21AG[6:8])) {
  media_dia_21IAG[[i]] <- mean(ifelse(Dia_21AG[6:8][[i]]<=0,NA,Dia_21AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_21IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_21AG[6:8])) {
  max_dia_21IAG[[i]] <- max(Dia_21AG[6:8][[i]])
}

###Dia 22###
media_dia_22IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_22AG[6:8])) {
  media_dia_22IAG <- mean(ifelse(Dia_22AG[6:8][[i]]<=0,NA,Dia_22AG[6:8][[i]]),
                          na.rm = T) 
}

max_dia_22IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_22AG[6:8])) {
  max_dia_22IAG[[i]] <- max(Dia_22AG[6:8][[i]])
}


###Dia 23###
media_dia_23IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_23AG[6:8])) {
  media_dia_23IAG[[i]] <- mean(ifelse(Dia_23AG[6:8][[i]]<=0,NA,Dia_23AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_23IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_23AG[6:8])) {
  max_dia_23IAG[[i]] <- max(Dia_23AG[6:8][[i]])
}


###Dia24###
media_dia_24IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_24AG[6:8])) {
  media_dia_24IAG[[i]] <- mean(ifelse(Dia_24AG[6:8][[i]]<=0,NA,Dia_24AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_24IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_24AG[6:8])) {
  max_dia_24IAG[[i]] <- max(Dia_24AG[6:8][[i]])
}


###Dia 25###
media_dia_25IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_25AG[6:8])) {
  media_dia_25IAG[[i]] <- mean(ifelse(Dia_25AG[6:8][[i]]<=0,NA,Dia_25AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_25IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_25AG[6:8])) {
  max_dia_25IAG[[i]] <- max(Dia_25AG[6:8][[i]])
}


###Dia 26###
media_dia_26IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_26AG[6:8])) {
  media_dia_26IAG[[i]] <- mean(ifelse(Dia_26AG[6:8][[i]]<=0,NA,Dia_26AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_26IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_26AG[6:8])) {
  max_dia_26IAG[[i]] <- max(Dia_26AG[6:8][[i]])
}


###Dia 27###
media_dia_27IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_27AG[6:8])) {
  media_dia_27IAG[[i]] <- mean(ifelse(Dia_27AG[6:8][[i]]<=0,NA,Dia_27AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_27IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_27AG[6:8])) {
  max_dia_27IAG[[i]] <- max(Dia_27AG[6:8][[i]])
}


###Dia 28 ###
media_dia_28IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_28AG[6:8])) {
  media_dia_28IAG[[i]] <- mean(ifelse(Dia_28AG[6:8][[i]]<=0,NA,Dia_28AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_28IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_28AG[6:8])) {
  max_dia_28IAG[[i]] <- max(Dia_28AG[6:8][[i]])
}


###Dia 29 ###
media_dia_29IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_29AG[6:8])) {
  media_dia_29IAG[[i]] <- mean(ifelse(Dia_29AG[6:8][[i]]<=0,NA,Dia_29AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_29IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_29AG[6:8])) {
  max_dia_29IAG[[i]] <- max(Dia_29AG[6:8][[i]])
}


###Dia 30 ###
media_dia_30IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_30AG[6:8])) {
  media_dia_30IAG[[i]] <- mean(ifelse(Dia_30AG[6:8][[i]]<=0,NA,Dia_30AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_30IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_30AG[6:8])) {
  max_dia_30IAG[[i]] <- max(Dia_30AG[6:8][[i]])
}


###Dia 31 ###
media_dia_31IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_31AG[6:8])) {
  media_dia_31IAG[[i]] <- mean(ifelse(Dia_31AG[6:8][[i]]<=0,NA,Dia_31AG[6:8][[i]]),
                               na.rm = T) 
}

max_dia_31IAG<-vector("numeric",  ncol(dfAG22[6:8]))

for (i in seq_along(Dia_31AG[6:8])) {
  max_dia_31IAG[[i]] <- max(Dia_31AG[6:8][[i]])
}


medias_diasIAG22 <- rbind(media_dia_1IAG,media_dia_2IAG,media_dia_3IAG,media_dia_4IAG,
                          media_dia_5IAG,media_dia_6IAG,media_dia_7IAG,media_dia_8IAG,
                          media_dia_9IAG,media_dia_10IAG,media_dia_11IAG,media_dia_12IAG,
                          media_dia_13IAG,media_dia_14IAG,media_dia_15IAG,media_dia_16IAG,
                          media_dia_17IAG,media_dia_18IAG,media_dia_19IAG,media_dia_20IAG,
                          media_dia_21IAG,media_dia_22IAG,media_dia_23IAG,media_dia_24IAG,
                          media_dia_25IAG,media_dia_26IAG,media_dia_27IAG,media_dia_28IAG,
                          media_dia_29IAG,media_dia_30IAG,media_dia_31IAG)


#View(medias_diasIAG22)


medias_diasIAG22 <- cbind(levels(DiasFAG),medias_diasIAG22)
medias_diasIAG22 <- as.data.frame(medias_diasIAG22)
#View(medias_diasIAG22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIAG22) <- nombres
medias_diasIAG22$Fecha <- as.Date(medias_diasIAG22$Fecha)
options(digits=5)
medias_diasIAG22$I1 <- as.numeric(medias_diasIAG22$I1)
medias_diasIAG22$I2 <- as.numeric(medias_diasIAG22$I2)
medias_diasIAG22$I3 <- as.numeric(medias_diasIAG22$I3)
#View(medias_diasIAG22)


max_diasIAG22 <- rbind(max_dia_1IAG,max_dia_2IAG,max_dia_3IAG,max_dia_4IAG,
                       max_dia_5IAG,max_dia_6IAG,max_dia_7IAG,max_dia_8IAG,
                       max_dia_9IAG,max_dia_10IAG,max_dia_11IAG,max_dia_12IAG,
                       max_dia_13IAG,max_dia_14IAG,max_dia_15IAG,max_dia_16IAG,
                       max_dia_17IAG,max_dia_18IAG,max_dia_19IAG,max_dia_20IAG,
                       max_dia_21IAG,max_dia_22IAG,max_dia_23IAG,max_dia_24IAG,
                       max_dia_25IAG,max_dia_26IAG,max_dia_27IAG,max_dia_28IAG,
                       max_dia_29IAG,max_dia_30IAG,max_dia_31IAG)


#View(max_diasIAG22)

max_diasIAG22 <- cbind(levels(DiasFAG),max_diasIAG22)
max_diasIAG22 <- as.data.frame(max_diasIAG22)
#View(max_diasIAG22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIAG22) <- nombres
max_diasIAG22$Fecha <- as.Date(max_diasIAG22$Fecha)
options(digits=5)
max_diasIAG22$I1 <- as.numeric(max_diasIAG22$I1)
max_diasIAG22$I2 <- as.numeric(max_diasIAG22$I2)
max_diasIAG22$I3 <- as.numeric(max_diasIAG22$I3)
#View(max_diasIAG22)

#####
Agosto <- rbind(medias_diasIAG13,medias_diasIAG14,medias_diasIAG15,
               medias_diasIAG16,medias_diasIAG17,medias_diasIAG18,
               medias_diasIAG22)
AgostoM <- rbind(max_diasIAG13,max_diasIAG14,max_diasIAG15,
                max_diasIAG16,max_diasIAG17,max_diasIAG18,
                max_diasIAG22)
dfAG <- rbind(dfAG13,dfAG14,dfAG15,dfAG16,dfAG17,dfAG18,dfAG22)



##Septiembre 2013####
dfSP13 <- read.csv("Data/Years/2013/SEPT2013.csv")
#View(dfSP13)
dfSP13 <- dfSP13[1:1550,1:8]
dfSP13[3:8] <- sapply(dfSP13[3:8],as.numeric)  

# dfSP13[3] <- dfSP13[3]/sqrt(3)
# dfSP13[4] <- dfSP13[4]/sqrt(3)
# dfSP13[5] <- dfSP13[5]/sqrt(3)
# 
# dfSP13[6] <- dfSP13[6]*2
# dfSP13[7] <- dfSP13[7]*2
# dfSP13[8] <- dfSP13[7]*2


dfSP13$Fecha.de.la.Medida <- as.Date(dfSP13$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfSP13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfSP13, Fecha.de.la.Medida==Dias[i]))
}

names(dfSP13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaISP <- vector("numeric", ncol(dfSP13[6:8]))
for (i in seq_along(dfSP13[6:8])) {# 2. secuencia
  mediaISP[[i]] <- mean(ifelse(dfSP13[6:8][[i]]<=0,NA,dfSP13[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxISP<- vector("numeric", ncol(dfSP13[6:8]))
for (i in seq_along(dfSP13[6:8])) {# 2. secuencia
  maxISP[[i]] <- max(dfSP13[6:8][[i]] )    # 3. cuerpo
}

##25-septiembre a 30-septiembre#
###Dia 25###
media_dia_25ISP<-vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25ISP[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25ISP <- vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25ISP[[i]] <- max(Dia_25[6:8][[i]]) 
}
###Dia 26###
media_dia_26ISP<-vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26ISP[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26ISP <- vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26ISP[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27ISP<-vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27ISP[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27ISP <- vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27ISP[[i]] <- max(Dia_27[6:8][[i]]) 
}
###Dia 28###
media_dia_28ISP<-vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28ISP[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28ISP <- vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28ISP[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29 ###
media_dia_29ISP<-vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29ISP[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29ISP <- vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29ISP[[i]] <- max(Dia_29[6:8][[i]]) 
}
###Dia 30###
media_dia_30ISP<-vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30ISP[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30ISP <- vector("numeric",  ncol(dfSP13[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30ISP[[i]] <- max(Dia_30[6:8][[i]]) 
}


medias_diasISP13 <- rbind(media_dia_25ISP,media_dia_26ISP,media_dia_27ISP,
                        media_dia_28ISP,media_dia_29ISP,media_dia_30ISP)
##View(medias_diasI)
medias_diasISP13 <- cbind(levels(DiasF),medias_diasISP13)
medias_diasISP13 <- as.data.frame(medias_diasISP13)
##View(medias_diasISP13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasISP13) <- nombres
medias_diasISP13$Fecha <- as.Date(medias_diasISP13$Fecha)
options(digits=5)
medias_diasISP13$I1 <- as.numeric(medias_diasISP13$I1)
medias_diasISP13$I2 <- as.numeric(medias_diasISP13$I2)
medias_diasISP13$I3 <- as.numeric(medias_diasISP13$I3)
##View(medias_diasISP13)

max_diasISP13 <- rbind(max_dia_25ISP,max_dia_26ISP,max_dia_27ISP,
                     max_dia_28ISP,max_dia_29ISP,max_dia_30ISP)
##View(max_diasISP13)
max_diasISP13 <- cbind(levels(DiasF),max_diasISP13)
max_diasISP13 <- as.data.frame(max_diasISP13)
##View(max_diasISP13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasISP13) <- nombres
max_diasISP13$Fecha <- as.Date(max_diasISP13$Fecha)
options(digits=5)
max_diasISP13$I1 <- as.numeric(max_diasISP13$I1)
max_diasISP13$I2 <- as.numeric(max_diasISP13$I2)
max_diasISP13$I3 <- as.numeric(max_diasISP13$I3)
##View(max_diasISP13)


##Septiembre 2014####
dfSP14 <- read.csv("Data/Years/2014/SEPT014.csv")
#View(dfSP14)
dfSP14 <- dfSP14[1:3744,c(1:8)] 
dfSP14[3:8] <- sapply(dfSP14[3:8],as.numeric)  

dfSP14[3] <- dfSP14[3]/sqrt(3)
dfSP14[4] <- dfSP14[4]/sqrt(3)
dfSP14[5] <- dfSP14[5]/sqrt(3)

# dfSP14[6] <- dfSP14[6]*2
# dfSP14[7] <- dfSP14[7]*2
# dfSP14[7] <- dfSP14[7]*2



dfSP14$Feha.de.la.Medda <- as.Date(dfSP14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfSP14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfSP14, Feha.de.la.Medda==Dias[i]))
}

names(dfSP14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaISP <- vector("numeric", ncol(dfSP14[6:8]))
for (i in seq_along(dfSP14[6:8])) {# 2. secuencia
  mediaISP[[i]] <- mean(ifelse(dfSP14[6:8][[i]]<=0,NA,dfSP14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxISP<- vector("numeric", ncol(dfSP14[6:8]))
for (i in seq_along(dfSP14[6:8])) {# 2. secuencia
  maxISP[[i]] <- max(dfSP14[6:8][[i]] )    # 3. cuerpo
}

##12-septiembre a 24-septiembre#
###Dia 12###
media_dia_12ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12ISP[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12ISP[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13ISP[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13ISP[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14ISP[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14ISP[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15ISP[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15ISP[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16ISP[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16ISP[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17ISP[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17ISP[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18ISP[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18ISP[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19ISP[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19ISP[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20ISP[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20ISP[[i]] <- max(Dia_20[6:8][[i]]) 
}
###Dia 21###
media_dia_21ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21ISP[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T)
}

max_dia_21ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21ISP[[i]] <- max(Dia_21[6:8][[i]]) 
}
###Dia 22###
media_dia_22ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22ISP[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22ISP[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23ISP[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23ISP[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24ISP<-vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24ISP[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24ISP <- vector("numeric",  ncol(dfSP14[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24ISP[[i]] <- max(Dia_24[6:8][[i]]) 
}


medias_diasISP14 <- rbind(media_dia_12ISP,media_dia_13ISP,media_dia_14ISP,
                        media_dia_15ISP,media_dia_16ISP,media_dia_17ISP,
                        media_dia_18ISP,media_dia_19ISP,media_dia_20ISP,
                        media_dia_21ISP,media_dia_22ISP,media_dia_23ISP,
                        media_dia_24ISP)
##View(medias_diasI)
medias_diasISP14 <- cbind(levels(DiasF),medias_diasISP14)
medias_diasISP14 <- as.data.frame(medias_diasISP14)
##View(medias_diasISP14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasISP14) <- nombres
medias_diasISP14$Fecha <- as.Date(medias_diasISP14$Fecha)
options(digits=5)
medias_diasISP14$I1 <- as.numeric(medias_diasISP14$I1)
medias_diasISP14$I2 <- as.numeric(medias_diasISP14$I2)
medias_diasISP14$I3 <- as.numeric(medias_diasISP14$I3)
##View(medias_diasISP14)

max_diasISP14 <- rbind(max_dia_12ISP,max_dia_13ISP,max_dia_14ISP,
                     max_dia_15ISP,max_dia_16ISP,max_dia_17ISP,
                     max_dia_18ISP,max_dia_19ISP,max_dia_20ISP,
                     max_dia_21ISP,max_dia_22ISP,max_dia_23ISP,
                     max_dia_24ISP)
##View(max_diasISP14)
max_diasISP14 <- cbind(levels(DiasF),max_diasISP14)
max_diasISP14 <- as.data.frame(max_diasISP14)
##View(max_diasISP14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasISP14) <- nombres
max_diasISP14$Fecha <- as.Date(max_diasISP14$Fecha)
options(digits=5)
max_diasISP14$I1 <- as.numeric(max_diasISP14$I1)
max_diasISP14$I2 <- as.numeric(max_diasISP14$I2)
max_diasISP14$I3 <- as.numeric(max_diasISP14$I3)
##View(max_diasISP14)




##Septiembre 2015####
dfSP15 <- read.csv("Data/Years/2015/SEPTIEMBRE2015.csv")
#View(dfSP15)
dfSP15 <- dfSP15[1:8314,c(1:8)] 
dfSP15[3:8] <- sapply(dfSP15[3:8],as.numeric)  

dfSP15[3] <- dfSP15[3]/sqrt(3)
dfSP15[4] <- dfSP15[4]/sqrt(3)
dfSP15[5] <- dfSP15[5]/sqrt(3)

dfSP15[6] <- dfSP15[6]*2
dfSP15[7] <- dfSP15[7]*2
dfSP15[8] <- dfSP15[8]*2



dfSP15$Feha.de.la.Medda <- as.Date(dfSP15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfSP15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfSP15, Feha.de.la.Medda==Dias[i]))
}

names(dfSP15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaISP <- vector("numeric", ncol(dfSP15[6:8]))
for (i in seq_along(dfSP15[6:8])) {# 2. secuencia
  mediaISP[[i]] <- mean(ifelse(dfSP15[6:8][[i]]<=0,NA,dfSP15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxISP<- vector("numeric", ncol(dfSP15[6:8]))
for (i in seq_along(dfSP15[6:8])) {# 2. secuencia
  maxISP[[i]] <- max(dfSP15[6:8][[i]] )    # 3. cuerpo
}

##1-Septiembre a 30-Septiembre#
###Dia 1###
media_dia_1ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1ISP[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1ISP[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2ISP[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2ISP[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3ISP[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3ISP[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4ISP[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4ISP[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5ISP[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5ISP[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6ISP[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6ISP[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7ISP[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7ISP[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8ISP[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8ISP[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9ISP[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9ISP[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10ISP[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10ISP[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11ISP[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11ISP[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12ISP[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12ISP[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13ISP[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13ISP[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14ISP[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14ISP[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15ISP[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15ISP[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16ISP[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16ISP[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17ISP[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17ISP[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18ISP[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18ISP[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19ISP[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19ISP[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20ISP[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20ISP[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21ISP[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21ISP[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22ISP[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22ISP[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23ISP[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23ISP[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24ISP[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24ISP[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25ISP[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25ISP[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26ISP[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26ISP[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27ISP[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27ISP[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28ISP[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28ISP[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29ISP[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29ISP[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30ISP<-vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30ISP[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30ISP <- vector("numeric",  ncol(dfSP15[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30ISP[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasISP15 <- rbind(media_dia_1ISP,
                        media_dia_2ISP,media_dia_3ISP,media_dia_4ISP,
                        media_dia_5ISP,media_dia_6ISP,media_dia_7ISP,
                        media_dia_8ISP,media_dia_9ISP,media_dia_10ISP,
                        media_dia_11ISP,media_dia_12ISP,media_dia_13ISP,
                        media_dia_14ISP,media_dia_15ISP,media_dia_16ISP,
                        media_dia_17ISP,
                        media_dia_18ISP,media_dia_19ISP,media_dia_20ISP,
                        media_dia_21ISP,media_dia_22ISP,media_dia_23ISP,
                        media_dia_24ISP,media_dia_25ISP,media_dia_26ISP,
                        media_dia_27ISP,
                        media_dia_28ISP,media_dia_29ISP,media_dia_30ISP)
##View(medias_diasI)
medias_diasISP15 <- cbind(levels(DiasF),medias_diasISP15)
medias_diasISP15 <- as.data.frame(medias_diasISP15)
##View(medias_diasISP15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasISP15) <- nombres
medias_diasISP15$Fecha <- as.Date(medias_diasISP15$Fecha)
options(digits=5)
medias_diasISP15$I1 <- as.numeric(medias_diasISP15$I1)
medias_diasISP15$I2 <- as.numeric(medias_diasISP15$I2)
medias_diasISP15$I3 <- as.numeric(medias_diasISP15$I3)
##View(medias_diasISP15)

max_diasISP15 <- rbind(max_dia_1ISP,
                     max_dia_2ISP,max_dia_3ISP,max_dia_4ISP,
                     max_dia_5ISP,max_dia_6ISP,max_dia_7ISP,
                     max_dia_8ISP,max_dia_9ISP,max_dia_10ISP,
                     max_dia_11ISP,max_dia_12ISP,max_dia_13ISP,
                     max_dia_14ISP,max_dia_15ISP,max_dia_16ISP,
                     max_dia_17ISP,
                     max_dia_18ISP,max_dia_19ISP,max_dia_20ISP,
                     max_dia_21ISP,max_dia_22ISP,max_dia_23ISP,
                     max_dia_24ISP,max_dia_25ISP,max_dia_26ISP,
                     max_dia_27ISP,
                     max_dia_28ISP,max_dia_29ISP,max_dia_30ISP)
##View(max_diasISP15)
max_diasISP15 <- cbind(levels(DiasF),max_diasISP15)
max_diasISP15 <- as.data.frame(max_diasISP15)
##View(max_diasISP15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasISP15) <- nombres
max_diasISP15$Fecha <- as.Date(max_diasISP15$Fecha)
options(digits=5)
max_diasISP15$I1 <- as.numeric(max_diasISP15$I1)
max_diasISP15$I2 <- as.numeric(max_diasISP15$I2)
max_diasISP15$I3 <- as.numeric(max_diasISP15$I3)
##View(max_diasISP15)





##Septiembre 2016####
dfSP16 <- read.csv("Data/Years/2016/sept2016.csv")
#View(dfSP16)
dfSP16 <- dfSP16[1:2333,c(1:8)] 
dfSP16[3:8] <- sapply(dfSP16[3:8],as.numeric)  

dfSP16[3] <- dfSP16[3]/sqrt(3)
dfSP16[4] <- dfSP16[4]/sqrt(3)
dfSP16[5] <- dfSP16[5]/sqrt(3)

dfSP16[6] <- dfSP16[6]*2
dfSP16[7] <- dfSP16[7]*2
dfSP16[8] <- dfSP16[8]*2



dfSP16$Fecha.de.la.Medida <- as.Date(dfSP16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfSP16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfSP16, Fecha.de.la.Medida==Dias[i]))
}

names(dfSP16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaISP <- vector("numeric", ncol(dfSP16[6:8]))
for (i in seq_along(dfSP16[6:8])) {# 2. secuencia
  mediaISP[[i]] <- mean(ifelse(dfSP16[6:8][[i]]<=0,NA,dfSP16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxISP<- vector("numeric", ncol(dfSP16[6:8]))
for (i in seq_along(dfSP16[6:8])) {# 2. secuencia
  maxISP[[i]] <- max(dfSP16[6:8][[i]] )    # 3. cuerpo
}

##6-Septiembre a 30-Septiembre#
###Dia 6###
media_dia_6ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6ISP[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6ISP[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7ISP[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7ISP[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8ISP[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8ISP[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9ISP[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9ISP[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10ISP[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10ISP[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11ISP[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11ISP[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12ISP[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12ISP[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13ISP[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13ISP[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14ISP[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14ISP[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15ISP[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15ISP[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16ISP[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16ISP[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17ISP[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17ISP[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18ISP[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18ISP[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19ISP[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19ISP[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20ISP[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20ISP[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21ISP[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21ISP[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22ISP[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22ISP[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23ISP[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23ISP[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24ISP[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24ISP[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25ISP[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25ISP[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26ISP[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26ISP[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27ISP[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27ISP[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28ISP[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28ISP[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29ISP[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29ISP[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30ISP<-vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30ISP[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30ISP <- vector("numeric",  ncol(dfSP16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30ISP[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasISP16 <- rbind(media_dia_6ISP,media_dia_7ISP,
                        media_dia_8ISP,media_dia_9ISP,media_dia_10ISP,
                        media_dia_11ISP,media_dia_12ISP,media_dia_13ISP,
                        media_dia_14ISP,media_dia_15ISP,media_dia_16ISP,
                        media_dia_17ISP,
                        media_dia_18ISP,media_dia_19ISP,media_dia_20ISP,
                        media_dia_21ISP,media_dia_22ISP,media_dia_23ISP,
                        media_dia_24ISP,media_dia_25ISP,media_dia_26ISP,
                        media_dia_27ISP,
                        media_dia_28ISP,media_dia_29ISP,media_dia_30ISP)
##View(medias_diasI)
medias_diasISP16 <- cbind(levels(DiasF),medias_diasISP16)
medias_diasISP16 <- as.data.frame(medias_diasISP16)
##View(medias_diasISP16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasISP16) <- nombres
medias_diasISP16$Fecha <- as.Date(medias_diasISP16$Fecha)
options(digits=5)
medias_diasISP16$I1 <- as.numeric(medias_diasISP16$I1)
medias_diasISP16$I2 <- as.numeric(medias_diasISP16$I2)
medias_diasISP16$I3 <- as.numeric(medias_diasISP16$I3)
##View(medias_diasISP16)

max_diasISP16<- rbind(max_dia_6ISP,max_dia_7ISP,
                     max_dia_8ISP,max_dia_9ISP,max_dia_10ISP,
                     max_dia_11ISP,max_dia_12ISP,max_dia_13ISP,
                     max_dia_14ISP,max_dia_15ISP,max_dia_16ISP,
                     max_dia_17ISP,
                     max_dia_18ISP,max_dia_19ISP,max_dia_20ISP,
                     max_dia_21ISP,max_dia_22ISP,max_dia_23ISP,
                     max_dia_24ISP,max_dia_25ISP,max_dia_26ISP,
                     max_dia_27ISP,
                     max_dia_28ISP,max_dia_29ISP,max_dia_30ISP)
##View(max_diasISP)
max_diasISP16<- cbind(levels(DiasF),max_diasISP16)
max_diasISP16<- as.data.frame(max_diasISP16)
##View(max_diasISP)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasISP16) <- nombres
max_diasISP16$Fecha <- as.Date(max_diasISP16$Fecha)
options(digits=5)
max_diasISP16$I1 <- as.numeric(max_diasISP16$I1)
max_diasISP16$I2 <- as.numeric(max_diasISP16$I2)
max_diasISP16$I3 <- as.numeric(max_diasISP16$I3)
##View(max_diasISP)




##Septiembre 2018####
dfSP18 <- read.csv("Data/Years/2018/SEP2018.csv")
#View(dfSP18)
dfSP18 <- dfSP18[1:684,c(1:8)] 
dfSP18[3:8] <- sapply(dfSP18[3:8],as.numeric)  

dfSP18[3] <- dfSP18[3]/sqrt(3)
dfSP18[4] <- dfSP18[4]/sqrt(3)
dfSP18[5] <- dfSP18[5]/sqrt(3)

# dfSP18[6] <- dfSP18[6]*2
# dfSP18[7] <- dfSP18[7]*2
# dfSP18[8] <- dfSP18[8]*2



dfSP18$Fecha.de.la.Medida <- as.Date(dfSP18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfSP18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)

for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfSP18, Fecha.de.la.Medida==Dias[i]))
}

names(dfSP18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaISP <- vector("numeric", ncol(dfSP18[6:8]))
for (i in seq_along(dfSP18[6:8])) {# 2. secuencia
  mediaISP[[i]] <- mean(ifelse(dfSP18[6:8][[i]]<=0,NA,dfSP18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxISP<- vector("numeric", ncol(dfSP18[6:8]))
for (i in seq_along(dfSP18[6:8])) {# 2. secuencia
  maxISP[[i]] <- max(dfSP18[6:8][[i]] )    # 3. cuerpo
}

##21-Septiembre a 28-Septiembre#
###Dia 21###
media_dia_21ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21ISP[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21ISP[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22ISP[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22ISP[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23ISP[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23ISP[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24ISP[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24ISP[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25ISP[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25ISP[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26ISP[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26ISP[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27ISP[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27ISP[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28ISP<-vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28ISP[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28ISP <- vector("numeric",  ncol(dfSP18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28ISP[[i]] <- max(Dia_28[6:8][[i]]) 
}


medias_diasISP18 <- rbind(media_dia_21ISP,media_dia_22ISP,media_dia_23ISP,
                        media_dia_24ISP,media_dia_25ISP,media_dia_26ISP,
                        media_dia_27ISP,
                        media_dia_28ISP)
##View(medias_diasI)
medias_diasISP18 <- cbind(levels(DiasF),medias_diasISP18)
medias_diasISP18 <- as.data.frame(medias_diasISP18)
##View(medias_diasISP18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasISP18) <- nombres
medias_diasISP18$Fecha <- as.Date(medias_diasISP18$Fecha)
options(digits=5)
medias_diasISP18$I1 <- as.numeric(medias_diasISP18$I1)
medias_diasISP18$I2 <- as.numeric(medias_diasISP18$I2)
medias_diasISP18$I3 <- as.numeric(medias_diasISP18$I3)
##View(medias_diasISP18)

max_diasISP18 <- rbind(max_dia_21ISP,max_dia_22ISP,max_dia_23ISP,
                     max_dia_24ISP,max_dia_25ISP,max_dia_26ISP,
                     max_dia_27ISP,
                     max_dia_28ISP)
##View(max_diasISP18)
max_diasISP18 <- cbind(levels(DiasF),max_diasISP18)
max_diasISP18 <- as.data.frame(max_diasISP18)
##View(max_diasISP18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasISP18) <- nombres
max_diasISP18$Fecha <- as.Date(max_diasISP18$Fecha)
options(digits=5)
max_diasISP18$I1 <- as.numeric(max_diasISP18$I1)
max_diasISP18$I2 <- as.numeric(max_diasISP18$I2)
max_diasISP18$I3 <- as.numeric(max_diasISP18$I3)
##View(max_diasISP18)



##Septiembre 2022####
dfSP22 <- read.csv("Data/Years/2022/Septiembre.csv")

#View(dfSP22)
dfSP22 <- dfSP22[1:5121,1:8]


dfSP22$Fecha <- as.Date(dfSP22$Fecha,format="%d/%m/%Y")
DiasFSP <- as.factor(dfSP22$Fecha)
DiasSP<- levels(DiasFSP)
date <- as.Date(DiasSP)
dfSP22[3:8] <-sapply(dfSP22[3:8],as.numeric)

for (i in min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysSP <- gsub("$","SP",gsub("^","Dia_",as.character( seq(1:length(DiasSP)))))
  assign(DaysSP[i],filter(dfSP22, Fecha==DiasSP[i]))
}
names(dfSP22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

####I1-I3###
mediaISP <- vector("numeric", ncol(dfSP22[6:8]))


for (i in seq_along(dfSP22[6:8])) {# 2. secuencia
  mediaISP[[i]] <- mean(dfSP22[6:8][[i]])    # 3. cuerpo
}

maxISP <- vector("numeric", ncol(dfSP22[6:8]))

for (i in seq_along(dfSP22[6:8])) {# 2. secuencia
  maxISP[[i]] <- max(dfSP22[6:8][[i]])     # 3. cuerpo
}

##01-agosto a 10-agosto###
###Dia 1###
media_dia_1ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_1SP[6:8])) {
  media_dia_1ISP[[i]] <- mean(ifelse(Dia_1SP[6:8][[i]]<=0,NA,Dia_1SP[6:8][[i]]),
                              na.rm = T)
}

max_dia_1ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_1SP[6:8])) {
  max_dia_1ISP[[i]] <- max(Dia_1SP[6:8][[i]])
}



###Dia 2###
media_dia_2ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_2SP[6:8])) {
  media_dia_2ISP[[i]] <- mean(ifelse(Dia_2SP[6:8][[i]]<=0,NA,Dia_2SP[6:8][[i]]),
                              na.rm = T) 
}


max_dia_2ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_2SP[6:8])) {
  max_dia_2ISP[[i]] <- max(Dia_2SP[6:8][[i]])
}



###Dia 3###
media_dia_3ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_3SP[6:8])) {
  media_dia_3ISP[[i]] <- mean(ifelse(Dia_3SP[6:8][[i]]<=0,NA,Dia_3SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_3ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_3SP[6:8])) {
  max_dia_3ISP[[i]] <- max(Dia_3SP[6:8][[i]])
}



###Dia 4###
media_dia_4ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_4SP[6:8])) {
  media_dia_4ISP[[i]] <- mean(ifelse(Dia_4SP[6:8][[i]]<=0,NA,Dia_4SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_4ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_4SP[6:8])) {
  max_dia_4ISP[[i]] <- max(Dia_4SP[6:8][[i]])
}


###Dia 5###

media_dia_5ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_5SP[6:8])) {
  media_dia_5ISP[[i]] <- mean(ifelse(Dia_5SP[6:8][[i]]<=0,NA,Dia_5SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_5ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_5SP[6:8])) {
  max_dia_5ISP[[i]] <- max(Dia_5SP[6:8][[i]])
}



###Dia 6###

media_dia_6ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_6SP[6:8])) {
  media_dia_6ISP[[i]] <- mean(ifelse(Dia_6SP[6:8][[i]]<=0,NA,Dia_6SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_6ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_6SP[6:8])) {
  max_dia_6ISP[[i]] <- max(Dia_6SP[6:8][[i]])
}



###Dia 7###

media_dia_7ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_7SP[6:8])) {
  media_dia_7ISP[[i]] <- mean(ifelse(Dia_7SP[6:8][[i]]<=0,NA,Dia_7SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_7ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_7SP[6:8])) {
  max_dia_7ISP[[i]] <- max(Dia_7SP[6:8][[i]])
}



###Dia 8 ###

media_dia_8ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_8SP[6:8])) {
  media_dia_8ISP[[i]] <- mean(ifelse(Dia_8SP[6:8][[i]]<=0,NA,Dia_8SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_8ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_8SP[6:8])) {
  max_dia_8ISP[[i]] <- max(Dia_8SP[6:8][[i]])
}



###Dia 9 ###

media_dia_9ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_9SP[6:8])) {
  media_dia_9ISP[[i]] <- mean(ifelse(Dia_9SP[6:8][[i]]<=0,NA,Dia_9SP[6:8][[i]]),
                              na.rm = T) 
}

max_dia_9ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_9SP[6:8])) {
  max_dia_9ISP[[i]] <- max(Dia_9SP[6:8][[i]])
}



###Dia 10 ###
media_dia_10ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_10SP[6:8])) {
  media_dia_10ISP[[i]] <- mean(ifelse(Dia_10SP[6:8][[i]]<=0,NA,Dia_10SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_10ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_10SP[6:8])) {
  max_dia_10ISP[[i]] <- max(Dia_10SP[6:8][[i]])
}






##11-agosto a 20-agosto###
###Dia 11###
media_dia_11ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_11SP[6:8])) {
  media_dia_11ISP[[i]] <- mean(ifelse(Dia_11SP[6:8][[i]]<=0,NA,Dia_11SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_11ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_11SP[6:8])) {
  max_dia_11ISP[[i]] <- max(Dia_11SP[6:8][[i]])
}

###Dia 12###
media_dia_12ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_12SP[6:8])) {
  media_dia_12ISP[[i]] <- mean(ifelse(Dia_12SP[6:8][[i]]<=0,NA,Dia_12SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_12ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_12SP[6:8])) {
  max_dia_12ISP[[i]] <- max(Dia_12SP[6:8][[i]])
}

###Dia 13###
media_dia_13ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_13SP[6:8])) {
  media_dia_13ISP[[i]] <- mean(ifelse(Dia_13SP[6:8][[i]]<=0,NA,Dia_13SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_13ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_13SP[6:8])) {
  max_dia_13ISP[[i]] <- max(Dia_13SP[6:8][[i]])
}


###Dia 14###
media_dia_14ISP<-vector("numeric",  ncol(dfSP22[6:8]))


for (i in seq_along(Dia_14SP[6:8])) {
  media_dia_14ISP[[i]] <- mean(ifelse(Dia_14SP[6:8][[i]]<=0,NA,Dia_14SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_14ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_14SP[6:8])) {
  max_dia_14ISP[[i]] <- max(Dia_14SP[6:8][[i]])
}

###Dia 15###
media_dia_15ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_15SP[6:8])) {
  media_dia_15ISP[[i]] <- mean(ifelse(Dia_15SP[6:8][[i]]<=0,NA,Dia_15SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_15ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_15SP[6:8])) {
  max_dia_15ISP[[i]] <- max(Dia_15SP[6:8][[i]])
}

###Dia 16###
media_dia_16ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_16SP[6:8])) {
  media_dia_16ISP[[i]] <- mean(ifelse(Dia_16SP[6:8][[i]]<=0,NA,Dia_16SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_16ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_16SP[6:8])) {
  max_dia_16ISP[[i]] <- max(Dia_16SP[6:8][[i]])
}

###Dia 17###
media_dia_17ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_17SP[6:8])) {
  media_dia_17ISP[[i]] <- mean(ifelse(Dia_17SP[6:8][[i]]<=0,NA,Dia_17SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_17ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_17SP[6:8])) {
  max_dia_17ISP[[i]] <- max(Dia_17SP[6:8][[i]])
}

###Dia 18 ###
media_dia_18ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_18SP[6:8])) {
  media_dia_18ISP[[i]] <- mean(ifelse(Dia_18SP[6:8][[i]]<=0,NA,Dia_18SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_18ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_18SP[6:8])) {
  max_dia_18ISP[[i]] <- max(Dia_18SP[6:8][[i]])
}

###Dia 19 ###
media_dia_19ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_19SP[6:8])) {
  media_dia_19ISP[[i]] <- mean(ifelse(Dia_19SP[6:8][[i]]<=0,NA,Dia_19SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_19ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_19SP[6:8])) {
  max_dia_19ISP[[i]] <- max(Dia_19SP[6:8][[i]])
}

###Dia 20 ###
media_dia_20ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_20SP[6:8])) {
  media_dia_20ISP[[i]] <- mean(ifelse(Dia_20SP[6:8][[i]]<=0,NA,Dia_20SP[6:8][[i]]),
                               na.rm = T) 
}

max_dia_20ISP<-vector("numeric",  ncol(dfSP22[6:8]))

for (i in seq_along(Dia_20SP[6:8])) {
  max_dia_20ISP[[i]] <- max(Dia_20SP[6:8][[i]])
}


medias_diasISP22 <- rbind(media_dia_1ISP,media_dia_2ISP,media_dia_3ISP,media_dia_4ISP,
                          media_dia_5ISP,media_dia_6ISP,media_dia_7ISP,media_dia_8ISP,
                          media_dia_9ISP,media_dia_10ISP,media_dia_11ISP,media_dia_12ISP,
                          media_dia_13ISP,media_dia_14ISP,media_dia_15ISP,media_dia_16ISP,
                          media_dia_17ISP,media_dia_18ISP,media_dia_19ISP,media_dia_20ISP)


#View(medias_diasISP22)


medias_diasISP22 <- cbind(levels(DiasFSP),medias_diasISP22)
medias_diasISP22 <- as.data.frame(medias_diasISP22)
#View(medias_diasISP22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasISP22) <- nombres
medias_diasISP22$Fecha <- as.Date(medias_diasISP22$Fecha)
options(digits=5)
medias_diasISP22$I1 <- as.numeric(medias_diasISP22$I1)
medias_diasISP22$I2 <- as.numeric(medias_diasISP22$I2)
medias_diasISP22$I3 <- as.numeric(medias_diasISP22$I3)
#View(medias_diasISP22)

#View(Jun_AgosI)
max_diasISP22 <- rbind(max_dia_1ISP,max_dia_2ISP,max_dia_3ISP,max_dia_4ISP,
                       max_dia_5ISP,max_dia_6ISP,max_dia_7ISP,max_dia_8ISP,
                       max_dia_9ISP,max_dia_10ISP,max_dia_11ISP,max_dia_12ISP,
                       max_dia_13ISP,max_dia_14ISP,max_dia_15ISP,max_dia_16ISP,
                       max_dia_17ISP,max_dia_18ISP,max_dia_19ISP,max_dia_20ISP)


#View(max_diasISP22)

max_diasISP22 <- cbind(levels(DiasFSP),max_diasISP22)
max_diasISP22 <- as.data.frame(max_diasISP22)
#View(max_diasISP22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasISP22) <- nombres
max_diasISP22$Fecha <- as.Date(max_diasISP22$Fecha)
options(digits=5)
max_diasISP22$I1 <- as.numeric(max_diasISP22$I1)
max_diasISP22$I2 <- as.numeric(max_diasISP22$I2)
max_diasISP22$I3 <- as.numeric(max_diasISP22$I3)
#View(max_diasISP22)



#####
Septiembre <- rbind(medias_diasISP13,medias_diasISP14,medias_diasISP15,
                medias_diasISP16,medias_diasISP18,medias_diasISP22)
SeptiembreM <- rbind(max_diasISP13,max_diasISP14,max_diasISP15,
                    max_diasISP16,max_diasISP18,max_diasISP22)
dfSP <- rbind(dfSP13,dfSP14,dfSP15,dfSP16,dfSP18,dfSP22)




##Octubre 2014####
dfOC14 <- read.csv("Data/Years/2014/OCT014.csv")
#View(dfOC14)
dfOC14 <- dfOC14[1:4066,c(1:8)] 
dfOC14[3:8] <- sapply(dfOC14[3:8],as.numeric)  

dfOC14[3] <- dfOC14[3]/sqrt(3)
dfOC14[4] <- dfOC14[4]/sqrt(3)
dfOC14[5] <- dfOC14[5]/sqrt(3)

# dfOC14[6] <- dfOC14[6]*2
# dfOC14[7] <- dfOC14[7]*2
# dfOC14[7] <- dfOC14[7]*2



dfOC14$Feha.de.la.Medda <- as.Date(dfOC14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfOC14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)


for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfOC14, Feha.de.la.Medda==Dias[i]))
}

names(dfOC14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIOC <- vector("numeric", ncol(dfOC14[6:8]))
for (i in seq_along(dfOC14[6:8])) {# 2. secuencia
  mediaIOC[[i]] <- mean(ifelse(dfOC14[6:8][[i]]<=0,NA,dfOC14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIOC<- vector("numeric", ncol(dfOC14[6:8]))
for (i in seq_along(dfOC14[6:8])) {# 2. secuencia
  maxIOC[[i]] <- max(dfOC14[6:8][[i]] )    # 3. cuerpo
}

##17-octubre a 31-octubre#
###Dia 17###
media_dia_17IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IOC[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IOC[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IOC[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IOC[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IOC[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IOC[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IOC[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IOC[[i]] <- max(Dia_20[6:8][[i]]) 
}
###Dia 21###
media_dia_21IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IOC[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T)
}

max_dia_21IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IOC[[i]] <- max(Dia_21[6:8][[i]]) 
}
###Dia 22###
media_dia_22IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IOC[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IOC[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IOC[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IOC[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IOC[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IOC[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IOC[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IOC[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IOC[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IOC[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IOC[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IOC[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IOC[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IOC[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IOC[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IOC[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IOC[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IOC[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IOC<-vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IOC[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IOC <- vector("numeric",  ncol(dfOC14[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IOC[[i]] <- max(Dia_31[6:8][[i]]) 
}


medias_diasIOC14 <- rbind(media_dia_17IOC,media_dia_18IOC,media_dia_19IOC,
                        media_dia_20IOC,media_dia_21IOC,media_dia_22IOC,
                        media_dia_23IOC,media_dia_24IOC,media_dia_25IOC,
                        media_dia_26IOC,media_dia_27IOC,media_dia_28IOC,
                        media_dia_29IOC,media_dia_30IOC,media_dia_31IOC)
##View(medias_diasI)
medias_diasIOC14 <- cbind(levels(DiasF),medias_diasIOC14)
medias_diasIOC14 <- as.data.frame(medias_diasIOC14)
##View(medias_diasIOC14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIOC14) <- nombres
medias_diasIOC14$Fecha <- as.Date(medias_diasIOC14$Fecha)
options(digits=5)
medias_diasIOC14$I1 <- as.numeric(medias_diasIOC14$I1)
medias_diasIOC14$I2 <- as.numeric(medias_diasIOC14$I2)
medias_diasIOC14$I3 <- as.numeric(medias_diasIOC14$I3)
##View(medias_diasIOC14)

max_diasIOC14 <- rbind(max_dia_17IOC,max_dia_18IOC,max_dia_19IOC,
                     max_dia_20IOC,max_dia_21IOC,max_dia_22IOC,
                     max_dia_23IOC,max_dia_24IOC,max_dia_25IOC,
                     max_dia_26IOC,max_dia_27IOC,max_dia_28IOC,
                     max_dia_29IOC,max_dia_30IOC,max_dia_31IOC)
##View(max_diasIOC14)
max_diasIOC14 <- cbind(levels(DiasF),max_diasIOC14)
max_diasIOC14 <- as.data.frame(max_diasIOC14)
##View(max_diasIOC14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIOC14) <- nombres
max_diasIOC14$Fecha <- as.Date(max_diasIOC14$Fecha)
options(digits=5)
max_diasIOC14$I1 <- as.numeric(max_diasIOC14$I1)
max_diasIOC14$I2 <- as.numeric(max_diasIOC14$I2)
max_diasIOC14$I3 <- as.numeric(max_diasIOC14$I3)
##View(max_diasIOC14)






##Octubre 2015####
dfOC15 <- read.csv("Data/Years/2015/OCTUBRE2015.csv")
#View(dfOC15)
dfOC15 <- dfOC15[1:4255,c(1:8)] 
dfOC15[3:8] <- sapply(dfOC15[3:8],as.numeric)  

dfOC15[3] <- dfOC15[3]/sqrt(3)
dfOC15[4] <- dfOC15[4]/sqrt(3)
dfOC15[5] <- dfOC15[5]/sqrt(3)

# dfOC15[6] <- dfOC15[6]*2
# dfOC15[7] <- dfOC15[7]*2
# dfOC15[7] <- dfOC15[7]*2



dfOC15$Feha.de.la.Medda <- as.Date(dfOC15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfOC15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfOC15, Feha.de.la.Medda==Dias[i]))
}

names(dfOC15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIOC <- vector("numeric", ncol(dfOC15[6:8]))
for (i in seq_along(dfOC15[6:8])) {# 2. secuencia
  mediaIOC[[i]] <- mean(ifelse(dfOC15[6:8][[i]]<=0,NA,dfOC15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIOC<- vector("numeric", ncol(dfOC15[6:8]))
for (i in seq_along(dfOC15[6:8])) {# 2. secuencia
  maxIOC[[i]] <- max(dfOC15[6:8][[i]] )    # 3. cuerpo
}

##2-Octubre a 31-Octubre#
###Dia 2###
media_dia_2IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IOC [[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                               na.rm=T) 
}

max_dia_2IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IOC [[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IOC [[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                               na.rm=T) 
}

max_dia_3IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IOC [[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IOC [[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                               na.rm=T) 
}

max_dia_4IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IOC [[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IOC [[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                               na.rm=T) 
}

max_dia_5IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IOC [[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IOC [[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                               na.rm=T)
}

max_dia_6IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IOC [[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IOC [[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                               na.rm=T) 
}

max_dia_7IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IOC [[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IOC [[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                               na.rm=T) 
}

max_dia_8IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IOC [[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IOC [[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                               na.rm=T) 
}

max_dia_9IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IOC [[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IOC [[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                                na.rm=T) 
}

max_dia_10IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IOC [[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IOC [[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                                na.rm=T) 
}

max_dia_11IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IOC [[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IOC [[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                                na.rm=T) 
}

max_dia_12IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IOC [[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IOC [[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                                na.rm=T) 
}

max_dia_13IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IOC [[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IOC [[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                                na.rm=T) 
}

max_dia_14IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IOC [[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IOC [[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                                na.rm=T) 
}

max_dia_15IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IOC [[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IOC [[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                                na.rm=T) 
}

max_dia_16IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IOC [[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IOC [[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                                na.rm=T) 
}

max_dia_17IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IOC [[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IOC [[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                                na.rm=T) 
}

max_dia_18IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IOC [[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IOC [[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                                na.rm=T) 
}

max_dia_19IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IOC [[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IOC [[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                                na.rm=T) 
}

max_dia_20IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IOC [[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IOC [[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                                na.rm=T) 
}

max_dia_21IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IOC [[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IOC [[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                                na.rm=T) 
}

max_dia_22IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IOC [[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IOC [[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                                na.rm=T) 
}

max_dia_23IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IOC [[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IOC [[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                                na.rm=T) 
}

max_dia_24IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IOC [[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IOC [[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                                na.rm=T) 
}

max_dia_25IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IOC [[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IOC [[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                                na.rm=T) 
}

max_dia_26IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IOC [[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IOC [[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                                na.rm=T) 
}

max_dia_27IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IOC [[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IOC [[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                                na.rm=T) 
}

max_dia_28IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IOC [[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IOC [[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                                na.rm=T) 
}

max_dia_29IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IOC [[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IOC [[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                                na.rm=T) 
}

max_dia_30IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IOC [[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IOC <-vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IOC [[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                                na.rm=T) 
}

max_dia_31IOC  <- vector("numeric",  ncol(dfOC15 [6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IOC [[i]] <- max(Dia_31[6:8][[i]]) 
}


medias_diasIOC15  <- rbind(media_dia_2IOC ,media_dia_3IOC ,media_dia_4IOC ,
                         media_dia_5IOC ,media_dia_6IOC ,media_dia_7IOC ,
                         media_dia_8IOC ,media_dia_9IOC ,media_dia_10IOC ,
                         media_dia_11IOC ,media_dia_12IOC ,media_dia_13IOC ,
                         media_dia_14IOC ,media_dia_15IOC ,media_dia_16IOC ,
                         media_dia_17IOC ,
                         media_dia_18IOC ,media_dia_19IOC ,media_dia_20IOC ,
                         media_dia_21IOC ,media_dia_22IOC ,media_dia_23IOC ,
                         media_dia_24IOC ,media_dia_25IOC ,media_dia_26IOC ,
                         media_dia_27IOC ,
                         media_dia_28IOC ,media_dia_29IOC ,media_dia_30IOC,
                         media_dia_31IOC)
##View(medias_diasI)
medias_diasIOC15  <- cbind(levels(DiasF),medias_diasIOC15 )
medias_diasIOC15  <- as.data.frame(medias_diasIOC15 )
##View(medias_diasIOC15 )
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIOC15 ) <- nombres
medias_diasIOC15 $Fecha <- as.Date(medias_diasIOC15 $Fecha)
options(digits=5)
medias_diasIOC15 $I1 <- as.numeric(medias_diasIOC15 $I1)
medias_diasIOC15 $I2 <- as.numeric(medias_diasIOC15 $I2)
medias_diasIOC15 $I3 <- as.numeric(medias_diasIOC15 $I3)
##View(medias_diasIOC15 )

max_diasIOC15  <- rbind(max_dia_2IOC ,max_dia_3IOC ,max_dia_4IOC ,
                      max_dia_5IOC ,max_dia_6IOC ,max_dia_7IOC ,
                      max_dia_8IOC ,max_dia_9IOC ,max_dia_10IOC ,
                      max_dia_11IOC ,max_dia_12IOC ,max_dia_13IOC ,
                      max_dia_14IOC ,max_dia_15IOC ,max_dia_16IOC ,
                      max_dia_17IOC ,
                      max_dia_18IOC ,max_dia_19IOC ,max_dia_20IOC ,
                      max_dia_21IOC ,max_dia_22IOC ,max_dia_23IOC ,
                      max_dia_24IOC ,max_dia_25IOC ,max_dia_26IOC ,
                      max_dia_27IOC ,
                      max_dia_28IOC ,max_dia_29IOC ,max_dia_30IOC,
                      max_dia_31IOC)
##View(max_diasIOC15 )
max_diasIOC15  <- cbind(levels(DiasF),max_diasIOC15 )
max_diasIOC15  <- as.data.frame(max_diasIOC15 )
##View(max_diasIOC15 )
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIOC15 ) <- nombres
max_diasIOC15 $Fecha <- as.Date(max_diasIOC15 $Fecha)
options(digits=5)
max_diasIOC15 $I1 <- as.numeric(max_diasIOC15 $I1)
max_diasIOC15 $I2 <- as.numeric(max_diasIOC15 $I2)
max_diasIOC15 $I3 <- as.numeric(max_diasIOC15 $I3)
##View(max_diasIOC15 )



##Octubre 2016####
dfOC16 <- read.csv("Data/Years/2016/oct2016.csv")
#View(dfOC16)
dfOC16 <- dfOC16[1:2065,c(1:8)] 
dfOC16[3:8] <- sapply(dfOC16[3:8],as.numeric)  

dfOC16[3] <- dfOC16[3]/sqrt(3)
dfOC16[4] <- dfOC16[4]/sqrt(3)
dfOC16[5] <- dfOC16[5]/sqrt(3)

dfOC16[6] <- dfOC16[6]*2
dfOC16[7] <- dfOC16[7]*2
dfOC16[8] <- dfOC16[8]*2



dfOC16$Fecha.de.la.Medida <- as.Date(dfOC16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfOC16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfOC16, Fecha.de.la.Medida==Dias[i]))
}

names(dfOC16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIOC <- vector("numeric", ncol(dfOC16[6:8]))
for (i in seq_along(dfOC16[6:8])) {# 2. secuencia
  mediaIOC[[i]] <- mean(ifelse(dfOC16[6:8][[i]]<=0,NA,dfOC16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIOC<- vector("numeric", ncol(dfOC16[6:8]))
for (i in seq_along(dfOC16[6:8])) {# 2. secuencia
  maxIOC[[i]] <- max(dfOC16[6:8][[i]] )    # 3. cuerpo
}

##10-Octubre a 31-Octubre#
###Dia 10###
media_dia_10IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IOC[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IOC[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IOC[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IOC[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IOC[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IOC[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IOC[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IOC[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IOC[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IOC[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IOC[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IOC[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IOC[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IOC[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17IOC[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17IOC[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18IOC[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18IOC[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19IOC[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19IOC[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20IOC[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20IOC[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21IOC[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21IOC[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22IOC[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22IOC[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23IOC[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23IOC[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IOC[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IOC[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IOC[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IOC[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IOC[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IOC[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IOC[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IOC[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IOC[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IOC[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IOC[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IOC[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IOC[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IOC[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IOC<-vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IOC[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IOC <- vector("numeric",  ncol(dfOC16[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IOC[[i]] <- max(Dia_31[6:8][[i]]) 
}



medias_diasIOC16 <- rbind(media_dia_10IOC,
                        media_dia_11IOC,media_dia_12IOC,media_dia_13IOC,
                        media_dia_14IOC,media_dia_15IOC,media_dia_16IOC,
                        media_dia_17IOC,
                        media_dia_18IOC,media_dia_19IOC,media_dia_20IOC,
                        media_dia_21IOC,media_dia_22IOC,media_dia_23IOC,
                        media_dia_24IOC,media_dia_25IOC,media_dia_26IOC,
                        media_dia_27IOC,
                        media_dia_28IOC,media_dia_29IOC,media_dia_30IOC,
                        media_dia_31IOC)
##View(medias_diasI)
medias_diasIOC16 <- cbind(levels(DiasF),medias_diasIOC16)
medias_diasIOC16 <- as.data.frame(medias_diasIOC16)
##View(medias_diasIOC16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIOC16) <- nombres
medias_diasIOC16$Fecha <- as.Date(medias_diasIOC16$Fecha)
options(digits=5)
medias_diasIOC16$I1 <- as.numeric(medias_diasIOC16$I1)
medias_diasIOC16$I2 <- as.numeric(medias_diasIOC16$I2)
medias_diasIOC16$I3 <- as.numeric(medias_diasIOC16$I3)
##View(medias_diasIOC16)

max_diasIOC16 <- rbind(max_dia_10IOC,
                     max_dia_11IOC,max_dia_12IOC,max_dia_13IOC,
                     max_dia_14IOC,max_dia_15IOC,max_dia_16IOC,
                     max_dia_17IOC,
                     max_dia_18IOC,max_dia_19IOC,max_dia_20IOC,
                     max_dia_21IOC,max_dia_22IOC,max_dia_23IOC,
                     max_dia_24IOC,max_dia_25IOC,max_dia_26IOC,
                     max_dia_27IOC,
                     max_dia_28IOC,max_dia_29IOC,max_dia_30IOC,
                     max_dia_31IOC)
##View(max_diasIOC16)
max_diasIOC16 <- cbind(levels(DiasF),max_diasIOC16)
max_diasIOC16 <- as.data.frame(max_diasIOC16)
##View(max_diasIOC16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIOC16) <- nombres
max_diasIOC16$Fecha <- as.Date(max_diasIOC16$Fecha)
options(digits=5)
max_diasIOC16$I1 <- as.numeric(max_diasIOC16$I1)
max_diasIOC16$I2 <- as.numeric(max_diasIOC16$I2)
max_diasIOC16$I3 <- as.numeric(max_diasIOC16$I3)
##View(max_diasIOC16)




##Octubre 2017####
dfOC17 <- read.csv("Data/Years/2017/OCT2017.csv")
#View(dfOC17)
dfOC17 <- dfOC17[1:1488,c(1:8)] 
dfOC17[3:8] <- sapply(dfOC17[3:8],as.numeric)  

dfOC17[3] <- dfOC17[3]/sqrt(3)
dfOC17[4] <- dfOC17[4]/sqrt(3)
dfOC17[5] <- dfOC17[5]/sqrt(3)

# dfOC17[6] <- dfOC17[6]*2
# dfOC17[7] <- dfOC17[7]*2
# dfOC17[8] <- dfOC17[8]*2



dfOC17$Fecha.de.la.Medida <- as.Date(dfOC17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfOC17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfOC17, Fecha.de.la.Medida==Dias[i]))
}

names(dfOC17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIOC <- vector("numeric", ncol(dfOC17[6:8]))
for (i in seq_along(dfOC17[6:8])) {# 2. secuencia
  mediaIOC[[i]] <- mean(ifelse(dfOC17[6:8][[i]]<=0,NA,dfOC17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIOC<- vector("numeric", ncol(dfOC17[6:8]))
for (i in seq_along(dfOC17[6:8])) {# 2. secuencia
  maxIOC[[i]] <- max(dfOC17[6:8][[i]] )    # 3. cuerpo
}

##25-Octubre a 30-Octubre#
###Dia 25###
media_dia_25IOC<-vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IOC[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IOC <- vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IOC[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IOC<-vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IOC[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IOC <- vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IOC[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IOC<-vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IOC[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IOC <- vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IOC[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IOC<-vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IOC[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IOC <- vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IOC[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IOC<-vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IOC[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IOC <- vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IOC[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IOC<-vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IOC[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IOC <- vector("numeric",  ncol(dfOC17[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IOC[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasIOC17 <- rbind(media_dia_25IOC,media_dia_26IOC,
                        media_dia_27IOC,
                        media_dia_28IOC,media_dia_29IOC,media_dia_30IOC)
##View(medias_diasI)
medias_diasIOC17 <- cbind(levels(DiasF),medias_diasIOC17)
medias_diasIOC17 <- as.data.frame(medias_diasIOC17)
##View(medias_diasIOC17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIOC17) <- nombres
medias_diasIOC17$Fecha <- as.Date(medias_diasIOC17$Fecha)
options(digits=5)
medias_diasIOC17$I1 <- as.numeric(medias_diasIOC17$I1)
medias_diasIOC17$I2 <- as.numeric(medias_diasIOC17$I2)
medias_diasIOC17$I3 <- as.numeric(medias_diasIOC17$I3)
##View(medias_diasIOC17)

max_diasIOC17 <- rbind(max_dia_25IOC,max_dia_26IOC,
                     max_dia_27IOC,
                     max_dia_28IOC,max_dia_29IOC,max_dia_30IOC)
##View(max_diasIOC17)
max_diasIOC17 <- cbind(levels(DiasF),max_diasIOC17)
max_diasIOC17 <- as.data.frame(max_diasIOC17)
##View(max_diasIOC17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIOC17) <- nombres
max_diasIOC17$Fecha <- as.Date(max_diasIOC17$Fecha)
options(digits=5)
max_diasIOC17$I1 <- as.numeric(max_diasIOC17$I1)
max_diasIOC17$I2 <- as.numeric(max_diasIOC17$I2)
max_diasIOC17$I3 <- as.numeric(max_diasIOC17$I3)
##View(max_diasIOC17)




##Octubre 2018####
dfOC18 <- read.csv("Data/Years/2018/OCT2018.csv")
#View(dfOC18)
dfOC18 <- dfOC18[1:653,c(1:8)] 
dfOC18[3:8] <- sapply(dfOC18[3:8],as.numeric)  

dfOC18[3] <- dfOC18[3]/sqrt(3)
dfOC18[4] <- dfOC18[4]/sqrt(3)
dfOC18[5] <- dfOC18[5]/sqrt(3)
# 
# dfOC18[6] <- dfOC18[6]*2
# dfOC18[7] <- dfOC18[7]*2
# dfOC18[8] <- dfOC18[8]*2



dfOC18$Fecha.de.la.Medida <- as.Date(dfOC18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfOC18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfOC18, Fecha.de.la.Medida==Dias[i]))
}

names(dfOC18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIOC <- vector("numeric", ncol(dfOC18[6:8]))
for (i in seq_along(dfOC18[6:8])) {# 2. secuencia
  mediaIOC[[i]] <- mean(ifelse(dfOC18[6:8][[i]]<=0,NA,dfOC18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIOC<- vector("numeric", ncol(dfOC18[6:8]))
for (i in seq_along(dfOC18[6:8])) {# 2. secuencia
  maxIOC[[i]] <- max(dfOC18[6:8][[i]] )    # 3. cuerpo
}

##24-Octubre a 31-Octubre#
###Dia 24###
media_dia_24IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24IOC[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24IOC[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25IOC[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25IOC[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26IOC[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26IOC[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27IOC[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27IOC[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28IOC[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28IOC[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29IOC[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29IOC[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30IOC[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30IOC[[i]] <- max(Dia_30[6:8][[i]]) 
}

###Dia 31###
media_dia_31IOC<-vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  media_dia_31IOC[[i]] <- mean(ifelse(Dia_31[6:8][[i]]<=0,NA,Dia_31[6:8][[i]]),
                               na.rm=T) 
}

max_dia_31IOC <- vector("numeric",  ncol(dfOC18[6:8]))
for (i in seq_along(Dia_31[6:8])) {
  max_dia_31IOC[[i]] <- max(Dia_31[6:8][[i]]) 
}



medias_diasIOC18 <- rbind(media_dia_24IOC,media_dia_25IOC,media_dia_26IOC,
                        media_dia_27IOC,
                        media_dia_28IOC,media_dia_29IOC,media_dia_30IOC,
                        media_dia_31IOC)
##View(medias_diasI)
medias_diasIOC18 <- cbind(levels(DiasF),medias_diasIOC18)
medias_diasIOC18 <- as.data.frame(medias_diasIOC18)
##View(medias_diasIOC18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIOC18) <- nombres
medias_diasIOC18$Fecha <- as.Date(medias_diasIOC18$Fecha)
options(digits=5)
medias_diasIOC18$I1 <- as.numeric(medias_diasIOC18$I1)
medias_diasIOC18$I2 <- as.numeric(medias_diasIOC18$I2)
medias_diasIOC18$I3 <- as.numeric(medias_diasIOC18$I3)
##View(medias_diasIOC18)

max_diasIOC18 <- rbind(max_dia_24IOC,max_dia_25IOC,max_dia_26IOC,
                     max_dia_27IOC,
                     max_dia_28IOC,max_dia_29IOC,max_dia_30IOC,
                     max_dia_31IOC)
##View(max_diasIOC18)
max_diasIOC18 <- cbind(levels(DiasF),max_diasIOC18)
max_diasIOC18 <- as.data.frame(max_diasIOC18)
##View(max_diasIOC18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIOC18) <- nombres
max_diasIOC18$Fecha <- as.Date(max_diasIOC18$Fecha)
options(digits=5)
max_diasIOC18$I1 <- as.numeric(max_diasIOC18$I1)
max_diasIOC18$I2 <- as.numeric(max_diasIOC18$I2)
max_diasIOC18$I3 <- as.numeric(max_diasIOC18$I3)
##View(max_diasIOC18)




##Octubre 2022####
dfOC22 <- read.csv("Data/Years/2022/Octubre.csv")
#View(dfOC22)
#rev(unique(dfOC22$Fecha))
dfOC22 <- dfOC22[1:780,1:8]

dfOC22$Fecha <- as.Date(dfOC22$Fecha,format="%d/%m/%Y")
DiasFOC <- as.factor(dfOC22$Fecha)
DiasOC<- levels(DiasFOC)
date <- as.Date(DiasOC)
dfOC22[3:8] <-sapply(dfOC22[3:8],as.numeric)

for (i in min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysOC <- gsub("$","OC",gsub("^","Dia_",as.character( seq(1:length(DiasOC)))))
  assign(DaysOC[i],filter(dfOC22, Fecha==DiasOC[i]))
}
names(dfOC22) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

####I1-I3###
mediaIOC <- vector("numeric", ncol(dfOC22[6:8]))


for (i in seq_along(dfOC22[6:8])) {# 2. secuencia
  mediaIOC[[i]] <- mean(dfOC22[6:8][[i]])    # 3. cuerpo
}

mediaIOC

maxIOC <- vector("numeric", ncol(dfOC22[6:8]))

for (i in seq_along(dfOC22[6:8])) {# 2. secuencia
  maxIOC[[i]] <- max(dfOC22[6:8][[i]])     # 3. cuerpo
}

maxIOC



##01- a -agosto###
###Dia 1###
media_dia_1IOC<-vector("numeric",  ncol(dfOC22[6:8]))


for (i in seq_along(Dia_1OC[6:8])) {
  media_dia_1IOC[[i]] <- mean(ifelse(Dia_1OC[6:8][[i]]<=0,NA,Dia_1OC[6:8][[i]]),
                              na.rm = T)
}

max_dia_1IOC<-vector("numeric",  ncol(dfOC22[6:8]))


for (i in seq_along(Dia_1OC[6:8])) {
  max_dia_1IOC[[i]] <- max(Dia_1OC[6:8][[i]])
}



###Dia 2###
media_dia_2IOC<-vector("numeric",  ncol(dfOC22[6:8]))


for (i in seq_along(Dia_2OC[6:8])) {
  media_dia_2IOC[[i]] <- mean(ifelse(Dia_2OC[6:8][[i]]<=0,NA,Dia_2OC[6:8][[i]]),
                              na.rm = T) 
}


max_dia_2IOC<-vector("numeric",  ncol(dfOC22[6:8]))


for (i in seq_along(Dia_2OC[6:8])) {
  max_dia_2IOC[[i]] <- max(Dia_2OC[6:8][[i]])
}



###Dia 3###
media_dia_3IOC<-vector("numeric",  ncol(dfOC22[6:8]))

for (i in seq_along(Dia_3OC[6:8])) {
  media_dia_3IOC[[i]] <- mean(ifelse(Dia_3OC[6:8][[i]]<=0,NA,Dia_3OC[6:8][[i]]),
                              na.rm = T) 
}

max_dia_3IOC<-vector("numeric",  ncol(dfOC22[6:8]))


for (i in seq_along(Dia_3OC[6:8])) {
  max_dia_3IOC[[i]] <- max(Dia_3OC[6:8][[i]])
}




medias_diasIOC22 <- rbind(media_dia_1IOC,media_dia_2IOC,media_dia_3IOC)


#View(medias_diasIOC22)


medias_diasIOC22 <- cbind(levels(DiasFOC),medias_diasIOC22)
medias_diasIOC22 <- as.data.frame(medias_diasIOC22)
#View(medias_diasIOC22)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIOC22) <- nombres
medias_diasIOC22$Fecha <- as.Date(medias_diasIOC22$Fecha)
options(digits=5)
medias_diasIOC22$I1 <- as.numeric(medias_diasIOC22$I1)
medias_diasIOC22$I2 <- as.numeric(medias_diasIOC22$I2)
medias_diasIOC22$I3 <- as.numeric(medias_diasIOC22$I3)
#View(medias_diasIOC22)

#View(Jun_AgosI)
max_diasIOC22 <- rbind(max_dia_1IOC,max_dia_2IOC,max_dia_3IOC)


#View(max_diasIOC22)

max_diasIOC22 <- cbind(levels(DiasFOC),max_diasIOC22)
max_diasIOC22 <- as.data.frame(max_diasIOC22)
#View(max_diasIOC22)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIOC22) <- nombres
max_diasIOC22$Fecha <- as.Date(max_diasIOC22$Fecha)
options(digits=5)
max_diasIOC22$I1 <- as.numeric(max_diasIOC22$I1)
max_diasIOC22$I2 <- as.numeric(max_diasIOC22$I2)
max_diasIOC22$I3 <- as.numeric(max_diasIOC22$I3)
#View(max_diasIOC22)



#####
Octubre <- rbind(medias_diasIOC14,medias_diasIOC15,
                medias_diasIOC16, medias_diasIOC17,
                medias_diasIOC18,medias_diasIOC22)
OctubreM <- rbind(max_diasIOC14,max_diasIOC15,
                 max_diasIOC16, max_diasIOC17,
                 max_diasIOC18, medias_diasIOC22)
dfOC <- rbind(dfOC14,dfOC15,dfOC16,dfOC17,dfOC18,dfOC22)



##Noviembre 2013 (Aunque estaba en Octubre)####
dfNV13 <- read.csv("Data/Years/2013/OCT13.csv")
#View(dfNV13)
dfNV13 <- dfNV13[1:2591,1:8]
dfNV13[3:8] <- sapply(dfNV13[3:8],as.numeric)  

# dfNV13[3] <- dfNV13[3]/sqrt(3)
# dfNV13[4] <- dfNV13[4]/sqrt(3)
# dfNV13[5] <- dfNV13[5]/sqrt(3)
# 
# dfNV13[6] <- dfNV13[6]*2
# dfNV13[7] <- dfNV13[7]*2
# dfNV13[8] <- dfNV13[7]*2


dfNV13$Fecha.de.la.Medida <- as.Date(dfNV13$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfNV13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfNV13, Fecha.de.la.Medida==Dias[i]))
}

names(dfNV13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaINV <- vector("numeric", ncol(dfNV13[6:8]))
for (i in seq_along(dfNV13[6:8])) {# 2. secuencia
  mediaINV[[i]] <- mean(ifelse(dfNV13[6:8][[i]]<=0,NA,dfNV13[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxINV<- vector("numeric", ncol(dfNV13[6:8]))
for (i in seq_along(dfNV13[6:8])) {# 2. secuencia
  maxINV[[i]] <- max(dfNV13[6:8][[i]] )    # 3. cuerpo
}

##7-noviembre a 16-noviembre#
###Dia 7###
media_dia_7INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7INV[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7INV[[i]] <- max(Dia_7[6:8][[i]]) 
}
###Dia 8###
media_dia_8INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8INV[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8INV[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9INV[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9INV[[i]] <- max(Dia_9[6:8][[i]]) 
}
###Dia 10###
media_dia_10INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10INV[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10INV[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11 ###
media_dia_11INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11INV[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11INV[[i]] <- max(Dia_11[6:8][[i]]) 
}
###Dia 12###
media_dia_12INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12INV[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12INV[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13INV[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13INV[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14INV[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14INV[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15INV[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15INV[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16INV<-vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16INV[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16INV <- vector("numeric",  ncol(dfNV13[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16INV[[i]] <- max(Dia_16[6:8][[i]]) 
}




medias_diasINO13 <- rbind(media_dia_7INV,media_dia_8INV,media_dia_9INV,
                        media_dia_10INV,media_dia_11INV,media_dia_12INV,
                        media_dia_13INV,media_dia_14INV,media_dia_15INV,
                        media_dia_16INV)
##View(medias_diasI)
medias_diasINO13 <- cbind(levels(DiasF),medias_diasINO13)
medias_diasINO13 <- as.data.frame(medias_diasINO13)
##View(medias_diasINO13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasINO13) <- nombres
medias_diasINO13$Fecha <- as.Date(medias_diasINO13$Fecha)
options(digits=5)
medias_diasINO13$I1 <- as.numeric(medias_diasINO13$I1)
medias_diasINO13$I2 <- as.numeric(medias_diasINO13$I2)
medias_diasINO13$I3 <- as.numeric(medias_diasINO13$I3)
##View(medias_diasINO13)

max_diasINO13 <- rbind(max_dia_7INV,max_dia_8INV,max_dia_9INV,
                     max_dia_10INV,max_dia_11INV,max_dia_12INV,
                     max_dia_13INV,max_dia_14INV,max_dia_15INV,
                     max_dia_16INV)
##View(max_diasINO13)
max_diasINO13 <- cbind(levels(DiasF),max_diasINO13)
max_diasINO13 <- as.data.frame(max_diasINO13)
##View(max_diasINO13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasINO13) <- nombres
max_diasINO13$Fecha <- as.Date(max_diasINO13$Fecha)
options(digits=5)
max_diasINO13$I1 <- as.numeric(max_diasINO13$I1)
max_diasINO13$I2 <- as.numeric(max_diasINO13$I2)
max_diasINO13$I3 <- as.numeric(max_diasINO13$I3)
##View(max_diasINO13)


##Noviembre 2014####
dfNV14 <- read.csv("Data/Years/2014/NOV014.csv")
#View(dfNV14)
dfNV14 <- dfNV14[1:7619,c(1:8)] 
dfNV14[3:8] <- sapply(dfNV14[3:8],as.numeric)  

dfNV14[3] <- dfNV14[3]/sqrt(3)
dfNV14[4] <- dfNV14[4]/sqrt(3)
dfNV14[5] <- dfNV14[5]/sqrt(3)

# dfNV14[6] <- dfNV14[6]*2
# dfNV14[7] <- dfNV14[7]*2
# dfNV14[7] <- dfNV14[7]*2



dfNV14$Feha.de.la.Medda <- as.Date(dfNV14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfNV14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfNV14, Feha.de.la.Medda==Dias[i]))
}

names(dfNV14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaINO <- vector("numeric", ncol(dfNV14[6:8]))
for (i in seq_along(dfNV14[6:8])) {# 2. secuencia
  mediaINO[[i]] <- mean(ifelse(dfNV14[6:8][[i]]<=0,NA,dfNV14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxINO<- vector("numeric", ncol(dfNV14[6:8]))
for (i in seq_along(dfNV14[6:8])) {# 2. secuencia
  maxINO[[i]] <- max(dfNV14[6:8][[i]] )    # 3. cuerpo
}

##4-noviembre a 30-noviembre#
###Dia 4###
media_dia_4INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4INO[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4INO[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5INO[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5INO[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6INO[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6INO[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7INO[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7INO[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8INO[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8INO[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9INO[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9INO[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10INO[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10INO[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11INO[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11INO[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12INO[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12INO[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13INO[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13INO[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14INO[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14INO[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15INO[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15INO[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16INO[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16INO[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17INO[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17INO[[i]] <- max(Dia_17[6:8][[i]]) 
}
###Dia 18###
media_dia_18INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18INO[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18INO[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19INO[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19INO[[i]] <- max(Dia_19[6:8][[i]]) 
}
###Dia 20###
media_dia_20INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20INO[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20INO[[i]] <- max(Dia_20[6:8][[i]]) 
}
###Dia 21###
media_dia_21INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21INO[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T)
}

max_dia_21INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21INO[[i]] <- max(Dia_21[6:8][[i]]) 
}
###Dia 22###
media_dia_22INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22INO[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22INO[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23INO[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23INO[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24INO[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24INO[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25INO[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25INO[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26INO[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26INO[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27INO[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27INO[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28INO[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28INO[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29INO[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29INO[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30INO<-vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30INO[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30INO <- vector("numeric",  ncol(dfNV14[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30INO[[i]] <- max(Dia_30[6:8][[i]]) 
}

medias_diasINO14 <- rbind(media_dia_4INO,
                        media_dia_5INO,media_dia_6INO,media_dia_7INO,
                        media_dia_8INO,media_dia_9INO,media_dia_10INO,
                        media_dia_11INO,media_dia_12INO,media_dia_13INO,
                        media_dia_14INO,media_dia_15INO,media_dia_16INO,
                        media_dia_17INO,media_dia_18INO,media_dia_19INO,
                        media_dia_20INO,media_dia_21INO,media_dia_22INO,
                        media_dia_23INO,media_dia_24INO,media_dia_25INO,
                        media_dia_26INO,media_dia_27INO,media_dia_28INO,
                        media_dia_29INO,media_dia_30INO)
##View(medias_diasI)
medias_diasINO14 <- cbind(levels(DiasF),medias_diasINO14)
medias_diasINO14 <- as.data.frame(medias_diasINO14)
##View(medias_diasINO14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasINO14) <- nombres
medias_diasINO14$Fecha <- as.Date(medias_diasINO14$Fecha)
options(digits=5)
medias_diasINO14$I1 <- as.numeric(medias_diasINO14$I1)
medias_diasINO14$I2 <- as.numeric(medias_diasINO14$I2)
medias_diasINO14$I3 <- as.numeric(medias_diasINO14$I3)
##View(medias_diasINO14)

max_diasINO14 <- rbind(max_dia_4INO,
                     max_dia_5INO,max_dia_6INO,max_dia_7INO,
                     max_dia_8INO,max_dia_9INO,max_dia_10INO,
                     max_dia_11INO,max_dia_12INO,max_dia_13INO,
                     max_dia_14INO,max_dia_15INO,max_dia_16INO,
                     max_dia_17INO,max_dia_18INO,max_dia_19INO,
                     max_dia_20INO,max_dia_21INO,max_dia_22INO,
                     max_dia_23INO,max_dia_24INO,max_dia_25INO,
                     max_dia_26INO,max_dia_27INO,max_dia_28INO,
                     max_dia_29INO,max_dia_30INO)
##View(max_diasINO14)
max_diasINO14 <- cbind(levels(DiasF),max_diasINO14)
max_diasINO14 <- as.data.frame(max_diasINO14)
##View(max_diasINO14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasINO14) <- nombres
max_diasINO14$Fecha <- as.Date(max_diasINO14$Fecha)
options(digits=5)
max_diasINO14$I1 <- as.numeric(max_diasINO14$I1)
max_diasINO14$I2 <- as.numeric(max_diasINO14$I2)
max_diasINO14$I3 <- as.numeric(max_diasINO14$I3)
##View(max_diasINO14)




##Noviembre 2015####
dfNV15 <- read.csv("Data/Years/2015/NOV2015.csv")
#View(dfNV15)
dfNV15 <- dfNV15[1:3311,c(1:8)] 
dfNV15[3:8] <- sapply(dfNV15[3:8],as.numeric)  

dfNV15[3] <- dfNV15[3]/sqrt(3)
dfNV15[4] <- dfNV15[4]/sqrt(3)
dfNV15[5] <- dfNV15[5]/sqrt(3)

dfNV15[6] <- dfNV15[6]*2
dfNV15[7] <- dfNV15[7]*2
dfNV15[8] <- dfNV15[8]*2



dfNV15$Feha.de.la.Medda <- as.Date(dfNV15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfNV15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfNV15, Feha.de.la.Medda==Dias[i]))
}

names(dfNV15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaINO <- vector("numeric", ncol(dfNV15[6:8]))
for (i in seq_along(dfNV15[6:8])) {# 2. secuencia
  mediaINO[[i]] <- mean(ifelse(dfNV15[6:8][[i]]<=0,NA,dfNV15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxINO<- vector("numeric", ncol(dfNV15[6:8]))
for (i in seq_along(dfNV15[6:8])) {# 2. secuencia
  maxINO[[i]] <- max(dfNV15[6:8][[i]] )    # 3. cuerpo
}

##1-Noviembre a 30-Noviembre#
###Dia 1###
media_dia_1INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1INO[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1INO[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2INO[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2INO[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3INO[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3INO[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4INO[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4INO[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5INO[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5INO[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6INO[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6INO[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7INO[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7INO[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8INO[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8INO[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9INO[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9INO[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10INO[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10INO[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11INO[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11INO[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12INO[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12INO[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13INO[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13INO[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14INO[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14INO[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15INO[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15INO[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16INO[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16INO[[i]] <- max(Dia_16[6:8][[i]]) 
}

###Dia 17###
media_dia_17INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  media_dia_17INO[[i]] <- mean(ifelse(Dia_17[6:8][[i]]<=0,NA,Dia_17[6:8][[i]]),
                               na.rm=T) 
}

max_dia_17INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_17[6:8])) {
  max_dia_17INO[[i]] <- max(Dia_17[6:8][[i]]) 
}

###Dia 18###
media_dia_18INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  media_dia_18INO[[i]] <- mean(ifelse(Dia_18[6:8][[i]]<=0,NA,Dia_18[6:8][[i]]),
                               na.rm=T) 
}

max_dia_18INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_18[6:8])) {
  max_dia_18INO[[i]] <- max(Dia_18[6:8][[i]]) 
}

###Dia 19###
media_dia_19INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  media_dia_19INO[[i]] <- mean(ifelse(Dia_19[6:8][[i]]<=0,NA,Dia_19[6:8][[i]]),
                               na.rm=T) 
}

max_dia_19INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_19[6:8])) {
  max_dia_19INO[[i]] <- max(Dia_19[6:8][[i]]) 
}

###Dia 20###
media_dia_20INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  media_dia_20INO[[i]] <- mean(ifelse(Dia_20[6:8][[i]]<=0,NA,Dia_20[6:8][[i]]),
                               na.rm=T) 
}

max_dia_20INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_20[6:8])) {
  max_dia_20INO[[i]] <- max(Dia_20[6:8][[i]]) 
}

###Dia 21###
media_dia_21INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21INO[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21INO[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22INO[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22INO[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23INO[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23INO[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24INO<-vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24INO[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24INO <- vector("numeric",  ncol(dfNV15[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24INO[[i]] <- max(Dia_24[6:8][[i]]) 
}



medias_diasINO15 <- rbind(media_dia_1INO,
                        media_dia_2INO,media_dia_3INO,media_dia_4INO,
                        media_dia_5INO,media_dia_6INO,media_dia_7INO,
                        media_dia_8INO,media_dia_9INO,media_dia_10INO,
                        media_dia_11INO,media_dia_12INO,media_dia_13INO,
                        media_dia_14INO,media_dia_15INO,media_dia_16INO,
                        media_dia_17INO,
                        media_dia_18INO,media_dia_19INO,media_dia_20INO,
                        media_dia_21INO,media_dia_22INO,media_dia_23INO,
                        media_dia_24INO)
##View(medias_diasI)
medias_diasINO15 <- cbind(levels(DiasF),medias_diasINO15)
medias_diasINO15 <- as.data.frame(medias_diasINO15)
##View(medias_diasINO15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasINO15) <- nombres
medias_diasINO15$Fecha <- as.Date(medias_diasINO15$Fecha)
options(digits=5)
medias_diasINO15$I1 <- as.numeric(medias_diasINO15$I1)
medias_diasINO15$I2 <- as.numeric(medias_diasINO15$I2)
medias_diasINO15$I3 <- as.numeric(medias_diasINO15$I3)
##View(medias_diasINO15)

max_diasINO15 <- rbind(max_dia_1INO,
                     max_dia_2INO,max_dia_3INO,max_dia_4INO,
                     max_dia_5INO,max_dia_6INO,max_dia_7INO,
                     max_dia_8INO,max_dia_9INO,max_dia_10INO,
                     max_dia_11INO,max_dia_12INO,max_dia_13INO,
                     max_dia_14INO,max_dia_15INO,max_dia_16INO,
                     max_dia_17INO,
                     max_dia_18INO,max_dia_19INO,max_dia_20INO,
                     max_dia_21INO,max_dia_22INO,max_dia_23INO,
                     max_dia_24INO)
##View(max_diasINO15)
max_diasINO15 <- cbind(levels(DiasF),max_diasINO15)
max_diasINO15 <- as.data.frame(max_diasINO15)
##View(max_diasINO15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasINO15) <- nombres
max_diasINO15$Fecha <- as.Date(max_diasINO15$Fecha)
options(digits=5)
max_diasINO15$I1 <- as.numeric(max_diasINO15$I1)
max_diasINO15$I2 <- as.numeric(max_diasINO15$I2)
max_diasINO15$I3 <- as.numeric(max_diasINO15$I3)
##View(max_diasINO15)





##Noviembre 2016####
dfNV16 <- read.csv("Data/Years/2016/NOV2016.csv")
#View(dfNV16)
dfNV16 <- dfNV16[1:810,c(1:8)] 
dfNV16[3:8] <- sapply(dfNV16[3:8],as.numeric)  

dfNV16[3] <- dfNV16[3]/sqrt(3)
dfNV16[4] <- dfNV16[4]/sqrt(3)
dfNV16[5] <- dfNV16[5]/sqrt(3)

dfNV16[6] <- dfNV16[6]*2
dfNV16[7] <- dfNV16[7]*2
dfNV16[8] <- dfNV16[8]*2



dfNV16$Fecha.de.la.Medida <- as.Date(dfNV16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfNV16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfNV16, Fecha.de.la.Medida==Dias[i]))
}

names(dfNV16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaINO <- vector("numeric", ncol(dfNV16[6:8]))
for (i in seq_along(dfNV16[6:8])) {# 2. secuencia
  mediaINO[[i]] <- mean(ifelse(dfNV16[6:8][[i]]<=0,NA,dfNV16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxINO<- vector("numeric", ncol(dfNV16[6:8]))
for (i in seq_along(dfNV16[6:8])) {# 2. secuencia
  maxINO[[i]] <- max(dfNV16[6:8][[i]] )    # 3. cuerpo
}

##22-Noviembre a 30-Noviembre#
###Dia 22###
media_dia_22INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22INO[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22INO[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23INO[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23INO[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24INO[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24INO[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25INO[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25INO[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26INO[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26INO[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27INO[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27INO[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28INO[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28INO[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29INO[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29INO[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30INO<-vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30INO[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30INO <- vector("numeric",  ncol(dfNV16[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30INO[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasINO16 <- rbind(media_dia_22INO,media_dia_23INO,
                        media_dia_24INO,media_dia_25INO,media_dia_26INO,
                        media_dia_27INO,
                        media_dia_28INO,media_dia_29INO,media_dia_30INO)
##View(medias_diasI)
medias_diasINO16 <- cbind(levels(DiasF),medias_diasINO16)
medias_diasINO16 <- as.data.frame(medias_diasINO16)
##View(medias_diasINO16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasINO16) <- nombres
medias_diasINO16$Fecha <- as.Date(medias_diasINO16$Fecha)
options(digits=5)
medias_diasINO16$I1 <- as.numeric(medias_diasINO16$I1)
medias_diasINO16$I2 <- as.numeric(medias_diasINO16$I2)
medias_diasINO16$I3 <- as.numeric(medias_diasINO16$I3)
##View(medias_diasINO16)

max_diasINO16 <- rbind(max_dia_22INO,max_dia_23INO,
                     max_dia_24INO,max_dia_25INO,max_dia_26INO,
                     max_dia_27INO,
                     max_dia_28INO,max_dia_29INO,max_dia_30INO)
##View(max_diasINO16)
max_diasINO16 <- cbind(levels(DiasF),max_diasINO16)
max_diasINO16 <- as.data.frame(max_diasINO16)
##View(max_diasINO16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasINO16) <- nombres
max_diasINO16$Fecha <- as.Date(max_diasINO16$Fecha)
options(digits=5)
max_diasINO16$I1 <- as.numeric(max_diasINO16$I1)
max_diasINO16$I2 <- as.numeric(max_diasINO16$I2)
max_diasINO16$I3 <- as.numeric(max_diasINO16$I3)
##View(max_diasINO16)





##Noviembre 2017####
dfNV17 <- read.csv("Data/Years/2017/NOV2017.csv")
#View(dfNV17)
dfNV17 <- dfNV17[1:668,c(1:8)] 
dfNV17[3:8] <- sapply(dfNV17[3:8],as.numeric)  

dfNV17[3] <- dfNV17[3]/sqrt(3)
dfNV17[4] <- dfNV17[4]/sqrt(3)
dfNV17[5] <- dfNV17[5]/sqrt(3)

# dfNV17[6] <- dfNV17[6]*2
# dfNV17[7] <- dfNV17[7]*2
# dfNV17[8] <- dfNV17[8]*2



dfNV17$Fecha.de.la.Medida <- as.Date(dfNV17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfNV17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfNV17, Fecha.de.la.Medida==Dias[i]))
}

names(dfNV17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaINO <- vector("numeric", ncol(dfNV17[6:8]))
for (i in seq_along(dfNV17[6:8])) {# 2. secuencia
  mediaINO[[i]] <- mean(ifelse(dfNV17[6:8][[i]]<=0,NA,dfNV17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxINO<- vector("numeric", ncol(dfNV17[6:8]))
for (i in seq_along(dfNV17[6:8])) {# 2. secuencia
  maxINO[[i]] <- max(dfNV17[6:8][[i]] )    # 3. cuerpo
}

##21-Noviembre a 28-Noviembre#
###Dia 21###
media_dia_21INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  media_dia_21INO[[i]] <- mean(ifelse(Dia_21[6:8][[i]]<=0,NA,Dia_21[6:8][[i]]),
                               na.rm=T) 
}

max_dia_21INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_21[6:8])) {
  max_dia_21INO[[i]] <- max(Dia_21[6:8][[i]]) 
}

###Dia 22###
media_dia_22INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  media_dia_22INO[[i]] <- mean(ifelse(Dia_22[6:8][[i]]<=0,NA,Dia_22[6:8][[i]]),
                               na.rm=T) 
}

max_dia_22INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_22[6:8])) {
  max_dia_22INO[[i]] <- max(Dia_22[6:8][[i]]) 
}

###Dia 23###
media_dia_23INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23INO[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23INO[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24INO[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24INO[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25INO[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25INO[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26INO[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26INO[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27INO[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27INO[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28INO<-vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28INO[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28INO <- vector("numeric",  ncol(dfNV17[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28INO[[i]] <- max(Dia_28[6:8][[i]]) 
}


medias_diasINO17 <- rbind( media_dia_21INO,media_dia_22INO,media_dia_23INO,
                         media_dia_24INO,media_dia_25INO,media_dia_26INO,
                         media_dia_27INO,
                         media_dia_28INO)
##View(medias_diasI)
medias_diasINO17 <- cbind(levels(DiasF),medias_diasINO17)
medias_diasINO17 <- as.data.frame(medias_diasINO17)
##View(medias_diasINO17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasINO17) <- nombres
medias_diasINO17$Fecha <- as.Date(medias_diasINO17$Fecha)
options(digits=5)
medias_diasINO17$I1 <- as.numeric(medias_diasINO17$I1)
medias_diasINO17$I2 <- as.numeric(medias_diasINO17$I2)
medias_diasINO17$I3 <- as.numeric(medias_diasINO17$I3)
##View(medias_diasINO17)

max_diasINO17 <- rbind(max_dia_21INO,max_dia_22INO,max_dia_23INO,
                     max_dia_24INO,max_dia_25INO,max_dia_26INO,
                     max_dia_27INO,
                     max_dia_28INO)
##View(max_diasINO17)
max_diasINO17 <- cbind(levels(DiasF),max_diasINO17)
max_diasINO17 <- as.data.frame(max_diasINO17)
##View(max_diasINO17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasINO17) <- nombres
max_diasINO17$Fecha <- as.Date(max_diasINO17$Fecha)
options(digits=5)
max_diasINO17$I1 <- as.numeric(max_diasINO17$I1)
max_diasINO17$I2 <- as.numeric(max_diasINO17$I2)
max_diasINO17$I3 <- as.numeric(max_diasINO17$I3)
##View(max_diasINO17)




##Noviembre 2018####
dfNV18 <- read.csv("Data/Years/2018/NOV2018.csv")
#View(dfNV18)
dfNV18 <- dfNV18[1:670,c(1:8)] 
dfNV18[3:8] <- sapply(dfNV18[3:8],as.numeric)  

dfNV18[3] <- dfNV18[3]/sqrt(3)
dfNV18[4] <- dfNV18[4]/sqrt(3)
dfNV18[5] <- dfNV18[5]/sqrt(3)

# dfNV18[6] <- dfNV18[6]*2
# dfNV18[7] <- dfNV18[7]*2
# dfNV18[8] <- dfNV18[8]*2



dfNV18$Fecha.de.la.Medida <- as.Date(dfNV18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfNV18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfNV18, Fecha.de.la.Medida==Dias[i]))
}

names(dfNV18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaINO <- vector("numeric", ncol(dfNV18[6:8]))
for (i in seq_along(dfNV18[6:8])) {# 2. secuencia
  mediaINO[[i]] <- mean(ifelse(dfNV18[6:8][[i]]<=0,NA,dfNV18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxINO<- vector("numeric", ncol(dfNV18[6:8]))
for (i in seq_along(dfNV18[6:8])) {# 2. secuencia
  maxINO[[i]] <- max(dfNV18[6:8][[i]] )    # 3. cuerpo
}

##23-Noviembre a 30-Noviembre#
###Dia 23###
media_dia_23INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  media_dia_23INO[[i]] <- mean(ifelse(Dia_23[6:8][[i]]<=0,NA,Dia_23[6:8][[i]]),
                               na.rm=T) 
}

max_dia_23INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_23[6:8])) {
  max_dia_23INO[[i]] <- max(Dia_23[6:8][[i]]) 
}

###Dia 24###
media_dia_24INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  media_dia_24INO[[i]] <- mean(ifelse(Dia_24[6:8][[i]]<=0,NA,Dia_24[6:8][[i]]),
                               na.rm=T) 
}

max_dia_24INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_24[6:8])) {
  max_dia_24INO[[i]] <- max(Dia_24[6:8][[i]]) 
}

###Dia 25###
media_dia_25INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  media_dia_25INO[[i]] <- mean(ifelse(Dia_25[6:8][[i]]<=0,NA,Dia_25[6:8][[i]]),
                               na.rm=T) 
}

max_dia_25INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_25[6:8])) {
  max_dia_25INO[[i]] <- max(Dia_25[6:8][[i]]) 
}

###Dia 26###
media_dia_26INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  media_dia_26INO[[i]] <- mean(ifelse(Dia_26[6:8][[i]]<=0,NA,Dia_26[6:8][[i]]),
                               na.rm=T) 
}

max_dia_26INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_26[6:8])) {
  max_dia_26INO[[i]] <- max(Dia_26[6:8][[i]]) 
}

###Dia 27###
media_dia_27INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  media_dia_27INO[[i]] <- mean(ifelse(Dia_27[6:8][[i]]<=0,NA,Dia_27[6:8][[i]]),
                               na.rm=T) 
}

max_dia_27INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_27[6:8])) {
  max_dia_27INO[[i]] <- max(Dia_27[6:8][[i]]) 
}

###Dia 28###
media_dia_28INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  media_dia_28INO[[i]] <- mean(ifelse(Dia_28[6:8][[i]]<=0,NA,Dia_28[6:8][[i]]),
                               na.rm=T) 
}

max_dia_28INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_28[6:8])) {
  max_dia_28INO[[i]] <- max(Dia_28[6:8][[i]]) 
}

###Dia 29###
media_dia_29INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  media_dia_29INO[[i]] <- mean(ifelse(Dia_29[6:8][[i]]<=0,NA,Dia_29[6:8][[i]]),
                               na.rm=T) 
}

max_dia_29INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_29[6:8])) {
  max_dia_29INO[[i]] <- max(Dia_29[6:8][[i]]) 
}

###Dia 30###
media_dia_30INO<-vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  media_dia_30INO[[i]] <- mean(ifelse(Dia_30[6:8][[i]]<=0,NA,Dia_30[6:8][[i]]),
                               na.rm=T) 
}

max_dia_30INO <- vector("numeric",  ncol(dfNV18[6:8]))
for (i in seq_along(Dia_30[6:8])) {
  max_dia_30INO[[i]] <- max(Dia_30[6:8][[i]]) 
}



medias_diasINO18 <- rbind(media_dia_23INO,
                        media_dia_24INO,media_dia_25INO,media_dia_26INO,
                        media_dia_27INO,
                        media_dia_28INO,media_dia_29INO,media_dia_30INO)
##View(medias_diasI)
medias_diasINO18 <- cbind(levels(DiasF),medias_diasINO18)
medias_diasINO18 <- as.data.frame(medias_diasINO18)
##View(medias_diasINO18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasINO18) <- nombres
medias_diasINO18$Fecha <- as.Date(medias_diasINO18$Fecha)
options(digits=5)
medias_diasINO18$I1 <- as.numeric(medias_diasINO18$I1)
medias_diasINO18$I2 <- as.numeric(medias_diasINO18$I2)
medias_diasINO18$I3 <- as.numeric(medias_diasINO18$I3)
##View(medias_diasINO18)

max_diasINO18 <- rbind(max_dia_23INO,
                     max_dia_24INO,max_dia_25INO,max_dia_26INO,
                     max_dia_27INO,
                     max_dia_28INO,max_dia_29INO,max_dia_30INO)
##View(max_diasINO18)
max_diasINO18 <- cbind(levels(DiasF),max_diasINO18)
max_diasINO18 <- as.data.frame(max_diasINO18)
##View(max_diasINO18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasINO18) <- nombres
max_diasINO18$Fecha <- as.Date(max_diasINO18$Fecha)
options(digits=5)
max_diasINO18$I1 <- as.numeric(max_diasINO18$I1)
max_diasINO18$I2 <- as.numeric(max_diasINO18$I2)
max_diasINO18$I3 <- as.numeric(max_diasINO18$I3)
##View(max_diasINO18)



#####
Noviembre <- rbind(medias_diasINO13,medias_diasINO14,medias_diasINO15,
                medias_diasINO16,medias_diasINO17,medias_diasINO18)
NoviembreM <- rbind(max_diasINO13,max_diasINO14,max_diasINO15,
                   max_diasINO16,max_diasINO17,max_diasINO18)
dfNV <- rbind(dfNV13,dfNV14,dfNV15,dfNV16,dfNV17,dfNV18)




##Diciembre 2013####
dfDC13 <- read.csv("Data/Years/2013/DIC2013.csv")
#View(dfDC13)
dfDC13 <- dfDC13[1:1723,1:8]
dfDC13[3:8] <- sapply(dfDC13[3:8],as.numeric)  

# dfDC13[3] <- dfDC13[3]/sqrt(3)
# dfDC13[4] <- dfDC13[4]/sqrt(3)
# dfDC13[5] <- dfDC13[5]/sqrt(3)
# 
# dfDC13[6] <- dfDC13[6]*2
# dfDC13[7] <- dfDC13[7]*2
# dfDC13[8] <- dfDC13[7]*2


dfDC13$Fecha.de.la.Medida <- as.Date(dfDC13$Fecha.de.la.Medida,format="%d.%m.%Y")
DiasF <- as.factor(dfDC13$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfDC13, Fecha.de.la.Medida==Dias[i]))
}

names(dfDC13) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIDC <- vector("numeric", ncol(dfDC13[6:8]))
for (i in seq_along(dfDC13[6:8])) {# 2. secuencia
  mediaIDC[[i]] <- mean(ifelse(dfDC13[6:8][[i]]<=0,NA,dfDC13[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIDC<- vector("numeric", ncol(dfDC13[6:8]))
for (i in seq_along(dfDC13[6:8])) {# 2. secuencia
  maxIDC[[i]] <- max(dfDC13[6:8][[i]] )    # 3. cuerpo
}

##3-diciembre a 9-diciembre#
###Dia 3###
media_dia_3IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IDC[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IDC[[i]] <- max(Dia_3[6:8][[i]]) 
}
###Dia 4###
media_dia_4IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IDC[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IDC[[i]] <- max(Dia_4[6:8][[i]]) 
}

###Dia 5###
media_dia_5IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IDC[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IDC[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IDC[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T) 
}

max_dia_6IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IDC[[i]] <- max(Dia_6[6:8][[i]]) 
}

###Dia 7 ###
media_dia_7IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IDC[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IDC[[i]] <- max(Dia_7[6:8][[i]]) 
}
###Dia 8###
media_dia_8IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IDC[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IDC[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IDC<-vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IDC[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IDC <- vector("numeric",  ncol(dfDC13[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IDC[[i]] <- max(Dia_9[6:8][[i]]) 
}



medias_diasIDC13 <- rbind(media_dia_3IDC,media_dia_4IDC,media_dia_5IDC,
                        media_dia_6IDC,media_dia_7IDC,media_dia_8IDC,
                        media_dia_9IDC)
##View(medias_diasI)
medias_diasIDC13 <- cbind(levels(DiasF),medias_diasIDC13)
medias_diasIDC13 <- as.data.frame(medias_diasIDC13)
##View(medias_diasIDC13)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIDC13) <- nombres
medias_diasIDC13$Fecha <- as.Date(medias_diasIDC13$Fecha)
options(digits=5)
medias_diasIDC13$I1 <- as.numeric(medias_diasIDC13$I1)
medias_diasIDC13$I2 <- as.numeric(medias_diasIDC13$I2)
medias_diasIDC13$I3 <- as.numeric(medias_diasIDC13$I3)
##View(medias_diasIDC13)

max_diasIDC13 <- rbind(max_dia_3IDC,max_dia_4IDC,max_dia_5IDC,
                     max_dia_6IDC,max_dia_7IDC,max_dia_8IDC,
                     max_dia_9IDC)
##View(max_diasIDC13)
max_diasIDC13 <- cbind(levels(DiasF),max_diasIDC13)
max_diasIDC13 <- as.data.frame(max_diasIDC13)
##View(max_diasIDC13)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIDC13) <- nombres
max_diasIDC13$Fecha <- as.Date(max_diasIDC13$Fecha)
options(digits=5)
max_diasIDC13$I1 <- as.numeric(max_diasIDC13$I1)
max_diasIDC13$I2 <- as.numeric(max_diasIDC13$I2)
max_diasIDC13$I3 <- as.numeric(max_diasIDC13$I3)
##View(max_diasIDC13)



##Diciembre 2014####
dfDC14 <- read.csv("Data/Years/2014/DIC014.csv")
#View(dfDC14)
dfDC14 <- dfDC14[1:2726,c(1:8)] 
dfDC14[3:8] <- sapply(dfDC14[3:8],as.numeric)  

dfDC14[3] <- dfDC14[3]/sqrt(3)
dfDC14[4] <- dfDC14[4]/sqrt(3)
dfDC14[5] <- dfDC14[5]/sqrt(3)

# dfDC14[6] <- dfDC14[6]*2
# dfDC14[7] <- dfDC14[7]*2
# dfDC14[7] <- dfDC14[7]*2



dfDC14$Feha.de.la.Medda <- as.Date(dfDC14$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfDC14$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfDC14, Feha.de.la.Medda==Dias[i]))
}

names(dfDC14) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIDC <- vector("numeric", ncol(dfDC14[6:8]))
for (i in seq_along(dfDC14[6:8])) {# 2. secuencia
  mediaIDC[[i]] <- mean(ifelse(dfDC14[6:8][[i]]<=0,NA,dfDC14[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIDC<- vector("numeric", ncol(dfDC14[6:8]))
for (i in seq_along(dfDC14[6:8])) {# 2. secuencia
  maxIDC[[i]] <- max(dfDC14[6:8][[i]] )    # 3. cuerpo
}

##1-diciembre a 10-diciembre#
###Dia 1###
media_dia_1IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IDC[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IDC[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IDC[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IDC[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IDC[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IDC[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IDC[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IDC[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IDC[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IDC[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IDC[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IDC[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IDC[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IDC[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IDC[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IDC[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IDC[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IDC[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IDC<-vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IDC[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IDC <- vector("numeric",  ncol(dfDC14[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IDC[[i]] <- max(Dia_10[6:8][[i]]) 
}


medias_diasIDC14 <- rbind(media_dia_1IDC,media_dia_2IDC,media_dia_3IDC,
                        media_dia_4IDC,media_dia_5IDC,media_dia_6IDC,
                        media_dia_7IDC,media_dia_8IDC,media_dia_9IDC,
                        media_dia_10IDC)
##View(medias_diasI)
medias_diasIDC14 <- cbind(levels(DiasF),medias_diasIDC14)
medias_diasIDC14 <- as.data.frame(medias_diasIDC14)
##View(medias_diasIDC14)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIDC14) <- nombres
medias_diasIDC14$Fecha <- as.Date(medias_diasIDC14$Fecha)
options(digits=5)
medias_diasIDC14$I1 <- as.numeric(medias_diasIDC14$I1)
medias_diasIDC14$I2 <- as.numeric(medias_diasIDC14$I2)
medias_diasIDC14$I3 <- as.numeric(medias_diasIDC14$I3)
##View(medias_diasIDC14)

max_diasIDC14 <- rbind(max_dia_1IDC,max_dia_2IDC,max_dia_3IDC,
                     max_dia_4IDC,max_dia_5IDC,max_dia_6IDC,
                     max_dia_7IDC,max_dia_8IDC,max_dia_9IDC,
                     max_dia_10IDC)
##View(max_diasIDC14)
max_diasIDC14 <- cbind(levels(DiasF),max_diasIDC14)
max_diasIDC14 <- as.data.frame(max_diasIDC14)
##View(max_diasIDC14)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIDC14) <- nombres
max_diasIDC14$Fecha <- as.Date(max_diasIDC14$Fecha)
options(digits=5)
max_diasIDC14$I1 <- as.numeric(max_diasIDC14$I1)
max_diasIDC14$I2 <- as.numeric(max_diasIDC14$I2)
max_diasIDC14$I3 <- as.numeric(max_diasIDC14$I3)
##View(max_diasIDC14)




##Diciembre 2015####
dfDC15 <- read.csv("Data/Years/2015/DIC2015.csv")
#View(dfDC15)
dfDC15 <- dfDC15[1:1379,c(1:8)] 
dfDC15[3:8] <- sapply(dfDC15[3:8],as.numeric)  

dfDC15[3] <- dfDC15[3]/sqrt(3)
dfDC15[4] <- dfDC15[4]/sqrt(3)
dfDC15[5] <- dfDC15[5]/sqrt(3)
# 
# dfDC15[6] <- dfDC15[6]*2
# dfDC15[7] <- dfDC15[7]*2
# dfDC15[8] <- dfDC15[8]*2



dfDC15$Feha.de.la.Medda <- as.Date(dfDC15$Feha.de.la.Medda,format="%d/%m/%Y")
DiasF <- as.factor(dfDC15$Feha.de.la.Medda)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfDC15, Feha.de.la.Medda==Dias[i]))
}

names(dfDC15) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIDC <- vector("numeric", ncol(dfDC15[6:8]))
for (i in seq_along(dfDC15[6:8])) {# 2. secuencia
  mediaIDC[[i]] <- mean(ifelse(dfDC15[6:8][[i]]<=0,NA,dfDC15[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIDC<- vector("numeric", ncol(dfDC15[6:8]))
for (i in seq_along(dfDC15[6:8])) {# 2. secuencia
  maxIDC[[i]] <- max(dfDC15[6:8][[i]] )    # 3. cuerpo
}

##1-Diciembre a 10-Diciembre#
###Dia 1###
media_dia_1IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IDC[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IDC[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IDC[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IDC[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IDC[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IDC[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IDC[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IDC[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IDC[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IDC[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IDC[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IDC[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IDC[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IDC[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IDC[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IDC[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IDC[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IDC[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IDC<-vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IDC[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IDC <- vector("numeric",  ncol(dfDC15[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IDC[[i]] <- max(Dia_10[6:8][[i]]) 
}



medias_diasIDC15 <- rbind(media_dia_1IDC,
                        media_dia_2IDC,media_dia_3IDC,media_dia_4IDC,
                        media_dia_5IDC,media_dia_6IDC,media_dia_7IDC,
                        media_dia_8IDC,media_dia_9IDC,media_dia_10IDC)
##View(medias_diasI)
medias_diasIDC15 <- cbind(levels(DiasF),medias_diasIDC15)
medias_diasIDC15 <- as.data.frame(medias_diasIDC15)
##View(medias_diasIDC15)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIDC15) <- nombres
medias_diasIDC15$Fecha <- as.Date(medias_diasIDC15$Fecha)
options(digits=5)
medias_diasIDC15$I1 <- as.numeric(medias_diasIDC15$I1)
medias_diasIDC15$I2 <- as.numeric(medias_diasIDC15$I2)
medias_diasIDC15$I3 <- as.numeric(medias_diasIDC15$I3)
##View(medias_diasIDC15)

max_diasIDC15 <- rbind(max_dia_1IDC,
                     max_dia_2IDC,max_dia_3IDC,max_dia_4IDC,
                     max_dia_5IDC,max_dia_6IDC,max_dia_7IDC,
                     max_dia_8IDC,max_dia_9IDC,max_dia_10IDC)
##View(max_diasIDC15)
max_diasIDC15 <- cbind(levels(DiasF),max_diasIDC15)
max_diasIDC15 <- as.data.frame(max_diasIDC15)
##View(max_diasIDC15)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIDC15) <- nombres
max_diasIDC15$Fecha <- as.Date(max_diasIDC15$Fecha)
options(digits=5)
max_diasIDC15$I1 <- as.numeric(max_diasIDC15$I1)
max_diasIDC15$I2 <- as.numeric(max_diasIDC15$I2)
max_diasIDC15$I3 <- as.numeric(max_diasIDC15$I3)
##View(max_diasIDC15)





##Diciembre 2016####
dfDC16 <- read.csv("Data/Years/2016/DIC2016.csv")
#View(dfDC16)
dfDC16 <- dfDC16[1:1493,c(1:8)] 
dfDC16[3:8] <- sapply(dfDC16[3:8],as.numeric)  

dfDC16[3] <- dfDC16[3]/sqrt(3)
dfDC16[4] <- dfDC16[4]/sqrt(3)
dfDC16[5] <- dfDC16[5]/sqrt(3)

dfDC16[6] <- dfDC16[6]*2
dfDC16[7] <- dfDC16[7]*2
dfDC16[8] <- dfDC16[8]*2



dfDC16$Fecha.de.la.Medida <- as.Date(dfDC16$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfDC16$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfDC16, Fecha.de.la.Medida==Dias[i]))
}

names(dfDC16) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIDC <- vector("numeric", ncol(dfDC16[6:8]))
for (i in seq_along(dfDC16[6:8])) {# 2. secuencia
  mediaIDC[[i]] <- mean(ifelse(dfDC16[6:8][[i]]<=0,NA,dfDC16[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIDC<- vector("numeric", ncol(dfDC16[6:8]))
for (i in seq_along(dfDC16[6:8])) {# 2. secuencia
  maxIDC[[i]] <- max(dfDC16[6:8][[i]] )    # 3. cuerpo
}

##1-Diciembre a 16-Diciembre#
###Dia 1###
media_dia_1IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IDC[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IDC[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IDC[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IDC[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IDC[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IDC[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IDC[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IDC[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IDC[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IDC[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IDC[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IDC[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IDC[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IDC[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IDC[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IDC[[i]] <- max(Dia_8[6:8][[i]]) 
}

###Dia 9###
media_dia_9IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  media_dia_9IDC[[i]] <- mean(ifelse(Dia_9[6:8][[i]]<=0,NA,Dia_9[6:8][[i]]),
                              na.rm=T) 
}

max_dia_9IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_9[6:8])) {
  max_dia_9IDC[[i]] <- max(Dia_9[6:8][[i]]) 
}

###Dia 10###
media_dia_10IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  media_dia_10IDC[[i]] <- mean(ifelse(Dia_10[6:8][[i]]<=0,NA,Dia_10[6:8][[i]]),
                               na.rm=T) 
}

max_dia_10IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_10[6:8])) {
  max_dia_10IDC[[i]] <- max(Dia_10[6:8][[i]]) 
}

###Dia 11###
media_dia_11IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  media_dia_11IDC[[i]] <- mean(ifelse(Dia_11[6:8][[i]]<=0,NA,Dia_11[6:8][[i]]),
                               na.rm=T) 
}

max_dia_11IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_11[6:8])) {
  max_dia_11IDC[[i]] <- max(Dia_11[6:8][[i]]) 
}

###Dia 12###
media_dia_12IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  media_dia_12IDC[[i]] <- mean(ifelse(Dia_12[6:8][[i]]<=0,NA,Dia_12[6:8][[i]]),
                               na.rm=T) 
}

max_dia_12IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_12[6:8])) {
  max_dia_12IDC[[i]] <- max(Dia_12[6:8][[i]]) 
}

###Dia 13###
media_dia_13IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  media_dia_13IDC[[i]] <- mean(ifelse(Dia_13[6:8][[i]]<=0,NA,Dia_13[6:8][[i]]),
                               na.rm=T) 
}

max_dia_13IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_13[6:8])) {
  max_dia_13IDC[[i]] <- max(Dia_13[6:8][[i]]) 
}

###Dia 14###
media_dia_14IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  media_dia_14IDC[[i]] <- mean(ifelse(Dia_14[6:8][[i]]<=0,NA,Dia_14[6:8][[i]]),
                               na.rm=T) 
}

max_dia_14IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_14[6:8])) {
  max_dia_14IDC[[i]] <- max(Dia_14[6:8][[i]]) 
}

###Dia 15###
media_dia_15IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  media_dia_15IDC[[i]] <- mean(ifelse(Dia_15[6:8][[i]]<=0,NA,Dia_15[6:8][[i]]),
                               na.rm=T) 
}

max_dia_15IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_15[6:8])) {
  max_dia_15IDC[[i]] <- max(Dia_15[6:8][[i]]) 
}

###Dia 16###
media_dia_16IDC<-vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  media_dia_16IDC[[i]] <- mean(ifelse(Dia_16[6:8][[i]]<=0,NA,Dia_16[6:8][[i]]),
                               na.rm=T) 
}

max_dia_16IDC <- vector("numeric",  ncol(dfDC16[6:8]))
for (i in seq_along(Dia_16[6:8])) {
  max_dia_16IDC[[i]] <- max(Dia_16[6:8][[i]]) 
}



medias_diasIDC16 <- rbind(media_dia_1IDC,
                        media_dia_2IDC,media_dia_3IDC,media_dia_4IDC,
                        media_dia_5IDC,media_dia_6IDC,media_dia_7IDC,
                        media_dia_8IDC,media_dia_9IDC,media_dia_10IDC,
                        media_dia_11IDC,media_dia_12IDC,media_dia_13IDC,
                        media_dia_14IDC,media_dia_15IDC,media_dia_16IDC)
##View(medias_diasI)
medias_diasIDC16 <- cbind(levels(DiasF),medias_diasIDC16)
medias_diasIDC16 <- as.data.frame(medias_diasIDC16)
##View(medias_diasIDC16)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIDC16) <- nombres
medias_diasIDC16$Fecha <- as.Date(medias_diasIDC16$Fecha)
options(digits=5)
medias_diasIDC16$I1 <- as.numeric(medias_diasIDC16$I1)
medias_diasIDC16$I2 <- as.numeric(medias_diasIDC16$I2)
medias_diasIDC16$I3 <- as.numeric(medias_diasIDC16$I3)
##View(medias_diasIDC16)

max_diasIDC16 <- rbind(max_dia_1IDC,
                     max_dia_2IDC,max_dia_3IDC,max_dia_4IDC,
                     max_dia_5IDC,max_dia_6IDC,max_dia_7IDC,
                     max_dia_8IDC,max_dia_9IDC,max_dia_10IDC,
                     max_dia_11IDC,max_dia_12IDC,max_dia_13IDC,
                     max_dia_14IDC,max_dia_15IDC,max_dia_16IDC)
##View(max_diasIDC16)
max_diasIDC16 <- cbind(levels(DiasF),max_diasIDC16)
max_diasIDC16 <- as.data.frame(max_diasIDC16)
##View(max_diasIDC16)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIDC16) <- nombres
max_diasIDC16$Fecha <- as.Date(max_diasIDC16$Fecha)
options(digits=5)
max_diasIDC16$I1 <- as.numeric(max_diasIDC16$I1)
max_diasIDC16$I2 <- as.numeric(max_diasIDC16$I2)
max_diasIDC16$I3 <- as.numeric(max_diasIDC16$I3)
##View(max_diasIDC16)





##Diciembre 2017####
dfDC17 <- read.csv("Data/Years/2017/DIC2017.csv")
#View(dfDC17)
dfDC17 <- dfDC17[1:671,c(1:8)] 
dfDC17[3:8] <- sapply(dfDC17[3:8],as.numeric)  

dfDC17[3] <- dfDC17[3]/sqrt(3)
dfDC17[4] <- dfDC17[4]/sqrt(3)
dfDC17[5] <- dfDC17[5]/sqrt(3)

# dfDC17[6] <- dfDC17[6]*2
# dfDC17[7] <- dfDC17[7]*2
# dfDC17[8] <- dfDC17[8]*2



dfDC17$Fecha.de.la.Medida <- as.Date(dfDC17$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfDC17$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfDC17, Fecha.de.la.Medida==Dias[i]))
}
names(dfDC17) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")


mediaIDC <- vector("numeric", ncol(dfDC17[6:8]))
for (i in seq_along(dfDC17[6:8])) {# 2. secuencia
  mediaIDC[[i]] <- mean(ifelse(dfDC17[6:8][[i]]<=0,NA,dfDC17[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIDC<- vector("numeric", ncol(dfDC17[6:8]))
for (i in seq_along(dfDC17[6:8])) {# 2. secuencia
  maxIDC[[i]] <- max(dfDC17[6:8][[i]] )    # 3. cuerpo
}

##1-Diciembre a 8-Diciembre#
###Dia 1###
media_dia_1IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IDC[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IDC[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IDC[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IDC[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IDC[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IDC[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IDC[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IDC[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IDC[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IDC[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IDC[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IDC[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IDC[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IDC[[i]] <- max(Dia_7[6:8][[i]]) 
}

###Dia 8###
media_dia_8IDC<-vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  media_dia_8IDC[[i]] <- mean(ifelse(Dia_8[6:8][[i]]<=0,NA,Dia_8[6:8][[i]]),
                              na.rm=T) 
}

max_dia_8IDC <- vector("numeric",  ncol(dfDC17[6:8]))
for (i in seq_along(Dia_8[6:8])) {
  max_dia_8IDC[[i]] <- max(Dia_8[6:8][[i]]) 
}

medias_diasIDC17 <- rbind(media_dia_1IDC,
                        media_dia_2IDC,media_dia_3IDC,media_dia_4IDC,
                        media_dia_5IDC,media_dia_6IDC,media_dia_7IDC,
                        media_dia_8IDC)
##View(medias_diasI)
medias_diasIDC17 <- cbind(levels(DiasF),medias_diasIDC17)
medias_diasIDC17 <- as.data.frame(medias_diasIDC17)
##View(medias_diasIDC17)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIDC17) <- nombres
medias_diasIDC17$Fecha <- as.Date(medias_diasIDC17$Fecha)
options(digits=5)
medias_diasIDC17$I1 <- as.numeric(medias_diasIDC17$I1)
medias_diasIDC17$I2 <- as.numeric(medias_diasIDC17$I2)
medias_diasIDC17$I3 <- as.numeric(medias_diasIDC17$I3)
##View(medias_diasIDC17)

max_diasIDC17 <- rbind(max_dia_1IDC,
                     max_dia_2IDC,max_dia_3IDC,max_dia_4IDC,
                     max_dia_5IDC,max_dia_6IDC,max_dia_7IDC,
                     max_dia_8IDC)
##View(max_diasIDC17)
max_diasIDC17 <- cbind(levels(DiasF),max_diasIDC17)
max_diasIDC17 <- as.data.frame(max_diasIDC17)
##View(max_diasIDC17)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIDC17) <- nombres
max_diasIDC17$Fecha <- as.Date(max_diasIDC17$Fecha)
options(digits=5)
max_diasIDC17$I1 <- as.numeric(max_diasIDC17$I1)
max_diasIDC17$I2 <- as.numeric(max_diasIDC17$I2)
max_diasIDC17$I3 <- as.numeric(max_diasIDC17$I3)
##View(max_diasIDC17)




##Diciembre 2018####
dfDC18 <- read.csv("Data/Years/2018/DIC2018.csv")
#View(dfDC18)
dfDC18 <- dfDC18[1:642,c(1:8)] 
dfDC18[3:8] <- sapply(dfDC18[3:8],as.numeric)  

dfDC18[3] <- dfDC18[3]/sqrt(3)
dfDC18[4] <- dfDC18[4]/sqrt(3)
dfDC18[5] <- dfDC18[5]/sqrt(3)

# dfDC18[6] <- dfDC18[6]*2
# dfDC18[7] <- dfDC18[7]*2
# dfDC18[8] <- dfDC18[8]*2



dfDC18$Fecha.de.la.Medida <- as.Date(dfDC18$Fecha.de.la.Medida,format="%d/%m/%Y")
DiasF <- as.factor(dfDC18$Fecha.de.la.Medida)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfDC18, Fecha.de.la.Medida==Dias[i]))
}

names(dfDC18) <- c("Fecha","Hora","V1","V2","V3","I1","I2","I3")

mediaIDC <- vector("numeric", ncol(dfDC18[6:8]))
for (i in seq_along(dfDC18[6:8])) {# 2. secuencia
  mediaIDC[[i]] <- mean(ifelse(dfDC18[6:8][[i]]<=0,NA,dfDC18[6:8][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxIDC<- vector("numeric", ncol(dfDC18[6:8]))
for (i in seq_along(dfDC18[6:8])) {# 2. secuencia
  maxIDC[[i]] <- max(dfDC18[6:8][[i]] )    # 3. cuerpo
}

##1-Diciembre a 7-Diciembre#
###Dia 1###
media_dia_1IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  media_dia_1IDC[[i]] <- mean(ifelse(Dia_1[6:8][[i]]<=0,NA,Dia_1[6:8][[i]]),
                              na.rm=T) 
}

max_dia_1IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_1[6:8])) {
  max_dia_1IDC[[i]] <- max(Dia_1[6:8][[i]]) 
}
###Dia 2###
media_dia_2IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  media_dia_2IDC[[i]] <- mean(ifelse(Dia_2[6:8][[i]]<=0,NA,Dia_2[6:8][[i]]),
                              na.rm=T) 
}

max_dia_2IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_2[6:8])) {
  max_dia_2IDC[[i]] <- max(Dia_2[6:8][[i]]) 
}
###Dia 3###
media_dia_3IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  media_dia_3IDC[[i]] <- mean(ifelse(Dia_3[6:8][[i]]<=0,NA,Dia_3[6:8][[i]]),
                              na.rm=T) 
}

max_dia_3IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_3[6:8])) {
  max_dia_3IDC[[i]] <- max(Dia_3[6:8][[i]]) 
}

###Dia 4###
media_dia_4IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  media_dia_4IDC[[i]] <- mean(ifelse(Dia_4[6:8][[i]]<=0,NA,Dia_4[6:8][[i]]),
                              na.rm=T) 
}

max_dia_4IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_4[6:8])) {
  max_dia_4IDC[[i]] <- max(Dia_4[6:8][[i]]) 
}
###Dia 5###
media_dia_5IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  media_dia_5IDC[[i]] <- mean(ifelse(Dia_5[6:8][[i]]<=0,NA,Dia_5[6:8][[i]]),
                              na.rm=T) 
}

max_dia_5IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_5[6:8])) {
  max_dia_5IDC[[i]] <- max(Dia_5[6:8][[i]]) 
}
###Dia 6###
media_dia_6IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  media_dia_6IDC[[i]] <- mean(ifelse(Dia_6[6:8][[i]]<=0,NA,Dia_6[6:8][[i]]),
                              na.rm=T)
}

max_dia_6IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_6[6:8])) {
  max_dia_6IDC[[i]] <- max(Dia_6[6:8][[i]]) 
}
###Dia 7###
media_dia_7IDC<-vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  media_dia_7IDC[[i]] <- mean(ifelse(Dia_7[6:8][[i]]<=0,NA,Dia_7[6:8][[i]]),
                              na.rm=T) 
}

max_dia_7IDC <- vector("numeric",  ncol(dfDC18[6:8]))
for (i in seq_along(Dia_7[6:8])) {
  max_dia_7IDC[[i]] <- max(Dia_7[6:8][[i]]) 
}


medias_diasIDC18 <- rbind(media_dia_1IDC,
                        media_dia_2IDC,media_dia_3IDC,media_dia_4IDC,
                        media_dia_5IDC,media_dia_6IDC,media_dia_7IDC)
##View(medias_diasI)
medias_diasIDC18 <- cbind(levels(DiasF),medias_diasIDC18)
medias_diasIDC18 <- as.data.frame(medias_diasIDC18)
##View(medias_diasIDC18)
nombres <- c("Fecha","I1","I2","I3")
names(medias_diasIDC18) <- nombres
medias_diasIDC18$Fecha <- as.Date(medias_diasIDC18$Fecha)
options(digits=5)
medias_diasIDC18$I1 <- as.numeric(medias_diasIDC18$I1)
medias_diasIDC18$I2 <- as.numeric(medias_diasIDC18$I2)
medias_diasIDC18$I3 <- as.numeric(medias_diasIDC18$I3)
##View(medias_diasIDC18)

max_diasIDC18 <- rbind(max_dia_1IDC,
                     max_dia_2IDC,max_dia_3IDC,max_dia_4IDC,
                     max_dia_5IDC,max_dia_6IDC,max_dia_7IDC)
##View(max_diasIDC18)
max_diasIDC18 <- cbind(levels(DiasF),max_diasIDC18)
max_diasIDC18 <- as.data.frame(max_diasIDC18)
##View(max_diasIDC18)
nombres <- c("Fecha","I1","I2","I3")
names(max_diasIDC18) <- nombres
max_diasIDC18$Fecha <- as.Date(max_diasIDC18$Fecha)
options(digits=5)
max_diasIDC18$I1 <- as.numeric(max_diasIDC18$I1)
max_diasIDC18$I2 <- as.numeric(max_diasIDC18$I2)
max_diasIDC18$I3 <- as.numeric(max_diasIDC18$I3)
##View(max_diasIDC18)



#####
Diciembre <- rbind(medias_diasIDC13,medias_diasIDC14,medias_diasIDC15,
                   medias_diasIDC16,medias_diasIDC17,medias_diasIDC18)
DiciembreM <- rbind(max_diasIDC13,max_diasIDC14,max_diasIDC15,
                   max_diasIDC16,max_diasIDC17,max_diasIDC18)
dfDC <- rbind(dfDC13,dfDC14,dfDC15,dfDC16,dfDC17,dfDC18)





#####
library(lubridate)
Corriente <- rbind(Enero,Febrero,Marzo,Abril,Mayo,Junio,
                 Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre)
rownames(Corriente) <- NULL
Corriente$Fecha <-  as.Date(Corriente$Fecha,format="%d/%m/%Y")
Corriente$Ano <- as.numeric(format(Corriente$Fecha,'%Y'))
Corriente$Month <- as.factor(format(Corriente$Fecha,'%m'))
levels(Corriente$Month) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","
                           Julio","Agosto","Septiembre","Octubre","Noviembre",
                           "Diciembre")
#View(Corriente)
CorrienteF <- Corriente
CorrienteF$Fecha <- as.character(CorrienteF$Fecha)

#setwd("Data/files")

 # write.csv(Corriente,file = "Corriente.csv")
#  write.csv(CorrienteF,file = "CorrienteF.csv")

CorrienteM <- rbind(EneroM,FebreroM,MarzoM,AbrilM,MayoM,JunioM,
                  JulioM,AgostoM,SeptiembreM,OctubreM,NoviembreM,DiciembreM)
rownames(CorrienteM) <- NULL
CorrienteM$Fecha <-  as.Date(CorrienteM$Fecha,format="%d/%m/%Y")
CorrienteM$Ano <- as.numeric(format(CorrienteM$Fecha,'%Y'))
CorrienteM$Month <- as.factor(format(CorrienteM$Fecha,'%m'))
levels(CorrienteM$Month) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","
                           Julio","Agosto","Septiembre","Octubre","Noviembre",
                            "Diciembre")
#View(CorrienteM)
CorrienteFM <- CorrienteM
CorrienteFM$Fecha <- as.character(CorrienteFM$Fecha)

#write.csv(CorrienteM,file = "CorrienteM.csv")
#write.csv(CorrienteFM,file = "CorrienteFM.csv")

datos <- rbind(dfEN,dfFE,dfMA,dfAB,dfMY,dfJN,dfJL,dfAG,dfSP,dfOC,
               dfNV,dfDC)
rownames(datos) <- NULL
datos$Fecha <-  as.Date(datos$Fecha,format="%d/%m/%Y")
datos$Ano <- as.numeric(format(datos$Fecha,'%Y'))
datos$Month <- as.factor(format(datos$Fecha,'%m'))
levels(datos$Month) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","
                           Julio","Agosto","Septiembre","Octubre","Noviembre",
                              "Diciembre")
#View(datos)
#write.csv(datos,file = "datos.csv")






