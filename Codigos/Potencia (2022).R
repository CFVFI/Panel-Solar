library(dplyr)
###Enero####
dfEN22P <- read.csv("Data/Years/2022/Enero.csv")
#View(dfEN22P)
dfEN22P <- dfEN22P[1:3982,] 
dfEN22P[3:13] <- sapply(dfEN22P[3:13],as.numeric)  

dfEN22P$Fecha <- as.Date(dfEN22P$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfEN22P$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)


for (i in
     1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfEN22P, Fecha == Dias[i]))
}



####PENE1-PENE3###
mediaPENE <- vector("numeric", ncol(dfEN22P[10:13]))
for (i in seq_along(dfEN22P[10:13])) {# 2. secuencia
  mediaPENE[[i]] <- mean(ifelse(dfEN22P[10:13][[i]]<=0,NA,dfEN22P[10:13][[i]]),
                         na.rm = T )    # 3. cuerpo
}
maxPENE<- vector("numeric", ncol(dfEN22P[10:13]))
for (i in seq_along(dfEN22P[10:13])) {# 2. secuencia
  maxPENE[[i]] <- max(dfEN22P[10:13][[i]] )    # 3. cuerpo
}


###Dia 1###
media_dia_1PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_1[10:13])) {
  media_dia_1PENE[[i]] <- mean(ifelse(Dia_1[10:13][[i]]<=0,NA,Dia_1[10:13][[i]]),
                               na.rm=T)
}
max_dia_1PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_1[10:13])) {
  max_dia_1PENE[[i]] <- max(Dia_1[10:13][[i]])
}

###Dia 2###
media_dia_2PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_2[10:13])) {
  media_dia_2PENE[[i]] <- mean(ifelse(Dia_2[10:13][[i]]<=0,NA,Dia_2[10:13][[i]]),
                               na.rm=T)
}
max_dia_2PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_2[10:13])) {
  max_dia_2PENE[[i]] <- max(Dia_2[10:13][[i]])
}

###Dia 3###
media_dia_4PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_3[10:13])) {
  media_dia_4PENE[[i]] <- mean(ifelse(Dia_3[10:13][[i]]<=0,NA,Dia_3[10:13][[i]]),
                               na.rm=T)
}
max_dia_3PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_3[10:13])) {
  max_dia_3PENE[[i]] <- max(Dia_3[10:13][[i]])
}

###Dia 4###
media_dia_4PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  media_dia_4PENE[[i]] <- mean(ifelse(Dia_4[10:13][[i]]<=0,NA,Dia_4[10:13][[i]]),
                               na.rm=T)
}
max_dia_4PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  max_dia_4PENE[[i]] <- max(Dia_4[10:13][[i]])
}

###Dia 5###
media_dia_5PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  media_dia_5PENE[[i]] <- mean(ifelse(Dia_5[10:13][[i]]<=0,NA,Dia_5[10:13][[i]]),
                               na.rm=T)
}
max_dia_5PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  max_dia_5PENE[[i]] <- max(Dia_5[10:13][[i]])
}

###Dia 6###
media_dia_6PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  media_dia_6PENE[[i]] <- mean(ifelse(Dia_6[10:13][[i]]<=0,NA,Dia_6[10:13][[i]]),
                               na.rm=T)
}
max_dia_6PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  max_dia_6PENE[[i]] <- max(Dia_6[10:13][[i]])
}

###Dia 7###
media_dia_7PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  media_dia_7PENE[[i]] <- mean(ifelse(Dia_7[10:13][[i]]<=0,NA,Dia_7[10:13][[i]]),
                               na.rm=T)
}
max_dia_7PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  max_dia_7PENE[[i]] <- max(Dia_7[10:13][[i]])
}

###Dia 8###
media_dia_8PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  media_dia_8PENE[[i]] <- mean(ifelse(Dia_8[10:13][[i]]<=0,NA,Dia_8[10:13][[i]]),
                               na.rm=T)
}
max_dia_8PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  max_dia_8PENE[[i]] <- max(Dia_8[10:13][[i]])
}

###Dia 9###
media_dia_9PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  media_dia_9PENE[[i]] <- mean(ifelse(Dia_9[10:13][[i]]<=0,NA,Dia_9[10:13][[i]]),
                               na.rm=T)
}
max_dia_9PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  max_dia_9PENE[[i]] <- max(Dia_9[10:13][[i]])
}

###Dia 10###
media_dia_10PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  media_dia_10PENE[[i]] <- mean(ifelse(Dia_10[10:13][[i]]<=0,NA,Dia_10[10:13][[i]]),
                                na.rm=T)
}
max_dia_10PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  max_dia_10PENE[[i]] <- max(Dia_10[10:13][[i]])
}

###Dia 11###
media_dia_11PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  media_dia_11PENE[[i]] <- mean(ifelse(Dia_11[10:13][[i]]<=0,NA,Dia_11[10:13][[i]]),
                                na.rm=T)
}
max_dia_11PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  max_dia_11PENE[[i]] <- max(Dia_11[10:13][[i]])
}

###Dia 12###
media_dia_12PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  media_dia_12PENE[[i]] <- mean(ifelse(Dia_12[10:13][[i]]<=0,NA,Dia_12[10:13][[i]]),
                                na.rm=T)
}
max_dia_12PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  max_dia_12PENE[[i]] <- max(Dia_12[10:13][[i]])
}

###Dia 13###
media_dia_13PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  media_dia_13PENE[[i]] <- mean(ifelse(Dia_13[10:13][[i]]<=0,NA,Dia_13[10:13][[i]]),
                                na.rm=T)
}
max_dia_13PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  max_dia_13PENE[[i]] <- max(Dia_13[10:13][[i]])
}

###Dia 14###
media_dia_14PENE<-vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  media_dia_14PENE[[i]] <- mean(ifelse(Dia_14[10:13][[i]]<=0,NA,Dia_14[10:13][[i]]),
                                na.rm=T)
}
max_dia_14PENE <- vector("numeric",  ncol(dfEN22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  max_dia_14PENE[[i]] <- max(Dia_14[10:13][[i]])
}

medias_diasPEN22 <- rbind(media_dia_1PENE,media_dia_2PENE,media_dia_4PENE,
                          media_dia_4PENE,media_dia_5PENE,media_dia_6PENE,
                          media_dia_7PENE,media_dia_8PENE,media_dia_9PENE,
                          media_dia_10PENE,media_dia_11PENE,media_dia_12PENE,
                          media_dia_13PENE,media_dia_14PENE)
##View(medias_diasPEN22)
medias_diasPEN22 <- cbind(levels(DiasF),medias_diasPEN22)
medias_diasPEN22 <- as.data.frame(medias_diasPEN22)
##View(medias_diasPEN22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPEN22) <- nombres
medias_diasPEN22$Fecha <- as.Date(medias_diasPEN22$Fecha)
options(digits=5)
medias_diasPEN22$P1 <- as.numeric(medias_diasPEN22$P1)
medias_diasPEN22$P2 <- as.numeric(medias_diasPEN22$P2)
medias_diasPEN22$P3 <- as.numeric(medias_diasPEN22$P3)
medias_diasPEN22$PT <- as.numeric(medias_diasPEN22$PT)
rownames(medias_diasPEN22) <- NULL

##View(medias_diasPEN22)

max_diasPEN22 <- rbind(max_dia_1PENE,max_dia_2PENE,max_dia_3PENE,
                       max_dia_4PENE,max_dia_5PENE,max_dia_6PENE,
                       max_dia_7PENE,max_dia_8PENE,max_dia_9PENE,
                       max_dia_10PENE,max_dia_11PENE,max_dia_12PENE,
                       max_dia_13PENE,max_dia_14PENE)
##View(max_diasPEN22)
max_diasPEN22 <- cbind(levels(DiasF),max_diasPEN22)
max_diasPEN22 <- as.data.frame(max_diasPEN22)
##View(max_diasPEN22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPEN22) <- nombres
max_diasPEN22$Fecha <- as.Date(max_diasPEN22$Fecha)
options(digits=5)
max_diasPEN22$P1 <- as.numeric(max_diasPEN22$P1)
max_diasPEN22$P2 <- as.numeric(max_diasPEN22$P2)
max_diasPEN22$P3 <- as.numeric(max_diasPEN22$P3)
max_diasPEN22$PT <- as.numeric(max_diasPEN22$PT)
rownames(max_diasPEN22) <- NULL
##View(max_diasPEN22)

dfEN22P <- select(dfEN22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfEN22P) <- c('Fecha','Hora','P1','P2','P3','P3')
##Febrero####

dfFE22P <- read.csv("Data/Years/2022/Febrero.csv")
View(dfFE22P)

dfFE22P <- dfFE22P[1:1883,] 
dfFE22P[3:13] <- sapply(dfFE22P[3:13],as.numeric)  


dfFE22P$Fecha <- as.Date(dfFE22P$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfFE22P$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfFE22P, Fecha==Dias[i]))
}


mediaPFE <- vector("numeric", ncol(dfFE22P[10:13]))
for (i in seq_along(dfFE22P[10:13])) {# 2. secuencia
  mediaPFE[[i]] <- mean(ifelse(dfFE22P[10:13][[i]]<=0,NA,dfFE22P[10:13][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxPFE<- vector("numeric", ncol(dfFE22P[10:13]))
for (i in seq_along(dfFE22P[10:13])) {# 2. secuencia
  maxPFE[[i]] <- max(dfFE22P[10:13][[i]] )    # 3. cuerpo
}


###Dia 22###
media_dia_22PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_22[10:13])) {
  media_dia_22PFE[[i]] <- mean(ifelse(Dia_22[10:13][[i]]<=0,NA,Dia_22[10:13][[i]]),
                               na.rm=T) 
}

max_dia_22PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_22[10:13])) {
  max_dia_22PFE[[i]] <- max(Dia_22[10:13][[i]]) 
}
###Dia 23###
media_dia_23PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_23[10:13])) {
  media_dia_23PFE[[i]] <- mean(ifelse(Dia_23[10:13][[i]]<=0,NA,Dia_23[10:13][[i]]),
                               na.rm=T) 
}

max_dia_23PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_23[10:13])) {
  max_dia_23PFE[[i]] <- max(Dia_23[10:13][[i]]) 
}
###Dia 24###
media_dia_24PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_24[10:13])) {
  media_dia_24PFE[[i]] <- mean(ifelse(Dia_24[10:13][[i]]<=0,NA,Dia_24[10:13][[i]]),
                               na.rm=T) 
}

max_dia_24PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_24[10:13])) {
  max_dia_24PFE[[i]] <- max(Dia_24[10:13][[i]]) 
}
###Dia 25###
media_dia_25PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_25[10:13])) {
  media_dia_25PFE[[i]] <- mean(ifelse(Dia_25[10:13][[i]]<=0,NA,Dia_25[10:13][[i]]),
                               na.rm=T) 
}

max_dia_25PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_25[10:13])) {
  max_dia_25PFE[[i]] <- max(Dia_25[10:13][[i]]) 
}
###Dia 26###
media_dia_26PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_26[10:13])) {
  media_dia_26PFE[[i]] <- mean(ifelse(Dia_26[10:13][[i]]<=0,NA,Dia_26[10:13][[i]]),
                               na.rm=T) 
}

max_dia_26PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_26[10:13])) {
  max_dia_26PFE[[i]] <- max(Dia_26[10:13][[i]]) 
}

###Dia 27###
media_dia_27PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_27[10:13])) {
  media_dia_27PFE[[i]] <- mean(ifelse(Dia_27[10:13][[i]]<=0,NA,Dia_27[10:13][[i]]),
                               na.rm=T) 
}

max_dia_27PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_27[10:13])) {
  max_dia_27PFE[[i]] <- max(Dia_27[10:13][[i]]) 
}

###Dia 28###
media_dia_28PFE<-vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_28[10:13])) {
  media_dia_28PFE[[i]] <- mean(ifelse(Dia_28[10:13][[i]]<=0,NA,Dia_28[10:13][[i]]),
                               na.rm=T) 
}

max_dia_28PFE <- vector("numeric",  ncol(dfFE22P[10:13]))
for (i in seq_along(Dia_28[10:13])) {
  max_dia_28PFE[[i]] <- max(Dia_28[10:13][[i]]) 
}

medias_diasPFE22 <- rbind(media_dia_22PFE,media_dia_23PFE,media_dia_24PFE,
                          media_dia_25PFE,media_dia_26PFE,media_dia_27PFE,
                          media_dia_28PFE)
##View(medias_diasP)
medias_diasPFE22 <- cbind(levels(DiasF),medias_diasPFE22)
medias_diasPFE22 <- as.data.frame(medias_diasPFE22)
##View(medias_diasPFE22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPFE22) <- nombres
medias_diasPFE22$Fecha <- as.Date(medias_diasPFE22$Fecha)
options(digits=5)
medias_diasPFE22$P1 <- as.numeric(medias_diasPFE22$P1)
medias_diasPFE22$P2 <- as.numeric(medias_diasPFE22$P2)
medias_diasPFE22$P3 <- as.numeric(medias_diasPFE22$P3)
medias_diasPFE22$PT <- as.numeric(medias_diasPFE22$PT)
##View(medias_diasPFE22)

max_diasPFE22 <- rbind(max_dia_22PFE,max_dia_23PFE,max_dia_24PFE,
                       max_dia_25PFE,max_dia_26PFE,max_dia_27PFE,
                       max_dia_28PFE)
##View(max_diasPFE22)
max_diasPFE22 <- cbind(levels(DiasF),max_diasPFE22)
max_diasPFE22 <- as.data.frame(max_diasPFE22)
##View(max_diasPFE22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPFE22) <- nombres
max_diasPFE22$Fecha <- as.Date(max_diasPFE22$Fecha)
options(digits=5)
max_diasPFE22$P1 <- as.numeric(max_diasPFE22$P1)
max_diasPFE22$P2 <- as.numeric(max_diasPFE22$P2)
max_diasPFE22$P3 <- as.numeric(max_diasPFE22$P3)
max_diasPFE22$PT <- as.numeric(max_diasPFE22$PT)

dfFE22P <- select(dfFE22P,Fecha,Hora,P1..5.min.,P2..5.min., 
                  P3..5.min.,PT..5.min. )
names(dfFE22P) <- c('Fecha','Hora','P1','P2','P3','P3')
##Abril####

dfAB22P <- read.csv("Data/Years/2022//Abril.csv")
#View(dfAB22P)
dfAB22P <- dfAB22P[1:6932,] 
dfAB22P[3:13] <- sapply(dfAB22P[3:13],as.numeric)  

dfAB22P$Fecha <- as.Date(dfAB22P$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfAB22P$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfAB22P, Fecha==Dias[i]))
}


mediaPAB <- vector("numeric", ncol(dfAB22P[10:13]))
for (i in seq_along(dfAB22P[10:13])) {# 2. secuencia
  mediaPAB[[i]] <- mean(ifelse(dfAB22P[10:13][[i]]<=0,NA,dfAB22P[10:13][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxPAB<- vector("numeric", ncol(dfAB22P[10:13]))
for (i in seq_along(dfAB22P[10:13])) {# 2. secuencia
  maxPAB[[i]] <- max(dfAB22P[10:13][[i]] )    # 3. cuerpo
}

###abril 4 / abril 30

###Dia 4###
media_dia_4PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  media_dia_4PAB[[i]] <- mean(ifelse(Dia_4[10:13][[i]]<=0,NA,Dia_4[10:13][[i]]),
                              na.rm=T) 
}

max_dia_4PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  max_dia_4PAB[[i]] <- max(Dia_4[10:13][[i]]) 
}
###Dia 5###
media_dia_5PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  media_dia_5PAB[[i]] <- mean(ifelse(Dia_5[10:13][[i]]<=0,NA,Dia_5[10:13][[i]]),
                              na.rm=T) 
}

max_dia_5PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  max_dia_5PAB[[i]] <- max(Dia_5[10:13][[i]]) 
}
###Dia 6###
media_dia_6PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  media_dia_6PAB[[i]] <- mean(ifelse(Dia_6[10:13][[i]]<=0,NA,Dia_6[10:13][[i]]),
                              na.rm=T)
}

max_dia_6PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  max_dia_6PAB[[i]] <- max(Dia_6[10:13][[i]]) 
}
###Dia 7###
media_dia_7PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  media_dia_7PAB[[i]] <- mean(ifelse(Dia_7[10:13][[i]]<=0,NA,Dia_7[10:13][[i]]),
                              na.rm=T) 
}

max_dia_7PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  max_dia_7PAB[[i]] <- max(Dia_7[10:13][[i]]) 
}

###Dia 8###
media_dia_8PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  media_dia_8PAB[[i]] <- mean(ifelse(Dia_8[10:13][[i]]<=0,NA,Dia_8[10:13][[i]]),
                              na.rm=T) 
}

max_dia_8PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  max_dia_8PAB[[i]] <- max(Dia_8[10:13][[i]]) 
}

###Dia 9###
media_dia_9PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  media_dia_9PAB[[i]] <- mean(ifelse(Dia_9[10:13][[i]]<=0,NA,Dia_9[10:13][[i]]),
                              na.rm=T) 
}

max_dia_9PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  max_dia_9PAB[[i]] <- max(Dia_9[10:13][[i]]) 
}

###Dia 10###
media_dia_10PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  media_dia_10PAB[[i]] <- mean(ifelse(Dia_10[10:13][[i]]<=0,NA,Dia_10[10:13][[i]]),
                               na.rm=T) 
}

max_dia_10PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  max_dia_10PAB[[i]] <- max(Dia_10[10:13][[i]]) 
}

###Dia 11###
media_dia_11PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  media_dia_11PAB[[i]] <- mean(ifelse(Dia_11[10:13][[i]]<=0,NA,Dia_11[10:13][[i]]),
                               na.rm=T) 
}

max_dia_11PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  max_dia_11PAB[[i]] <- max(Dia_11[10:13][[i]]) 
}

###Dia 12###
media_dia_12PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  media_dia_12PAB[[i]] <- mean(ifelse(Dia_12[10:13][[i]]<=0,NA,Dia_12[10:13][[i]]),
                               na.rm=T) 
}

max_dia_12PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  max_dia_12PAB[[i]] <- max(Dia_12[10:13][[i]]) 
}

###Dia 13###
media_dia_13PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  media_dia_13PAB[[i]] <- mean(ifelse(Dia_13[10:13][[i]]<=0,NA,Dia_13[10:13][[i]]),
                               na.rm=T) 
}

max_dia_13PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  max_dia_13PAB[[i]] <- max(Dia_13[10:13][[i]]) 
}

###Dia 14###
media_dia_14PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  media_dia_14PAB[[i]] <- mean(ifelse(Dia_14[10:13][[i]]<=0,NA,Dia_14[10:13][[i]]),
                               na.rm=T) 
}

max_dia_14PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  max_dia_14PAB[[i]] <- max(Dia_14[10:13][[i]]) 
}

###Dia 15###
media_dia_15PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_15[10:13])) {
  media_dia_15PAB[[i]] <- mean(ifelse(Dia_15[10:13][[i]]<=0,NA,Dia_15[10:13][[i]]),
                               na.rm=T) 
}

max_dia_15PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_15[10:13])) {
  max_dia_15PAB[[i]] <- max(Dia_15[10:13][[i]]) 
}

###Dia 16###
media_dia_16PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_16[10:13])) {
  media_dia_16PAB[[i]] <- mean(ifelse(Dia_16[10:13][[i]]<=0,NA,Dia_16[10:13][[i]]),
                               na.rm=T) 
}

max_dia_16PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_16[10:13])) {
  max_dia_16PAB[[i]] <- max(Dia_16[10:13][[i]]) 
}

###Dia 17###
media_dia_17PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_17[10:13])) {
  media_dia_17PAB[[i]] <- mean(ifelse(Dia_17[10:13][[i]]<=0,NA,Dia_17[10:13][[i]]),
                               na.rm=T) 
}

max_dia_17PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_17[10:13])) {
  max_dia_17PAB[[i]] <- max(Dia_17[10:13][[i]]) 
}
###Dia 18###
media_dia_18PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_18[10:13])) {
  media_dia_18PAB[[i]] <- mean(ifelse(Dia_18[10:13][[i]]<=0,NA,Dia_18[10:13][[i]]),
                               na.rm=T) 
}

max_dia_18PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_18[10:13])) {
  max_dia_18PAB[[i]] <- max(Dia_18[10:13][[i]]) 
}

###Dia 19###
media_dia_19PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_19[10:13])) {
  media_dia_19PAB[[i]] <- mean(ifelse(Dia_19[10:13][[i]]<=0,NA,Dia_19[10:13][[i]]),
                               na.rm=T) 
}

max_dia_19PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_19[10:13])) {
  max_dia_19PAB[[i]] <- max(Dia_19[10:13][[i]]) 
}

###Dia 20###
media_dia_20PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_20[10:13])) {
  media_dia_20PAB[[i]] <- mean(ifelse(Dia_20[10:13][[i]]<=0,NA,Dia_20[10:13][[i]]),
                               na.rm=T) 
}

max_dia_20PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_20[10:13])) {
  max_dia_20PAB[[i]] <- max(Dia_20[10:13][[i]]) 
}

###Dia 21###
media_dia_21PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_21[10:13])) {
  media_dia_21PAB[[i]] <- mean(ifelse(Dia_21[10:13][[i]]<=0,NA,Dia_21[10:13][[i]]),
                               na.rm=T) 
}

max_dia_21PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_21[10:13])) {
  max_dia_21PAB[[i]] <- max(Dia_21[10:13][[i]]) 
}

###Dia 22###
media_dia_22PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_22[10:13])) {
  media_dia_22PAB[[i]] <- mean(ifelse(Dia_22[10:13][[i]]<=0,NA,Dia_22[10:13][[i]]),
                               na.rm=T) 
}

max_dia_22PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_22[10:13])) {
  max_dia_22PAB[[i]] <- max(Dia_22[10:13][[i]]) 
}

###Dia 23###
media_dia_23PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_23[10:13])) {
  media_dia_23PAB[[i]] <- mean(ifelse(Dia_23[10:13][[i]]<=0,NA,Dia_23[10:13][[i]]),
                               na.rm=T) 
}

max_dia_23PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_23[10:13])) {
  max_dia_23PAB[[i]] <- max(Dia_23[10:13][[i]]) 
}

###Dia 24###
media_dia_24PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_24[10:13])) {
  media_dia_24PAB[[i]] <- mean(ifelse(Dia_24[10:13][[i]]<=0,NA,Dia_24[10:13][[i]]),
                               na.rm=T) 
}

max_dia_24PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_24[10:13])) {
  max_dia_24PAB[[i]] <- max(Dia_24[10:13][[i]]) 
}

###Dia 25###
media_dia_25PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_25[10:13])) {
  media_dia_25PAB[[i]] <- mean(ifelse(Dia_25[10:13][[i]]<=0,NA,Dia_25[10:13][[i]]),
                               na.rm=T) 
}

max_dia_25PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_25[10:13])) {
  max_dia_25PAB[[i]] <- max(Dia_25[10:13][[i]]) 
}

###Dia 26###
media_dia_26PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_26[10:13])) {
  media_dia_26PAB[[i]] <- mean(ifelse(Dia_26[10:13][[i]]<=0,NA,Dia_26[10:13][[i]]),
                               na.rm=T) 
}

max_dia_26PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_26[10:13])) {
  max_dia_26PAB[[i]] <- max(Dia_26[10:13][[i]]) 
}

###Dia 27###
media_dia_27PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_27[10:13])) {
  media_dia_27PAB[[i]] <- mean(ifelse(Dia_27[10:13][[i]]<=0,NA,Dia_27[10:13][[i]]),
                               na.rm=T) 
}

max_dia_27PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_27[10:13])) {
  max_dia_27PAB[[i]] <- max(Dia_27[10:13][[i]]) 
}

###Dia 28###
media_dia_28PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_28[10:13])) {
  media_dia_28PAB[[i]] <- mean(ifelse(Dia_28[10:13][[i]]<=0,NA,Dia_28[10:13][[i]]),
                               na.rm=T) 
}

max_dia_28PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_28[10:13])) {
  max_dia_28PAB[[i]] <- max(Dia_28[10:13][[i]]) 
}

###Dia 29###
media_dia_29PAB<-vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_29[10:13])) {
  media_dia_29PAB[[i]] <- mean(ifelse(Dia_29[10:13][[i]]<=0,NA,Dia_29[10:13][[i]]),
                               na.rm=T) 
}

max_dia_29PAB <- vector("numeric",  ncol(dfAB22P[10:13]))
for (i in seq_along(Dia_29[10:13])) {
  max_dia_29PAB[[i]] <- max(Dia_29[10:13][[i]]) 
}



medias_diasPAB22 <- rbind(media_dia_4PAB,media_dia_5PAB,media_dia_6PAB,
                          media_dia_7PAB,media_dia_8PAB,media_dia_9PAB,
                          media_dia_10PAB,media_dia_11PAB,media_dia_12PAB,
                          media_dia_13PAB,media_dia_14PAB,media_dia_15PAB,
                          media_dia_16PAB,media_dia_17PAB,media_dia_18PAB,
                          media_dia_19PAB,media_dia_20PAB,media_dia_21PAB,
                          media_dia_22PAB,media_dia_23PAB,media_dia_24PAB,
                          media_dia_25PAB,media_dia_26PAB,media_dia_27PAB,
                          media_dia_28PAB,media_dia_29PAB)
##View(medias_diasP)
medias_diasPAB22 <- cbind(levels(DiasF),medias_diasPAB22)
medias_diasPAB22 <- as.data.frame(medias_diasPAB22)
##View(medias_diasPAB22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPAB22) <- nombres
medias_diasPAB22$Fecha <- as.Date(medias_diasPAB22$Fecha)
options(digits=5)
medias_diasPAB22$P1 <- as.numeric(medias_diasPAB22$P1)
medias_diasPAB22$P2 <- as.numeric(medias_diasPAB22$P2)
medias_diasPAB22$P3 <- as.numeric(medias_diasPAB22$P3)
medias_diasPAB22$PT <- as.numeric(medias_diasPAB22$PT)
##View(medias_diasPAB22)

max_diasPAB22 <- rbind(max_dia_4PAB,max_dia_5PAB,max_dia_6PAB,
                       max_dia_7PAB,max_dia_8PAB,max_dia_9PAB,
                       max_dia_10PAB,max_dia_11PAB,max_dia_12PAB,
                       max_dia_13PAB,max_dia_14PAB,max_dia_15PAB,
                       max_dia_16PAB,max_dia_17PAB,max_dia_18PAB,
                       max_dia_19PAB,max_dia_20PAB,max_dia_21PAB,
                       max_dia_22PAB,max_dia_23PAB,max_dia_24PAB,
                       max_dia_25PAB,max_dia_26PAB,max_dia_27PAB,
                       max_dia_28PAB,max_dia_29PAB)
##View(max_diasPAB22)
max_diasPAB22 <- cbind(levels(DiasF),max_diasPAB22)
max_diasPAB22 <- as.data.frame(max_diasPAB22)
##View(max_diasPAB22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPAB22) <- nombres
max_diasPAB22$Fecha <- as.Date(max_diasPAB22$Fecha)
options(digits=5)
max_diasPAB22$P1 <- as.numeric(max_diasPAB22$P1)
max_diasPAB22$P2 <- as.numeric(max_diasPAB22$P2)
max_diasPAB22$P3 <- as.numeric(max_diasPAB22$P3)
max_diasPAB22$PT <- as.numeric(max_diasPAB22$PT)

dfAB22P <- select(dfAB22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfAB22P) <- c('Fecha','Hora','P1','P2','P3','P3')
##View(max_diasPAB22)
#Mayo####


dfMY22P <- read.csv("Data/Years/2022//Mayo.csv")
#View(dfMY22P)
dfMY22P <- dfMY22P[1:5449,] 
dfMY22P[3:13] <- sapply(dfMY22P[3:13],as.numeric)  

dfMY22P$Fecha <- as.Date(dfMY22P$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfMY22P$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)



for (i in 1:length(date)) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq( min(as.numeric(format(date,'%d'))),max(as.numeric(format(date,'%d')))))))
  assign(Days[i],filter(dfMY22P, Fecha==Dias[i]))
}


mediaPMY <- vector("numeric", ncol(dfMY22P[10:13]))
for (i in seq_along(dfMY22P[10:13])) {# 2. secuencia
  mediaPMY[[i]] <- mean(ifelse(dfMY22P[10:13][[i]]<=0,NA,dfMY22P[10:13][[i]]),
                        na.rm = T )    # 3. cuerpo
}

maxPMY<- vector("numeric", ncol(dfMY22P[10:13]))
for (i in seq_along(dfMY22P[10:13])) {# 2. secuencia
  maxPMY[[i]] <- max(dfMY22P[10:13][[i]] )    # 3. cuerpo
}

##1-mayo a 16-mayo#
###Dia 2###
media_dia_1PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_1[10:13])) {
  media_dia_1PMY[[i]] <- mean(ifelse(Dia_1[10:13][[i]]<=0,NA,Dia_1[10:13][[i]]),
                              na.rm=T) 
}

max_dia_1PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_1[10:13])) {
  max_dia_1PMY[[i]] <- max(Dia_1[10:13][[i]]) 
}

###Dia 2###
media_dia_2PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_2[10:13])) {
  media_dia_2PMY[[i]] <- mean(ifelse(Dia_2[10:13][[i]]<=0,NA,Dia_2[10:13][[i]]),
                              na.rm=T) 
}

max_dia_2PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_2[10:13])) {
  max_dia_2PMY[[i]] <- max(Dia_2[10:13][[i]]) 
}
###Dia 3###
media_dia_4PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_3[10:13])) {
  media_dia_4PMY[[i]] <- mean(ifelse(Dia_3[10:13][[i]]<=0,NA,Dia_3[10:13][[i]]),
                              na.rm=T) 
}

max_dia_3PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_3[10:13])) {
  max_dia_3PMY[[i]] <- max(Dia_3[10:13][[i]]) 
}

###Dia 4###
media_dia_4PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  media_dia_4PMY[[i]] <- mean(ifelse(Dia_4[10:13][[i]]<=0,NA,Dia_4[10:13][[i]]),
                              na.rm=T) 
}

max_dia_4PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  max_dia_4PMY[[i]] <- max(Dia_4[10:13][[i]]) 
}
###Dia 5###
media_dia_5PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  media_dia_5PMY[[i]] <- mean(ifelse(Dia_5[10:13][[i]]<=0,NA,Dia_5[10:13][[i]]),
                              na.rm=T) 
}

max_dia_5PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  max_dia_5PMY[[i]] <- max(Dia_5[10:13][[i]]) 
}
###Dia 6###
media_dia_6PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  media_dia_6PMY[[i]] <- mean(ifelse(Dia_6[10:13][[i]]<=0,NA,Dia_6[10:13][[i]]),
                              na.rm=T)
}

max_dia_6PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  max_dia_6PMY[[i]] <- max(Dia_6[10:13][[i]]) 
}
###Dia 7###
media_dia_7PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  media_dia_7PMY[[i]] <- mean(ifelse(Dia_7[10:13][[i]]<=0,NA,Dia_7[10:13][[i]]),
                              na.rm=T) 
}

max_dia_7PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  max_dia_7PMY[[i]] <- max(Dia_7[10:13][[i]]) 
}

###Dia 8###
media_dia_8PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  media_dia_8PMY[[i]] <- mean(ifelse(Dia_8[10:13][[i]]<=0,NA,Dia_8[10:13][[i]]),
                              na.rm=T) 
}

max_dia_8PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  max_dia_8PMY[[i]] <- max(Dia_8[10:13][[i]]) 
}

###Dia 9###
media_dia_9PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  media_dia_9PMY[[i]] <- mean(ifelse(Dia_9[10:13][[i]]<=0,NA,Dia_9[10:13][[i]]),
                              na.rm=T) 
}

max_dia_9PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  max_dia_9PMY[[i]] <- max(Dia_9[10:13][[i]]) 
}

###Dia 10###
media_dia_10PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  media_dia_10PMY[[i]] <- mean(ifelse(Dia_10[10:13][[i]]<=0,NA,Dia_10[10:13][[i]]),
                               na.rm=T) 
}

max_dia_10PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  max_dia_10PMY[[i]] <- max(Dia_10[10:13][[i]]) 
}

###Dia 11###
media_dia_11PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  media_dia_11PMY[[i]] <- mean(ifelse(Dia_11[10:13][[i]]<=0,NA,Dia_11[10:13][[i]]),
                               na.rm=T) 
}

max_dia_11PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  max_dia_11PMY[[i]] <- max(Dia_11[10:13][[i]]) 
}

###Dia 12###
media_dia_12PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  media_dia_12PMY[[i]] <- mean(ifelse(Dia_12[10:13][[i]]<=0,NA,Dia_12[10:13][[i]]),
                               na.rm=T) 
}

max_dia_12PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  max_dia_12PMY[[i]] <- max(Dia_12[10:13][[i]]) 
}

###Dia 13###
media_dia_13PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  media_dia_13PMY[[i]] <- mean(ifelse(Dia_13[10:13][[i]]<=0,NA,Dia_13[10:13][[i]]),
                               na.rm=T) 
}

max_dia_13PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  max_dia_13PMY[[i]] <- max(Dia_13[10:13][[i]]) 
}

###Dia 14###
media_dia_14PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  media_dia_14PMY[[i]] <- mean(ifelse(Dia_14[10:13][[i]]<=0,NA,Dia_14[10:13][[i]]),
                               na.rm=T) 
}

max_dia_14PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  max_dia_14PMY[[i]] <- max(Dia_14[10:13][[i]]) 
}

###Dia 15###
media_dia_15PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_15[10:13])) {
  media_dia_15PMY[[i]] <- mean(ifelse(Dia_15[10:13][[i]]<=0,NA,Dia_15[10:13][[i]]),
                               na.rm=T) 
}

max_dia_15PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_15[10:13])) {
  max_dia_15PMY[[i]] <- max(Dia_15[10:13][[i]]) 
}

###Dia 16###
media_dia_16PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_16[10:13])) {
  media_dia_16PMY[[i]] <- mean(ifelse(Dia_16[10:13][[i]]<=0,NA,Dia_16[10:13][[i]]),
                               na.rm=T) 
}

max_dia_16PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_16[10:13])) {
  max_dia_16PMY[[i]] <- max(Dia_16[10:13][[i]]) 
}

###Dia 17###
media_dia_17PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_17[10:13])) {
  media_dia_17PMY[[i]] <- mean(ifelse(Dia_17[10:13][[i]]<=0,NA,Dia_17[10:13][[i]]),
                               na.rm=T) 
}

max_dia_17PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_17[10:13])) {
  max_dia_17PMY[[i]] <- max(Dia_17[10:13][[i]]) 
}

###Dia 18###
media_dia_18PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_18[10:13])) {
  media_dia_18PMY[[i]] <- mean(ifelse(Dia_18[10:13][[i]]<=0,NA,Dia_18[10:13][[i]]),
                               na.rm=T) 
}

max_dia_18PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_18[10:13])) {
  max_dia_18PMY[[i]] <- max(Dia_18[10:13][[i]]) 
}

###Dia 19###
media_dia_19PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_19[10:13])) {
  media_dia_19PMY[[i]] <- mean(ifelse(Dia_19[10:13][[i]]<=0,NA,Dia_19[10:13][[i]]),
                               na.rm=T) 
}

max_dia_19PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_19[10:13])) {
  max_dia_19PMY[[i]] <- max(Dia_19[10:13][[i]]) 
}

###Dia 20###
media_dia_20PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_20[10:13])) {
  media_dia_20PMY[[i]] <- mean(ifelse(Dia_20[10:13][[i]]<=0,NA,Dia_20[10:13][[i]]),
                               na.rm=T) 
}

max_dia_20PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_20[10:13])) {
  max_dia_20PMY[[i]] <- max(Dia_20[10:13][[i]]) 
}

###Dia 21###
media_dia_21PMY<-vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_21[10:13])) {
  media_dia_21PMY[[i]] <- mean(ifelse(Dia_21[10:13][[i]]<=0,NA,Dia_21[10:13][[i]]),
                               na.rm=T) 
}

max_dia_21PMY <- vector("numeric",  ncol(dfMY22P[10:13]))
for (i in seq_along(Dia_21[10:13])) {
  max_dia_21PMY[[i]] <- max(Dia_21[10:13][[i]]) 
}


medias_diasPMY22 <- rbind(media_dia_1PMY,
                          media_dia_2PMY,media_dia_4PMY,media_dia_4PMY,
                          media_dia_5PMY,media_dia_6PMY,media_dia_7PMY,
                          media_dia_8PMY,media_dia_9PMY,media_dia_10PMY,
                          media_dia_11PMY,media_dia_12PMY,media_dia_13PMY,
                          media_dia_14PMY,media_dia_15PMY,media_dia_16PMY,
                          media_dia_17PMY,media_dia_18PMY,media_dia_19PMY,
                          media_dia_20PMY,media_dia_21PMY
)
##View(medias_diasP)
medias_diasPMY22 <- cbind(levels(DiasF),medias_diasPMY22)
medias_diasPMY22 <- as.data.frame(medias_diasPMY22)
##View(medias_diasPMY22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPMY22) <- nombres
medias_diasPMY22$Fecha <- as.Date(medias_diasPMY22$Fecha)
options(digits=5)
medias_diasPMY22$P1 <- as.numeric(medias_diasPMY22$P1)
medias_diasPMY22$P2 <- as.numeric(medias_diasPMY22$P2)
medias_diasPMY22$P3 <- as.numeric(medias_diasPMY22$P3)
medias_diasPMY22$PT <- as.numeric(medias_diasPMY22$PT)
##View(medias_diasPMY22)

max_diasPMY22 <- rbind(max_dia_1PMY,
                       max_dia_2PMY,max_dia_3PMY,max_dia_4PMY,
                       max_dia_5PMY,max_dia_6PMY,max_dia_7PMY,
                       max_dia_8PMY,max_dia_9PMY,max_dia_10PMY,
                       max_dia_11PMY,max_dia_12PMY,max_dia_13PMY,
                       max_dia_14PMY,max_dia_15PMY,max_dia_16PMY,
                       max_dia_17PMY,max_dia_18PMY,max_dia_19PMY,
                       max_dia_20PMY,max_dia_21PMY)
##View(max_diasPMY22)
max_diasPMY22 <- cbind(levels(DiasF),max_diasPMY22)
max_diasPMY22 <- as.data.frame(max_diasPMY22)
##View(max_diasPMY22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPMY22) <- nombres
max_diasPMY22$Fecha <- as.Date(max_diasPMY22$Fecha)
options(digits=5)
max_diasPMY22$P1 <- as.numeric(max_diasPMY22$P1)
max_diasPMY22$P2 <- as.numeric(max_diasPMY22$P2)
max_diasPMY22$P3 <- as.numeric(max_diasPMY22$P3)
max_diasPMY22$PT <- as.numeric(max_diasPMY22$PT)
##View(max_diasPMY22)

dfMY22P <- select(dfMY22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfMY22P) <- c('Fecha','Hora','P1','P2','P3','P3')
##Junio####

dfJN22P <- read.csv("Data/Years/2022/Junio.csv")
##View(dfJN22P)
#str(dfJN22P)

dfJN22P <- dfJN22P[1:8328,]

dfJN22P$Fecha <- as.Date(dfJN22P$Fecha,format="%d/%m/%Y")
DiasF <- as.factor(dfJN22P$Fecha)
Dias<- levels(DiasF)
date <- as.Date(Dias)

dfJN22P[3:13] <-sapply(dfJN22P[3:13],as.numeric)

for (i in 
     min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  Days <- gsub("$","",gsub("^","Dia_",as.character( seq(1:length(Dias)))))
  assign(Days[i],filter(dfJN22P, Fecha==Dias[i]))
}




####P1-P3###
mediaP <- vector("numeric", ncol(dfJN22P[10:13]))


for (i in seq_along(dfJN22P[10:13])) {# 2. secuencia
  mediaP[[i]] <- mean(ifelse(dfJN22P[10:13][[i]]<=0,NA,dfJN22P[10:13][[i]]),
                      na.rm = T )    # 3. cuerpo
}

maxP<- vector("numeric", ncol(dfJN22P[10:13]))
for (i in seq_along(dfJN22P[10:13])) {# 2. secuencia
  maxP[[i]] <- max(dfJN22P[10:13][[i]] )    # 3. cuerpo
}


##01-junio a 10-junio###
###Dia 1###
media_dia_1P<-vector("numeric",  ncol(dfJN22P[10:13]))
media_dia_1P


for (i in seq_along(Dia_1[10:13])) {
  media_dia_1P[[i]] <- mean(ifelse(Dia_1[10:13][[i]]<=0,NA,Dia_1[10:13][[i]]),
                            na.rm=T) 
}


max_dia_1P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_1[10:13])) {
  max_dia_1P[[i]] <- max(Dia_1[10:13][[i]]) 
}

max_dia_1P


###Dia 2###
media_dia_2P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_2[10:13])) {
  media_dia_2P[[i]] <- mean(ifelse(Dia_2[10:13][[i]]<=0,NA,Dia_2[10:13][[i]]),
                            na.rm=T)
}

max_dia_2P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_2[10:13])) {
  max_dia_2P[[i]] <- max(Dia_2[10:13][[i]]) 
}


###Dia 3###
media_dia_4P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_3[10:13])) {
  media_dia_4P[[i]] <- mean(ifelse(Dia_3[10:13][[i]]<=0,NA,Dia_3[10:13][[i]]),
                            na.rm=T) 
}

max_dia_3P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_3[10:13])) {
  max_dia_3P[[i]] <- max(Dia_3[10:13][[i]]) 
}


###Dia 4###
media_dia_4P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_4[10:13])) {
  media_dia_4P[[i]] <- mean(ifelse(Dia_4[10:13][[i]]<=0,NA,Dia_4[10:13][[i]]),
                            na.rm=T) 
}

max_dia_4P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_4[10:13])) {
  max_dia_4P[[i]] <- max(Dia_4[10:13][[i]]) 
}



###Dia 5###

media_dia_5P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_5[10:13])) {
  media_dia_5P[[i]] <- mean(ifelse(Dia_5[10:13][[i]]<=0,NA,Dia_5[10:13][[i]]),
                            na.rm=T) 
}

max_dia_5P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_5[10:13])) {
  max_dia_5P[[i]] <- max(Dia_5[10:13][[i]]) 
}



###Dia 6###

media_dia_6P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_6[10:13])) {
  media_dia_6P[[i]] <- mean(ifelse(Dia_6[10:13][[i]]<=0,NA,Dia_6[10:13][[i]]),
                            na.rm=T) 
}

max_dia_6P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_6[10:13])) {
  max_dia_6P[[i]] <- max(Dia_6[10:13][[i]]) 
}


###Dia 7###

media_dia_7P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_7[10:13])) {
  media_dia_7P[[i]] <- mean(ifelse(Dia_7[10:13][[i]]<=0,NA,Dia_7[10:13][[i]]),
                            na.rm=T) 
}

max_dia_7P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_7[10:13])) {
  max_dia_7P[[i]] <- max(Dia_7[10:13][[i]]) 
}


###Dia 8 ###

media_dia_8P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_8[10:13])) {
  media_dia_8P[[i]] <- mean(ifelse(Dia_8[10:13][[i]]<=0,NA,Dia_8[10:13][[i]]),
                            na.rm=T) 
}

max_dia_8P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_8[10:13])) {
  max_dia_8P[[i]] <- max(Dia_8[10:13][[i]]) 
}


###Dia 9 ###

media_dia_9P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_9[10:13])) {
  media_dia_9P[[i]] <- mean(ifelse(Dia_9[10:13][[i]]<=0,NA,Dia_9[10:13][[i]]),
                            na.rm=T) 
}

max_dia_9P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_9[10:13])) {
  max_dia_9P[[i]] <- max(Dia_9[10:13][[i]]) 
}



###Dia 10 ###
media_dia_10P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_10[10:13])) {
  media_dia_10P[[i]] <- mean(ifelse(Dia_10[10:13][[i]]<=0,NA,Dia_10[10:13][[i]]),
                             na.rm=T) 
}

max_dia_10P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_10[10:13])) {
  max_dia_10P[[i]] <- max(Dia_10[10:13][[i]]) 
}





##11-junio a 20-junio###
###Dia 11###
media_dia_11P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_11[10:13])) {
  media_dia_11P[[i]] <- mean(ifelse(Dia_11[10:13][[i]]<=0,NA,Dia_11[10:13][[i]]),
                             na.rm=T) 
}

max_dia_11P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_11[10:13])) {
  max_dia_11P[[i]] <- max(Dia_11[10:13][[i]]) 
}



###Dia 12###
media_dia_12P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_12[10:13])) {
  media_dia_12P[[i]] <- mean(ifelse(Dia_12[10:13][[i]]<=0,NA,Dia_12[10:13][[i]]),
                             na.rm=T) 
}

max_dia_12P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_12[10:13])) {
  max_dia_12P[[i]] <- max(Dia_12[10:13][[i]]) 
}


###Dia 13###
media_dia_13P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_13[10:13])) {
  media_dia_13P[[i]] <- mean(ifelse(Dia_13[10:13][[i]]<=0,NA,Dia_13[10:13][[i]]),
                             na.rm=T) 
}

max_dia_13P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_13[10:13])) {
  max_dia_13P[[i]] <- max(Dia_13[10:13][[i]]) 
}



###Dia 14###
media_dia_14P<-vector("numeric",  ncol(dfJN22P[10:13]))


for (i in seq_along(Dia_14[10:13])) {
  media_dia_14P[[i]] <- mean(ifelse(Dia_14[10:13][[i]]<=0,NA,Dia_14[10:13][[i]]),
                             na.rm=T) 
}

max_dia_14P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_14[10:13])) {
  max_dia_14P[[i]] <- max(Dia_14[10:13][[i]]) 
}


###Dia 15###
media_dia_15P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_15[10:13])) {
  media_dia_15P[[i]] <- mean(ifelse(Dia_15[10:13][[i]]<=0,NA,Dia_15[10:13][[i]]),
                             na.rm=T) 
}

max_dia_15P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_15[10:13])) {
  max_dia_15P[[i]] <- max(Dia_15[10:13][[i]]) 
}


###Dia 16###
media_dia_16P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_16[10:13])) {
  media_dia_16P[[i]] <- mean(ifelse(Dia_16[10:13][[i]]<=0,NA,Dia_16[10:13][[i]]),
                             na.rm=T) 
}

max_dia_16P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_16[10:13])) {
  max_dia_16P[[i]] <- max(Dia_16[10:13][[i]]) 
}


###Dia 17###
media_dia_17P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_17[10:13])) {
  media_dia_17P[[i]] <- mean(ifelse(Dia_17[10:13][[i]]<=0,NA,Dia_17[10:13][[i]]),
                             na.rm=T) 
}

max_dia_17P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_17[10:13])) {
  max_dia_17P[[i]] <- max(Dia_17[10:13][[i]]) 
}


###Dia 18 ###
media_dia_18P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_18[10:13])) {
  media_dia_18P[[i]] <- mean(ifelse(Dia_18[10:13][[i]]<=0,NA,Dia_18[10:13][[i]]),
                             na.rm=T) 
}

max_dia_18P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_18[10:13])) {
  max_dia_18P[[i]] <- max(Dia_18[10:13][[i]]) 
}


###Dia 19 ###
media_dia_19P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_19[10:13])) {
  media_dia_19P[[i]] <- mean(ifelse(Dia_19[10:13][[i]]<=0,NA,Dia_19[10:13][[i]]),
                             na.rm=T) 
}

max_dia_19P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_19[10:13])) {
  max_dia_19P[[i]] <- max(Dia_19[10:13][[i]]) 
}


###Dia 20 ###
media_dia_20P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_20[10:13])) {
  media_dia_20P[[i]] <- mean(ifelse(Dia_20[10:13][[i]]<=0,NA,Dia_20[10:13][[i]]),
                             na.rm=T) 
}

max_dia_20P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_20[10:13])) {
  max_dia_20P[[i]] <- max(Dia_20[10:13][[i]]) 
}


##21-junio a 30-junio###
###Dia 21###
media_dia_21P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_21[10:13])) {
  media_dia_21P[[i]] <- mean(ifelse(Dia_21[10:13][[i]]<=0,NA,Dia_21[10:13][[i]]),
                             na.rm=T) 
}

max_dia_21P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_21[10:13])) {
  max_dia_21P[[i]] <- max(Dia_21[10:13][[i]]) 
}

###Dia 22###
media_dia_22P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_22[10:13])) {
  media_dia_22P[[i]] <- mean(ifelse(Dia_22[10:13][[i]]<=0,NA,Dia_22[10:13][[i]]),
                             na.rm=T) 
}

max_dia_22P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_22[10:13])) {
  max_dia_22P[[i]] <- max(Dia_22[10:13][[i]]) 
}

###Dia 23###
media_dia_23P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_23[10:13])) {
  media_dia_23P[[i]] <- mean(ifelse(Dia_23[10:13][[i]]<=0,NA,Dia_23[10:13][[i]]),
                             na.rm=T) 
}

max_dia_23P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_23[10:13])) {
  max_dia_23P[[i]] <- max(Dia_23[10:13][[i]]) 
}

###Dia24###
media_dia_24P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_24[10:13])) {
  media_dia_24P[[i]] <- mean(ifelse(Dia_24[10:13][[i]]<=0,NA,Dia_24[10:13][[i]]),
                             na.rm=T) 
}

max_dia_24P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_24[10:13])) {
  max_dia_24P[[i]] <- max(Dia_24[10:13][[i]]) 
}

###Dia 25###
media_dia_25P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_25[10:13])) {
  media_dia_25P[[i]] <- mean(ifelse(Dia_25[10:13][[i]]<=0,NA,Dia_25[10:13][[i]]),
                             na.rm=T) 
}

max_dia_25P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_25[10:13])) {
  max_dia_25P[[i]] <- max(Dia_25[10:13][[i]]) 
}

###Dia 26###
media_dia_26P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_26[10:13])) {
  media_dia_26P[[i]] <- mean(ifelse(Dia_26[10:13][[i]]<=0,NA,Dia_26[10:13][[i]]),
                             na.rm=T) 
}

max_dia_26P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_26[10:13])) {
  max_dia_26P[[i]] <- max(Dia_26[10:13][[i]]) 
}

###Dia 27###
media_dia_27P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_27[10:13])) {
  media_dia_27P[[i]] <- mean(ifelse(Dia_27[10:13][[i]]<=0,NA,Dia_27[10:13][[i]]),
                             na.rm=T) 
}

max_dia_27P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_27[10:13])) {
  max_dia_27P[[i]] <- max(Dia_27[10:13][[i]]) 
}

###Dia 28 ###
media_dia_28P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_28[10:13])) {
  media_dia_28P[[i]] <- mean(ifelse(Dia_28[10:13][[i]]<=0,NA,Dia_28[10:13][[i]]),
                             na.rm=T) 
}

max_dia_28P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_28[10:13])) {
  max_dia_28P[[i]] <- max(Dia_28[10:13][[i]]) 
}

###Dia 29 ###
media_dia_29P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_29[10:13])) {
  media_dia_29P[[i]] <- mean(ifelse(Dia_29[10:13][[i]]<=0,NA,Dia_29[10:13][[i]]),
                             na.rm=T) 
}

max_dia_29P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_29[10:13])) {
  max_dia_29P[[i]] <- max(Dia_29[10:13][[i]]) 
}

###Dia 30 ###
media_dia_30P<-vector("numeric",  ncol(dfJN22P[10:13]))

for (i in seq_along(Dia_30[10:13])) {
  media_dia_30P[[i]] <- mean(ifelse(Dia_30[10:13][[i]]<=0,NA,Dia_30[10:13][[i]]),
                             na.rm=T) 
}

max_dia_30P <- vector("numeric",  ncol(dfJN22P[10:13]))
for (i in seq_along(Dia_30[10:13])) {
  max_dia_30P[[i]] <- max(Dia_30[10:13][[i]]) 
}

medias_diasPJN22 <- rbind(media_dia_1P,media_dia_2P,media_dia_4P,media_dia_4P,
                      media_dia_5P,media_dia_6P,media_dia_7P,media_dia_8P,
                      media_dia_9P,media_dia_10P,media_dia_11P,media_dia_12P,
                      media_dia_13P,media_dia_14P,media_dia_15P,media_dia_16P,
                      media_dia_17P,media_dia_18P,media_dia_19P,media_dia_20P,
                      media_dia_21P,media_dia_22P,media_dia_23P,media_dia_24P,
                      media_dia_25P,media_dia_26P,media_dia_27P,media_dia_28P,
                      media_dia_29P,media_dia_30P)


##View(medias_diasPJN22)


medias_diasPJN22 <- cbind(levels(DiasF),medias_diasPJN22)
medias_diasPJN22 <- as.data.frame(medias_diasPJN22)
##View(medias_diasPJN22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPJN22) <- nombres
medias_diasPJN22$Fecha <- as.Date(medias_diasPJN22$Fecha)
options(digits=5)
medias_diasPJN22$P1 <- as.numeric(medias_diasPJN22$P1)
medias_diasPJN22$P2 <- as.numeric(medias_diasPJN22$P2)
medias_diasPJN22$P3 <- as.numeric(medias_diasPJN22$P3)
medias_diasPJN22$PT <- as.numeric(medias_diasPJN22$PT)
##View(medias_diasPJN22)

max_diasPJN22 <- rbind(max_dia_1P,max_dia_2P,max_dia_3P,max_dia_4P,
                   max_dia_5P,max_dia_6P,max_dia_7P,max_dia_8P,
                   max_dia_9P,max_dia_10P,max_dia_11P,max_dia_12P,
                   max_dia_13P,max_dia_14P,max_dia_15P,max_dia_16P,
                   max_dia_17P,max_dia_18P,max_dia_19P,max_dia_20P,
                   max_dia_21P,max_dia_22P,max_dia_23P,max_dia_24P,
                   max_dia_25P,max_dia_26P,max_dia_27P,max_dia_28P,
                   max_dia_29P,max_dia_30P)


##View(max_diasPJN22)


max_diasPJN22 <- cbind(levels(DiasF),max_diasPJN22)
max_diasPJN22 <- as.data.frame(max_diasPJN22)
##View(max_diasPJN22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPJN22) <- nombres
max_diasPJN22$Fecha <- as.Date(max_diasPJN22$Fecha)
options(digits=5)
max_diasPJN22$P1 <- as.numeric(max_diasPJN22$P1)
max_diasPJN22$P2 <- as.numeric(max_diasPJN22$P2)
max_diasPJN22$P3 <- as.numeric(max_diasPJN22$P3)
max_diasPJN22$PT <- as.numeric(max_diasPJN22$PT)
##View(max_diasP)

dfJN22P <- select(dfJN22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfJN22P) <- c('Fecha','Hora','P1','P2','P3','P3')



##Julio####

dfJL22P <- read.csv("Data/Years/2022/Julio.csv")
##View(dfJL22P)
#str(dfJL22P)

dfJL22P <- dfJL22P[1:8569,]

dfJL22P$Fecha <- as.Date(dfJL22P$Fecha,format="%d/%m/%Y")
DiasFJL <- as.factor(dfJL22P$Fecha)
DiasJL<- levels(DiasFJL)
date <- as.Date(DiasJL)


dfJL22P[3:13] <-sapply(dfJL22P[3:13],as.numeric)
dfJL22P[15:26] <- sapply(dfJL22P[15:26],as.numeric)

for (i in 
     min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysJL <- gsub("$","JL",gsub("^","Dia_",as.character( seq(1:length(DiasJL)))))
  assign(DaysJL[i],filter(dfJL22P, Fecha==DiasJL[i]))
}

####P1-P3###
mediaPJL <- vector("numeric", ncol(dfJL22P[10:13]))


for (i in seq_along(dfJL22P[10:13])) {# 2. secuencia
  mediaPJL[[i]] <- mean(dfJL22P[10:13][[i]])    # 3. cuerpo
}



maxPJL <- vector("numeric", ncol(dfJL22P[10:13]))

for (i in seq_along(dfJL22P[10:13])) {# 2. secuencia
  maxPJL[[i]] <- max(dfJL22P[10:13][[i]])     # 3. cuerpo
}




##01-junio a 10-junio###
###Dia 1###
media_dia_1PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_1JL[10:13])) {
  media_dia_1PJL[[i]] <- mean(ifelse(Dia_1JL[10:13][[i]]<=0,NA,Dia_1JL[10:13][[i]]),
                              na.rm = T)
}

max_dia_1PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_1JL[10:13])) {
  max_dia_1PJL[[i]] <- max(Dia_1JL[10:13][[i]])
}



###Dia 2###
media_dia_2PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_2JL[10:13])) {
  media_dia_2PJL[[i]] <- mean(ifelse(Dia_2JL[10:13][[i]]<=0,NA,Dia_2JL[10:13][[i]]),
                              na.rm = T) 
}


max_dia_2PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_2JL[10:13])) {
  max_dia_2PJL[[i]] <- max(Dia_2JL[10:13][[i]])
}



###Dia 3###
media_dia_4PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_3JL[10:13])) {
  media_dia_4PJL[[i]] <- mean(ifelse(Dia_3JL[10:13][[i]]<=0,NA,Dia_3JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_3PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_3JL[10:13])) {
  max_dia_3PJL[[i]] <- max(Dia_3JL[10:13][[i]])
}



###Dia 4###
media_dia_4PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_4JL[10:13])) {
  media_dia_4PJL[[i]] <- mean(ifelse(Dia_4JL[10:13][[i]]<=0,NA,Dia_4JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_4PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_4JL[10:13])) {
  max_dia_4PJL[[i]] <- max(Dia_4JL[10:13][[i]])
}


###Dia 5###

media_dia_5PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_5JL[10:13])) {
  media_dia_5PJL[[i]] <- mean(ifelse(Dia_5JL[10:13][[i]]<=0,NA,Dia_5JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_5PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_5JL[10:13])) {
  max_dia_5PJL[[i]] <- max(Dia_5JL[10:13][[i]])
}



###Dia 6###

media_dia_6PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_6JL[10:13])) {
  media_dia_6PJL[[i]] <- mean(ifelse(Dia_6JL[10:13][[i]]<=0,NA,Dia_6JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_6PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_6JL[10:13])) {
  max_dia_6PJL[[i]] <- max(Dia_6JL[10:13][[i]])
}



###Dia 7###

media_dia_7PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_7JL[10:13])) {
  media_dia_7PJL[[i]] <- mean(ifelse(Dia_7JL[10:13][[i]]<=0,NA,Dia_7JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_7PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_7JL[10:13])) {
  max_dia_7PJL[[i]] <- max(Dia_7JL[10:13][[i]])
}



###Dia 8 ###

media_dia_8PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_8JL[10:13])) {
  media_dia_8PJL[[i]] <- mean(ifelse(Dia_8JL[10:13][[i]]<=0,NA,Dia_8JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_8PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_8JL[10:13])) {
  max_dia_8PJL[[i]] <- max(Dia_8JL[10:13][[i]])
}



###Dia 9 ###

media_dia_9PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_9JL[10:13])) {
  media_dia_9PJL[[i]] <- mean(ifelse(Dia_9JL[10:13][[i]]<=0,NA,Dia_9JL[10:13][[i]]),
                              na.rm = T) 
}

max_dia_9PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_9JL[10:13])) {
  max_dia_9PJL[[i]] <- max(Dia_9JL[10:13][[i]])
}



###Dia 10 ###
media_dia_10PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_10JL[10:13])) {
  media_dia_10PJL[[i]] <- mean(ifelse(Dia_10JL[10:13][[i]]<=0,NA,Dia_10JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_10PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_10JL[10:13])) {
  max_dia_10PJL[[i]] <- max(Dia_10JL[10:13][[i]])
}






##11-junio a 20-junio###
###Dia 11###
media_dia_11PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_11JL[10:13])) {
  media_dia_11PJL[[i]] <- mean(ifelse(Dia_11JL[10:13][[i]]<=0,NA,Dia_11JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_11PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_11JL[10:13])) {
  max_dia_11PJL[[i]] <- max(Dia_11JL[10:13][[i]])
}

###Dia 12###
media_dia_12PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_12JL[10:13])) {
  media_dia_12PJL[[i]] <- mean(ifelse(Dia_12JL[10:13][[i]]<=0,NA,Dia_12JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_12PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_12JL[10:13])) {
  max_dia_12PJL[[i]] <- max(Dia_12JL[10:13][[i]])
}

###Dia 13###
media_dia_13PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_13JL[10:13])) {
  media_dia_13PJL[[i]] <- mean(ifelse(Dia_13JL[10:13][[i]]<=0,NA,Dia_13JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_13PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_13JL[10:13])) {
  max_dia_13PJL[[i]] <- max(Dia_13JL[10:13][[i]])
}


###Dia 14###
media_dia_14PJL<-vector("numeric",  ncol(dfJL22P[10:13]))


for (i in seq_along(Dia_14JL[10:13])) {
  media_dia_14PJL[[i]] <- mean(ifelse(Dia_14JL[10:13][[i]]<=0,NA,Dia_14JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_14PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_14JL[10:13])) {
  max_dia_14PJL[[i]] <- max(Dia_14JL[10:13][[i]])
}

###Dia 15###
media_dia_15PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_15JL[10:13])) {
  media_dia_15PJL[[i]] <- mean(ifelse(Dia_15JL[10:13][[i]]<=0,NA,Dia_15JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_15PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_15JL[10:13])) {
  max_dia_15PJL[[i]] <- max(Dia_15JL[10:13][[i]])
}

###Dia 16###
media_dia_16PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_16JL[10:13])) {
  media_dia_16PJL[[i]] <- mean(ifelse(Dia_16JL[10:13][[i]]<=0,NA,Dia_16JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_16PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_16JL[10:13])) {
  max_dia_16PJL[[i]] <- max(Dia_16JL[10:13][[i]])
}

###Dia 17###
media_dia_17PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_17JL[10:13])) {
  media_dia_17PJL[[i]] <- mean(ifelse(Dia_17JL[10:13][[i]]<=0,NA,Dia_17JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_17PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_17JL[10:13])) {
  max_dia_17PJL[[i]] <- max(Dia_17JL[10:13][[i]])
}

###Dia 18 ###
media_dia_18PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_18JL[10:13])) {
  media_dia_18PJL[[i]] <- mean(ifelse(Dia_18JL[10:13][[i]]<=0,NA,Dia_18JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_18PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_18JL[10:13])) {
  max_dia_18PJL[[i]] <- max(Dia_18JL[10:13][[i]])
}

###Dia 19 ###
media_dia_19PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_19JL[10:13])) {
  media_dia_19PJL[[i]] <- mean(ifelse(Dia_19JL[10:13][[i]]<=0,NA,Dia_19JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_19PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_19JL[10:13])) {
  max_dia_19PJL[[i]] <- max(Dia_19JL[10:13][[i]])
}

###Dia 20 ###
media_dia_20PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_20JL[10:13])) {
  media_dia_20PJL[[i]] <- mean(ifelse(Dia_20JL[10:13][[i]]<=0,NA,Dia_20JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_20PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_20JL[10:13])) {
  max_dia_20PJL[[i]] <- max(Dia_20JL[10:13][[i]])
}

##21-junio a 31-junio###
###Dia 21###
media_dia_21PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_21JL[10:13])) {
  media_dia_21PJL[[i]] <- mean(ifelse(Dia_21JL[10:13][[i]]<=0,NA,Dia_21JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_21PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_21JL[10:13])) {
  max_dia_21PJL[[i]] <- max(Dia_21JL[10:13][[i]])
}

###Dia 22###
media_dia_22PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_22JL[10:13])) {
  media_dia_22PJL <- mean(ifelse(Dia_22JL[10:13][[i]]<=0,NA,Dia_22JL[10:13][[i]]),
                          na.rm = T) 
}

max_dia_22PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_22JL[10:13])) {
  max_dia_22PJL[[i]] <- max(Dia_22JL[10:13][[i]])
}


###Dia 23###
media_dia_23PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_23JL[10:13])) {
  media_dia_23PJL[[i]] <- mean(ifelse(Dia_23JL[10:13][[i]]<=0,NA,Dia_23JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_23PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_23JL[10:13])) {
  max_dia_23PJL[[i]] <- max(Dia_23JL[10:13][[i]])
}


###Dia24###
media_dia_24PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_24JL[10:13])) {
  media_dia_24PJL[[i]] <- mean(ifelse(Dia_24JL[10:13][[i]]<=0,NA,Dia_24JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_24PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_24JL[10:13])) {
  max_dia_24PJL[[i]] <- max(Dia_24JL[10:13][[i]])
}


###Dia 25###
media_dia_25PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_25JL[10:13])) {
  media_dia_25PJL[[i]] <- mean(ifelse(Dia_25JL[10:13][[i]]<=0,NA,Dia_25JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_25PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_25JL[10:13])) {
  max_dia_25PJL[[i]] <- max(Dia_25JL[10:13][[i]])
}


###Dia 26###
media_dia_26PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_26JL[10:13])) {
  media_dia_26PJL[[i]] <- mean(ifelse(Dia_26JL[10:13][[i]]<=0,NA,Dia_26JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_26PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_26JL[10:13])) {
  max_dia_26PJL[[i]] <- max(Dia_26JL[10:13][[i]])
}


###Dia 27###
media_dia_27PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_27JL[10:13])) {
  media_dia_27PJL[[i]] <- mean(ifelse(Dia_27JL[10:13][[i]]<=0,NA,Dia_27JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_27PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_27JL[10:13])) {
  max_dia_27PJL[[i]] <- max(Dia_27JL[10:13][[i]])
}


###Dia 28 ###
media_dia_28PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_28JL[10:13])) {
  media_dia_28PJL[[i]] <- mean(ifelse(Dia_28JL[10:13][[i]]<=0,NA,Dia_28JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_28PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_28JL[10:13])) {
  max_dia_28PJL[[i]] <- max(Dia_28JL[10:13][[i]])
}


###Dia 29 ###
media_dia_29PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_29JL[10:13])) {
  media_dia_29PJL[[i]] <- mean(ifelse(Dia_29JL[10:13][[i]]<=0,NA,Dia_29JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_29PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_29JL[10:13])) {
  max_dia_29PJL[[i]] <- max(Dia_29JL[10:13][[i]])
}


###Dia 30 ###
media_dia_30PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_30JL[10:13])) {
  media_dia_30PJL[[i]] <- mean(ifelse(Dia_30JL[10:13][[i]]<=0,NA,Dia_30JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_30PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_30JL[10:13])) {
  max_dia_30PJL[[i]] <- max(Dia_30JL[10:13][[i]])
}


###Dia 31 ###
media_dia_31PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_31JL[10:13])) {
  media_dia_31PJL[[i]] <- mean(ifelse(Dia_31JL[10:13][[i]]<=0,NA,Dia_31JL[10:13][[i]]),
                               na.rm = T) 
}

max_dia_31PJL<-vector("numeric",  ncol(dfJL22P[10:13]))

for (i in seq_along(Dia_31JL[10:13])) {
  max_dia_31PJL[[i]] <- max(Dia_31JL[10:13][[i]])
}


medias_diasPJL22 <- rbind(media_dia_1PJL,media_dia_2PJL,media_dia_4PJL,media_dia_4PJL,
                          media_dia_5PJL,media_dia_6PJL,media_dia_7PJL,media_dia_8PJL,
                          media_dia_9PJL,media_dia_10PJL,media_dia_11PJL,media_dia_12PJL,
                          media_dia_13PJL,media_dia_14PJL,media_dia_15PJL,media_dia_16PJL,
                          media_dia_17PJL,media_dia_18PJL,media_dia_19PJL,media_dia_20PJL,
                          media_dia_21PJL,media_dia_22PJL,media_dia_23PJL,media_dia_24PJL,
                          media_dia_25PJL,media_dia_26PJL,media_dia_27PJL,media_dia_28PJL,
                          media_dia_29PJL,media_dia_30PJL,media_dia_31PJL)


#View(medias_diasPJL22)


medias_diasPJL22 <- cbind(levels(DiasFJL),medias_diasPJL22)
medias_diasPJL22 <- as.data.frame(medias_diasPJL22)
#View(medias_diasPJL22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPJL22) <- nombres
medias_diasPJL22$Fecha <- as.Date(medias_diasPJL22$Fecha)
options(digits=5)
medias_diasPJL22$P1 <- as.numeric(medias_diasPJL22$P1)
medias_diasPJL22$P2 <- as.numeric(medias_diasPJL22$P2)
medias_diasPJL22$P3 <- as.numeric(medias_diasPJL22$P3)
medias_diasPJL22$PT <- as.numeric(medias_diasPJL22$PT)
#View(medias_diasPJL22)



max_diasPJL22 <- rbind(max_dia_1PJL,max_dia_2PJL,max_dia_3PJL,max_dia_4PJL,
                       max_dia_5PJL,max_dia_6PJL,max_dia_7PJL,max_dia_8PJL,
                       max_dia_9PJL,max_dia_10PJL,max_dia_11PJL,max_dia_12PJL,
                       max_dia_13PJL,max_dia_14PJL,max_dia_15PJL,max_dia_16PJL,
                       max_dia_17PJL,max_dia_18PJL,max_dia_19PJL,max_dia_20PJL,
                       max_dia_21PJL,max_dia_22PJL,max_dia_23PJL,max_dia_24PJL,
                       max_dia_25PJL,max_dia_26PJL,max_dia_27PJL,max_dia_28PJL,
                       max_dia_29PJL,max_dia_30PJL,max_dia_31PJL)


#View(max_diasPJL22)


max_diasPJL22 <- cbind(levels(DiasFJL),max_diasPJL22)
max_diasPJL22 <- as.data.frame(max_diasPJL22)
#View(max_diasPJL22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPJL22) <- nombres
max_diasPJL22$Fecha <- as.Date(max_diasPJL22$Fecha)
options(digits=5)
max_diasPJL22$P1 <- as.numeric(max_diasPJL22$P1)
max_diasPJL22$P2 <- as.numeric(max_diasPJL22$P2)
max_diasPJL22$P3 <- as.numeric(max_diasPJL22$P3)
max_diasPJL22$PT <- as.numeric(max_diasPJL22$PT)
#View(max_diasPJL22)

dfJL22P <- select(dfJL22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfJL22P) <- c('Fecha','Hora','P1','P2','P3','P3')

##Agosto####

dfAG22P <- read.csv("Data/Years/2022/Agosto.csv")
##View(dfAG22P)
#str(dfAG22P)


dfAG22P$Fecha <- as.Date(dfAG22P$Fecha,format="%d/%m/%Y")
DiasFAG <- as.factor(dfAG22P$Fecha)
DiasAG<- levels(DiasFAG)
date <- as.Date(DiasAG)
dfAG22P[3:13] <-sapply(dfAG22P[3:13],as.numeric)

# for (i in 1:length(DiasAG)) {
#   DaysAG <- gsub("$","AG",gsub("^","Dia_",as.character( seq(1:length(DiasAG)))))
#   assign(DaysAG[i],filter(dfAG22P, Fecha==DiasAG[i]))
# }

for (i in min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysAG <- gsub("$","AG",gsub("^","Dia_",as.character( seq(1:length(DiasAG)))))
  assign(DaysAG[i],filter(dfAG22P, Fecha==DiasAG[i]))
}


####P1-P3###
mediaPAG <- vector("numeric", ncol(dfAG22P[10:13]))


for (i in seq_along(dfAG22P[10:13])) {# 2. secuencia
  mediaPAG[[i]] <- mean(dfAG22P[10:13][[i]])    # 3. cuerpo
}

mediaPAG

maxPAG <- vector("numeric", ncol(dfAG22P[10:13]))

for (i in seq_along(dfAG22P[10:13])) {# 2. secuencia
  maxPAG[[i]] <- max(dfAG22P[10:13][[i]])     # 3. cuerpo
}

maxPAG



##01-agosto a 10-agosto###
###Dia 1###
media_dia_1PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_1AG[10:13])) {
  media_dia_1PAG[[i]] <- mean(ifelse(Dia_1AG[10:13][[i]]<=0,NA,Dia_1AG[10:13][[i]]),
                              na.rm = T)
}

max_dia_1PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_1AG[10:13])) {
  max_dia_1PAG[[i]] <- max(Dia_1AG[10:13][[i]])
}



###Dia 2###
media_dia_2PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_2AG[10:13])) {
  media_dia_2PAG[[i]] <- mean(ifelse(Dia_2AG[10:13][[i]]<=0,NA,Dia_2AG[10:13][[i]]),
                              na.rm = T) 
}


max_dia_2PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_2AG[10:13])) {
  max_dia_2PAG[[i]] <- max(Dia_2AG[10:13][[i]])
}



###Dia 3###
media_dia_4PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_3AG[10:13])) {
  media_dia_4PAG[[i]] <- mean(ifelse(Dia_3AG[10:13][[i]]<=0,NA,Dia_3AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_3PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_3AG[10:13])) {
  max_dia_3PAG[[i]] <- max(Dia_3AG[10:13][[i]])
}



###Dia 4###
media_dia_4PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_4AG[10:13])) {
  media_dia_4PAG[[i]] <- mean(ifelse(Dia_4AG[10:13][[i]]<=0,NA,Dia_4AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_4PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_4AG[10:13])) {
  max_dia_4PAG[[i]] <- max(Dia_4AG[10:13][[i]])
}


###Dia 5###

media_dia_5PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_5AG[10:13])) {
  media_dia_5PAG[[i]] <- mean(ifelse(Dia_5AG[10:13][[i]]<=0,NA,Dia_5AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_5PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_5AG[10:13])) {
  max_dia_5PAG[[i]] <- max(Dia_5AG[10:13][[i]])
}



###Dia 6###

media_dia_6PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_6AG[10:13])) {
  media_dia_6PAG[[i]] <- mean(ifelse(Dia_6AG[10:13][[i]]<=0,NA,Dia_6AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_6PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_6AG[10:13])) {
  max_dia_6PAG[[i]] <- max(Dia_6AG[10:13][[i]])
}



###Dia 7###

media_dia_7PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_7AG[10:13])) {
  media_dia_7PAG[[i]] <- mean(ifelse(Dia_7AG[10:13][[i]]<=0,NA,Dia_7AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_7PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_7AG[10:13])) {
  max_dia_7PAG[[i]] <- max(Dia_7AG[10:13][[i]])
}



###Dia 8 ###

media_dia_8PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_8AG[10:13])) {
  media_dia_8PAG[[i]] <- mean(ifelse(Dia_8AG[10:13][[i]]<=0,NA,Dia_8AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_8PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_8AG[10:13])) {
  max_dia_8PAG[[i]] <- max(Dia_8AG[10:13][[i]])
}



###Dia 9 ###

media_dia_9PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_9AG[10:13])) {
  media_dia_9PAG[[i]] <- mean(ifelse(Dia_9AG[10:13][[i]]<=0,NA,Dia_9AG[10:13][[i]]),
                              na.rm = T) 
}

max_dia_9PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_9AG[10:13])) {
  max_dia_9PAG[[i]] <- max(Dia_9AG[10:13][[i]])
}



###Dia 10 ###
media_dia_10PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_10AG[10:13])) {
  media_dia_10PAG[[i]] <- mean(ifelse(Dia_10AG[10:13][[i]]<=0,NA,Dia_10AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_10PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_10AG[10:13])) {
  max_dia_10PAG[[i]] <- max(Dia_10AG[10:13][[i]])
}






##11-agosto a 20-agosto###
###Dia 11###
media_dia_11PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_11AG[10:13])) {
  media_dia_11PAG[[i]] <- mean(ifelse(Dia_11AG[10:13][[i]]<=0,NA,Dia_11AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_11PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_11AG[10:13])) {
  max_dia_11PAG[[i]] <- max(Dia_11AG[10:13][[i]])
}

###Dia 12###
media_dia_12PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_12AG[10:13])) {
  media_dia_12PAG[[i]] <- mean(ifelse(Dia_12AG[10:13][[i]]<=0,NA,Dia_12AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_12PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_12AG[10:13])) {
  max_dia_12PAG[[i]] <- max(Dia_12AG[10:13][[i]])
}

###Dia 13###
media_dia_13PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_13AG[10:13])) {
  media_dia_13PAG[[i]] <- mean(ifelse(Dia_13AG[10:13][[i]]<=0,NA,Dia_13AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_13PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_13AG[10:13])) {
  max_dia_13PAG[[i]] <- max(Dia_13AG[10:13][[i]])
}


###Dia 14###
media_dia_14PAG<-vector("numeric",  ncol(dfAG22P[10:13]))


for (i in seq_along(Dia_14AG[10:13])) {
  media_dia_14PAG[[i]] <- mean(ifelse(Dia_14AG[10:13][[i]]<=0,NA,Dia_14AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_14PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_14AG[10:13])) {
  max_dia_14PAG[[i]] <- max(Dia_14AG[10:13][[i]])
}

###Dia 15###
media_dia_15PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_15AG[10:13])) {
  media_dia_15PAG[[i]] <- mean(ifelse(Dia_15AG[10:13][[i]]<=0,NA,Dia_15AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_15PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_15AG[10:13])) {
  max_dia_15PAG[[i]] <- max(Dia_15AG[10:13][[i]])
}

###Dia 16###
media_dia_16PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_16AG[10:13])) {
  media_dia_16PAG[[i]] <- mean(ifelse(Dia_16AG[10:13][[i]]<=0,NA,Dia_16AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_16PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_16AG[10:13])) {
  max_dia_16PAG[[i]] <- max(Dia_16AG[10:13][[i]])
}

###Dia 17###
media_dia_17PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_17AG[10:13])) {
  media_dia_17PAG[[i]] <- mean(ifelse(Dia_17AG[10:13][[i]]<=0,NA,Dia_17AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_17PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_17AG[10:13])) {
  max_dia_17PAG[[i]] <- max(Dia_17AG[10:13][[i]])
}

###Dia 18 ###
media_dia_18PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_18AG[10:13])) {
  media_dia_18PAG[[i]] <- mean(ifelse(Dia_18AG[10:13][[i]]<=0,NA,Dia_18AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_18PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_18AG[10:13])) {
  max_dia_18PAG[[i]] <- max(Dia_18AG[10:13][[i]])
}

###Dia 19 ###
media_dia_19PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_19AG[10:13])) {
  media_dia_19PAG[[i]] <- mean(ifelse(Dia_19AG[10:13][[i]]<=0,NA,Dia_19AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_19PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_19AG[10:13])) {
  max_dia_19PAG[[i]] <- max(Dia_19AG[10:13][[i]])
}

###Dia 20 ###
media_dia_20PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_20AG[10:13])) {
  media_dia_20PAG[[i]] <- mean(ifelse(Dia_20AG[10:13][[i]]<=0,NA,Dia_20AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_20PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_20AG[10:13])) {
  max_dia_20PAG[[i]] <- max(Dia_20AG[10:13][[i]])
}

##21-agosto a 31-agosto###
###Dia 21###
media_dia_21PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_21AG[10:13])) {
  media_dia_21PAG[[i]] <- mean(ifelse(Dia_21AG[10:13][[i]]<=0,NA,Dia_21AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_21PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_21AG[10:13])) {
  max_dia_21PAG[[i]] <- max(Dia_21AG[10:13][[i]])
}

###Dia 22###
media_dia_22PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_22AG[10:13])) {
  media_dia_22PAG <- mean(ifelse(Dia_22AG[10:13][[i]]<=0,NA,Dia_22AG[10:13][[i]]),
                          na.rm = T) 
}

max_dia_22PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_22AG[10:13])) {
  max_dia_22PAG[[i]] <- max(Dia_22AG[10:13][[i]])
}


###Dia 23###
media_dia_23PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_23AG[10:13])) {
  media_dia_23PAG[[i]] <- mean(ifelse(Dia_23AG[10:13][[i]]<=0,NA,Dia_23AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_23PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_23AG[10:13])) {
  max_dia_23PAG[[i]] <- max(Dia_23AG[10:13][[i]])
}


###Dia24###
media_dia_24PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_24AG[10:13])) {
  media_dia_24PAG[[i]] <- mean(ifelse(Dia_24AG[10:13][[i]]<=0,NA,Dia_24AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_24PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_24AG[10:13])) {
  max_dia_24PAG[[i]] <- max(Dia_24AG[10:13][[i]])
}


###Dia 25###
media_dia_25PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_25AG[10:13])) {
  media_dia_25PAG[[i]] <- mean(ifelse(Dia_25AG[10:13][[i]]<=0,NA,Dia_25AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_25PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_25AG[10:13])) {
  max_dia_25PAG[[i]] <- max(Dia_25AG[10:13][[i]])
}


###Dia 26###
media_dia_26PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_26AG[10:13])) {
  media_dia_26PAG[[i]] <- mean(ifelse(Dia_26AG[10:13][[i]]<=0,NA,Dia_26AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_26PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_26AG[10:13])) {
  max_dia_26PAG[[i]] <- max(Dia_26AG[10:13][[i]])
}


###Dia 27###
media_dia_27PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_27AG[10:13])) {
  media_dia_27PAG[[i]] <- mean(ifelse(Dia_27AG[10:13][[i]]<=0,NA,Dia_27AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_27PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_27AG[10:13])) {
  max_dia_27PAG[[i]] <- max(Dia_27AG[10:13][[i]])
}


###Dia 28 ###
media_dia_28PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_28AG[10:13])) {
  media_dia_28PAG[[i]] <- mean(ifelse(Dia_28AG[10:13][[i]]<=0,NA,Dia_28AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_28PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_28AG[10:13])) {
  max_dia_28PAG[[i]] <- max(Dia_28AG[10:13][[i]])
}


###Dia 29 ###
media_dia_29PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_29AG[10:13])) {
  media_dia_29PAG[[i]] <- mean(ifelse(Dia_29AG[10:13][[i]]<=0,NA,Dia_29AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_29PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_29AG[10:13])) {
  max_dia_29PAG[[i]] <- max(Dia_29AG[10:13][[i]])
}


###Dia 30 ###
media_dia_30PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_30AG[10:13])) {
  media_dia_30PAG[[i]] <- mean(ifelse(Dia_30AG[10:13][[i]]<=0,NA,Dia_30AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_30PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_30AG[10:13])) {
  max_dia_30PAG[[i]] <- max(Dia_30AG[10:13][[i]])
}


###Dia 31 ###
media_dia_31PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_31AG[10:13])) {
  media_dia_31PAG[[i]] <- mean(ifelse(Dia_31AG[10:13][[i]]<=0,NA,Dia_31AG[10:13][[i]]),
                               na.rm = T) 
}

max_dia_31PAG<-vector("numeric",  ncol(dfAG22P[10:13]))

for (i in seq_along(Dia_31AG[10:13])) {
  max_dia_31PAG[[i]] <- max(Dia_31AG[10:13][[i]])
}


medias_diasPAG22 <- rbind(media_dia_1PAG,media_dia_2PAG,media_dia_4PAG,media_dia_4PAG,
                          media_dia_5PAG,media_dia_6PAG,media_dia_7PAG,media_dia_8PAG,
                          media_dia_9PAG,media_dia_10PAG,media_dia_11PAG,media_dia_12PAG,
                          media_dia_13PAG,media_dia_14PAG,media_dia_15PAG,media_dia_16PAG,
                          media_dia_17PAG,media_dia_18PAG,media_dia_19PAG,media_dia_20PAG,
                          media_dia_21PAG,media_dia_22PAG,media_dia_23PAG,media_dia_24PAG,
                          media_dia_25PAG,media_dia_26PAG,media_dia_27PAG,media_dia_28PAG,
                          media_dia_29PAG,media_dia_30PAG,media_dia_31PAG)


#View(medias_diasPAG22)


medias_diasPAG22 <- cbind(levels(DiasFAG),medias_diasPAG22)
medias_diasPAG22 <- as.data.frame(medias_diasPAG22)
#View(medias_diasPAG22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPAG22) <- nombres
medias_diasPAG22$Fecha <- as.Date(medias_diasPAG22$Fecha)
options(digits=5)
medias_diasPAG22$P1 <- as.numeric(medias_diasPAG22$P1)
medias_diasPAG22$P2 <- as.numeric(medias_diasPAG22$P2)
medias_diasPAG22$P3 <- as.numeric(medias_diasPAG22$P3)
medias_diasPAG22$PT <- as.numeric(medias_diasPAG22$PT)
#View(medias_diasPAG22)


#View(Jun_AgosP)
max_diasPAG22 <- rbind(max_dia_1PAG,max_dia_2PAG,max_dia_3PAG,max_dia_4PAG,
                       max_dia_5PAG,max_dia_6PAG,max_dia_7PAG,max_dia_8PAG,
                       max_dia_9PAG,max_dia_10PAG,max_dia_11PAG,max_dia_12PAG,
                       max_dia_13PAG,max_dia_14PAG,max_dia_15PAG,max_dia_16PAG,
                       max_dia_17PAG,max_dia_18PAG,max_dia_19PAG,max_dia_20PAG,
                       max_dia_21PAG,max_dia_22PAG,max_dia_23PAG,max_dia_24PAG,
                       max_dia_25PAG,max_dia_26PAG,max_dia_27PAG,max_dia_28PAG,
                       max_dia_29PAG,max_dia_30PAG,max_dia_31PAG)


#View(max_diasPAG22)

max_diasPAG22 <- cbind(levels(DiasFAG),max_diasPAG22)
max_diasPAG22 <- as.data.frame(max_diasPAG22)
#View(max_diasPAG22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPAG22) <- nombres
max_diasPAG22$Fecha <- as.Date(max_diasPAG22$Fecha)
options(digits=5)
max_diasPAG22$P1 <- as.numeric(max_diasPAG22$P1)
max_diasPAG22$P2 <- as.numeric(max_diasPAG22$P2)
max_diasPAG22$P3 <- as.numeric(max_diasPAG22$P3)
max_diasPAG22$PT <- as.numeric(max_diasPAG22$PT)
#View(max_diasPAG22)

dfAG22P <- select(dfAG22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfAG22P) <- c('Fecha','Hora','P1','P2','P3','P3')

##Septiembre####
dfSP22P <- read.csv("Data/Years/2022/Septiembre.csv")
#View(dfSP22P)



dfSP22P$Fecha <- as.Date(dfSP22P$Fecha,format="%d/%m/%Y")
DiasFSP <- as.factor(dfSP22P$Fecha)
DiasSP<- levels(DiasFSP)
date <- as.Date(DiasSP)
dfSP22P[3:13] <-sapply(dfSP22P[3:13],as.numeric)

for (i in min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysSP <- gsub("$","SP",gsub("^","Dia_",as.character( seq(1:length(DiasSP)))))
  assign(DaysSP[i],filter(dfSP22P, Fecha==DiasSP[i]))
}

####P1-P3###
mediaPSP <- vector("numeric", ncol(dfSP22P[10:13]))


for (i in seq_along(dfSP22P[10:13])) {# 2. secuencia
  mediaPSP[[i]] <- mean(dfSP22P[10:13][[i]])    # 3. cuerpo
}

mediaPSP

maxPSP <- vector("numeric", ncol(dfSP22P[10:13]))

for (i in seq_along(dfSP22P[10:13])) {# 2. secuencia
  maxPSP[[i]] <- max(dfSP22P[10:13][[i]])     # 3. cuerpo
}

maxPSP



##01-agosto a 10-agosto###
###Dia 1###
media_dia_1PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_1SP[10:13])) {
  media_dia_1PSP[[i]] <- mean(ifelse(Dia_1SP[10:13][[i]]<=0,NA,Dia_1SP[10:13][[i]]),
                              na.rm = T)
}

max_dia_1PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_1SP[10:13])) {
  max_dia_1PSP[[i]] <- max(Dia_1SP[10:13][[i]])
}



###Dia 2###
media_dia_2PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_2SP[10:13])) {
  media_dia_2PSP[[i]] <- mean(ifelse(Dia_2SP[10:13][[i]]<=0,NA,Dia_2SP[10:13][[i]]),
                              na.rm = T) 
}


max_dia_2PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_2SP[10:13])) {
  max_dia_2PSP[[i]] <- max(Dia_2SP[10:13][[i]])
}



###Dia 3###
media_dia_4PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_3SP[10:13])) {
  media_dia_4PSP[[i]] <- mean(ifelse(Dia_3SP[10:13][[i]]<=0,NA,Dia_3SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_3PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_3SP[10:13])) {
  max_dia_3PSP[[i]] <- max(Dia_3SP[10:13][[i]])
}



###Dia 4###
media_dia_4PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_4SP[10:13])) {
  media_dia_4PSP[[i]] <- mean(ifelse(Dia_4SP[10:13][[i]]<=0,NA,Dia_4SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_4PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_4SP[10:13])) {
  max_dia_4PSP[[i]] <- max(Dia_4SP[10:13][[i]])
}


###Dia 5###

media_dia_5PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_5SP[10:13])) {
  media_dia_5PSP[[i]] <- mean(ifelse(Dia_5SP[10:13][[i]]<=0,NA,Dia_5SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_5PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_5SP[10:13])) {
  max_dia_5PSP[[i]] <- max(Dia_5SP[10:13][[i]])
}



###Dia 6###

media_dia_6PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_6SP[10:13])) {
  media_dia_6PSP[[i]] <- mean(ifelse(Dia_6SP[10:13][[i]]<=0,NA,Dia_6SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_6PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_6SP[10:13])) {
  max_dia_6PSP[[i]] <- max(Dia_6SP[10:13][[i]])
}



###Dia 7###

media_dia_7PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_7SP[10:13])) {
  media_dia_7PSP[[i]] <- mean(ifelse(Dia_7SP[10:13][[i]]<=0,NA,Dia_7SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_7PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_7SP[10:13])) {
  max_dia_7PSP[[i]] <- max(Dia_7SP[10:13][[i]])
}



###Dia 8 ###

media_dia_8PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_8SP[10:13])) {
  media_dia_8PSP[[i]] <- mean(ifelse(Dia_8SP[10:13][[i]]<=0,NA,Dia_8SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_8PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_8SP[10:13])) {
  max_dia_8PSP[[i]] <- max(Dia_8SP[10:13][[i]])
}



###Dia 9 ###

media_dia_9PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_9SP[10:13])) {
  media_dia_9PSP[[i]] <- mean(ifelse(Dia_9SP[10:13][[i]]<=0,NA,Dia_9SP[10:13][[i]]),
                              na.rm = T) 
}

max_dia_9PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_9SP[10:13])) {
  max_dia_9PSP[[i]] <- max(Dia_9SP[10:13][[i]])
}



###Dia 10 ###
media_dia_10PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_10SP[10:13])) {
  media_dia_10PSP[[i]] <- mean(ifelse(Dia_10SP[10:13][[i]]<=0,NA,Dia_10SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_10PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_10SP[10:13])) {
  max_dia_10PSP[[i]] <- max(Dia_10SP[10:13][[i]])
}






##11-agosto a 20-agosto###
###Dia 11###
media_dia_11PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_11SP[10:13])) {
  media_dia_11PSP[[i]] <- mean(ifelse(Dia_11SP[10:13][[i]]<=0,NA,Dia_11SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_11PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_11SP[10:13])) {
  max_dia_11PSP[[i]] <- max(Dia_11SP[10:13][[i]])
}

###Dia 12###
media_dia_12PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_12SP[10:13])) {
  media_dia_12PSP[[i]] <- mean(ifelse(Dia_12SP[10:13][[i]]<=0,NA,Dia_12SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_12PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_12SP[10:13])) {
  max_dia_12PSP[[i]] <- max(Dia_12SP[10:13][[i]])
}

###Dia 13###
media_dia_13PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_13SP[10:13])) {
  media_dia_13PSP[[i]] <- mean(ifelse(Dia_13SP[10:13][[i]]<=0,NA,Dia_13SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_13PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_13SP[10:13])) {
  max_dia_13PSP[[i]] <- max(Dia_13SP[10:13][[i]])
}


###Dia 14###
media_dia_14PSP<-vector("numeric",  ncol(dfSP22P[10:13]))


for (i in seq_along(Dia_14SP[10:13])) {
  media_dia_14PSP[[i]] <- mean(ifelse(Dia_14SP[10:13][[i]]<=0,NA,Dia_14SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_14PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_14SP[10:13])) {
  max_dia_14PSP[[i]] <- max(Dia_14SP[10:13][[i]])
}

###Dia 15###
media_dia_15PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_15SP[10:13])) {
  media_dia_15PSP[[i]] <- mean(ifelse(Dia_15SP[10:13][[i]]<=0,NA,Dia_15SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_15PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_15SP[10:13])) {
  max_dia_15PSP[[i]] <- max(Dia_15SP[10:13][[i]])
}

###Dia 16###
media_dia_16PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_16SP[10:13])) {
  media_dia_16PSP[[i]] <- mean(ifelse(Dia_16SP[10:13][[i]]<=0,NA,Dia_16SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_16PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_16SP[10:13])) {
  max_dia_16PSP[[i]] <- max(Dia_16SP[10:13][[i]])
}

###Dia 17###
media_dia_17PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_17SP[10:13])) {
  media_dia_17PSP[[i]] <- mean(ifelse(Dia_17SP[10:13][[i]]<=0,NA,Dia_17SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_17PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_17SP[10:13])) {
  max_dia_17PSP[[i]] <- max(Dia_17SP[10:13][[i]])
}

###Dia 18 ###
media_dia_18PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_18SP[10:13])) {
  media_dia_18PSP[[i]] <- mean(ifelse(Dia_18SP[10:13][[i]]<=0,NA,Dia_18SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_18PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_18SP[10:13])) {
  max_dia_18PSP[[i]] <- max(Dia_18SP[10:13][[i]])
}

###Dia 19 ###
media_dia_19PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_19SP[10:13])) {
  media_dia_19PSP[[i]] <- mean(ifelse(Dia_19SP[10:13][[i]]<=0,NA,Dia_19SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_19PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_19SP[10:13])) {
  max_dia_19PSP[[i]] <- max(Dia_19SP[10:13][[i]])
}

###Dia 20 ###
media_dia_20PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_20SP[10:13])) {
  media_dia_20PSP[[i]] <- mean(ifelse(Dia_20SP[10:13][[i]]<=0,NA,Dia_20SP[10:13][[i]]),
                               na.rm = T) 
}

max_dia_20PSP<-vector("numeric",  ncol(dfSP22P[10:13]))

for (i in seq_along(Dia_20SP[10:13])) {
  max_dia_20PSP[[i]] <- max(Dia_20SP[10:13][[i]])
}


medias_diasPSP22 <- rbind(media_dia_1PSP,media_dia_2PSP,media_dia_4PSP,media_dia_4PSP,
                          media_dia_5PSP,media_dia_6PSP,media_dia_7PSP,media_dia_8PSP,
                          media_dia_9PSP,media_dia_10PSP,media_dia_11PSP,media_dia_12PSP,
                          media_dia_13PSP,media_dia_14PSP,media_dia_15PSP,media_dia_16PSP,
                          media_dia_17PSP,media_dia_18PSP,media_dia_19PSP,media_dia_20PSP)


#View(medias_diasPSP22)


medias_diasPSP22 <- cbind(levels(DiasFSP),medias_diasPSP22)
medias_diasPSP22 <- as.data.frame(medias_diasPSP22)
#View(medias_diasPSP22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPSP22) <- nombres
medias_diasPSP22$Fecha <- as.Date(medias_diasPSP22$Fecha)
options(digits=5)
medias_diasPSP22$P1 <- as.numeric(medias_diasPSP22$P1)
medias_diasPSP22$P2 <- as.numeric(medias_diasPSP22$P2)
medias_diasPSP22$P3 <- as.numeric(medias_diasPSP22$P3)
medias_diasPSP22$PT <- as.numeric(medias_diasPSP22$PT)
#View(medias_diasPSP22)

#View(Jun_AgosP)
max_diasPSP22 <- rbind(max_dia_1PSP,max_dia_2PSP,max_dia_3PSP,max_dia_4PSP,
                       max_dia_5PSP,max_dia_6PSP,max_dia_7PSP,max_dia_8PSP,
                       max_dia_9PSP,max_dia_10PSP,max_dia_11PSP,max_dia_12PSP,
                       max_dia_13PSP,max_dia_14PSP,max_dia_15PSP,max_dia_16PSP,
                       max_dia_17PSP,max_dia_18PSP,max_dia_19PSP,max_dia_20PSP)


#View(max_diasPSP22)

max_diasPSP22 <- cbind(levels(DiasFSP),max_diasPSP22)
max_diasPSP22 <- as.data.frame(max_diasPSP22)
#View(max_diasPSP22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPSP22) <- nombres
max_diasPSP22$Fecha <- as.Date(max_diasPSP22$Fecha)
options(digits=5)
max_diasPSP22$P1 <- as.numeric(max_diasPSP22$P1)
max_diasPSP22$P2 <- as.numeric(max_diasPSP22$P2)
max_diasPSP22$P3 <- as.numeric(max_diasPSP22$P3)
max_diasPSP22$PT <- as.numeric(max_diasPSP22$PT)
#View(max_diasPSP22)

dfSP22P <- select(dfSP22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfSP22P) <- c('Fecha','Hora','P1','P2','P3','P3')

##Octubre####

dfOC22P <- read.csv("Data/Years/2022/Octubre.csv")
#View(dfOC22P)
#rev(unique(dfOC22P$Fecha))


dfOC22P$Fecha <- as.Date(dfOC22P$Fecha,format="%d/%m/%Y")
DiasFOC <- as.factor(dfOC22P$Fecha)
DiasOC<- levels(DiasFOC)
date <- as.Date(DiasOC)
dfOC22P[3:9] <-sapply(dfOC22P[3:9],as.numeric)

for (i in min(as.numeric(format(date,'%d'))):max(as.numeric(format(date,'%d')))) {
  DaysOC <- gsub("$","OC",gsub("^","Dia_",as.character( seq(1:length(DiasOC)))))
  assign(DaysOC[i],filter(dfOC22P, Fecha==DiasOC[i]))
}

####P1-P3###
mediaPOC <- vector("numeric", ncol(dfOC22P[6:9]))


for (i in seq_along(dfOC22P[6:9])) {# 2. secuencia
  mediaPOC[[i]] <- mean(dfOC22P[6:9][[i]])    # 3. cuerpo
}

mediaPOC

maxPOC <- vector("numeric", ncol(dfOC22P[6:9]))

for (i in seq_along(dfOC22P[6:9])) {# 2. secuencia
  maxPOC[[i]] <- max(dfOC22P[6:9][[i]])     # 3. cuerpo
}

maxPOC



##01- a -agosto###
###Dia 1###
media_dia_1POC<-vector("numeric",  ncol(dfOC22P[6:9]))


for (i in seq_along(Dia_1OC[6:9])) {
  media_dia_1POC[[i]] <- mean(ifelse(Dia_1OC[6:9][[i]]<=0,NA,Dia_1OC[6:9][[i]]),
                              na.rm = T)
}

max_dia_1POC<-vector("numeric",  ncol(dfOC22P[6:9]))


for (i in seq_along(Dia_1OC[6:9])) {
  max_dia_1POC[[i]] <- max(Dia_1OC[6:9][[i]])
}



###Dia 2###
media_dia_2POC<-vector("numeric",  ncol(dfOC22P[6:9]))


for (i in seq_along(Dia_2OC[6:9])) {
  media_dia_2POC[[i]] <- mean(ifelse(Dia_2OC[6:9][[i]]<=0,NA,Dia_2OC[6:9][[i]]),
                              na.rm = T) 
}


max_dia_2POC<-vector("numeric",  ncol(dfOC22P[6:9]))


for (i in seq_along(Dia_2OC[6:9])) {
  max_dia_2POC[[i]] <- max(Dia_2OC[6:9][[i]])
}



###Dia 3###
media_dia_4POC<-vector("numeric",  ncol(dfOC22P[6:9]))

for (i in seq_along(Dia_3OC[6:9])) {
  media_dia_4POC[[i]] <- mean(ifelse(Dia_3OC[6:9][[i]]<=0,NA,Dia_3OC[6:9][[i]]),
                              na.rm = T) 
}

max_dia_3POC<-vector("numeric",  ncol(dfOC22P[6:9]))


for (i in seq_along(Dia_3OC[6:9])) {
  max_dia_3POC[[i]] <- max(Dia_3OC[6:9][[i]])
}




medias_diasPOC22 <- rbind(media_dia_1POC,media_dia_2POC,media_dia_4POC)


#View(medias_diasPOC22)


medias_diasPOC22 <- cbind(levels(DiasFOC),medias_diasPOC22)
medias_diasPOC22 <- as.data.frame(medias_diasPOC22)
#View(medias_diasPOC22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(medias_diasPOC22) <- nombres
medias_diasPOC22$Fecha <- as.Date(medias_diasPOC22$Fecha)
options(digits=5)
medias_diasPOC22$P1 <- as.numeric(medias_diasPOC22$P1)
medias_diasPOC22$P2 <- as.numeric(medias_diasPOC22$P2)
medias_diasPOC22$P3 <- as.numeric(medias_diasPOC22$P3)
medias_diasPOC22$PT <- as.numeric(medias_diasPOC22$PT)
#View(medias_diasPOC22)

#View(Jun_AgosP)
max_diasPOC22 <- rbind(max_dia_1POC,max_dia_2POC,max_dia_3POC)


#View(max_diasPOC22)

max_diasPOC22 <- cbind(levels(DiasFOC),max_diasPOC22)
max_diasPOC22 <- as.data.frame(max_diasPOC22)
#View(max_diasPOC22)
nombres <- c("Fecha","P1","P2","P3","PT")
names(max_diasPOC22) <- nombres
max_diasPOC22$Fecha <- as.Date(max_diasPOC22$Fecha)
options(digits=5)
max_diasPOC22$P1 <- as.numeric(max_diasPOC22$P1)
max_diasPOC22$P2 <- as.numeric(max_diasPOC22$P2)
max_diasPOC22$P3 <- as.numeric(max_diasPOC22$P3)
max_diasPOC22$PT <- as.numeric(max_diasPOC22$PT)
#View(max_diasPOC22)

dfOC22P <- select(dfOC22P,Fecha,Hora,P1..W...5.min.,P2..W...5.min., 
                  P3..W...5.min.,PT..W...5.min. )
names(dfOC22P) <- c('Fecha','Hora','P1','P2','P3','P3')

##Noviembre####
##Diciembre####


##RegistroP####
Potencia <- rbind(medias_diasPEN22,medias_diasPFE22,
                  medias_diasPAB22,medias_diasPMY22,medias_diasPJN22,
                  medias_diasPJL22,medias_diasPAG22,medias_diasPSP22,
                  medias_diasPOC22)
rownames(Potencia) <- NULL
Potencia$Fecha <-  as.Date(Potencia$Fecha,format="%d/%m/%Y")
Potencia$Ano <- as.numeric(format(Potencia$Fecha,'%Y'))
Potencia$Month <- as.factor(format(Potencia$Fecha,'%m'))
levels(Potencia$Month) <- c("Enero","Febrero","Abril","Mayo","Junio","Julio",
                            "Agosto","Septiembre","Octubre","Noviembre",
                             "Diciembre")


#View(Potencia)
PotenciaF <- Potencia
PotenciaF$Fecha <- as.character(PotenciaF$Fecha)

#setwd("Data/files")

#write.csv(Potencia,file = "Potencia.csv")
#write.csv(PotenciaF,file = "PotenciaF.csv")

PotenciaM <- rbind(max_diasPEN22,max_diasPFE22,
                   max_diasPAB22,max_diasPMY22,max_diasPJN22,
                   max_diasPJL22,max_diasPAG22,max_diasPSP22,
                   max_diasPOC22)
rownames(PotenciaM) <- NULL
PotenciaM$Fecha <-  as.Date(PotenciaM$Fecha,format="%d/%m/%Y")
PotenciaM$Ano <- as.numeric(format(PotenciaM$Fecha,'%Y'))
PotenciaM$Month <- as.factor(format(PotenciaM$Fecha,'%m'))
levels(PotenciaM$Month) <- c("Enero","Febrero","Abril","Mayo","Junio",
                             "Julio","Agosto","Septiembre","Octubre","Noviembre",
                              "Diciembre")
#View(PotenciaM)
PotenciaFM <- PotenciaM
PotenciaFM$Fecha <- as.character(PotenciaFM$Fecha)

#write.csv(PotenciaM,file = "PotenciaM.csv")
#write.csv(PotenciaFM,file = "PotenciaFM.csv")

datosC <- rbind(dfEN22P,dfFE22P,dfAB22P,dfMY22P,dfJN22P,dfJL22P,dfAG22P,dfSP22P,
                dfOC22P)
rownames(datosC) <- NULL
datosC$Fecha <-  as.Date(datosC$Fecha,format="%d/%m/%Y")
datosC$Ano <- as.numeric(format(datosC$Fecha,'%Y'))
datosC$Month <- as.factor(format(datosC$Fecha,'%m'))
levels(datosC$Month) <- c("Enero","Febrero","Abril","Mayo","Junio","Julio",
                          "Agosto","Septiembre","Octubre","Noviembre",
                         "Diciembre")
#View(datosC)

#write.csv(datosC,file = "datosC.csv")
