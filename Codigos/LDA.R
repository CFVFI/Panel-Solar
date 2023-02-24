View(datos_O)

##VANILLA####
 x1 =datos_O$I1
 x2 = datos_O$I2
 x3 = datos_O$I3
 summary(x1)
  summary(x2) 
summary(x3)  

library(psych)
describe.by(datos_O$I1,datos_O$Month)
describe.by(datos_O$I2,datos_O$Month)
describe.by(datos_O$I3,datos_O$Month)

library(MASS)
fit.LDA = lda(Month ~ I1+I2+I3,datos_O)
fit.LDA

fit.LDA.C = predict(fit.LDA,newdata = datos_O[,c(6,7,8)])$class
fit.LDA.C

table(datos_O$Month,fit.LDA.C)


df <- datos_O
df$Month <- as.factor(df$Month)
df$Fecha <- as.Date(df$Fecha,format="%d/%m/%Y")

h <- plot(df[,6:8],col=df$Month,pch=19)
##TRIMESTRE####

library(dplyr)
 
T1 <- df %>% 
  filter(Month== c('Enero','Febrero','Marzo')) %>% 
  mutate(Trimestre='1_trimestre')


T2 <- df %>% 
  filter(Month==c('Abril','Mayo','Junio')) %>% 
  mutate(Trimestre='2_trimestre')

T3 <- df %>% 
  filter(Month==c('Julio','Agosto','Septiembre')) %>% 
  mutate(Trimestre='3_trimestre')


T4 <- df %>% 
  filter(Month==c('Octubre','Noviembre','Diciembre')) %>% 
  mutate(Trimestre='4_trimestre')

df1 <- rbind(T1,T2,T3,T4)
df1$Trimestre <- as.factor(df1$Trimestre)

#plot(df1[,6:8],col=df1$Trimestre,pch=19)
View(df1)
     


x1 =df1$I1
x2 = df1$I2
x3 = df1$I3
summary(x1)
summary(x2) 
summary(x3)  

library(psych)
describe.by(df1$I1,df1$Month)
describe.by(df1$I2,df1$Month)
describe.by(df1$I3,df1$Month)

library(MASS)
fit.LDA = lda(Trimestre ~ I1+I2+I3,df1)
fit.LDA

fit.LDA.C = predict(fit.LDA,newdata = df1[,c(6,7,8)])$class
fit.LDA.C

table(df1$Trimestre,fit.LDA.C)

##ESTACIONES####
#Primeravera#### 


E1_13 <- df %>% 
  filter(Fecha>='2013-03-20' & Fecha <='2013-06-21') %>% 
  mutate(Estacion='Primavera')
E1_14 <- df %>% 
  filter(Fecha>='2014-03-20' & Fecha<='2014-06-21') %>% 
  mutate(Estacion='Primavera')
E1_15 <- df %>% 
  filter(Fecha>='2015-03-20' & Fecha<='2015-06-21') %>% 
  mutate(Estacion='Primavera')
E1_16 <- df %>% 
  filter(Fecha>='2016-03-20' & Fecha<='2016-06-21') %>% 
  mutate(Estacion='Primavera')
E1_17 <- df %>% 
  filter(Fecha>='2017-03-20' & Fecha<='2017-06-21') %>% 
  mutate(Estacion='Primavera')
E1_18 <- df %>% 
  filter(Fecha>='2018-03-20' & Fecha<='2018-06-21') %>% 
  mutate(Estacion='Primavera')
E1_22 <- df %>% 
  filter(Fecha>='2022-03-20' & Fecha<='2022-06-21') %>% 
  mutate(Estacion='Primavera')

E1 <- rbind(E1_13,E1_14,E1_15,E1_16,E1_17,E1_18,E1_22)


##Verano####
E2_13 <- df %>% 
  filter(Fecha>='2013-06-21' & Fecha<='2013-09-23') %>% 
  mutate(Estacion='Verano')
E2_14 <- df %>% 
  filter(Fecha>='2014-06-21' & Fecha<='2014-09-23') %>% 
  mutate(Estacion='Verano')
E2_15 <- df %>% 
  filter(Fecha>='2015-06-21' & Fecha<='2015-09-23') %>% 
  mutate(Estacion='Verano')
E2_16 <- df %>% 
  filter(Fecha>='2016-06-21' & Fecha<='2016-09-23') %>% 
  mutate(Estacion='Verano')
E2_17 <- df %>% 
  filter(Fecha>='2017-06-21' & Fecha<='2017-09-23') %>% 
  mutate(Estacion='Verano')
E2_18 <- df %>% 
  filter(Fecha>='2018-06-21' & Fecha<='2018-09-23') %>% 
  mutate(Estacion='Verano')
E2_22 <- df %>% 
  filter(Fecha>='2022-06-21' & Fecha<='2022-09-23') %>% 
  mutate(Estacion='Verano')

E2 <- rbind(E2_13,E2_14,E2_15,E2_16,E2_17,E2_18,E2_22)


##Otoño####
E3_13 <- df %>% 
  filter(Fecha>='2013-09-23' & Fecha<='2013-12-21') %>% 
  mutate(Estacion='Otoño')
E3_14 <- df %>% 
  filter(Fecha>='2014-09-23' & Fecha<='2014-12-21') %>% 
  mutate(Estacion='Otoño')
E3_15 <- df %>% 
  filter(Fecha>='2015-09-23' & Fecha<='2015-12-21') %>% 
  mutate(Estacion='Otoño')
E3_16 <- df %>% 
  filter(Fecha>='2016-09-23' & Fecha<='2016-12-21') %>% 
  mutate(Estacion='Otoño')
E3_17 <- df %>% 
  filter(Fecha>='2017-09-23' & Fecha<='2017-12-21') %>% 
  mutate(Estacion='Otoño')
E3_18 <- df %>% 
  filter(Fecha>='2018-09-23' & Fecha<='2018-12-21') %>% 
  mutate(Estacion='Otoño')
E3_22 <- df %>% 
  filter(Fecha>='2022-09-23' & Fecha<='2022-12-21') %>% 
  mutate(Estacion='Otoño')

E3 <- rbind(E3_13,E3_14,E3_15,E3_16,E3_17,E3_18,E3_22)

##Invierno####
E4_13 <- df %>% 
  filter(Fecha>='2013-12-21' & Fecha<='2014-03-20') %>% 
  mutate(Estacion='Invierno')
E4_14 <- df %>% 
  filter(Fecha>='2014-12-21' & Fecha<='2015-03-20') %>% 
  mutate(Estacion='Invierno')
E4_15 <- df %>% 
  filter(Fecha>='2015-12-21' & Fecha<='2016-03-20') %>% 
  mutate(Estacion='Invierno')
E4_16 <- df %>% 
  filter(Fecha>='2016-12-21' & Fecha<='2017-03-20') %>% 
  mutate(Estacion='Invierno')
E4_17 <- df %>% 
  filter(Fecha>='2017-12-21' & Fecha<='2018-03-20') %>% 
  mutate(Estacion='Invierno')
E4_18 <- df %>% 
  filter(Fecha>='2018-12-21' & Fecha<='2018-12-31') %>% 
  mutate(Estacion='Invierno')
E4_22 <- df %>% 
  filter(Fecha>='2022-12-21' & Fecha<='2014-03-20') %>% 
  mutate(Estacion='Invierno')

E4 <- rbind(E4_13,E4_14,E4_15,E4_16,E4_17,E4_18,E4_22)




df2 <- rbind(E1,E2,E3,E4) %>% 
  select(I1,I2,I3,Estacion)
 


df2$Estacion <- as.factor(df2$Estacion)

#plot(df2[,6:8],col=df2$Estacion,pch=19)
View(df2)



x1 =df2$I1
x2 = df2$I2
x3 = df2$I3
summary(x1)
summary(x2) 
summary(x3)  

library(psych)
describe.by(df2$I1,df2$Month)
describe.by(df2$I2,df2$Month)
describe.by(df2$I3,df2$Month)

library(MASS)
fit.LDA = lda(Estacion ~ I1+I2+I3,df2)
fit.LDA

fit.LDA.C = predict(fit.LDA,newdata = df2)
fit.LDA.C

table(df2$Estacion,fit.LDA.C$class)
g
library(ggpubr)
p1 <- ggplot(df2,aes(I1,fill=Estacion))+
  geom_histogram(position = 'identity',alpha=0.5)
p2 <- ggplot(df2,aes(I2,fill=Estacion))+
  geom_histogram(position = 'identity',alpha=0.5)
p3 <- ggplot(df2,aes(I3,fill=Estacion))+
  geom_histogram(position = 'identity',alpha=0.5)


