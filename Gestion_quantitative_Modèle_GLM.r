install.packages('xts')
install.packages('sp')
library(xts)
library(sp)
install.packages("vcd")
library(vcd)

install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)

install.packages('lsr')
library(lsr)
# Importer des données
data <- read.csv("C:/Users/liang/Desktop/Actuariat/Tarification_Github/Base original/Tarification_base_freq.csv", 
                 header = TRUE,   # si la première ligne contient les noms de colonnes
                 sep = ",")       # séparateur, normalement ',' pour CSV

data_c <- read.csv("C:/Users/liang/Desktop/Actuariat/Tarification_Github/Base original/Tarification_base_CM.csv", 
                 header = TRUE,   # si la première ligne contient les noms de colonnes
                 sep = ",")       # séparateur, normalement ',' pour CSV

data_frq <- data
data_cout <- data_c

data_frq
data_cout

head(data_cout)
data_cout

dim(data_frq) 
dim(data_cout) 

summary(data_frq)
summary(data_cout)

#la base freq
summary(data_frq)# statisque descriptive  pour le projet
str(data_frq) # type de variable
plot(density())
# etape 2 calculer la fr?quence et le cout
names(data_frq)# pour avoir le nom de toutes la variables des colonnes 
data_frq$ClaimNb
table(data_frq$ClaimNb)


# pour la frequence 
calcul_freq=sum(data_frq$ClaimNb)/sum(data_frq$Exposure)
print(calcul_freq)
#calcul du cout
calcul_cout=sum(data_cout$ClaimAmount)/sum(data_frq$ClaimNb)
print(calcul_cout)
pp=calcul_freq*calcul_cout
pp
#base frequence est agrege la base cout est detailler 
Cout=14633+2*726+3*28+3*4
Cout
#####

var_quanti=c(5,6,10)
var_quali=c(4,7,8,9)

install.packages('Hmisc')
library(Hmisc)
#les variables quantitatives
mcor <- cor(data_frq[,var_quanti])
mcor

nom<-colnames(data_frq[,var_quanti])
n<-dim(data_frq[,var_quanti])[2]
n
nom

CramerV(data_frq[,var_qual]) # pour les correlation oubien la boucle regrde avec lou
boxplot(data_frq$CarAge)


# Installer et charger vcd
install.packages("vcd")
library(vcd)

# Variables qualitatives (indices)
var_quali <- c(4,7,8,9)

# Boucle pour calculer Cramér's V pour toutes les paires
for(i in 1:(length(var_quali)-1)){
  for(j in (i+1):length(var_quali)){
    tab <- table(data_frq[, var_quali[i]], data_frq[, var_quali[j]])
    cat(colnames(data_frq)[var_quali[i]], "-", colnames(data_frq)[var_quali[j]], ":",
        CramerV(tab), "\n")
  }
}



# Installer et charger le package vcd
install.packages("vcd")   # à faire une seule fois
library(vcd)

# Variables qualitatives (indices)
var_quali <- c(4,7,8,9)

# Boucle pour calculer Cramér's V pour toutes les paires
for(i in 1:(length(var_quali)-1)){
  for(j in (i+1):length(var_quali)){
    tab <- table(data_frq[, var_quali[i]], data_frq[, var_quali[j]])
    cat(colnames(data_frq)[var_quali[i]], "-", colnames(data_frq)[var_quali[j]], ":",
        CramerV(tab), "\n")
  }
}

#pour les variables qualitatives
install.packages('lsr')
library(lsr)
CramerV(data_frq[,var_qual]) # pour les correlation oubien la boucle regrde avec lou
boxplot(data_frq$CarAge)

#il faut treacer le frequence et les couts

#frequence 
N=data_frq$ClaimNb
Expo=data_frq$Exposure
vec=data_frq$Brand
FUN.FREQ =function (N=data_frq$ClaimNb,Expo=data_frq$Exposure,vec,nm)
{
  R<- by(N,vec,sum)
  E<- by(Expo,vec,sum)
  
  name_R<-names(R)
  name_E<-names(E)
  
  R<- as.vector(R)
  E<- as.vector(E)
  
  FREQ<- R/E
  DATA=data.frame(FREQ=FREQ, EXP=E)
  rownames(DATA)<-name_R
  #graphique
  plot(c(1:dim(DATA)[1]),DATA[,1],type='h',axes=FALSE,col=3,lwd=1,main=paste('Frequence par',nm,sep=' '),xlab=paste(nm),ylab='Frequence')
  axis(side=1,at=c(1:dim(DATA)[1]),labels=rownames(DATA))
  axis(side=2,col ="gold",lty=2,lwd=0.5)
  box()
  
  DATA
}

str(data_frq)
FUN.FREQ(vec=data_frq$Brand,nm='Brand')      
FUN.FREQ(vec=data_frq$Gas,nm='Gas') 
FUN.FREQ(vec=data_frq$CarAge,nm='CarAge')
FUN.FREQ(vec=data_frq$Power,nm='Power')
FUN.FREQ(vec=data_frq$DriverAge,nm='DriverAge')
FUN.FREQ(vec=data_frq$Region,nm='Region')
FUN.FREQ(vec=data_frq$Density,nm='Density')
FREQ

FREQ.B=data_frq ###copie de la base 
FREQ.B

FREQ.B$DriveAge <- cut(data_frq$DriverAge,c(18,20,23,32,52,84,Inf),include.lowest = TRUE)
FREQ.B$DriveAge

FREQ.B$CarAge <- cut(data_frq$CarAge,c(0,18,27,Inf),include.lowest = TRUE)

FREQ.B$Density <- cut(data_frq$Density,c(2,363,1663,Inf),include.lowest = TRUE)

#############################
# Partie Fréquence 
# Power
#############################

FREQ.B$Power2=as.character(data_frq$Power)
FREQ.B$Power2[FREQ.B$Power %in% c("d")]<-"P1"
FREQ.B$Power2[FREQ.B$Power %in% c("e","f","l","g","h")]<-"P2"
FREQ.B$Power2[FREQ.B$Power %in% c("m","n","o","k","j","i")]<-"P3"

FREQ.B$Power= as.factor(FREQ.B$Power2)
FREQ.B=FREQ.B[,-dim(FREQ.B)[2]]# je suprime power 2
table(FREQ.B$Power)

#############################
# Region
#############################

FREQ.B$Region2=as.character(data_frq$Region)
FREQ.B$Region2[FREQ.B$Region %in% c("Aquitaine","Pays-de-la-Loire","Poitou-Charentes")]<-"R1"
FREQ.B$Region2[FREQ.B$Region %in% c("Centre","Basse-Normandie","Bretagne","Haute-Normandie")]<-"R2"
FREQ.B$Region2[FREQ.B$Region %in% c("Ile-de-France")]<-"R3"
FREQ.B$Region2[FREQ.B$Region %in% c("Limousin ","Nord-Pas-de-Calais")]<-"R4"

FREQ.B$Region= as.factor(FREQ.B$Region2)
FREQ.B=FREQ.B[,-dim(FREQ.B)[2]]
table(FREQ.B$Region)


#############################
# Brand
#############################

FREQ.B$Brand2=as.character(data_frq$Brand)
FREQ.B$Brand2[FREQ.B$Brand %in% c("Japanese (except Nissan) or Korean","Renault, Nissan or Citroen")]<-"V1"
FREQ.B$Brand2[FREQ.B$Brand %in% c("Fiat","other")]<-"V2"
FREQ.B$Brand2[FREQ.B$Brand %in% c("Mercedes, Chrysler or BMW","Opel, General Motors or Ford","Volkswagen, Audi, Skoda or Seat")]<-"V3"

FREQ.B$Brand= as.factor(FREQ.B$Brand2)
FREQ.B=FREQ.B[,-dim(FREQ.B)[2]]
table(FREQ.B$Brand)

table(FREQ.B$Brand)

#Power
sort(table(FREQ.B$Power))[length(table(FREQ.B$Power))]
FREQ.B$Power_m <- relevel(FREQ.B$Power,ref = "P2")

#Region
sort(table(FREQ.B$Region))[length(table(FREQ.B$Region))]
FREQ.B$Region_m <- relevel(FREQ.B$Region,ref = "R2")

#Brand
sort(table(FREQ.B$Brand))[length(table(FREQ.B$Brand))]
FREQ.B$Brand_m <- relevel(FREQ.B$Brand,ref = "V1")

sort(table(FREQ.B$DriveAge))[length(table(FREQ.B$DriveAge))]
FREQ.B$DriveAge_m <- relevel(FREQ.B$DriveAge,ref = "(32,52]")

sort(table(FREQ.B$CarAge))[length(table(FREQ.B$CarAge))]
FREQ.B$CarAge_m <- relevel(FREQ.B$CarAge,ref = "[0,18]")

sort(table(FREQ.B$Density))[length(table(FREQ.B$Density))]
FREQ.B$Density_m <- relevel(FREQ.B$Density,ref = "[2,363]")

sort(table(FREQ.B$Gas))[length(table(FREQ.B$Gas))]
FREQ.B$Gas_m <- relevel(FREQ.B$Gas,ref = "Regular")


#GLM
Model_REF=glm(ClaimNb ~  Power_m  +  Brand_m+   Region_m+  DriveAge_m+CarAge_m+Density_m+Gas_m
              +offset(Exposure), family=poisson(link = "log"), data=FREQ.B)
summary(Model_REF)
names(FREQ.B)

FREQ.B
#Sans region =< c'est tres bien 
Model_REF=glm(ClaimNb ~  Power_m  +  Brand_m+  DriveAge_m + CarAge_m + Density_m + Gas_m
              +offset(Exposure), family=poisson(link = "log"), data=FREQ.B)
summary(Model_REF)


#Sans density 
Model_REF=glm(ClaimNb ~  Power_m  +  Brand_m+  DriveAge_m + CarAge_m + Gas_m
              +offset(Exposure), family=poisson(link = "log"), data=FREQ.B)
summary(Model_REF)

#PRED_GlM<-predict(Model_cout,newdata = ,type = "response")
PRED_POIfreq<-predict(Model_REF,newdata =FREQ.B ,type = "response")
PRED_POIfreq
###################

#GLM
Model_REF=glm(ClaimNb ~  Power_m  +  Brand_m+   Region_m+  DriveAge_m+CarAge_m+Density_m+Gas_m
              +offset(Exposure), family=poisson(link = "log"), data=FREQ.B)
summary(Model_REF)

names(FREQ.B)
FREQ.B
#Sans region
Model_REF=glm(ClaimNb ~  Power_m  +  Brand_m+  DriveAge_m+CarAge_m+Density_m+Gas_m
              +offset(Exposure), family=poisson(link = "log"), data=FREQ.B)
summary(Model_REF)

###################

names(data_cout)
data_cout$ClaimAmount
summary(data_cout)


#######################
#  Modele Cout Moyen 
#######################

table(data_frq$ClaimNb)
Cout=14633+2*726+3*28+3*4
Cout
#la base cout est bonne je valide 
str(data_cout)
?merge
CM_M=merge(data_frq,data_cout, by = "PolicyID",all.y = TRUE)
dim(CM_M)
names(CM_M)
CM_M 

#Hypothese pour un gamma => charge suitun loi gamma
plot(density(CM_M$ClaimAmount))
summary(CM_M$ClaimAmount)

# Combien de sinistre est sup?rieur ? 500000, il n'y a que deux sup?rieur ? 500000
CM_M$ClaimAmount[which((CM_M$ClaimAmount>=500000))]

# 
#valeur extreme => seuil des graves  
install.packages("tea")
install.packages("eva")
library(tea)
help(tea)
?tea
Res1=dAMSE(CM_M$ClaimAmount)
Res2=DK(CM_M$ClaimAmount)
Res3=eye(CM_M$ClaimAmount)

Res1
Res2
Res3

#la fonction 
FIT_Dist=function(charge,seuils)
{
  k=length(seuils)
  par(mfrow=(2, k))
  for (i in 1 :k){
    plot(density(charge[which(charge<seuils[i])]),xlab='',main=paste('seuils<=',seuils[i],sep=''))
  }
  for (i in 1 :k){ 
    plot(density(log(charge[which(charge<seuils[i])])),xlab='',main=paste('seuils<=',seuils[i],sep=''))
  }
}

### --------------------------------------------------------------------###
#                    Graphique 
### --------------------------------------------------------------------###

table(CM_M$ClaimAmount)

CM_M$ClaimAmount[which((CM_M$ClaimAmount<950))]
sum(CM_M$ClaimAmount<950)

#seuil grave 7000 et seuil min 950
sum(CM_M$ClaimAmount[which((CM_M$ClaimAmount<950))])
sum(CM_M$ClaimAmount[which((CM_M$ClaimAmount<1000))])


sum(CM_M$ClaimAmount[which((CM_M$ClaimAmount<950),)]) #c'est le bon 
sum(CM_M[which((CM_M$ClaimAmount<950),])CM_M$ClainnB) 
sum(CM_M$ClaimAmount>17000)
sum(CM_M$ClaimAmount[which((CM_M$ClaimAmount>17000))]) 


##################################################################################################################
#######################                     Nouvelle Base                   ######################################
##################################################################################################################

CM_M.B=CM_M[which(CM_M$ClaimAmount>=950 & CM_M$ClaimAmount<=17000),]
CM_M.B

Res1=dAMSE(CM_M.B$ClaimAmount)
Res2=DK(CM_M.B$ClaimAmount)
Res3=eye(CM_M.B$ClaimAmount)

Res1
Res2
Res3
#la fonction 
FIT_Dist=function(charge,seuils)
{
  k=length(seuils)
  par(mfrow=(2, k))
  for (i in 1 :k){
    plot(density(charge[which(charge<seuils[i])]),xlab='',main=paste('seuils<=',seuils[i],sep=''))
  }
  for (i in 1 :k){ 
    plot(density(log(charge[which(charge<seuils[i])])),xlab='',main=paste('seuils<=',seuils[i],sep=''))
  }
}

table(CM_M.B$ClaimAmount)
names(CM_M.B)

#### la fonction    
CM_M.B$ClaimNb=1
names(CM_M.B)
FUN.cou =function(N=CM_M.B$ClaimAmount,Expo=CM_M.B$ClaimNb,vec,nm)
{
  R<- by(N,vec,sum)
  E<- by(Expo,vec,sum)
  
  name_R<-names(R)
  name_E<-names(E)
  
  R<- as.vector(R)
  E<- as.vector(E)
  
  cou <- R/E
  DATA=data.frame(Cout=cou, NBR=E)
  rownames(DATA)<-name_R
  #graphique
  plot(c(1:dim(DATA)[1]),DATA[,1],type='h',axes=FALSE,col=4,lwd=1,main=paste('cout_moyen par',nm,sep=' '),xlab=paste(nm),ylab='cout_moyen')
  axis(side=1,at=c(1:dim(DATA)[1]),labels=rownames(DATA))
  axis(side=2,col ="gold",lty=2,lwd=0.5)
  box()
  
  DATA
}
#decoupage pour modelisation de CM

FUN.cou(vec=CM_M.B$Brand,nm='Brand') 
FUN.cou(vec=CM_M.B$Gas,nm='Gas')
FUN.cou(vec=CM_M.B$CarAge,nm='CarAge')
FUN.cou(vec=CM_M.B$Power,nm='Power')
FUN.cou(vec=CM_M.B$DriverAge,nm='DriverAge')
FUN.cou(vec=CM_M.B$Region,nm='Region')
FUN.cou(vec=CM_M.B$Density,nm='Density')

CM_M.B2=CM_M.B ###copie de la base 
names(CM_M.B2)

CM_M.B2$DriveAge <- cut(CM_M.B$DriverAge,c(18,27,48,Inf),include.lowest = TRUE)
CM_M.B2$DriveAge

CM_M.B2$CarAge <- cut(CM_M.B$CarAge,c(0,10,21,Inf),include.lowest = TRUE)
CM_M.B2$CarAge

CM_M.B2$Density <- cut(CM_M.B$Density,c(2,363,1663,Inf),include.lowest = TRUE)
CM_M.B2$Density

str(CM_M.B2)

############
# Power   #
############

CM_M.B2$Power3=as.character(CM_M.B$Power)

CM_M.B2$Power3[CM_M.B2$Power %in% c("d","n")]<-"P1"
CM_M.B2$Power3[CM_M.B2$Power %in% c("e","f","h")]<-"P2"
CM_M.B2$Power3[CM_M.B2$Power %in% c("m","g","k","j","i")]<-"P3"
CM_M.B2$Power3[CM_M.B2$Power %in% c("o","l")]<-"P4"

# On suprime power 2
CM_M.B2$Power= as.factor(CM_M.B2$Power3)
CM_M.B2=CM_M.B2[,-dim(CM_M.B2)[2]]# je suprime power 2
table(CM_M.B2$Power)

############
# Region   #
############

CM_M.B2$Region3=as.character(CM_M.B$Region)
CM_M.B2$Region3[CM_M.B2$Region %in% c("Limousin ","Pays-de-la-Loire","Basse-Normandie",)]<-"R1"
CM_M.B2$Region3[CM_M.B2$Region %in% c("Pays-de-la-Loire","Poitou-Charentes")]<-"R2"
CM_M.B2$Region3[CM_M.B2$Region %in% c("Centre","Bretagne","Ile-de-France")]<-"R3"
CM_M.B2$Region3[CM_M.B2$Region %in% c("Aquitaine","Nord-Pas-de-Calais","Haute-Normandie")]<-"R4"


CM_M.B2$Region= as.factor(CM_M.B2$Region3)
CM_M.B2=CM_M.B2[,-dim(CM_M.B2)[2]]
table(CM_M.B2$Region)

############
# Brand   #
############

CM_M.B2$Brand3=as.character(CM_M.B$Brand)
CM_M.B2$Brand3[CM_M.B2$Brand %in% c("Japanese (except Nissan) or Korean","Mercedes, Chrysler or BMW","Volkswagen, Audi, Skoda or Seat")]<-"V1"
CM_M.B2$Brand3[CM_M.B2$Brand %in% c("Fiat","other","Opel, General Motors or Ford","Renault, Nissan or Citroen")]<-"v2"

CM_M.B2$Brand= as.factor(CM_M.B2$Brand3)
CM_M.B2=CM_M.B2[,-dim(CM_M.B2)[2]]
table(CM_M.B2$Brand)

names(CM_M.B2)

########################

#Power
#(changer "p2"
sort(table(CM_M.B2$Power))[length(table(CM_M.B2$Power))]
CM_M.B2$Power_m <- relevel(CM_M.B2$Power,ref = "P2")

#Region
sort(table(CM_M.B2$Region))[length(table(CM_M.B2$Region))]
CM_M.B2$Region_m <- relevel(CM_M.B2$Region,ref = "R2")

#Brand
sort(table(CM_M.B2$Brand))[length(table(CM_M.B2$Brand))]
CM_M.B2$Brand_m <- relevel(CM_M.B2$Brand,ref = "v2")


#DriveAge
sort(table(CM_M.B2$DriveAge))[length(table(CM_M.B2$DriveAge))]
CM_M.B2$DriveAge_m <- relevel(CM_M.B2$DriveAge,ref = "(27,48]")

#CarAge
sort(table(CM_M.B2$CarAge))[length(table(CM_M.B2$CarAge))]
CM_M.B2$CarAge_m <- relevel(CM_M.B2$CarAge,ref = "[0,10]")

#Density
sort(table(CM_M.B2$Density))[length(table(CM_M.B2$Density))]
CM_M.B2$Density_m <- relevel(CM_M.B2$Density,ref = "[2,363]")

#Gas
sort(table(CM_M.B2$Gas))[length(table(CM_M.B2$Gas))]
CM_M.B2$Gas_m <- relevel(CM_M.B2$Gas,ref = "Diesel")

names(CM_M.B2)
#GLM
Model_cout=glm(ClaimAmount~  Power_m  +  Brand_m+   Region_m+  DriveAge_m + CarAge_m + Density_m + Gas_m
               , family=Gamma(link = "log"), data=CM_M.B2)
summary(Model_cout) 


PRED_GAMfreq<-predict(Model_cout,newdata =CM_M.B2 ,type = "response")
PRED_GAMfreq

MOD_GAMMA=glm(ClaimAmount ~   Power_m  +  Brand_m+   Region_m+  DriveAge_m + CarAge_m + Density_m + Gas_m
              , family=Gamma(link = "log"), data=CM_M.B2)
summary(MOD_GAMMA)

#PRED_GlM<-predict(Model_cout,newdata = ,type = "response")
PRED_POIfreq<-predict(Model_REF,newdata =FREQ.B ,type = "response")


MOD_GAMMA=glm(ClaimAmount ~   Exposure+    Power+  CarAge    +
                DriverAge+   Brand+       Gas+    Region  +Density , family=Gamma(link = "log"), data=CM_M)
summary(MOD_GAMMA)

names(MOD_GAMMA)

# comparaison des modeles
PRED_GAM <- predict(MOD_GAMMA, newdata=test, type = 'response')


#pour freq
MOD_GAMMA=glm(ClaimAmount ~   Exposure+    Power+  CarAge    +
                DriverAge+   Brand+       Gas+    Region  +Density , family=Gamma(link = "log"), data=CM_M)
summary(MOD_GAMMA)

PRED_GAM <- predict(MOD_P, newdata=test, type = 'response')
OPS_GAM<-test$C
PRED_POIfreq
