library(RJSONIO)
library(lme4)
library(lmerTest)
library(ggplot2)
library (mlmRev)
library(rjson)
library(jsonlite)
library (foreign)
library(curl)
library (mlmRev)
library(tidyverse)
library(purrr)
library(tidyr)
library(dplyr)
library(reshape2)
library(RColorBrewer)



part1 = data.frame(as.list(fromJSON(txt = "https://rafael.laboissiere.net/m1-miashs-2021/Wax9le/data/2021-05-12-07-32-08.json"))) 
part2 = data.frame(as.list(fromJSON(txt ="https://rafael.laboissiere.net/m1-miashs-2021/Wax9le/data/2021-05-13-19-27-50.json" ))) 
part3 = data.frame(as.list(fromJSON(txt = "https://rafael.laboissiere.net/m1-miashs-2021/Wax9le/data/2021-05-13-20-10-26.json")))
part4 = data.frame(as.list(fromJSON(txt = "https://rafael.laboissiere.net/m1-miashs-2021/Wax9le/data/2021-05-13-22-24-49.json")))
part5 = data.frame(as.list(fromJSON(txt = "https://rafael.laboissiere.net/m1-miashs-2021/Wax9le/data/2021-05-16-15-14-26.json")))
part6 = data.frame(as.list(fromJSON(txt = "https://rafael.laboissiere.net/m1-miashs-2021/Wax9le/data/2021-05-16-19-23-07.json")))




#met tous les data frames des participants dans un seul dataframe
tousPart = rbind(part1, part2, part3, part4, part5, part6)
##

##creation d'un data frame

subject <- c(tousPart$id)
frequency <- c(2)
congruency <- c(tousPart$essais.congruent)
MT <- c(tousPart$essais.reactionTime)
AUC <- c()
erreurs <- c(tousPart$essais.erreur)
IT <- c(tousPart$essais.tempsInit) 
groupe <- c(tousPart$groupe)


#########################################################
### fonction pour calculer l'aire sous la courbe

polyarea <- function (x, y) {
  n <- length (x)
  return (abs (sum (x [1 : (n - 1)] * y [2 : n] - y [1 : (n - 1)] * x [2 : n])) / 2)
}
x <- c (0, 0, 50, 100, 100, 0)
y <- c (0, 100, 150, 100, 0, 0)
plot (x, y, asp = 1)
lines (x, y)
polyarea (x, y)

x <- c (0, 0, 50, 100, 100, 0)
y <- c (0, 100, 150, 100, 0, 0)
plot (x, y, asp = 1)
lines (x, y)
polyarea (x, y)
############################################################


#########################################################
#boucle qui affiches tous les courbes et calcule l'aires sous la courbe de chaque essais

xa <- c()
yb <- c()
loop <- tousPart

for(i in 1:nrow(loop)) {       # for-loop over rows
  
  xa[i] <- loop[i, 8]
  yb[i] <- loop[i, 9]
  
  plot (xa[[i]], yb[[i]], pch = 19, col = "#00000040", asp = 1)
  
  xa[[i]] <- c (xa[[i]], xa[[i]][1])
  yb[[i]] <- c (yb[[i]], yb[[i]][1])
  
  lines (xa[[i]], yb[[i]])
  
  AUC[i] <- polyarea (xa[[i]], yb[[i]])
  
  polygon (xa[[i]], yb[[i]], col = "#ff000040")
  
  
}


#######################################################

## créé après avoir rempli le vecteur AUC
dataframe = data.frame(subject, frequency, congruency, MT, AUC, IT, groupe, tousPart$essais.couleur, tousPart$essais.mot)
##################################################
#MC/MI
for (i in 1:nrow(dataframe)) {
  

if(dataframe$congruency[i] == "0"){
  
  congruency[i] <- c("I")
}
else if (dataframe$congruency[i] == "1"){
  
  congruency[i] <- c("C")
}
}


 for (i in 1:nrow(dataframe)) {

    if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "blue" && dataframe$tousPart.essais.mot[i] == "BLEU" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "yellow" && dataframe$tousPart.essais.mot[i] == "JAUNE" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "yellow" && dataframe$tousPart.essais.mot[i] == "BLEU" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "blue" && dataframe$tousPart.essais.mot[i] == "JAUNE" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "green" && dataframe$tousPart.essais.mot[i] == "ROUGE" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "green" && dataframe$tousPart.essais.mot[i] == "VERT" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "red" && dataframe$tousPart.essais.mot[i] == "ROUGE" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "1" && dataframe$tousPart.essais.couleur[i] == "red" && dataframe$tousPart.essais.mot[i] == "VERT" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "blue" && dataframe$tousPart.essais.mot[i] == "BLEU" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "yellow" && dataframe$tousPart.essais.mot[i] == "JAUNE" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "yellow" && dataframe$tousPart.essais.mot[i] == "BLEU" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "blue" && dataframe$tousPart.essais.mot[i] == "JAUNE" ){
      frequency[i] <- c("MI")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "green" && dataframe$tousPart.essais.mot[i] == "ROUGE" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "green" && dataframe$tousPart.essais.mot[i] == "VERT" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "red" && dataframe$tousPart.essais.mot[i] == "ROUGE" ){
      frequency[i] <- c("MC")
    }
    else if(dataframe$groupe[i] == "2" && dataframe$tousPart.essais.couleur[i] == "red" && dataframe$tousPart.essais.mot[i] == "VERT" ){
      frequency[i] <- c("MC")
    }
  }
    
dataframe = data.frame(subject, frequency, congruency, MT, AUC, IT, groupe)

##########################

##########################
#####moyenne des courbes MC vs MI


cxmcc <- cxmci <- cxmic <- cxmii <- list()

cymcc <- cymci <- cymic <- cymii <- list()

##### x coordinates gp1

for (i in 1:nrow(tousPart)) {
  if(tousPart$essais.mot[i] == "BLEU" && tousPart$essais.couleur[i] == "blue" && tousPart$groupe[i] == "1"){
    cxmcc[i] <- tousPart$essais.courbeX[i]
  }
  else if(tousPart$essais.mot[i] == "JAUNE" && tousPart$essais.couleur[i] == "blue" && tousPart$groupe[i] == "1"){
    cxmci[i] <- tousPart$essais.courbeX[i]
  }
  else if(tousPart$essais.mot[i] == "VERT" && tousPart$essais.couleur[i] == "green" && tousPart$groupe[i] == "1"){
    cxmic[i] <- tousPart$essais.courbeX[i]
  }
  else if(tousPart$essais.mot[i] == "ROUGE" && tousPart$essais.couleur[i] == "green" && tousPart$groupe[i] == "1"){
    cxmii[i] <- tousPart$essais.courbeX[i]
  }
}
cxmcc = cxmcc[-which(sapply(cxmcc, is.null))]
cxmci = cxmci[-which(sapply(cxmci, is.null))]
cxmic = cxmic[-which(sapply(cxmic, is.null))]
cxmii = cxmii[-which(sapply(cxmii, is.null))]

##### Y coordinates gp1

for (i in 1:nrow(tousPart)) {
  if(tousPart$essais.mot[i] == "BLEU" && tousPart$essais.couleur[i] == "blue" && tousPart$groupe[i] == "1"){
    cymcc[i] <- tousPart$essais.courbeY[i]
  }
  else if(tousPart$essais.mot[i] == "JAUNE" && tousPart$essais.couleur[i] == "blue" && tousPart$groupe[i] == "1"){
    cymci[i] <- tousPart$essais.courbeY[i]
  }
  else if(tousPart$essais.mot[i] == "VERT" && tousPart$essais.couleur[i] == "green" && tousPart$groupe[i] == "1"){
    cymic[i] <- tousPart$essais.courbeY[i]
  }
  else if(tousPart$essais.mot[i] == "ROUGE" && tousPart$essais.couleur[i] == "green" && tousPart$groupe[i] == "1"){
    cymii[i] <- tousPart$essais.courbeY[i]
  }
}
cymcc = cymcc[-which(sapply(cymcc, is.null))]
cymci = cymci[-which(sapply(cymci, is.null))]
cymic = cymic[-which(sapply(cymic, is.null))]
cymii = cymii[-which(sapply(cymii, is.null))]


########################################

## X & Y coordinates gp2


cxmcc2 <- cxmci2 <- cxmic2 <- cxmii2 <- list()

cymcc2 <- cymci2 <- cymic2 <- cymii2 <- list()

##### x coordinates gp2

for (i in 1:nrow(tousPart)) {
  if(tousPart$essais.mot[i] == "ROUGE" && tousPart$essais.couleur[i] == "red" && tousPart$groupe[i] == "2"){
    cxmcc2[i] <- tousPart$essais.courbeX[i]
  }
  else if(tousPart$essais.mot[i] == "VERT" && tousPart$essais.couleur[i] == "red" && tousPart$groupe[i] == "2"){
    cxmci2[i] <- tousPart$essais.courbeX[i]
  }
  else if(tousPart$essais.mot[i] == "JAUNE" && tousPart$essais.couleur[i] == "yellow" && tousPart$groupe[i] == "2"){
    cxmic2[i] <- tousPart$essais.courbeX[i]
  }
  else if(tousPart$essais.mot[i] == "BLEU" && tousPart$essais.couleur[i] == "yellow" && tousPart$groupe[i] == "2"){
    cxmii2[i] <- tousPart$essais.courbeX[i]
  }
}
cxmcc2 = cxmcc2[-which(sapply(cxmcc2, is.null))]
cxmci2 = cxmci2[-which(sapply(cxmci2, is.null))]
cxmic2 = cxmic2[-which(sapply(cxmic2, is.null))]
cxmii2 = cxmii2[-which(sapply(cxmii2, is.null))]

##### Y coordinates gp2

for (i in 1:nrow(tousPart)) {
  if(tousPart$essais.mot[i] == "ROUGE" && tousPart$essais.couleur[i] == "red" && tousPart$groupe[i] == "2"){
    cymcc2[i] <- tousPart$essais.courbeY[i]
  }
  else if(tousPart$essais.mot[i] == "VERT" && tousPart$essais.couleur[i] == "red" && tousPart$groupe[i] == "2"){
    cymci2[i] <- tousPart$essais.courbeY[i]
  }
  else if(tousPart$essais.mot[i] == "JAUNE" && tousPart$essais.couleur[i] == "yellow" && tousPart$groupe[i] == "2"){
    cymic2[i] <- tousPart$essais.courbeY[i]
  }
  else if(tousPart$essais.mot[i] == "BLEU" && tousPart$essais.couleur[i] == "yellow" && tousPart$groupe[i] == "2"){
    cymii2[i] <- tousPart$essais.courbeY[i]
  }
}
cymcc2 = cymcc2[-which(sapply(cymcc2, is.null))]
cymci2 = cymci2[-which(sapply(cymci2, is.null))]
cymic2 = cymic2[-which(sapply(cymic2, is.null))]
cymii2 = cymii2[-which(sapply(cymii2, is.null))]

################################################################


####plot for MC c*I   gp2

mcc2 <- data.frame( cbind(cxmcc2, cymcc2)) 
mci2 <- data.frame( cbind(cxmci2, cymci2)) 

a <- b <- xcc2 <- ycc2 <- c()

for (j in 1:150) {
  for (i in 1:nrow(mcc2)) {
    a[i] <- mcc2$cxmcc2[[i]][j]
    b[i] <- mcc2$cymcc2[[i]][j]
  }
  xcc2[j] <- mean(a)
  ycc2[j] <- mean(b)
}

xcc2 <- na.omit(xcc2)
ycc2 <- na.omit(ycc2) 


a <- b <- xci2 <- yci2 <- c()

for (j in 1:150) {
  for (i in 1:nrow(mci2)) {
    a[i] <- mci2$cxmci2[[i]][j]
    b[i] <- mci2$cymci2[[i]][j]
  }
  xci2[j] <- mean(a)
  yci2[j] <- mean(b)
}


xci2 <- na.omit(xci2)
yci2 <- na.omit(yci2) 


####################################3

#plot for Mi c*I  gp2


mic2 <- data.frame( cbind(cxmic2, cymic2)) 

a <- b <- xic2 <- yic2 <- c()

for (j in 1:150) {
  for (i in 1:nrow(mic2)) {
    a[i] <- mic2$cxmic2[[i]][j]
    b[i] <- mic2$cymic2[[i]][j]
  }
  xic2[j] <- mean(a)
  yic2[j] <- mean(b)
}

xic2 <- na.omit(xic2)
yic2 <- na.omit(yic2) 

mii2 <- data.frame( cbind(cxmii2, cymii2)) 

a <- b <- xii2 <- yii2 <- c()

for (j in 1:150) {
  for (i in 1:nrow(mii2)) {
    a[i] <- mii2$cxmii[[i]][j]
    b[i] <- mii2$cymii[[i]][j]
  }
  xii2[j] <- mean(a)
  yii2[j] <- mean(b)
}

xii2 <- na.omit(xii2)
yii2 <- na.omit(yii2) 



##################

plot( xcc2, ycc2, type="l", col="red", ylab = "Vertical mouse position (y-coordinate)", xlab = "Horizontal mouse position (x-coordinate)")
legend("bottomright", 95, legend=c("congruent", "incongruent"),
       col=c("red", "green"), lty=1:1, cex=0.8)
legend("top", 95, legend=c("80% Congruent"))
par(new=TRUE)
plot( xci2, yci2, type="l", col="green", axes=FALSE,ann=FALSE, )

################
plot( xic2, yic2, type="l", col="red", ylab = "Vertical mouse position (y-coordinate)", xlab = "Horizontal mouse position (x-coordinate)")
legend("bottomleft", 95, legend=c("congruent", "incongruent"),
       col=c("red", "green"), lty=1:1, cex=0.8)
legend("top", 95, legend=c("20% Congruent"))
par(new=TRUE)
plot( xii2, yii2, type="l", col="green", axes=FALSE,ann=FALSE, )






########################################

####plot for MC c*I   gp1

mcc <- data.frame( cbind(cxmcc, cymcc)) 
mci <- data.frame( cbind(cxmci, cymci)) 

a <- b <- xcc <- ycc <- c()

for (j in 1:150) {
  for (i in 1:nrow(mcc)) {
    a[i] <- mcc$cxmcc[[i]][j]
    b[i] <- mcc$cymcc[[i]][j]
  }
  xcc[j] <- mean(a)
  ycc[j] <- mean(b)
}

xcc <- na.omit(xcc)
ycc <- na.omit(ycc) 


a <- b <- xci <- yci <- c()

for (j in 1:150) {
  for (i in 1:nrow(mci)) {
    a[i] <- mci$cxmci[[i]][j]
    b[i] <- mci$cymci[[i]][j]
  }
  xci[j] <- mean(a)
  yci[j] <- mean(b)
}

      
xci <- na.omit(xci)
yci <- na.omit(yci) 


####################################3

#plot for Mi c*I  gp1


mic <- data.frame( cbind(cxmic, cymic)) 

a <- b <- xic <- yic <- c()

for (j in 1:150) {
  for (i in 1:nrow(mic)) {
    a[i] <- mic$cxmic[[i]][j]
    b[i] <- mic$cymic[[i]][j]
  }
  xic[j] <- mean(a)
  yic[j] <- mean(b)
}

xic <- na.omit(xic)
yic <- na.omit(yic) 

mii <- data.frame( cbind(cxmii, cymii)) 

a <- b <- xii <- yii <- c()

for (j in 1:150) {
  for (i in 1:nrow(mii)) {
    a[i] <- mii$cxmii[[i]][j]
    b[i] <- mii$cymii[[i]][j]
  }
  xii[j] <- mean(a)
  yii[j] <- mean(b)
}

xii <- na.omit(xii)
yii <- na.omit(yii) 



##################

plot( xcc, ycc, type="l", col="red", ylab = "Vertical mouse position (y-coordinate)", xlab = "Horizontal mouse position (x-coordinate)")
legend("bottomright", 95, legend=c("congruent", "incongruent"),
       col=c("red", "green"), lty=1:1, cex=0.8)
legend("top", 95, legend=c("80% Congruent"))
par(new=TRUE)
plot( xci, yci, type="l", col="green", axes=FALSE,ann=FALSE, )

################
plot( xic, yic, type="l", col="red", ylab = "Vertical mouse position (y-coordinate)", xlab = "Horizontal mouse position (x-coordinate)")
legend("bottomleft", 95, legend=c("congruent", "incongruent"),
       col=c("red", "green"), lty=1:1, cex=0.8)
legend("top", 95, legend=c("20% Congruent"))
par(new=TRUE)
plot( xii, yii, type="l", col="green", axes=FALSE,ann=FALSE, )




#########################################################################

#######################################################################
## Analyse des donnees


## AUC et MT pour groupe 1

freq <- gp <- auc <- cong <- mt <-it <- c() 

     for (i in 1:nrow(dataframe)) {
       
  if(dataframe$congruency[i] == "C" && dataframe$frequency[i] == "MC" && dataframe$groupe[i] == "1"){
   
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
    
  }
     }

dat.MCC <- data.frame (gp, cong, freq, auc, mt, it) 
dat.MCC <- na.omit(dat.MCC)

freq <- gp <- auc <- cong <- mt <-it <- c()
for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "I" && dataframe$frequency[i] == "MC" && dataframe$groupe[i] == "1"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MCI <- data.frame (gp, cong, freq, auc, mt, it)
dat.MCI <- na.omit(dat.MCI)

freq <- gp <- auc <- cong <- mt <-it <- c()
for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "C" && dataframe$frequency[i] == "MI" && dataframe$groupe[i] == "1"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MIC <- data.frame (gp, cong, freq, auc, mt, it)
dat.MIC <- na.omit(dat.MIC)

freq <- gp <- auc <- cong <- mt <-it <- c()
for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "I" && dataframe$frequency[i] == "MI" && dataframe$groupe[i] == "1"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MII <- data.frame (gp, cong, freq, auc, mt, it)
dat.MII <- na.omit(dat.MII)

lauc <- as.vector( rbind( mean(dat.MCC$auc),mean(dat.MCI$auc),mean(dat.MIC$auc), mean(dat.MII$auc) )[,1] )

barplot(lauc, ylim = c(0,40000), col=c("pink","lightblue"), xaxt="n",ylab = "Mean AUC",legend.text = c("congruent","incongruent"), 
        args.legend = list(x = "topright"))
axis(1, at = c(1.4, 0, 3.6, 0), labels = c("80% Congruent","","20% Congruent", "") ,tick=FALSE, cex=0.5)

boxplot(dat.MCC$auc, dat.MCI$auc, dat.MIC$auc, dat.MII$auc)


lmt <- as.vector( rbind(mean(dat.MCC$mt),mean(dat.MCI$mt),mean(dat.MIC$mt), mean(dat.MII$mt) )[,1] )
barplot(lmt, ylim = c(0,2.5), col=c("pink","lightblue"), xaxt="n",ylab = "Mean MT",legend.text = c("congruent","incongruent"), 
        args.legend = list(x = "topright"))
axis(1, at = c(1.4, 0, 3.5, 0), labels = c("80% Congruent","","20% Congruent", "") ,tick=FALSE, cex=0.5)

boxplot(dat.MCC$mt, dat.MCI$mt, dat.MIC$mt, dat.MII$mt)


############################################################3

## AUC et MT pour groupe 2 


freq <- gp <- auc <- cong <- mt <-it <- c() 

for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "C" && dataframe$frequency[i] == "MC" && dataframe$groupe[i] == "2"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MCC2 <- data.frame (gp, cong, freq, auc, mt, it) 
dat.MCC2 <- na.omit(dat.MCC2)

freq <- gp <- auc <- cong <- mt <-it <- c()
for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "I" && dataframe$frequency[i] == "MC" && dataframe$groupe[i] == "2"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MCI2 <- data.frame (gp, cong, freq, auc, mt, it)
dat.MCI2 <- na.omit(dat.MCI2)

freq <- gp <- auc <- cong <- mt <-it <- c()
for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "C" && dataframe$frequency[i] == "MI" && dataframe$groupe[i] == "2"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MIC2 <- data.frame (gp, cong, freq, auc, mt, it)
dat.MIC2 <- na.omit(dat.MIC2)

freq <- gp <- auc <- cong <- mt <-it <- c()
for (i in 1:nrow(dataframe)) {
  
  if(dataframe$congruency[i] == "I" && dataframe$frequency[i] == "MI" && dataframe$groupe[i] == "2"){
    
    cong[i] <- dataframe$congruency[i]
    gp[i] <- dataframe$groupe[i]
    auc[i] <- dataframe$AUC[i]
    freq[i] <- dataframe$frequency[i]
    mt[i] <- dataframe$MT[i]
    it[i] <- dataframe$IT[i]
    
  }
}

dat.MII2 <- data.frame (gp, cong, freq, auc, mt, it)
dat.MII2 <- na.omit(dat.MII2)

lauc2 <- as.vector( rbind( mean(dat.MCC2$auc),mean(dat.MCI2$auc),mean(dat.MIC2$auc), mean(dat.MII2$auc) )[,1] )

barplot(lauc2, ylim = c(0,60000), col=c("pink","lightblue"), xaxt="n",ylab = "Mean AUC",legend.text = c("congruent","incongruent"), 
        args.legend = list(x = "topright"))
axis(1, at = c(1.4, 0, 3.6, 0), labels = c("80% Congruent","","20% Congruent", "") ,tick=FALSE, cex=0.5)

boxplot(dat.MCC2$auc,dat.MCI2$auc,dat.MIC2$auc, dat.MII2$auc)

lmt2 <- as.vector( rbind(mean(dat.MCC2$mt),mean(dat.MCI2$mt),mean(dat.MIC2$mt), mean(dat.MII2$mt) )[,1] )
barplot(lmt2, ylim = c(0,2.5), col=c("pink","lightblue"), xaxt="n",ylab = "Mean MT",legend.text = c("congruent","incongruent"), 
        args.legend = list(x = "topright"))
axis(1, at = c(1.4, 0, 3.5, 0), labels = c("80% Congruent","","20% Congruent", "") ,tick=FALSE, cex=0.5)

boxplot(dat.MCC2$mt, dat.MCI2$mt, dat.MIC2$mt, dat.MII2$mt)

#############################################################
###anova


# Compute the analysis of variance
mtc.aov <- aov(MT ~ congruency, data = dataframe)
mtf.aov <- aov(MT ~ frequency, data = dataframe)

aucc.aov <- aov(AUC ~ congruency, data = dataframe)
aucf.aov <- aov(AUC ~ frequency, data = dataframe)

itc.aov <- aov(IT ~ congruency, data = dataframe)
itf.aov <- aov(IT ~ frequency, data = dataframe)

# Summary of the analysis
summary(mtc.aov)
summary(mtf.aov)

summary(aucc.aov)
summary(aucf.aov)

summary(itc.aov)
summary(itf.aov)

################################################################3

## test

str (part1)
names(part1)



#un tableau qui montre chaque mot et la couleur dans laquelle il existe
table(part1$essais.couleur, part1$essais.mot)
#un tableau qui montre chaque mot et la couleur dans laquelle il existe
table(part2$essais.couleur, part2$essais.mot)


#######################################3

