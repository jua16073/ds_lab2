setwd("D:/data science/lab1DataScience")

library(dplyr)
library(plyr)
library(fitdistrplus)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(ggfortify)
library(factoextra)
library(caret)
library(cluster)
entrenamiento = read.csv("train.csv") 
prueba = read.csv("test.csv")
entrenamiento1 <- data.frame(entrenamiento)
prueba1 <- data.frame(prueba)
total <- merge(entrenamiento1, prueba1, all = TRUE)

#resumen de los datos

summary(total)

#tablas de frecuencia

#clase de edificacion
mssubclass <- table(total$MSSubClass)
mssubclass

#clasificacion de zona
mszoning <- table(total$MSZoning)
mszoning

#tipo de calle
street <- table(total$Street)
street

#tipo de acceso a alley
alley <- table(total$Alley)
alley

#forma del terreno
lotshape <- table(total$LotShape)
lotshape

#que tan plano es el terreno
lotcontour <-table(total$LandContour)
lotcontour

#utilidades
utilidades <- table(total$Utilities)
utilidades

#configuracion del lote
lotconfig <- table(total$LotConfig)
lotconfig

#inclinacion de la propiedad
landslope <- table(total$LandSlope)
landslope

#barrio
barrio <- table(total$Neighborhood)
barrio

#cercania a calle o rieles
condition <- table(total$Condition1)
condition

#cercania a calle o rieles 2
condition2 <- table(total$Condition2)
condition2

#tipo de construccion
bldgtype <- table(total$BldgType)
bldgtype

#estilo de casa
housestyle <- table(total$HouseStyle)
housestyle

#calidad general
overalquall <- table(total$OverallQual)
overalquall

#condicion general
overallcond <- table(total$OverallCond)
overallcond

#estilo de techo
roofstyle <- table(total$RoofStyle)
roofstyle

#material del techo
roofmat <- table(total$RoofMatl)
roofmat

#cubierta exterior
exterior1st <- table(total$Exterior1st)
exterior1st

#cuberta exterior 2
exterior2nd <- table(total$Exterior2nd)
exterior2nd

#masonry veneer type
msnvnrtype <- table(total$MasVnrType)
msnvnrtype

#calidad exterior
exterqual <- table(total$ExterQual)
exterqual

#condicion exterior
externcond <- table(total$ExterCond)
externcond

#foundation
foundation <- table(total$Foundation)
foundation

#altura de sotano
bsmtqual <- table(total$BsmtQual)
bsmtqual

#condicion del sotano
bstmcond <- table(total$BsmtCond)
bstmcond

#accesibildiad a sotano
bstmexpo <- table(total$BsmtExposure)
bstmexpo

#calidad de sotano terminada
bstmfintype <- table(total$BsmtFinType1)
bstmfintype

#calidad de sotano terminado 2
bstmfintype2 <- table(total$BsmtFinType2)
bstmfintype2

#calefaccion
heating <- table(total$Heating)
heating

#calidad de la calefaccion
heatingQC <- table(total$HeatingQC)
heatingQC

#aire condicionado
centralair <- table(total$CentralAir)
centralair

#sistema electrico
electric <- table(total$Electrical)
electric

#calidad de cocinas
kitchenqual <- table(total$KitchenQual)
kitchenqual

#funcionalidad de la casa 
functional <- table(total$Functional)
functional

#calidad de chimeneas
fireplacequ <- table(total$FireplaceQu)
fireplacequ

#tipo de garaje
garageType <- table(total$GarageType)
garageType

#acabado de garaje
garageFinish <- table(total$GarageFinish)
garageFinish

#calidad de garaje
garageQual <- table(total$GarageQual)
garageQual

#condicion de garaje
garageCond <- table(total$GarageCond)
garageCond

#pavimentizada 
paveddrive <- table(total$PavedDrive)
paveddrive

#calidad de piscina 
poolqc <- table(total$PoolQC)
poolqc

#calidad de reja
fence <- table(total$Fence)
fence

#miscelaneos
misc <- table(total$MiscFeature)
misc

#mes de venta
mosold <- table(total$MoSold)
mosold

#tipo de venta
saletype <- table(total$SaleType)
saletype

#condicion de venta
salecond <- table(total$SaleCondition)
salecond


#correlacion entre todas las variables
datosR1 <- within(total, rm("Id"))
t <- sapply(datosR1,as.numeric)
correl1 <- rcorr(as.matrix(t))
correl1
M <- correl1$r
M
M <- M[complete.cases(M),]
p_mat <- correl1$P
corrplot(M, order = "hclust", p.mat= p_mat, sig.level = 0.01, na.label = "NA")


#agrupacion de variabls numericas

total <- within(total, rm("Id", "MSSubClass", "MSZoning", "Street", "Alley",
                          "LotShape","LandContour","LandSlope"))
total<-within(total, rm("Neighborhood","Condition1","Condition2","BldgType","HouseStyle",
                        "OverallQual","OverallCond","YearBuilt","YearRemodAdd","RoofStyle"))
total<- within(total, rm("Utilities","LotConfig","RoofMatl","Exterior1st","Exterior2nd",
                         "MasVnrType","ExterQual","ExterCond","Foundation","BsmtQual"))
total <- within(total, rm("BsmtCond","BsmtExposure", "BsmtFinType1", "BsmtFinType2", "Heating",
                          "HeatingQC","CentralAir","Electrical","KitchenQual","Functional",
                          "FireplaceQu","GarageType","GarageYrBlt","GarageFinish","GarageQual",
                          "GarageCond","PavedDrive","PoolQC","Fence","MiscFeature","SaleType","SaleCondition"))

#correlacion variables numericas
t <- sapply(total,as.numeric)
correl2 <- rcorr(as.matrix(t))
correl2
M <- correl2$r
M
M <- M[complete.cases(M),]
p_mat <- correl2$P
corrplot(M, order = "hclust", p.mat= p_mat, sig.level = 0.01, na.label = "NA")


#regresion lineal
set.seed(452)

trainingRow <- sample(1:nrow(total), 0.6*nrow(total))
trainingData <-total[trainingRow,]
testing <- total[-trainingRow,]
trainingData <- trainingData[complete.cases(trainingData),]
testing <- testing[complete.cases(testing),]
trainingData <- within(trainingData, rm("TotalBsmtSF","GrLivArea"))
testing <- within(testing,rm("TotalBsmtSF","GrLivArea"))
#hacer modelo lineal
lmodel <- lm(SalePrice ~ ., data = trainingData)
summary(lmodel)
sigma(lmodel)/mean(trainingData$SalePrice)
#prediccion
predL<-predict(lmodel, newdata = testing, interval = "confidence", level = .95)
predL

resultados<-data.frame(testing$SalePrice,predL)
dif<-abs(resultados$testing.SalePrice-resultados$predL)
dif
summary(dif)

#categorizar
total$rangos <- "1"
total[which(total$SalePrice>=100000 & total$SalePrice<300000), 'rangos'] <- "2"
total[which(total$SalePrice>=300000 & total$SalePrice <= 755000), 'rangos'] <- "3"
set.seed(100)

trainingRow <- sample(1:nrow(total), 0.6*nrow(total))
trainingData <-total[trainingRow,]
testing <- total[-trainingRow,]
trainingData <- trainingData[complete.cases(trainingData),]
testing <- testing[complete.cases(testing),]
trainingData <- within(trainingData, rm("TotalBsmtSF","GrLivArea"))
testing <- within(testing,rm("TotalBsmtSF","GrLivArea"))


#KNN
trainingData$rangos <- as.numeric(trainingData$rangos)
testing$rangos <- as.numeric(testing$rangos)

library(class)
predKnn<-knn(trainingData,testing,trainingData$rangos,k=3)
predKnn
cfm<-confusionMatrix(as.factor(testing$rangos),predKnn)
cfm

