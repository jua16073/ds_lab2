setwd("Documents/Data_Science/ds_lab2/")
getwd()
entrenamiento = read.csv("train.csv") 
prueba = read.csv("test.csv")
total <- merge(entrenamiento, prueba, all = TRUE)

set.seed(135)