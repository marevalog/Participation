
# Setting the path:
#setwd("/Users/ricardocasney/Documents/Work/Chagas/Participation/DBs")
# Reading the data (dataset has many blanks, therefore na.strings...)
setwd("~/Downloads")
sensis <- read.csv("Sensiibilizacion_Participacion - Hoja 1.csv", na.strings = c("", "NA"),
                   nrows = 3149)
install.packages("lubridate")
library(lubridate)

# Making sure that blanks are NAs now
unique(sensis$HORA_VISITA)
sum(is.na(sensis$HORA_VISITA))
head(sensis)
tail(sensis)

##  Missing data in "time of visit"
# creating missing indicator variable
sensis$hora_miss[is.na(sensis$HORA_VISITA)] <- 1
sensis$hora_miss[!is.na(sensis$HORA_VISITA)] <- 0
# number of houses visited by sensi with and without missing data
cant_hour <- table(sensis$SENSIBILIZADOR, sensis$hora_miss)

# proportion of houses visited by sensi with and without missing data
prop_hour <- prop.table(table(sensis$SENSIBILIZADOR, sensis$hora_miss), 1)
# changing table to matrix
prop_hour_mat <- prop_hour
dimnames(prop_hour_mat) <- NULL
# total number of visited houses
tot_visits <- rowSums(cant_hour)
dimnames(tot_visits) <- NULL
# barplot of missing data per sensi
barplot(prop_hour[-c(11),2], horiz = T, las = 2, cex.names = 0.6)

text((prop_hour_mat[-c(11),2])+0.04, (c(0:20)+0.6)*1.20, labels = tot_visits[-(11)])
text(0.95, 2.6*1.2, "71")
title("Proportion of missing data in variable
      'Hora de Visita' in 'Sensibilizacion' dataset")
dev.print(dev = pdf, "Missing data in Hora de Visita. Sensibilizacion 090615.pdf")

#define year, month, day for dates
month <- sensis$MES
day <- sensis$DIA
year <- 2015

#defining first date and following dates for calculations
date <- ISOdatetime(year,month,day, hour = 12,min = 0, sec = 0, tz = "GMT")
date1 <- ISOdatetime(2015,04,05,hour = 12,min = 0, sec = 0, tz = "GMT")
data <- data.frame(sensis$SENSIBILIZADOR, sensis$hora_miss)

## WEEK 1
#creating table for week 1
week1 <- data[which(date < date1 + weeks(1)),]
prop.table(table(week1),1) -> prop.hr.w1
# changing table to matrix
prop_hour_mat1 <- prop.hr.w1
dimnames(prop_hour_mat1) <- NULL
# total number of visited houses
total_visits1 <- rowSums(table(week1))
dimnames(total_visits1) <- NULL
# barplot of missing data per sensi
barplot(prop.hr.w1[-c(11),1], horiz = T, las = 2, cex.names = 0.6, col = "light blue")
#labels with total number of visits this week
text((prop_hour_mat1[-c(11),1])-0.03, (c(0:20)+0.6)*1.20, labels = total_visits1[-(11)], cex =0.8, font = 2)

title("Proportion of missing data in variable
      'Hora de Visita' WEEK 1")
dev.print(dev = pdf, "Missing data in Hora de Visita. Sensibilizacion WEEK 1.pdf")

## WEEK 2
#creating table for week 2
data <- data.frame(sensis$SENSIBILIZADOR, sensis$hora_miss)
week2 <- data[which(date >= date1 + weeks(1) & date < date1 + weeks(2)),]
#No data collected on week 2 

## WEEK 3
#creating table for week 3
data <- data.frame(sensis$SENSIBILIZADOR, sensis$hora_miss)
week3 <- data[which(date >= date1 + weeks(2)  & date < date1 + weeks(3)),]
#No data collected on week 3

## WEEK 4
#creating table for week 4
data <- data.frame(sensis$SENSIBILIZADOR, sensis$hora_miss)
week4 <- data[which(date >= date1 + weeks(3)  & date < date1 + weeks(4)),]
#No data collected on week 4

## WEEK 5
#creating table for week 5
data <- data.frame(sensis$SENSIBILIZADOR, sensis$hora_miss)
week5 <- data[which(date >= date1 + weeks(4)  & date < date1 + weeks(5)),]
prop.table(table(week5),1) -> prop.hr.w5
# changing table to matrix
prop_hour_mat5 <- prop.hr.w5
dimnames(prop_hour_mat5) <- NULL
# total number of visited houses
total_visits5 <- rowSums(table(week5))
dimnames(total_visits5) <- NULL
# barplot of missing data per sensi
barplot(prop.hr.w5[-c(11),1], horiz = T, las = 2, cex.names = 0.6, col = "plum2")
#labels with total number of visits this week
text((prop_hour_mat5[-c(11),1])-0.03, (c(0:20)+0.6)*1.20, labels = total_visits5[-c(11)], cex = 0.7, font =2)

title("Proportion of missing data in variable
      'Hora de Visita' WEEK 5")
dev.print(dev = pdf, "Missing data in Hora de Visita. Sensibilizacion WEEK 4.pdf")

sensis$MonthDay <- paste( month.name[sensis$MES], sensis$DIA, sep="-") 

barplot(prop_hour[which(week1 = TRUE)[-c(11),2], horiz = T, las = 1, cex.names = 0.6)










# Missing data in "locality"
#creating missing indicator variable
sensis$loc_miss[is.na(sensis$L)] <- 1
sensis$loc_miss[!is.na(sensis$L)] <- 0
cant_loc <- table(sensis$SENSIBILIZADOR, sensis$dia_miss)
prop_loc <- prop.table(table(sensis$SENSIBILIZADOR, sensis$loc_miss), 1)
barplot(prop_loc[,2], horiz = T, las = 2)

# Missing data in "Vivienda"
sensis$viv_miss[is.na(sensis$V)] <- 1
sensis$viv_miss[!is.na(sensis$V)] <- 0
cant_viv <- table(sensis$SENSIBILIZADOR, sensis$viv_miss)
prop_viv <- prop.table(table(sensis$SENSIBILIZADOR, sensis$viv_miss), 1)
barplot(prop_viv[,2], horiz = T, las = 2)

# Missing data in "Programmed date"
sensis$dia_miss[is.na(sensis$DIA_PROG)] <- 1
sensis$dia_miss[!is.na(sensis$DIA_PROG)] <- 0
cant_diaprog <- table(sensis$SENSIBILIZADOR, sensis$dia_miss)
prop_diaprog <- prop.table(table(sensis$SENSIBILIZADOR, sensis$dia_miss), 1)
barplot(prop_diaprog[,2], horiz = T, las = 2)

# Missing data in "Result" (sprayed, closed, reluctant, etc.)
sensis$result_miss[is.na(sensis$RESULTADO)] <- 1
sensis$result_miss[!is.na(sensis$RESULTADO)] <- 0
cant_result <- table(sensis$SENSIBILIZADOR, sensis$result_miss)
prop_result <- prop.table(table(sensis$SENSIBILIZADOR, sensis$result_miss), 1)
prop_hour_mat <- prop_hour
dimnames(prop_hour_mat) <- NULL
tot_visits <- rowSums(cant_hour)
barplot(prop_result[,2], horiz = T, las = 2)
text((prop_hour_mat[-c(11),2])+0.03, (c(0:20)+0.6)*1.20, labels = tot_visits[-(11)])
text(0.95, 2.6*1.2, "71")
title("Proportion of missing data in variable
      'Hora de Visita' in 'Sensibilizacion' dataset")
dev.print(dev = pdf, "Missing data in Hora de Visita. Sensibilizacion 090615.pdf")

# Missing data in "Cause of Reprogramming"
sensis$result_miss[is.na(sensis$RESULTADO)] <- 1
sensis$result_miss[!is.na(sensis$RESULTADO)] <- 0
cant_result <- table(sensis$SENSIBILIZADOR, sensis$result_miss)
prop_result <- prop.table(table(sensis$SENSIBILIZADOR, sensis$result_miss), 1)
barplot(prop_result[,2], horiz = T, las = 2)


