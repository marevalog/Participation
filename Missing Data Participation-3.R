
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
#sensis$hora_miss[is.na(sensis$HORA_VISITA)] <- 1
#sensis$hora_miss[!is.na(sensis$HORA_VISITA)] <- 0
# number of houses visited by sensi with and without missing data
#cant_hour <- table(sensis$SENSIBILIZADOR, sensis$hora_miss)
# proportion of houses visited by sensi with and without missing data
#prop_hour <- prop.table(table(sensis$SENSIBILIZADOR, sensis$hora_miss), 1)
# changing table to matrix
#prop_hour_mat <- prop_hour
#dimnames(prop_hour_mat) <- NULL
# total number of visited houses
#tot_visits <- rowSums(cant_hour)
#dimnames(tot_visits) <- NULL
# barplot of missing data per sensi
#barplot(prop_hour[,2], horiz = T, las = 2, cex.names = 0.6)
#text((prop_hour_mat[,2])+0.03, (c(0:21)+0.6)*1.2, labels = tot_visits[], cex = 0.6)
#text(0.95, 2.6*1.2, cex = 0.6, "71")
#text(0.95, 2.6*4.9, cex = 0.6, "27")

#title("Proportion of missing data in variable
  #    'Hora de Visita' in 'Sensibilizacion' dataset")

#define year, month, day for dates
month <- sensis$MES
day <- sensis$DIA
year <- 2015

#defining first date and following dates for calculations
date <- ISOdatetime(year,month,day, hour = 12,min = 0, sec = 0, tz = "GMT")
#create reference date 
date1 <- ISOdatetime(2015,04,05,hour = 12,min = 0, sec = 0, tz = "GMT")




# creating missing indicator variable for time of visit
sensis$hora_miss[is.na(sensis$HORA_VISITA)] <- 1
sensis$hora_miss[!is.na(sensis$HORA_VISITA)] <- 0

# creating missing indicator variable for locality
sensis$loc_miss[is.na(sensis$L)] <- 1
sensis$loc_miss[!is.na(sensis$L)] <- 0

# creating missing indicator variable for vivienda
sensis$viv_miss[is.na(sensis$V)] <- 1
sensis$viv_miss[!is.na(sensis$V)] <- 0

# creating missing indicator variable for date programmed
sensis$dia_miss[is.na(sensis$DIA_PROG)] <- 1
sensis$dia_miss[!is.na(sensis$DIA_PROG)] <- 0

# creating missing indicator variable for results
sensis$result_miss[is.na(sensis$RESULTADO)] <- 1
sensis$result_miss[!is.na(sensis$RESULTADO)] <- 0

# creating missing indicator variable for cause of reprogramming
sensis$cause_miss[is.na(sensis$MOTIVO_REPROG)] <- 1
sensis$cause_miss[!is.na(sensis$MOTIVO_REPROG)] <- 0


#CREATING A FUNCTION FOR THE MISSING DATA IN A COLUMN
#where the two variables are the weeks, and the third is the column we have previously defined
##with the binary system

miss<- function(a,b,column){
 data<- data.frame(sensis$SENSIBILIZADOR,column)
 barplot(prop.table(table(data[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "lightskyblue", main=paste("Proportion of missing data in", ' ' ,  "\nfrom Week ",a, " to ",b))
 prop_mat <- prop.table(table(data[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
 dimnames(prop_mat) <- NULL
 total_visits <- rowSums(table(data[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
 dimnames(total_visits) <- NULL
 text((prop_mat[,2])+0.02, (c(0:21)+0.6)*1.20, labels = total_visits[], cex =0.6, font = 2)
}


#  Missing data in "time of visit"
miss.hora<- function(a,b){
  data_hora <- data.frame(sensis$SENSIBILIZADOR, sensis$hora_miss)
  barplot(prop.table(table(data_hora[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "lightgreen", main=paste("Proportion of missing data in 'Hora Visita'","\nfrom Week ",a, " to ",b))
  prop_hora_mat <- prop.table(table(data_hora[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
  dimnames(prop_hora_mat) <- NULL
  total_visits_hora <- rowSums(table(data_hora[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
  dimnames(total_visits_hora) <- NULL
  text((prop_hora_mat[,2])+0.02, (c(0:21)+0.6)*1.20, labels = total_visits_hora[], cex =0.6, font = 2)
}
#no data for weeks 2 through 4 -> shows error in prop.table : subscript out of bounds 


# Missing data in "locality"
#cant_loc <- table(sensis$SENSIBILIZADOR, sensis$L)
#prop_loc <- prop.table(table(sensis$SENSIBILIZADOR, sensis$loc_miss), 1)
#barplot(prop_loc[,2], horiz = T, las = 2)

##CREATING A FUNCTION FOR THE MISSING DATA IN "LOCALIDAD"
#where the two variables are the weeks
miss.loc<- function(a,b){
  data_loc <- data.frame(sensis$SENSIBILIZADOR, sensis$loc_miss)
  barplot(prop.table(table(data_loc[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "plum1", main=paste("Proportion of missing data in 'Localidad'","\nfrom Week ",a, " to ",b))
  prop_loc_mat <- prop.table(table(data_loc[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
  dimnames(prop_loc_mat) <- NULL
  total_visits_loc <- rowSums(table(data_loc[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
  dimnames(total_visits_loc) <- NULL
  text((prop_loc_mat[,2])+0.01, (c(0:21)+0.6)*1.20, labels = total_visits_loc[], cex =0.6, font = 2)
}
#No missing data for weeks 1 through 7, therefore no column of '1' -> shows error in prop.table 
  #because we are looking at the second column and it's non-existing for these weeks == no missing data
 

# Missing data in "Vivienda"
#Number of houses visited with and without missing data
#cant_viv <- table(sensis$SENSIBILIZADOR, sensis$viv_miss)

##CREATING A FUNCTION FOR THE MISSING DATA IN "VIVIENDA"
#where the two variables are the weeks
miss.viv<- function(a,b){
  data_viv <- data.frame(sensis$SENSIBILIZADOR, sensis$viv_miss)
  barplot(prop.table(table(data_viv[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "olivedrab1", main=paste("Proportion of missing data in 'Vivienda'","\n from Week",a, " to ",b))
  prop_viv_mat <- prop.table(table(data_viv[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
  dimnames(prop_viv_mat) <- NULL
  total_visits_viv <- rowSums(table(data_viv[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
  dimnames(total_visits_viv) <- NULL
  text((prop_viv_mat[,2])+0.0008, (c(0:21)+0.6)*1.20, labels = total_visits_viv[], cex =0.6, font = 2)
}
#No missing data/ no Data at all for weeks 1 through 5, therefore no column of '1' -> shows error in prop.table 
#because we are looking at the second column and it's non-existing for these weeks == no missing data


# Missing data in "Programmed date"
#cant_diaprog <- table(sensis$SENSIBILIZADOR, sensis$dia_miss)
#prop_diaprog <- prop.table(table(sensis$SENSIBILIZADOR, sensis$dia_miss), 1)
#barplot(prop_diaprog[,2], horiz = T, las = 2)

##CREATING A FUNCTION FOR THE MISSING DATA IN "PROGRAMMED DATE"
#where the two variables are the weeks
#using only sensis$DIA_PROG because it also coincides with all missing sensis$MES_PROG

miss.prog<- function(a,b){
  data_prog<- data.frame(sensis$SENSIBILIZADOR, sensis$dia_miss)
  barplot(prop.table(table(data_prog[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "lightsalmon", main=paste("Proportion of missing data in 'Programmed day'","\n from Week",a, " to ",b))
  prop_prog_mat <- prop.table(table(data_prog[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
  dimnames(prop_prog_mat) <- NULL
  total_visits_prog <- rowSums(table(data_prog[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
  dimnames(total_visits_prog) <- NULL
  text((prop_prog_mat[,2])+0.03, (c(0:21)+0.6)*1.20, labels = total_visits_prog[], cex =0.6, font = 2)
}
#no data at all for weeks 2 through 4 -> shows error in prop.table as Subscript out of bounds



# Missing data in "Results" (sprayed, closed, reluctant, etc.)
#cant_result <- table(sensis$SENSIBILIZADOR, sensis$result_miss)
#prop_result <- prop.table(table(sensis$SENSIBILIZADOR, sensis$result_miss), 1)

##CREATING A FUNCTION FOR THE MISSING DATA IN "RESULTS"
#where the two variables are the weeks

miss.result<- function(a,b){
  data_result <- data.frame(sensis$SENSIBILIZADOR, sensis$result_miss)
  barplot(prop.table(table(data_result[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "lightsalmon", main=paste("Proportion of missing data in 'Results'","\n from Week",a, " to ",b))
  prop_result_mat <- prop.table(table(data_result[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
  dimnames(prop_result_mat) <- NULL
  total_visits_result <- rowSums(table(data_result[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
  dimnames(total_visits_result) <- NULL
  text((prop_result_mat[,2])+0.013, (c(0:21)+0.6)*1.20, labels = total_visits_result[], cex =0.6, font = 2)
}
#no data/ no missing data for weeks 2 through 4 -> shows error in prop.table as Subscript out of bounds
 

# Missing data in "Cause of Reprogramming"
#cant_result <- table(sensis$SENSIBILIZADOR, sensis$result_miss)
#prop_result <- prop.table(table(sensis$SENSIBILIZADOR, sensis$result_miss), 1)
#barplot(prop_result[,2], horiz = T, las = 2)

##CREATING A FUNCTION FOR THE MISSING DATA IN "CAUSE OF REPROGRAMMING"
#where the two variables are the weeks
miss.cause<- function(a,b){
  data_cause <- data.frame(sensis$SENSIBILIZADOR,sensis$cause_miss)
  barplot(prop.table(table(data_cause[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
          [,2],  horiz = T, las = 2, cex.names = 0.6, col = "khaki1", main=paste("Proportion of missing data in \n'Cause of Reprogramming' from Week",a, " to ",b))
  prop_cause_mat <- prop.table(table(data_cause[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]),1)
  dimnames(prop_cause_mat) <- NULL
  total_visits_cause <- rowSums(table(data_cause[which(date >= date1 + weeks(a) & date < date1 + weeks(b)),]))
  dimnames(total_visits_cause) <- NULL
  text((prop_cause_mat[,2])+0.025, (c(0:21)+0.6)*1.20, labels = total_visits_cause[], cex =0.6, font = 2)
}
#no data/ no missing data for weeks 1 through 5 -> shows error in prop.table as Subscript out of bounds

