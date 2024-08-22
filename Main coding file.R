#Load NALSYS_18 data
library(readxl)
NALSYS_18_read <- read_excel("C:/Users/mengz/OneDrive/Desktop/Thesis/NALSYS_18.xlsx")

#Create a new var/column "Last" to mark the most recent LHD survey
NALSYS_18_read$Last <- 0

#Loop through and mark the last obs. of an ID as "1" for each ID
for(i in 1:(nrow(NALSYS_18_read)-1) ){
  if(!(NALSYS_18_read$nacchoid[i] == NALSYS_18_read$nacchoid[i+1])){NALSYS_18_read$Last[i] <- 1}
  if(i == nrow(NALSYS_18_read)-1){NALSYS_18_read$Last[i+1] <- 1}
}

#Subset the NALSYS data so only survey data that has “Last = 1” is covered
NALSYS_18 <- NALSYS_18_read[which(NALSYS_18_read$Last==1),]

#Load NACCHO_19 data
library(readxl)
NACCHO_19 <- read_excel("C:/Users/mengz/OneDrive/Desktop/Thesis/NACCHO_19.xlsx")

#Natural Join: NACCHO_NALSYS ####
NACCHO_NALSYS <- merge(x=NACCHO_19,y=NALSYS_18,by="nacchoid",all=FALSE)
write.csv(NACCHO_NALSYS, file='C:/Users/mengz/OneDrive/Desktop/Thesis/NACCHO_NALSYS.csv')

#Load in infrastructure data frame####
#edits: moved vars need to be excluded forward & delete Last column
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis')
dat <-read.csv('NACCHO_NALSYS_infrastructure.csv')

# Transform the 19 Serv variables ####
#so that there is one variable per activity category (i.e. job, house, food, cash) #
# To be used to describe the activities of the PH dept. as a desriptive summary measure #

#1. house variables ####
house <- dat[,1460:1478] # pull out the 'house variables #

# loop through house variables and if any of them are 'checked' we check the new variable
# house_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$house_any <- 0
  for(j in 1:19){
    dat$house_any[house[,j]==1] <- 1
  }
}

#2. job variables ####
job <- dat[,1484:1502] # pull out the 'job variables #

# loop through job variables and if any of them are 'checked' we check the new variable
# job_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$job_any <- 0
  for(j in 1:19){
    dat$job_any[job[,j]==1] <- 1
  }
}

#3. food variables ####
food <- dat[,1504:1522] # pull out the 'food variables #

# loop through food variables and if any of them are 'checked' we check the new variable
# food_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$food_any <- 0
  for(j in 1:19){
    dat$food_any[food[,j]==1] <- 1
  }
}

#4. child variables ####
child <- dat[,1524:1542] # pull out the 'child variables #

# loop through child variables and if any of them are 'checked' we check the new variable
# child_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$child_any <- 0
  for(j in 1:19){
    dat$child_any[child[,j]==1] <- 1
  }
}

#5. cash variables ####
cash <- dat[,1544:1562] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# cash_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$cash_any <- 0
  for(j in 1:19){
    dat$cash_any[cash[,j]==1] <- 1
  }
}

#6. park variables ####
park <- dat[,1568:1586] # pull out the 'park variables #

# loop through park variables and if any of them are 'checked' we check the new variable
# park_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$park_any <- 0
  for(j in 1:19){
    dat$park_any[park[,j]==1] <- 1
  }
}

#7. trans variables ####
trans <- dat[,1588:1606] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# cash_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$trans_any <- 0
  for(j in 1:19){
    dat$trans_any[trans[,j]==1] <- 1
  }
}

#8. older variables ####
older <- dat[,1608:1626] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$older_any <- 0
  for(j in 1:19){
    dat$older_any[older[,j]==1] <- 1
  }
}

#9. disab variables ####
disab <- dat[,1628:1646] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$disab_any <- 0
  for(j in 1:19){
    dat$disab_any[disab[,j]==1] <- 1
  }
}

#10. vet variables ####
vet <- dat[,1648:1666] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$vet_any <- 0
  for(j in 1:19){
    dat$vet_any[vet[,j]==1] <- 1
  }
}

#11. land variables ####
land <- dat[,1672:1690] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$land_any <- 0
  for(j in 1:19){
    dat$land_any[land[,j]==1] <- 1
  }
}

#12. legal variables ####
legal <- dat[,1696:1714] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$legal_any <- 0
  for(j in 1:19){
    dat$legal_any[legal[,j]==1] <- 1
  }
}

#13. lawenf variables ####
lawenf <- dat[,1716:1734] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$lawenf_any <- 0
  for(j in 1:19){
    dat$lawenf_any[lawenf[,j]==1] <- 1
  }
}

#14. corr variables ####
corr <- dat[,1736:1754] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$corr_any <- 0
  for(j in 1:19){
    dat$corr_any[corr[,j]==1] <- 1
  }
}

#15. arts variables ####
arts <- dat[,1756:1774] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$arts_any <- 0
  for(j in 1:19){
    dat$arts_any[arts[,j]==1] <- 1
  }
}

#16. env variables ####
env <- dat[,1776:1794] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$env_any <- 0
  for(j in 1:19){
    dat$env_any[env[,j]==1] <- 1
  }
}

#17. econ variables ####
econ <- dat[,1796:1814] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$econ_any <- 0
  for(j in 1:19){
    dat$econ_any[econ[,j]==1] <- 1
  }
}

#18. agr variables ####
agr <- dat[,1816:1834] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$agr_any <- 0
  for(j in 1:19){
    dat$agr_any[agr[,j]==1] <- 1
  }
}

#19. noserv variables ####
noserv <- dat[,1836:1854] # pull out the 'cash variables #

# loop through cash variables and if any of them are 'checked' we check the new variable
# older_any (set equal to 1) #
for(i in 1:nrow(dat)){
  dat$noserv_any <- 0
  for(j in 1:19){
    dat$noserv_any[noserv[,j]==1] <- 1
  }
}

#Export Serv19 Variable Data
Serv19_Data <- dat[,1876:1894]
write.csv(Serv19_Data, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Serv19_Data.csv')

# 1. Narrow down to infrastructure variables (personnel, buildings, equipment) as candidate predictors
# 2. Figure out why so many missing variables - fix any of them?

#Missing Values ####
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
infra_vars <- dat[25:678]

# Count missing values in infrastructure variables
sort(apply(is.na(infra_vars),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(infra_vars),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Missingness count.csv')

# Count missing values in a single var
sum(is.na(infra_vars$c3q417m))

#Load in infrastructure data frame ####
#edits: moved vars need to be excluded forward & delete Last column
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis')
dat <-read.csv('NACCHO_NALSYS_infrastructure.csv')

#Bivariate Analysis####
#1.Loop through the independent variable list
#2.Fit a bivariate logistic regression
#3.Save results for each ind-dep pair: var name, coefficient, & p-value
--------------------------------------------------------------------------
##Bivariate-AV1####
#Create an empty data frame
#nrows=298-24=274
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av1 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av1) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av1~dat[,i+24],family=binomial,data=dat)
  results_av1[i,1] <- names(dat)[i+24]
  results_av1[i,2] <- coef(model)[2]
  results_av1[i,3] <- summary(model)[[13]][2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av1, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av1.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av1 <- results_av1$VarName[results_av1$Pvalue<=0.05]
write.csv(SignificantVar_av1, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av1_check.csv')
-------------------------------------------------
##Bivariate-AV2####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av2 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av2) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av2~dat[,i+24],family=binomial,data=dat)
  results_av2[i,1] <- names(dat)[i+24]
  results_av2[i,2] <- coef(model)[2]
  results_av2[i,3] <- summary(model)[[13]][2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av2, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av2.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av2 <- results_av2$VarName[results_av2$Pvalue<=0.05]
write.csv(SignificantVar_av2, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av2.csv')
--------------------------------
##Bivariate-AV3####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av3 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av3) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av3~dat[,i+24],family=binomial,data=dat)
  results_av3[i,1] <- names(dat)[i+24]
  results_av3[i,2] <- coef(model)[2]
  results_av3[i,3] <- summary(model)[[13]][2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av3, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av3.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av3 <- results_av3$VarName[results_av3$Pvalue<=0.05]
write.csv(SignificantVar_av3, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av3.csv')
-----------------------------------
##Bivariate-AV4####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av4 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av4) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av4~dat[,i+24],family=binomial,data=dat)
  results_av4[i,1] <- names(dat)[i+24]
  results_av4[i,2] <- coef(model)[2]
  results_av4[i,3] <- summary(model)[[13]][2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av4, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av4.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av4 <- results_av4$VarName[results_av4$Pvalue<=0.05]
write.csv(SignificantVar_av4, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av4.csv')
----------------------------------------
##Bivariate-AV5####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av5 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av5) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av5~dat[,i+24],family=binomial,data=dat)
  results_av5[i,1] <- names(dat)[i+24]
  results_av5[i,2] <- coef(model)[2]
  results_av5[i,3] <- summary(model)[[13]][2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av5, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av5.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av5 <- results_av5$VarName[results_av5$Pvalue<=0.05]
write.csv(SignificantVar_av5, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av5.csv')
----------------------------------
##Bivariate-AV6####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av6 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av6) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av6~dat[,i+24],family=binomial,data=dat)
  results_av6[i,1] <- names(dat)[i+24]
  results_av6[i,2] <- coef(model)[2]
  results_av6[i,3] <- summary(model)[[13]][2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av6, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av6.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av6 <- results_av6$VarName[results_av6$Pvalue<=0.05]
write.csv(SignificantVar_av6, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av6.csv')
--------------------------------------
##Bivariate-AV7####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av7 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av7) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av7~dat[,i+24],family=binomial,data=dat)
  results_av7[i,1] <- names(dat)[i+24]
  results_av7[i,2] <- coef(model)[2]
  results_av7[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av7, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av7.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av7 <- results_av7$VarName[results_av7$Pvalue<=0.05]
write.csv(SignificantVar_av7, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av7.csv')
------------------------------------
##Bivariate-AV8####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av8 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av8) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av8~dat[,i+24],family=binomial,data=dat)
  results_av8[i,1] <- names(dat)[i+24]
  results_av8[i,2] <- coef(model)[2]
  results_av8[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av8, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av8.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av8 <- results_av8$VarName[results_av8$Pvalue<=0.05]
write.csv(SignificantVar_av8, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av8.csv')
-----------------------------------------------------
##Bivariate-AV9####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av9 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av9) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av9~dat[,i+24],family=binomial,data=dat)
  results_av9[i,1] <- names(dat)[i+24]
  results_av9[i,2] <- coef(model)[2]
  results_av9[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av9, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av9.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av9 <- results_av9$VarName[results_av9$Pvalue<=0.05]
write.csv(SignificantVar_av9, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av9.csv')
-------------------------------
##Bivariate-AV10####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av10 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av10) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av10~dat[,i+24],family=binomial,data=dat)
  results_av10[i,1] <- names(dat)[i+24]
  results_av10[i,2] <- coef(model)[2]
  results_av10[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av10, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av10.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av10 <- results_av10$VarName[results_av10$Pvalue<=0.05]
write.csv(SignificantVar_av10, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av10.csv')
----------------------------------
##Bivariate-AV11####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av11 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av11) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av11~dat[,i+24],family=binomial,data=dat)
  results_av11[i,1] <- names(dat)[i+24]
  results_av11[i,2] <- coef(model)[2]
  results_av11[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av11, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av11.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av11 <- results_av11$VarName[results_av11$Pvalue<=0.05]
write.csv(SignificantVar_av11, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av11.csv')
---------------------------------------------------
##Bivariate-AV12####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av12 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av12) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av12~dat[,i+24],family=binomial,data=dat)
  results_av12[i,1] <- names(dat)[i+24]
  results_av12[i,2] <- coef(model)[2]
  results_av12[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av12, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av12.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av12 <- results_av12$VarName[results_av12$Pvalue<=0.05]
write.csv(SignificantVar_av12, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av12.csv')
---------------------------------------------
##Bivariate-AV13####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av13 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av13) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av13~dat[,i+24],family=binomial,data=dat)
  results_av13[i,1] <- names(dat)[i+24]
  results_av13[i,2] <- coef(model)[2]
  results_av13[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av13, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av13.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av13 <- results_av13$VarName[results_av13$Pvalue<=0.05]
write.csv(SignificantVar_av13, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av13.csv')
----------------------------------------------
##Bivariate-AV14####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av14 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av14) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av14~dat[,i+24],family=binomial,data=dat)
  results_av14[i,1] <- names(dat)[i+24]
  results_av14[i,2] <- coef(model)[2]
  results_av14[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av14, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av14.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av14 <- results_av14$VarName[results_av14$Pvalue<=0.05]
write.csv(SignificantVar_av14, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av14.csv')
-------------------------------------------------
##Bivariate-AV15####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av15 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av15) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av15~dat[,i+24],family=binomial,data=dat)
  results_av15[i,1] <- names(dat)[i+24]
  results_av15[i,2] <- coef(model)[2]
  results_av15[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av15, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av15.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av15 <- results_av15$VarName[results_av15$Pvalue<=0.05]
write.csv(SignificantVar_av15, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av15.csv')
------------------------------------------
##Bivariate-AV16####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av16 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av16) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av16~dat[,i+24],family=binomial,data=dat)
  results_av16[i,1] <- names(dat)[i+24]
  results_av16[i,2] <- coef(model)[2]
  results_av16[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av16, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av16.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av16 <- results_av16$VarName[results_av16$Pvalue<=0.05]
write.csv(SignificantVar_av16, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av16.csv')
-------------------------------------------
##Bivariate-AV17####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av17 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av17) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av17~dat[,i+24],family=binomial,data=dat)
  results_av17[i,1] <- names(dat)[i+24]
  results_av17[i,2] <- coef(model)[2]
  results_av17[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av17, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av17.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av17 <- results_av17$VarName[results_av17$Pvalue<=0.05]
write.csv(SignificantVar_av17, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av17.csv')
----------------------------------
##Bivariate-AV18####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av18 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av18) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av18~dat[,i+24],family=binomial,data=dat)
  results_av18[i,1] <- names(dat)[i+24]
  results_av18[i,2] <- coef(model)[2]
  results_av18[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av18, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av18.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av18 <- results_av18$VarName[results_av18$Pvalue<=0.05]
write.csv(SignificantVar_av18, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av18.csv')
----------------------------------
##Bivariate-AV19####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av19 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av19) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av19~dat[,i+24],family=binomial,data=dat)
  results_av19[i,1] <- names(dat)[i+24]
  results_av19[i,2] <- coef(model)[2]
  results_av19[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av19, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av19.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av19 <- results_av19$VarName[results_av19$Pvalue<=0.05]
write.csv(SignificantVar_av19, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av19.csv')
----------------------------
##Bivariate-AV20####
#Create an empty data frame
###4=nacchoid, year, county_fips1, lhd_name
###20=av1-av20
results_av20 <- data.frame(matrix(0,nrow=274,ncol=3))

#Name the columns for the data frame
colnames(results_av20) <- c('VarName','Coefficient','Pvalue')

#Store VarName, Coefficient & P value
#Loop it through all potential independent variables
#Exclusion: av1-av20, 1st & last two vars)
for(i in 1:274){
  model <- glm(av20~dat[,i+24],family=binomial,data=dat)
  results_av20[i,1] <- names(dat)[i+24]
  results_av20[i,2] <- coef(model)[2]
  results_av20[i,3] <- summary(model)$coefficients[2,4] 
}
#results_av1[i,1] <- row.names(summary(model)[[13]])[2]

#Use write.csv to locate which var error occurs
#error: subscript out of bounds
write.csv(results_av20, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/results_av20.csv')

#Keep vars that have P value <= 0.2
SignificantVar_av20 <- results_av20$VarName[results_av20$Pvalue<=0.05]
write.csv(SignificantVar_av20, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/SignificantVar_av20.csv')
-----------------------------------------
##Frequency of SigVars####
#Compile av results and look across variable rows to calculate how often var is significant #
master_av <- cbind(results_av1[,1],results_av1[,3],results_av2[,3],results_av3[,3],results_av4[,3],results_av5[,3],results_av6[,3],results_av7[,3],results_av8[,3],results_av9[,3],results_av10[,3],results_av11[,3],results_av12[,3],results_av13[,3],results_av14[,3],results_av15[,3],results_av16[,3],results_av17[,3],results_av18[,3],results_av19[,3],results_av20[,3]) 
colnames(master_av) <- c('Var Name','av1','av2','av3','av4','av5','av6','av7','av8','av9','av10','av11','av12','av13','av14','av15','av16','av17','av18','av19','av20')
Sig_Count <- cbind(master_av[,1],apply(master_av[,2:21]<0.05,MARGIN=1,FUN=sum))
colnames(Sig_Count) <- c('Var Name','Sig Count')
write.csv(master_av, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/!MASTER_av_pvalue.csv')
write.csv(Sig_Count, file='C:/Users/mengz/OneDrive/Desktop/Thesis/Bivariate Analysis/!Sig_Count.csv')
-------------------------------------------------------
#Load in infrastructure data frame ####
#edits: moved vars need to be excluded forward & delete Last column
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis')
dat <-read.csv('NACCHO_NALSYS_infrastructure.csv')

# add state variable
dat$state <- substring(dat$nacchoid,1,2)

# dat for full models (IRR & state control vars)
dat_FullModel <-read.csv('C:/Users/mengz/OneDrive/Desktop/Thesis/dat_w_rurality.csv')
dat_FullModel$state <- substring(dat$nacchoid,1,2)

------------------------------------------------
#Multivariable Analysis####

  ##Multi-AV1####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av1_lookup <- read.csv('SignificantVar_av1_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form1 <- paste(colnames(dat[,SignificantVar_av1_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in c(2:133)){
  form1 <-  paste(form1,colnames(dat[,SignificantVar_av1_lookup[,2]])[i],sep="+",collapse="")
}

formula1 <- paste('av1',form1,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av1a <- dat[,c(5,SignificantVar_av1_lookup[,2])]

#Count the number of missing in each row #
av1_missingrows <- apply(is.na(dat_av1a),1,sum)

#Delete rows listwise #
dat_av1 <- dat_av1a[av1_missingrows==0,]
dim(dat_av1)

#reduced model_revised
model_av1_reduced <- glm(av1 ~ c4q34_Bachelors + c4q34_Masters + c4q502c + 
                    c5q70 + c5q43a + c5q59a + c5q55a + c5q60a + c5q47a + c5q48a + 
                    c5q61a + c5q49a + c5q50a + c5q52a + c5q56a + c5q65a + c10q307 + 
                    c6q58i + c6q60f + c6q65a + c6q65b + c6q66a + c6q66b + 
                    c6q67a + c6q69a + c6q69f + c6q71a + c6q74a + c6q76f + c6q78a + 
                    c6q82g + c6q85g + c6q86a + c6q87g + c6q89a + c6q89f + c6q90a + 
                    c6q145a + c6q146a + c6q143a + c6q111f + c6q113g + c6q115f + 
                    c6q116g + c6q120a + c6q127f + c6q144f + c6q134a + c6q137a + 
                    c10q405a + c10q406a + c10q408a + c10q418a + c12q260w + c12q260q + 
                    c12q260s + c12q260t + c12q260u + c12q260j + c12q260k + 
                    c7q217_Yes1 + c7q217_Yes2 + c7q217_No4, family = binomial, 
                  data = dat_av1)

model_av1_reduced2 <- step(model_av1_reduced) #backward selection is the default
summary(model_av1_reduced2)

#full model_revised
model_av1_full <- glm(av1 ~ c4q34_Masters + c5q43a + c5q59a + c5q47a + 
                        c10q307 + c6q58i + c6q66b + c6q67a + c6q76f + c6q78a + c6q86a + 
                        c6q145a + c6q146a + c6q113g + c6q115f + c6q144f + c10q405a + 
                        c10q406a + c10q408a + c12q260s + c12q260u + c12q260k + c7q217_Yes1 + 
                        c7q217_Yes2 + c7q217_No4+IRR2010+state, family = binomial, 
                       data = dat_FullModel)

summary(model_av1_full)

#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
# Don't forget to add form# single variable in c()!
dat_av1_run2 <- dat[,SignificantVar_av1_lookup[,2]]
#write.csv(dat_av1_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av1_run2.csv')

# Count missing values
sort(apply(is.na(dat_av1_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av1_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/model_av1_missing count.csv')
-------------------------------------------------------
  ##Multi-AV2####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av2_lookup <- read.csv('SignificantVar_av2_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form2 <- paste(colnames(dat[,SignificantVar_av2_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in c(2:132)){
  form2 <-  paste(form2,colnames(dat[,SignificantVar_av2_lookup[,2]])[i],sep="+",collapse="")
}
formula2 <- paste('av2',form2,sep='~')
options(max.print =10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av2a <- dat[,c(6,SignificantVar_av2_lookup[,2])]

#Count the number of missing in each row #
av2_missingrows <- apply(is.na(dat_av2a),1,sum)

#Delete rows listwise #
dat_av2 <- dat_av2a[av2_missingrows==0,]

model_av2 <- glm(av2~.,data=dat_av2, family = binomial)
model_av2B <- step(model_av2) #backward selection is the default
summary(model_av2B)
dim(dat_av2)

#final model = full model w interaction terms
model_av2_full <- glm(av2 ~ c4q26 + c2q501 + c5q63a + c5q48a + c5q64a + 
                        c10q307 + c6q58a + c6q59b + c6q60a + c6q65a + c6q73f + c6q74g + 
                        c6q78a + c6q80a + c6q90a + c6q146i + c6q95b + c6q107f + c6q118a + 
                        c6q127g + c6q131i + c6q136i + c10q418a + c12q260f + c12q260s + 
                        c12q260z + c7q147_Yes1 + c7q147_Yes3 + c7q217_Yes1 + c10q301_greater+IRR2010+state
                       +c6q58a*IRR2010+c6q65a*IRR2010,family = binomial, data = dat_FullModel)

summary(model_av2_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av2_run2 <- dat[,SignificantVar_av2_lookup[,2]]
#write.csv(dat_av2_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/!multi-av2_run1.csv')

# Count missing values
sort(apply(is.na(dat_av2_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av2_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/model_av2_missing count.csv')

-------------------------------------------------------
  ##Multi-AV3####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av3_lookup <- read.csv('SignificantVar_av3_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form3 <- paste(colnames(dat[,SignificantVar_av3_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:53){
  form3 <-  paste(form3,colnames(dat[,SignificantVar_av3_lookup[,2]])[i],sep="+",collapse="")
}

formula3 <- paste('av3',form3,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av3a <- dat[,c(7,SignificantVar_av3_lookup[,2])]

#Count the number of missing in each row #
av3_missingrows <- apply(is.na(dat_av3a),1,sum)

#Delete rows listwise #
dat_av3 <- dat_av3a[av3_missingrows==0,]
dim(dat_av3)

#Reduced model_revised
model_av3_reduced <- glm(av3 ~ c5q63a + c5q55a + c5q48a + c6q56b + 
                           c6q93i + c6q95f+ c6q136i + c7q501, family = binomial, 
                  data = dat_av3)
model_av3_reduced2 <- step(model_av3_reduced)
summary(model_av3_reduced2) #NOTE: c5q48a is dropped after 2nd backward elimination 

##Full model_revised
model_av3_full <- glm(av3 ~ c5q63a + c5q55a + c6q56b + c6q93i + c6q136i + 
                        c7q501+IRR2010+state, family = binomial, data = dat_FullModel)

summary(model_av3_full)

#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av3_run2 <- dat[,SignificantVar_av3_lookup[c(4,7,12:14,18,35,37,40,43,46,47,52,54,56,66,74,77,78,87),2]]
#write.csv(dat_av3_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av3_run2.csv')

# Count missing values
sort(apply(is.na(dat_av3_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av3_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av3_run2_missing count.csv')

-------------------------------------------------------
  ##Multi-AV4####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av4_lookup <- read.csv('SignificantVar_av4_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form4 <- paste(colnames(dat[,SignificantVar_av4_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:60){
  form4 <-  paste(form4,colnames(dat[,SignificantVar_av4_lookup[,2]])[i],sep="+",collapse="")
}

formula4 <- paste('av4',form4,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av4a <- dat[,c(8,SignificantVar_av4_lookup[,2])]

#Count the number of missing in each row #
av4_missingrows <- apply(is.na(dat_av4a),1,sum)

#Delete rows listwise #
dat_av4 <- dat_av4a[av4_missingrows==0,]

model_av4 <- glm(av4~.,data=dat_av4, family = binomial)
model_av4B <- step(model_av4) #backward selection is the default
summary(model_av4B)
dim(dat_av4)

#full model
model_av4_full <- glm(av4 ~ c4q25 + c4q502b + c5q47a + c5q48a + c5q49a + 
                    c6q61g + c6q75f + c6q79g + c6q86a + c6q91g + c6q146g + c6q95a + 
                    c6q143b + c6q109g + c6q114g + c7q501 +IRR2010+ state, family = binomial, 
                  data = dat_FullModel)

summary(model_av4_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av4_run2 <- dat[,SignificantVar_av4_lookup[c(19,25,26,28:30,33,45,46,60,63,83,124,140,145,167),2]]
#write.csv(dat_av4_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av4_run2.csv')

# Count missing values
sort(apply(is.na(dat_av4_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av4_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av4_run2_missing count.csv')

-------------------------------------------------------
  ##Multi-AV5####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av5_lookup <- read.csv('SignificantVar_av5_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form5 <- paste(colnames(dat[,SignificantVar_av5_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in c(2:159)){
  form5 <-  paste(form5,colnames(dat[,SignificantVar_av5_lookup[,2]])[i],sep="+",collapse="")
}

formula5 <- paste('av5',form5,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av5a <- dat[,c(9,SignificantVar_av5_lookup[,2])]

#Count the number of missing in each row #
av5_missingrows <- apply(is.na(dat_av5a),1,sum)

#Delete rows listwise #
dat_av5 <- dat_av5a[av5_missingrows==0,]

model_av5 <- glm(av5~.,data=dat_av5, family = binomial)
model_av5B <- step(model_av5) #backward selection is the default
summary(model_av5B)
dim(dat_av5)

#final model = full model w interaction terms
model_av5_full <- glm(av5 ~ c4q34_Masters + c5q60a + c5q47a + c5q52a + 
                    c6q65b + c6q67a + c6q69b + c6q69i + c6q71a + c6q73f + c6q76g + 
                    c6q79f + c6q81b + c6q83f + c6q86a + c6q89b + c6q146b + c6q100b + 
                    c6q105f + c6q109g + c6q110f + c6q117g + c6q119f + c6q127f + 
                    c6q134a + c6q136i + c10q406a + c10q417a + c10q418a + c12q260q + 
                    c7q149_Yes + c7q217_Yes2 + c7q217_Yes3 + IRR2010+state+IRR2010*c6q86a
                    +IRR2010*c10q417a,family=binomial, data = dat_FullModel)

summary(model_av5_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av5_run2 <- dat[,SignificantVar_av5_lookup[,2]]
#write.csv(dat_av5_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/!multi-av5_run1.csv')

# Count missing values
sort(apply(is.na(dat_av5_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av5_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/model_av5_missing count.csv')

--------------------------------------------
  ##Multi-AV6####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av6_lookup <- read.csv('SignificantVar_av6_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form6 <- paste(colnames(dat[,SignificantVar_av6_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:74){
  form6 <-  paste(form6,colnames(dat[,SignificantVar_av6_lookup[,2]])[i],sep="+",collapse="")
}

formula6 <- paste('av6',form6,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av6a <- dat[,c(10,SignificantVar_av6_lookup[,2])]

#Count the number of missing in each row #
av6_missingrows <- apply(is.na(dat_av6a),1,sum)

#Delete rows listwise #
dat_av6 <- dat_av6a[av6_missingrows==0,]

model_av6 <- glm(av6~.,data=dat_av6,family=binomial)
summary(model_av6)
model_av6B <- step(model_av6) #backward selection is the default
summary(model_av6B)
dim(dat_av6)

#Full Model
model_av6_full <- glm(av6 ~ c5q36 + c5q37 + c5q51a + c6q61a + c6q65i + 
                    c6q80a + c6q81a + c6q83g + c6q86a + c6q86i + c6q89b + c6q145a + 
                    c6q107g + c6q120f + c10q418a + c12q260n + c12q260e + c12q260t + 
                    c12q260j + c12q501 + c7q149_Yes + IRR2010 +state, family = binomial, 
                  data = dat_FullModel)

summary(model_av6_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av6_run2 <- dat[,SignificantVar_av6_lookup[c(30,43,49,57,75,81,85,94,163,203,211,213,231,236,248,267,309,316,318),2]]
#write.csv(dat_av6_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av6_run2.csv')

# Count missing values
sort(apply(is.na(dat_av6_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av6_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av6_run2_missing count.csv')

--------------------------------------------
  ##Multi-AV7####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av7_lookup <- read.csv('SignificantVar_av7_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form7 <- paste(colnames(dat[,SignificantVar_av7_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:60){
  form7 <-  paste(form7,colnames(dat[,SignificantVar_av7_lookup[,2]])[i],sep="+",collapse="")
}

formula7 <- paste('av7',form7,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av7a <- dat[,c(11,SignificantVar_av7_lookup[,2])]

#Count the number of missing in each row #
av7_missingrows <- apply(is.na(dat_av7a),1,sum)

#Delete rows listwise #
dat_av7 <- dat_av7a[av7_missingrows==0,]

model_av7 <- glm(av7~.,data=dat_av7,family=binomial)
summary(model_av7)
model_av7B <- step(model_av7) #backward selection is the default
summary(model_av7B)
dim(dat_av7)

#Full Model
model_av7_full <- glm(av7 ~ c4q26 + c5q49a + c6q57b + c6q81a + c6q89a + 
                    c6q90a + c6q145f + c6q95b + c6q95f + c6q117f + c6q131f + 
                    c10q417a + c10q418a+IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av7_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av7_run2 <- dat[,SignificantVar_av7_lookup[c(1,5,12,13,33,36,40,53,61,64,86,87,137,142,162,192),2]]
#write.csv(dat_av7_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av7_run2.csv')

# Count missing values
sort(apply(is.na(dat_av7_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av7_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av7_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV8####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av8_lookup <- read.csv('SignificantVar_av8_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form8 <- paste(colnames(dat[,SignificantVar_av8_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:113){
  form8 <-  paste(form8,colnames(dat[,SignificantVar_av8_lookup[,2]])[i],sep="+",collapse="")
}

formula8 <- paste('av8',form8,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av8a <- dat[,c(12,SignificantVar_av8_lookup[,2])]

#Count the number of missing in each row #
av8_missingrows <- apply(is.na(dat_av8a),1,sum)

#Delete rows listwise #
dat_av8 <- dat_av8a[av8_missingrows==0,]

model_av8 <- glm(av8~.,data=dat_av8,family=binomial)
summary(model_av8)
model_av8B <- step(model_av8) #backward selection is the default
summary(model_av8B)
dim(dat_av8)

#Final model = full Model w interaction terms
model_av8_full <- glm(av8 ~ c4q34_Masters + c4q502c + c2q501 + c5q36 + 
                    c5q70 + c5q44a + c5q55a + c5q52a + c5q56a + c6q62a + c6q65b + 
                    c6q67b + c6q78a + c6q81i + c6q83g + c6q89i + c6q90a + c6q145a + 
                    c6q101g + c6q102a + c6q143a + c6q109g + c6q114a + c6q118a + 
                    c6q118i + c6q124f + c6q128g + c6q134b + c12q260f + c12q260q + 
                    c12q260t + c12q260x + c12q260y + c12q260z + c12q261 + c12q501+IRR2010
                    +state+IRR2010*c4q502c+IRR2010*c5q55a,data=dat_FullModel,family=binomial)

summary(model_av8_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av8_run2 <- dat[,SignificantVar_av8_lookup[c(22,55,59,82,105,121,183,213,229,230,242,293,298,305,313,320),2]]
#write.csv(dat_av8_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av8_run2.csv')

# Count missing values
sort(apply(is.na(dat_av8_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av8_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av8_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV9####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av9_lookup <- read.csv('SignificantVar_av9_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form9 <- paste(colnames(dat[,SignificantVar_av9_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:118){
  form9 <-  paste(form9,colnames(dat[,SignificantVar_av9_lookup[,2]])[i],sep="+",collapse="")
}

formula9 <- paste('av9',form9,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av9a <- dat[,c(13,SignificantVar_av9_lookup[,2])]

#Count the number of missing in each row #
av9_missingrows <- apply(is.na(dat_av9a),1,sum)

#Delete rows listwise #
dat_av9 <- dat_av9a[av9_missingrows==0,]

model_av9 <- glm(av9~.,data=dat_av9,family=binomial)
summary(model_av9)
model_av9B <- step(model_av9) #backward selection is the default
summary(model_av9B)
dim(dat_av9)

#Final model = full Model w interaction terms
model_av9_full <- glm(av9 ~ c4q26 + c2q301 + c5q37 + c5q55a + c5q47a + 
                     c5q61a + c5q52a + c10q308 + c6q55g + c6q61a + c6q62a + c6q66b + 
                     c6q74a + c6q78a + c6q80a + c6q81a + c6q83g + c6q85g + c10q418a + 
                     c12q260s + c12q260u + c12q260y + c7q149_Yes + c7q149_No4 + 
                     c7q217_Yes1 + c7q217_Yes2 + c7q217_No4+IRR2010+state
                     +IRR2010*c2q301+IRR2010*c5q37+IRR2010*c7q217_No4,data=dat_FullModel,family=binomial)

summary(model_av9_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av9_run2 <- dat[,SignificantVar_av9_lookup[c(34,82,119,121,176,192,230,286,332,344,351),2]]
#write.csv(dat_av9_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av9_run2.csv')

# Count missing values
sort(apply(is.na(dat_av9_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av9_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av9_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV10####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av10_lookup <- read.csv('SignificantVar_av10_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form10 <- paste(colnames(dat[,SignificantVar_av10_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:133){
  form10 <-  paste(form10,colnames(dat[,SignificantVar_av10_lookup[,2]])[i],sep="+",collapse="")
}

formula10 <- paste('av10',form10,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av10a <- dat[,c(14,SignificantVar_av10_lookup[,2])]

#Count the number of missing in each row #
av10_missingrows <- apply(is.na(dat_av10a),1,sum)

#Delete rows listwise #
dat_av10 <- dat_av10a[av10_missingrows==0,]

model_av10 <- glm(av10~.,data=dat_av10,family=binomial)
summary(model_av10)
model_av10B <- step(model_av10) #backward selection is the default
summary(model_av10B)
dim(dat_av10)

#Reduced model_revised
model_av10_reduced <- glm(av10 ~ c5q70 + c5q43a + c5q47a + c5q61a + c5q49a + 
                     c5q50a + c5q51a + c5q56a + c6q58a + c6q58b + c6q60a + c6q60f + 
                     c6q62a + c6q65a + c6q65f + c6q66b + c6q69f + c6q80a + c6q81a + 
                     c6q86a + c6q89a + c6q89i + c6q100b + c6q102a + 
                     c6q107g + c6q109f + c6q111f + c6q119f + c6q127f + c6q131f + 
                     c6q136g + c6q137a + c10q415a + c10q408a + c12q260e + c12q260w + 
                     c12q260r + c12q260u + c12q260k + c12q260y + 
                     c7q501 + c7q217_Yes1 + c7q217_Yes2 + c7q217_No4,data=dat_av10,family=binomial)

model_av10_reduced2 <- step(model_av10_reduced)
summary(model_av10_reduced)

#Full model_revised
model_av10_full <- glm(av10 ~ c5q70 + c5q43a + c5q47a + c5q61a + c5q49a + 
                         c5q50a + c5q51a + c5q56a + c6q58a + c6q58b + c6q60a + c6q60f + 
                         c6q62a + c6q65a + c6q65f + c6q66b + c6q69f + c6q80a + c6q81a + 
                         c6q86a + c6q89a + c6q89i + c6q100b + c6q102a + 
                         c6q107g + c6q109f + c6q111f + c6q119f + c6q127f + c6q131f + 
                         c6q136g + c6q137a + c10q415a + c10q408a + c12q260e + c12q260w + 
                         c12q260r + c12q260u + c12q260k + c12q260y + 
                         c7q501 + c7q217_Yes1 + c7q217_Yes2 + c7q217_No4+IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av10_full)


#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av10_run2 <- dat[,SignificantVar_av10_lookup[c(11,13,17,31,41,49,54,69,85,93,95,111,116,122,134,137:139,148,165,172,182,188,193,237,240,241,249,251,253,268,269,272,279,286,293,299,302,304,307,308,313,319,324,328,335,360),2]]
#write.csv(dat_av10_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av10_run2.csv')

# Count missing values
sort(apply(is.na(dat_av10_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av10_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av10_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV11####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av11_lookup <- read.csv('SignificantVar_av11_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form11 <- paste(colnames(dat[,SignificantVar_av11_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:112){
  form11 <-  paste(form11,colnames(dat[,SignificantVar_av11_lookup[,2]])[i],sep="+",collapse="")
}

formula11 <- paste('av11',form11,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av11a <- dat[,c(15,SignificantVar_av11_lookup[,2])]

#Count the number of missing in each row #
av11_missingrows <- apply(is.na(dat_av11a),1,sum)

#Delete rows listwise #
dat_av11 <- dat_av11a[av11_missingrows==0,]

model_av11 <- glm(av11~.,data=dat_av11,family=binomial)
model_av11B <- step(model_av11) #backward selection is the default
summary(model_av11B)
dim(dat_av11)

#Reduced Model
model_av11_reduced <- glm(av11 ~ c4q34_Masters + c5q60a + c5q50a + c5q56a + 
                     c5q64a + c6q58b + c6q59i + c6q60f + c6q66b + c6q76i + c6q81a + 
                     c6q86a + c6q145a + c6q146a + c6q97f + c6q119f + c6q127f + 
                     c6q134a + c6q134f + c10q405a + c12q260e + c12q260r + c12q260h + 
                     c7q147_Yes3 + c7q149_Yes + c7q149_Yes3 + c7q217_Yes1 + c7q217_Yes2 + 
                     c7q217_No4,data=dat_av11,family=binomial)

summary(model_av11_reduced)

#Full Model
model_av11_full <- glm(av11 ~ c4q34_Masters + c5q60a + c5q50a + c5q56a + 
                     c5q64a + c6q58b + c6q59i + c6q60f + c6q66b + c6q76i + c6q81a + 
                     c6q86a + c6q145a + c6q146a + c6q97f + c6q119f + c6q127f + 
                     c6q134a + c6q134f + c10q405a + c12q260e + c12q260r + c12q260h + 
                     c7q147_Yes3 + c7q149_Yes + c7q149_Yes3 + c7q217_Yes1 + c7q217_Yes2 + 
                     c7q217_No4+IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av11_full)
#copy tables to av# table word doc side by side by the old table


#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av11_run2 <- dat[,SignificantVar_av11_lookup[c(),2]]
#write.csv(dat_av11_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av11_run2.csv')

# Count missing values
sort(apply(is.na(dat_av11_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av11_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av11_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV12####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av12_lookup <- read.csv('SignificantVar_av12_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form12 <- paste(colnames(dat[,SignificantVar_av12_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:83){
  form12 <-  paste(form12,colnames(dat[,SignificantVar_av12_lookup[,2]])[i],sep="+",collapse="")
}

formula12 <- paste('av12',form12,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av12a <- dat[,c(16,SignificantVar_av12_lookup[,2])]

#Count the number of missing in each row #
av12_missingrows <- apply(is.na(dat_av12a),1,sum)

#Delete rows listwise #
dat_av12 <- dat_av12a[av12_missingrows==0,]

model_av12 <- glm(av12~.,data=dat_av12,family=binomial)
model_av12B <- step(model_av12) #backward selection is the default
summary(model_av12B)
dim(dat_av12)

#Add in state and rurality as control variables - Full Model
model_av12_full <- glm(av12 ~ c4q26 + c5q48a + c6q56i + c6q60b + c6q61a + 
                     c6q62a + c6q71i + c6q78b + c6q80a + c6q80i + c6q81b + c6q82g + 
                     c6q90a + c6q145a + c6q97f + c6q101f + c6q131b + c10q418a + 
                     c12q260r + c12q260t + c12q260u + c12q260k + c7q149_Yes3 + 
                     c7q217_No4+IRR2010+state+IRR2010*c6q145a,data=dat_FullModel,family=binomial)

summary(model_av12_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av12_run2 <- dat[,SignificantVar_av12_lookup[c(),2]]
#write.csv(dat_av12_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av12_run2.csv')

# Count missing values
sort(apply(is.na(dat_av12_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av12_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av12_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV13####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av13_lookup <- read.csv('SignificantVar_av13_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form13 <- paste(colnames(dat[,SignificantVar_av13_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:104){
  form13 <-  paste(form13,colnames(dat[,SignificantVar_av13_lookup[,2]])[i],sep="+",collapse="")
}

formula13 <- paste('av13',form13,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av13a <- dat[,c(17,SignificantVar_av13_lookup[,2])]

#Count the number of missing in each row #
av13_missingrows <- apply(is.na(dat_av13a),1,sum)

#Delete rows listwise #
dat_av13 <- dat_av13a[av13_missingrows==0,]

model_av13 <- glm(av13~.,data=dat_av13,family=binomial)
model_av13B <- step(model_av13) #backward selection is the default
summary(model_av13B)
dim(dat_av13)

#Full Model
model_av13_full <- glm(av13 ~ c4q34_Masters + c5q37 + c5q70 + c5q43a + 
                     c5q61a + c10q308 + c6q60f + c6q66b + c6q71i + c6q80a + c6q86a + 
                     c6q90a + c6q91b + c6q93a + c6q145f + c6q95b + c6q110g + c6q111f + 
                     c6q119f + c6q134f + c6q136f + c12q260f + c12q260r + c12q260t + 
                     c12q260y + c7q147_Yes1 + c7q149_No4 + c10q301_greater+IRR2010
                   +state+IRR2010*c4q34_Masters,data=dat_FullModel,family=binomial)

summary(model_av13_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av13_run2 <- dat[,SignificantVar_av13_lookup[c(),2]]
#write.csv(dat_av13_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av13_run2.csv')

# Count missing values
sort(apply(is.na(dat_av13_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av13_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av13_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV14####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av14_lookup <- read.csv('SignificantVar_av14_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form14 <- paste(colnames(dat[,SignificantVar_av14_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:93){
  form14 <-  paste(form14,colnames(dat[,SignificantVar_av14_lookup[,2]])[i],sep="+",collapse="")
}

formula14 <- paste('av14',form14,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av14a <- dat[,c(18,SignificantVar_av14_lookup[,2])]

#Count the number of missing in each row #
av14_missingrows <- apply(is.na(dat_av14a),1,sum)

#Delete rows listwise #
dat_av14 <- dat_av14a[av14_missingrows==0,]

model_av14 <- glm(av14~.,data=dat_av14,family=binomial)
model_av14B <- step(model_av14) #backward selection is the default
summary(model_av14B)
dim(dat_av14)

#Full Model
model_av14_full <- glm(av14 ~ c2q301 + c5q37 + c5q44a + c5q55a + c10q307 + 
                     c6q60i + c6q65a + c6q65b + c6q71i + c6q81a + c6q86i + c6q89i + 
                     c6q95b + c6q118i + c12q260n + c12q260t + c7q217_Yes1
                   +IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av14_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av14_run2 <- dat[,SignificantVar_av14_lookup[c(),2]]
#write.csv(dat_av14_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av14_run2.csv')

# Count missing values
sort(apply(is.na(dat_av14_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av14_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av14_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV15####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av15_lookup <- read.csv('SignificantVar_av15_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form15 <- paste(colnames(dat[,SignificantVar_av15_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:69){
  form15 <-  paste(form15,colnames(dat[,SignificantVar_av15_lookup[,2]])[i],sep="+",collapse="")
}

formula15 <- paste('av15',form15,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av15a <- dat[,c(19,SignificantVar_av15_lookup[,2])]

#Count the number of missing in each row #
av15_missingrows <- apply(is.na(dat_av15a),1,sum)

#Delete rows listwise #
dat_av15 <- dat_av15a[av15_missingrows==0,]

model_av15 <- glm(av15~.,data=dat_av15,family=binomial)
model_av15B <- step(model_av15) #backward selection is the default
summary(model_av15B)
dim(dat_av15)

#Full Model
model_av15_full <- glm(av15 ~ c5q36 + c5q37 + c5q52a + c6q56b + c6q75b + 
                     c6q86a + c6q89b + c6q145b + c6q107g + c6q121g + c10q418a + 
                     c12q260x + c10q303_less + c10q303_greater
                   +IRR2010+IRR2010*c6q86a+state,data=dat_FullModel,family=binomial)

summary(model_av15_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av15_run2 <- dat[,SignificantVar_av15_lookup[c(),2]]
#write.csv(dat_av15_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av15_run2.csv')

# Count missing values
sort(apply(is.na(dat_av15_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av15_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av15_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV16####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av16_lookup <- read.csv('SignificantVar_av16_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form16 <- paste(colnames(dat[,SignificantVar_av16_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:72){
  form16 <-  paste(form16,colnames(dat[,SignificantVar_av16_lookup[,2]])[i],sep="+",collapse="")
}

formula16 <- paste('av16',form16,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av16a <- dat[,c(20,SignificantVar_av16_lookup[,2])]

#Count the number of missing in each row #
av16_missingrows <- apply(is.na(dat_av16a),1,sum)

#Delete rows listwise #
dat_av16 <- dat_av16a[av16_missingrows==0,]

model_av16 <- glm(av16~.,data=dat_av16,family=binomial)
model_av16B <- step(model_av16) #backward selection is the default
summary(model_av16B)
dim(dat_av16)

#Add in state and rurality as control variables - Full Model
model_av16_full <- glm(av16 ~ c5q48a + c10q307 + c6q65b + c6q79f + c6q80a + 
                     c6q91a + c6q95b + c6q143i + c6q111f + c6q114i + c6q120i + 
                     c10q406a + c10q418a + c12q260w + c12q501 + c7q149_No4 + c7q217_Yes1
                     +IRR2010+state+IRR2010*c6q95b,data=dat_FullModel,family=binomial)

summary(model_av16_full)
#copy tables to av# table word doc side by side by the old table

#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av16_run2 <- dat[,SignificantVar_av16_lookup[c(),2]]
#write.csv(dat_av16_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av16_run2.csv')

# Count missing values
sort(apply(is.na(dat_av16_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av16_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av16_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV17####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av17_lookup <- read.csv('SignificantVar_av17_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form17 <- paste(colnames(dat[,SignificantVar_av17_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:107){
  form17 <-  paste(form17,colnames(dat[,SignificantVar_av17_lookup[,2]])[i],sep="+",collapse="")
}

formula17 <- paste('av17',form17,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av17a <- dat[,c(21,SignificantVar_av17_lookup[,2])]

#Count the number of missing in each row #
av17_missingrows <- apply(is.na(dat_av17a),1,sum)

#Delete rows listwise #
dat_av17 <- dat_av17a[av17_missingrows==0,]

model_av17 <- glm(av17~.,data=dat_av17,family=binomial)
summary(model_av17)
model_av17B <- step(model_av17) #backward selection is the default
summary(model_av17B)
dim(dat_av17)

#Add in state and rurality as control variables - Full Model
model_av17_full <- glm(av17 ~ c4q25 + c5q36 + c5q37 + c5q51a + c5q52a + 
                     c5q64a + c6q56i + c6q57a + c6q60f + c6q62a + c6q78a + c6q80i + 
                     c6q81a + c6q81b + c6q81i + c6q82g + c6q95a + c6q107g + c6q110g + 
                     c6q111f + c6q114i + c6q124f + c6q124g + c6q130a + c10q405a + 
                     c10q416a + c10q418a + c12q260e + c12q260q + c12q260h + c12q260t + 
                     c12q260x + c12q261 + c7q149_Yes + c7q149_Yes3 + c7q149_No4 + 
                     c7q501 + c7q217_Yes1 + c10q301_greater+IRR2010+state+IRR2010*c5q36
                    +IRR2010*c5q37+IRR2010*c12q261,data=dat_FullModel,family=binomial)

summary(model_av17_full)
#copy tables to av# table word doc side by side by the old table


#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av17_run2 <- dat[,SignificantVar_av17_lookup[c(),2]]
#write.csv(dat_av17_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av17_run2.csv')

# Count missing values
sort(apply(is.na(dat_av17_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av17_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av17_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV18####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av18_lookup <- read.csv('SignificantVar_av18_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form18 <- paste(colnames(dat[,SignificantVar_av18_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:91){
  form18 <-  paste(form18,colnames(dat[,SignificantVar_av18_lookup[,2]])[i],sep="+",collapse="")
}

formula18 <- paste('av18',form18,sep='~')
options(max.print = 10000)        # Change global options
# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av18a <- dat[,c(22,SignificantVar_av18_lookup[,2])]

#Count the number of missing in each row #
av18_missingrows <- apply(is.na(dat_av18a),1,sum)

#Delete rows listwise #
dat_av18 <- dat_av18a[av18_missingrows==0,]

model_av18 <- glm(av18~.,data=dat_av18,family=binomial)
model_av18B <- step(model_av18) #backward selection is the default
summary(model_av18B)
dim(dat_av18)

#Add in state and rurality as control variables - Full Model
model_av18_full <- glm(av18 ~ c5q60a + c5q47a + c5q51a + c6q57a + c6q65a + 
                     c6q80a + c6q89a + c6q89f + c6q146a + c6q100b + c6q116g + 
                     c6q124f + c6q130b + c12q260e + c12q260r + c7q149_No4 +
                    c7q217_Yes2+IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av18_full)
#copy tables to av# table word doc side by side by the old table


#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av18_run2 <- dat[,SignificantVar_av18_lookup[c(),2]]
#write.csv(dat_av18_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av18_run2.csv')

# Count missing values
sort(apply(is.na(dat_av18_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av18_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av18_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV19####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av19_lookup <- read.csv('SignificantVar_av19_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form19 <- paste(colnames(dat[,SignificantVar_av19_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:84){
  form19 <-  paste(form19,colnames(dat[,SignificantVar_av19_lookup[,2]])[i],sep="+",collapse="")
}

formula19 <- paste('av19',form19,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av19a <- dat[,c(23,SignificantVar_av19_lookup[,2])]

#Count the number of missing in each row #
av19_missingrows <- apply(is.na(dat_av19a),1,sum)

#Delete rows listwise #
dat_av19 <- dat_av19a[av19_missingrows==0,]

model_av19 <- glm(av19~.,data=dat_av19,family=binomial)
model_av19B <- step(model_av19) #backward selection is the default
summary(model_av19B)
dim(dat_av19)

#Full Model
model_av19_full <- glm(av19 ~ c4q34_Masters + c5q59a + c6q57a + c6q69a + 
                     c6q77a + c6q91b + c6q100a + c6q131f + c10q405a + c10q418a + 
                     c7q149_Yes2+IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av19_full)
#copy tables to av# table word doc side by side by the old table


#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av19_run2 <- dat[,SignificantVar_av19_lookup[c(),2]]
#write.csv(dat_av19_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av19_run2.csv')

# Count missing values
sort(apply(is.na(dat_av19_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av19_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av19_run2_missing count.csv')

------------------------------------------------
  ##Multi-AV20####
#Find all column # of infrastructure variables significant to av1
setwd('C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis')
SignificantVar_av20_lookup <- read.csv('SignificantVar_av20_lookup.csv')

#Use for loop to trim dat to only have predictors significant to av1
#form1 = first variable name
#find av#_lookup file: row#-1
form20 <- paste(colnames(dat[,SignificantVar_av20_lookup[,2]])[1],sep="+",collapse="")

#for loop starts with 2 because i=1 and i=2 repeats the first variable
for(i in 2:38){
  form20 <-  paste(form20,colnames(dat[,SignificantVar_av20_lookup[,2]])[i],sep="+",collapse="")
}

formula20 <- paste('av20',form20,sep='~')
options(max.print = 10000)        # Change global options

# Alternative Strategy to Backward Elimination #

#Remove all rows missing in the covariates #
#Note: 5 is the column position of av1 in dat data frame
dat_av20a <- dat[,c(24,SignificantVar_av20_lookup[,2])]

#Count the number of missing in each row #
av20_missingrows <- apply(is.na(dat_av20a),1,sum)

#Delete rows listwise #
dat_av20 <- dat_av20a[av20_missingrows==0,]

model_av20 <- glm(av20~.,data=dat_av20,family=binomial)
model_av20B <- step(model_av20) #backward selection is the default - Reduced Model
summary(model_av20B)
dim(dat_av20)

#Full Model
model_av20_full <- glm(av20 ~ c6q60a + c6q73g + c6q74i + c6q75b + c6q79b + 
                     c6q145a + c6q101f + c6q113g + c6q118f + c6q120b + c6q128f + 
                     c10q418a + c12q260j + c7q147_No4 + c7q149_Yes3 + c7q217_Yes1 + 
                     c10q301_greater+IRR2010+state,data=dat_FullModel,family=binomial)

summary(model_av20_full)
#copy tables to av# table word doc side by side by the old table


#Missing Values
# Note: Threshold=5% (=660*0.05=33; If a variable has at least 33 missing, then it's problematic - delete it)
# Trim infrastructure data to only variables of interests
dat_av20_run2 <- dat[,SignificantVar_av20_lookup[c(),2]]
#write.csv(dat_av20_run2,file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av20_run2.csv')

# Count missing values
sort(apply(is.na(dat_av20_run2),2,sum),decreasing = TRUE) # count and sort number of missing values for each variable
write.csv(sort(apply(is.na(dat_av20_run2),2,sum),decreasing = TRUE), file='C:/Users/mengz/OneDrive/Desktop/Thesis/Multivariable Analysis Results (missingness & bivariate @5%)/multi-av20_run2_missing count.csv')

------------------------------------------------

