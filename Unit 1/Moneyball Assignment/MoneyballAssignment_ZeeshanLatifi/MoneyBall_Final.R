#Zeeshan Latifi
######## Predict 411, Template for Unit 1
######## Video 1:  https://youtu.be/5DOzWVyfKoQ
######## Video 2:  https://youtu.be/68fb5vWDrFA

######## Download appropriate packages and install them from (https://cran.r-project.org/web/packages/available_packages_by_name.html)
library(lattice)
library(lme4)
library(Matrix)
library(mice)
library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(xlsxjars)
library(xlsx)
#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

setwd("/Users/Zeeshan/Desktop/PREDICT 411/Unit 1/Moneyball Assignment/")
moneyball=read.csv("moneyball.csv",header=T)

############## Part 1: Data Exploration ##########################################################################
str(moneyball)
summary(moneyball)

# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
par(mfrow=c(1,2))
hist(moneyball$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(moneyball$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

################# Batting ####################
# Hits and Doubles
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(moneyball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(moneyball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(moneyball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(moneyball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(moneyball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(moneyball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

################ Pitching ############
# Hits and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(moneyball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(moneyball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

############## Fielding ###########
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(moneyball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(moneyball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))


######## Scatterplot Matrix ##########

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Batting Stats and Wins
pairs(moneyball[2:8], lower.panel=panel.smooth, upper.panel = panel.cor)

#Baserunning  Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_BASERUN_CS + moneyball$TEAM_BASERUN_SB, lower.panel = panel.smooth)

#Pitcher Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_PITCHING_BB + moneyball$TEAM_PITCHING_H + 
        moneyball$TEAM_PITCHING_HR + moneyball$TEAM_PITCHING_SO, lower.panel = panel.smooth)

pairs(moneyball[2,9,10,11,12,13])

######################### Part 2: Data Preparation #####################

#Fix Missing Values Using MICE (imputation) of All Seasons
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(moneyball,2,pMiss)
apply(moneyball,1,pMiss)

md.pattern(moneyball)

tempData <- mice(moneyball,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
densityplot(tempData)
completedData <- complete(tempData,1)


#Straighten Relationships
completedData$TEAM_BATTING_1B <- completedData$TEAM_BATTING_H - completedData$TEAM_BATTING_HR - completedData$TEAM_BATTING_3B -
  completedData$TEAM_BATTING_2B
completedData$log_TEAM_BATTING_1B <- log(completedData$TEAM_BATTING_1B)
completedData$log_TEAM_BATTING_3B <- log(completedData$TEAM_BATTING_3B)
completedData$log_TEAM_BASERUN_SB <- log(completedData$TEAM_BASERUN_SB)
completedData$log_TEAM_BASERUN_CS <- log(completedData$TEAM_BASERUN_CS)
completedData$log_TEAM_BATTING_HR <- log(completedData$TEAM_BATTING_HR)
completedData$log_TEAM_BATTING_SO <- log(completedData$TEAM_BATTING_SO)
completedData$log_TEAM_PITCHING_HR <- log(completedData$TEAM_PITCHING_HR)


completedData$TEAM_FIELDING_E[(completedData$TEAM_FIELDING_E > 500)] = 500

#Check that na's are gone. 
summary(completedData)

#Remove bad data from data set

moneyball2 <- subset(completedData, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball3 <- subset(moneyball2, TEAM_PITCHING_H < 2000)

#################### Part 3: Model Creation ############################################

#Function for Mean Square Error Calculation
mse <- function(sm) 
  mean(sm$residuals^2)

library(leaps)

reg1 <- regsubsets(TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR +
                     TEAM_BATTING_H +
                     TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR +
                     TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B +
                     log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + log_TEAM_BASERUN_CS +
                     sqrt_TEAM_PITCHING_HR + log_TEAM_BATTING_SO + log_TEAM_BATTING_3B + log_TEAM_BATTING_HR +
                     log_TEAM_PITCHING_HR,  data = moneyball3, nvmax = 10)

summary(reg1)


#model 1--------------------------------------------------------------------------------------------
stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + TEAM_BATTING_SO +
                      TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_FIELDING_DP + TEAM_BATTING_H +
                      log_TEAM_BATTING_SO + log_TEAM_BATTING_HR + log_TEAM_BASERUN_CS, data = moneyball3)

stepwise <- stepAIC(stepwisemodel, direction = "both")

summary(stepwise)
vif(stepwise)
sqrt(vif(stepwise)) > 2



#model 2--------------------------------------------------------------------------------------------
fwd_model <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + 
                  TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_FIELDING_DP + TEAM_BATTING_H +
                  log_TEAM_BATTING_SO + log_TEAM_BATTING_HR, data = moneyball3)

forward <- stepAIC(fwd_model, direction = "forward")

summary(forward)
vif(forward)
sqrt(vif(forward)) > 2


#model 3--------------------------------------------------------------------------------------------
step_model <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + 
                   TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_BATTING_H +
                   log_TEAM_BATTING_SO + log_TEAM_BATTING_HR, data = moneyball3)

step_prime <- stepAIC(step_model, direction = "both")

summary(step_prime)
vif(step_prime)
sqrt(vif(step_prime)) > 2
 

#model 4--------------------------------------------------------------------------------------------
model4 <- lm(TARGET_WINS ~ 
               TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + log_TEAM_BATTING_HR + 
               TEAM_BASERUN_SB + TEAM_BASERUN_CS + log_TEAM_BATTING_SO +
               TEAM_FIELDING_E + TEAM_FIELDING_DP +
               TEAM_PITCHING_SO + TEAM_PITCHING_BB, data = moneyball3)
summary(model4)
vif(model4)

step_test <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + 
                  TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_BATTING_H +
                  log_TEAM_BATTING_SO + log_TEAM_BATTING_HR, data = moneyball3)

step_ptest <- stepAIC(step_test, direction = "both")

summary(step_ptest)
vif(step_ptest)
sqrt(vif(step_test)) > 2

######## Performance #######
AIC(stepwisemodel)
AIC(forward)
AIC(step_prime)
AIC(model4)

mse(stepwisemodel)
mse(forward)
mse(step_prime)
mse(model4)

#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

#################### Test Data ##########################
setwd("/Users/Zeeshan/Desktop/PREDICT 411/Unit 1/Moneyball Assignment/")
moneyball_test=read.csv("moneyball_test.csv",header=T)

summary(moneyball_test)

# Fixing na's

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(moneyball_test,2,pMiss)
apply(moneyball_test,1,pMiss)

md.pattern(moneyball_test)

test_tempData <- mice(moneyball_test,m=5,maxit=50,meth='pmm',seed=500)
summary(test_tempData)
densityplot(test_tempData)
test_completedData <- complete(test_tempData,1)

summary(test_completedData)

test_completedData$TEAM_BATTING_1B <- test_completedData$TEAM_BATTING_H - test_completedData$TEAM_BATTING_HR - test_completedData$TEAM_BATTING_3B -
  test_completedData$TEAM_BATTING_2B
test_completedData$log_TEAM_BATTING_1B <- log(test_completedData$TEAM_BATTING_1B)
test_completedData$log_TEAM_BATTING_3B <- log(test_completedData$TEAM_BATTING_3B)
test_completedData$log_TEAM_BASERUN_SB <- log(test_completedData$TEAM_BASERUN_SB)
test_completedData$log_TEAM_BASERUN_CS <- log(test_completedData$TEAM_BASERUN_CS)
test_completedData$log_TEAM_BATTING_HR <- log(test_completedData$TEAM_BATTING_HR)
test_completedData$log_TEAM_BATTING_SO <- log(test_completedData$TEAM_BATTING_SO)
test_completedData$log_TEAM_PITCHING_HR <- log(test_completedData$TEAM_PITCHING_HR)


test_completedData$TEAM_FIELDING_E[(test_completedData$TEAM_FIELDING_E > 500)] = 500


test_completedData$P_TARGET_WINS <- 65.812072 +
  0.155482* test_completedData$TEAM_BATTING_3B +
  0.031275* test_completedData$TEAM_BATTING_BB +
  0.080680* test_completedData$TEAM_BASERUN_SB -
  0.092339* test_completedData$TEAM_FIELDING_E +
  0.023852* test_completedData$TEAM_BATTING_H -
  6.824848* test_completedData$log_TEAM_BATTING_SO +
  1.904556* test_completedData$log_TEAM_BATTING_HR
#subset of data set for the deliverable "Scored data file"
prediction <- test_completedData[c("INDEX","P_TARGET_WINS")]

#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Prediction File 
write.xlsx(prediction, file = "write.xlsx", sheetName = "Predictions",
           col.names = TRUE)
