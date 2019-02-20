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
#completedData$sqrt_TEAM_PITCHING_HR <- sqrt(completedData$TEAM_PITCHING_HR)
#completedData$SB_PCT <- completedData$TEAM_BASERUN_SB/(1.0*completedData$TEAM_BASERUN_SB+completedData$TEAM_BASERUN_CS)

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


# Stepwise Approach
stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + TEAM_BATTING_SO +
                      TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_FIELDING_DP + TEAM_BATTING_H +
                      log_TEAM_BATTING_SO + log_TEAM_BATTING_HR + log_TEAM_BASERUN_CS, data = moneyball3)

stepwise <- stepAIC(stepwisemodel, direction = "both")

summary(stepwise)
vif(stepwise)
sqrt(vif(stepwise)) > 2



#forward model
fwd_model <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + 
                      TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_FIELDING_DP + TEAM_BATTING_H +
                      log_TEAM_BATTING_SO + log_TEAM_BATTING_HR, data = moneyball3)

forward <- stepAIC(fwd_model, direction = "forward")

summary(forward)
vif(forward)
sqrt(vif(forward)) > 2


step_model <- lm(formula = TARGET_WINS ~ TEAM_BATTING_3B + TEAM_BATTING_BB + 
                  TEAM_BASERUN_SB + TEAM_FIELDING_E + TEAM_BATTING_H +
                  log_TEAM_BATTING_SO + log_TEAM_BATTING_HR, data = moneyball3)

step_prime <- stepAIC(step_model, direction = "both")

summary(step_prime)
vif(step_prime)
sqrt(vif(step_prime)) > 2

# stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR +
#                       TEAM_BATTING_H +
#                       TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR +
#                       TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B +
#                       log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + log_TEAM_BASERUN_CS +
#                       sqrt_TEAM_PITCHING_HR, data = moneyball3)
# stepwise <- stepAIC(stepwisemodel, direction = "both")
# summary(stepwise)
# vif(stepwise)
# sqrt(vif(stepwise)) > 2

# All subsets regression
# subsets <- regsubsets(TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
#                         TEAM_BATTING_H + 
#                         TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
#                         TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
#                         SB_PCT + log_TEAM_BASERUN_CS + 
#                         sqrt_TEAM_PITCHING_HR,  data = moneyball3, nbest = 2)
# plot(subsets, scale="adjr2")
# 
# subset <- lm(TARGET_WINS ~ 
#                TEAM_BATTING_1B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_SO + TEAM_BASERUN_SB +
#                TEAM_BASERUN_CS + TEAM_FIELDING_E + TEAM_FIELDING_DP + SB_PCT, data = moneyball2)
# summary(subset)

# Model 3
# model3 <- lm(TARGET_WINS ~ 
#                TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB +
#                TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_PITCHING_HR + TEAM_PITCHING_BB + TEAM_PITCHING_SO +
#                TEAM_FIELDING_E + TEAM_FIELDING_DP + SB_PCT + log_TEAM_BASERUN_CS, data = moneyball2)
# summary(model3)
# vif(model3)

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

# test_completedData$TEAM_BATTING_1B <- test_completedData$TEAM_BATTING_H - test_completedData$TEAM_BATTING_HR - test_completedData$TEAM_BATTING_3B -
#   test_completedData$TEAM_BATTING_2B
# test_completedData$log_TEAM_BATTING_1B <- log(test_completedData$TEAM_BATTING_1B)
# test_completedData$log_TEAM_BATTING_3B <- log(test_completedData$TEAM_BATTING_3B)
# test_completedData$log_TEAM_BASERUN_SB <- log(test_completedData$TEAM_BASERUN_SB)
# test_completedData$log_TEAM_BASERUN_CS <- log(test_completedData$TEAM_BASERUN_CS)
# test_completedData$TEAM_BATTING_SO[is.na(test_completedData$TEAM_BATTING_SO)] = mean(test_completedData$TEAM_BATTING_SO, na.rm = TRUE)
# test_completedData$TEAM_FIELDING_E[(test_completedData$TEAM_FIELDING_E > 500)] = 500
# test_completedData$sqrt_TEAM_PITCHING_HR <- abs(sqrt(test_completedData$TEAM_PITCHING_HR))
# test_completedData$SB_PCT <- test_completedData$TEAM_BASERUN_SB/(1.0*test_completedData$TEAM_BASERUN_SB+test_completedData$TEAM_BASERUN_CS)


# Stand Alone Scoring
# test_completedData$P_TARGET_WINS <- 52.517499 +
#   0.033283 * test_completedData$TEAM_BATTING_1B -
#   0.014745* test_completedData$TEAM_BATTING_2B +
#   0.195413* test_completedData$TEAM_BATTING_3B +
#   0.117872* test_completedData$TEAM_BATTING_HR +
#   0.029135* test_completedData$TEAM_BATTING_BB -
#   0.016490* test_completedData$TEAM_BATTING_SO +
#   0.064974* test_completedData$TEAM_BASERUN_SB +
#   0* test_completedData$TEAM_PITCHING_HR +
#   0* test_completedData$TEAM_PITCHING_BB +
#   0* test_completedData$TEAM_PITCHING_SO -
#   0.113370* test_completedData$TEAM_FIELDING_E -
#   0.122184* test_completedData$TEAM_FIELDING_DP +
#   0* test_completedData$SB_PCT +
#   0.033003* test_completedData$log_TEAM_BASERUN_CS

#model 3
# test_completedData$P_TARGET_WINS <- 37.137685 +
#   0.031468* test_completedData$TEAM_BATTING_1B -
#   0.002385* test_completedData$TEAM_BATTING_2B +
#   0.173543* test_completedData$TEAM_BATTING_3B +
#   5.932178* test_completedData$log_TEAM_BATTING_HR +
#   0.063148* test_completedData$TEAM_BASERUN_SB +
#   0.031562* test_completedData$TEAM_BASERUN_CS -
#   0.679852* test_completedData$log_TEAM_BATTING_SO -
#   0.114343* test_completedData$TEAM_FIELDING_E -
#   0.127829* test_completedData$TEAM_FIELDING_DP -
#   0.009746* test_completedData$TEAM_PITCHING_SO +
#   0.033066* test_completedData$TEAM_PITCHING_BB

#stepwise model
# test_completedData$P_TARGET_WINS <- -75.833419 +
#   0.146688* test_completedData$TEAM_BATTING_3B +
#   0.033606* test_completedData$TEAM_BATTING_BB -
#   0.042652* test_completedData$TEAM_BATTING_SO +
#   0.072291* test_completedData$TEAM_BASERUN_SB -
#   0.104952* test_completedData$TEAM_FIELDING_E -
#   0.130524* test_completedData$TEAM_FIELDING_DP +
#   0.029211* test_completedData$TEAM_BATTING_H +
#   20.857343* test_completedData$log_TEAM_BATTING_SO +
#   3.125717* test_completedData$log_TEAM_BATTING_HR

#forward model*****************************************************
# test_completedData$P_TARGET_WINS <- 94.395789 +
#   0.149067* test_completedData$TEAM_BATTING_3B +
#   0.034566* test_completedData$TEAM_BATTING_BB +
#   0.071692* test_completedData$TEAM_BASERUN_SB -
#   0.103817* test_completedData$TEAM_FIELDING_E -
#   0.124573* test_completedData$TEAM_FIELDING_DP +
#   0.025378* test_completedData$TEAM_BATTING_H -
#   9.306043* test_completedData$log_TEAM_BATTING_SO +
#   3.122109* test_completedData$log_TEAM_BATTING_HR

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
