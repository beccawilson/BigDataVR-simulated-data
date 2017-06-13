#########################################################
# Big Data VR Project: Data simulation
# Demetris Avraam, Becca Wilson, Paul Burton
# Data 2 Knowledge Research Group, University of Bristol
#########################################################

#Script for simulating ALSPAC data for the BigDataVR project (project: B2506)

set.seed(48272)

path <- "[FILENAME]"

# Read the actual data
data <- read.csv(path, header=TRUE)
class(data)

# Return the dimensions of the dataframe and the names of the variables
dim(data)
names(data)

data[1:6, 1:5]

#######################################################################
#
#  CLEAN THE DATA 
#
#######################################################################

D <- data[-1,]  # remove the first row wich does not have any id
D[1:5, 1:5]
dim(D)

D1 <- D[, -c(1:6)] # remove the first 6 columns which include variables out of interest for this project
names(D1)
dim(D1)

D2 <- D1[, -2] # remove the variable "preferred birthweight"
names(D2)
dim(D2)

D3 <- D2[, -c(72:87)] # remove the last 16 columns which include variables out of interest for this project
names(D3)
dim(D3)

indx <- which(D3<0)
length(indx)

D3[D3<0] <- NA  # replace missing values indicated by -1, -2 or -9999 with NAs
indx <- which(D3<0)
length(indx)

# Check how many NAs exist
table(is.na(D3))

# Remove any rows that include NAs (case-wise deletion)
D4 <- D3[complete.cases(D3),] 
dim(D4)
names(D4)

# Collect sex, ages and bmi in a separate dataframe (called E1)
E1 <- D4[,c(1,7,11,14,20,24,29,32,38,42,49,56,64,70,71)]
names(E1)
dim(E1)

# Collect all the other variables (which are all continuous) in another dataframe (called E2)
E2 <- D4[,-c(1,7,11,14,20,24,29,32,38,42,49,56,64,70,71)]
names(E2)
dim(E2)

#######################################################################
#
#  Standardized the continuous variables
#
#######################################################################

E3 <- E2 # E3 is the same as D2 but with standardized variables
for (i in 1:dim(E2)[2]){
  E3[,i] <- (E2[,i]-mean(E2[,i]))/sd(E2[,i])  # we use the z-score transformation
}

mean.E2 <- c()
var.E2 <- c()
for (i in 1:dim(E2)[2]){
  mean.E2[i] <- mean(E2[,i])
  var.E2[i] <- var(E2[,i])
}

mean.E2
var.E2

# Check that the transformed variables have zero mean and variance equal to one
mean.E3 <- c()
var.E3 <- c()
for (i in 1:dim(E3)[2]){
  mean.E3[i] <- mean(E3[,i])
  var.E3[i] <- var(E3[,i])
}

mean.E3
var.E3

#######################################################################
#
#  Simulate three matrices of continuous variables using the mvnorm
#  pseudo-random generator.  
#
#######################################################################

cov.E3 <- cov(E3)

library(MASS)
random.data.mat1 <- mvrnorm(n = 15500, mu=mean.E3, Sigma=cov.E3, tol = 1e-6, empirical = TRUE, EISPACK = FALSE)
dim(random.data.mat1)
random.data.mat1[1:10,]

random.data.mat2 <- mvrnorm(n = 155000, mu=mean.E3, Sigma=cov.E3, tol = 1e-6, empirical = TRUE, EISPACK = FALSE)
dim(random.data.mat2)
random.data.mat2[1:10,]

random.data.mat3 <- mvrnorm(n = 1550000, mu=mean.E3, Sigma=cov.E3, tol = 1e-6, empirical = TRUE, EISPACK = FALSE)
dim(random.data.mat3)
random.data.mat3[1:10,]

#######################################################################
#
#  Rescale the simulated variables to their original mean and variance 
#
#######################################################################

class(random.data.mat1)
random.data.frame1 <- as.data.frame(random.data.mat1)
names(random.data.frame1)

for (i in 1:dim(random.data.frame1)[2]){
  random.data.frame1[,i] <- random.data.frame1[,i]* sqrt(var.E2[i]) + mean.E2[i]
}
random.data.frame1[1:10,]

class(random.data.mat2)
random.data.frame2 <- as.data.frame(random.data.mat2)
names(random.data.frame2)

for (i in 1:dim(random.data.frame2)[2]){
  random.data.frame2[,i] <- random.data.frame2[,i]* sqrt(var.E2[i]) + mean.E2[i]
}

class(random.data.mat3)
random.data.frame3 <- as.data.frame(random.data.mat3)
names(random.data.frame3)

for (i in 1:dim(random.data.frame3)[2]){
  random.data.frame3[,i] <- random.data.frame3[,i]* sqrt(var.E2[i]) + mean.E2[i]
}


#######################################################################
#
#  Simulate the variable 'Gender' using logistic regression
#
#######################################################################

table(E1$kz021)
sex.true <- E1$kz021-1 
table(sex.true)
table(sex.true)/dim(E1)[1]

R <- E2[,c(52:56)]
names(R)
names(random.data.frame1)

s1 <- glm(sex.true ~ ., data=R, family=binomial)
v1 <- s1$coefficients[1]
for (i in 1:5){
  v1 <- v1 + s1$coefficients[i+1]*random.data.frame1[,i+51]
}
fp1 <- exp(v1)/(1+exp(v1))  # calculate the log odds 
sex1 <- rbinom(15500,1,fp1)
table(sex1)
table(sex1)/dim(random.data.frame1)[1]


s2 <- glm(sex.true ~ ., data=R, family=binomial)
v2 <- s2$coefficients[1]
for (i in 1:5){
  v2 <- v2 + s2$coefficients[i+1]*random.data.frame2[,i+51]
}
fp2 <- exp(v2)/(1+exp(v2))  # calculate the log odds 
sex2 <- rbinom(155000,1,fp2)
table(sex2)
table(sex2)/dim(random.data.frame2)[1]


s3 <- glm(sex.true ~ ., data=R, family=binomial)
v3 <- s3$coefficients[1]
for (i in 1:5){
  v3 <- v3 + s3$coefficients[i+1]*random.data.frame3[,i+51]
}
fp3 <- exp(v3)/(1+exp(v3))  # calculate the log odds 
sex3 <- rbinom(1550000,1,fp3)
table(sex3)
table(sex3)/dim(random.data.frame3)[1]

#######################################################################
#
#  Change the column names in the dataframes
#
#######################################################################

names(random.data.frame1)
names(random.data.frame2)
names(random.data.frame3)

colnames(random.data.frame1) <- c("height.7", "height.sit.7", "waist.7", "hip.7", "weight.7", "sbp.7", "dbp.7", "pulse.7",
                                  "height.8", "weight.8", "height.9", "height.sit.9", "waist.9", "hip.9", "weight.9",
                                  "sbp.9", "dbp.9", "pulse.9", "height.10", "height.sit.10", "waist.10", "weight.10", "sbp.10",
                                  "dbp.10", "height.11", "height.sit.11", "waist.11", "hip.11", "weight.11", "sbp.11", "dbp.11",
                                  "pulse.11", "height.12", "height.sit.12", "waist.12", "sbp.12", "dbp.12", "pulse.12", 
                                  "height.13", "waist.13", "weight.13", "sbp.13", "dbp.13", "pulse.13", "height.15", "weight.15",
                                  "waist.15", "height.sit.15", "sbp.15", "dbp.15", "pulse.15", "height.17", "weight.17",
                                  "sbp.17", "dbp.17", "pulse.17")
names(random.data.frame1)
cbind(names(random.data.frame2), names(random.data.frame1)) # check that we have renamed the variables correctly

colnames(random.data.frame2) <- c("height.7", "height.sit.7", "waist.7", "hip.7", "weight.7", "sbp.7", "dbp.7", "pulse.7",
                                  "height.8", "weight.8", "height.9", "height.sit.9", "waist.9", "hip.9", "weight.9",
                                  "sbp.9", "dbp.9", "pulse.9", "height.10", "height.sit.10", "waist.10", "weight.10", "sbp.10",
                                  "dbp.10", "height.11", "height.sit.11", "waist.11", "hip.11", "weight.11", "sbp.11", "dbp.11",
                                  "pulse.11", "height.12", "height.sit.12", "waist.12", "sbp.12", "dbp.12", "pulse.12", 
                                  "height.13", "waist.13", "weight.13", "sbp.13", "dbp.13", "pulse.13", "height.15", "weight.15",
                                  "waist.15", "height.sit.15", "sbp.15", "dbp.15", "pulse.15", "height.17", "weight.17",
                                  "sbp.17", "dbp.17", "pulse.17")

colnames(random.data.frame3) <- c("height.7", "height.sit.7", "waist.7", "hip.7", "weight.7", "sbp.7", "dbp.7", "pulse.7",
                                  "height.8", "weight.8", "height.9", "height.sit.9", "waist.9", "hip.9", "weight.9",
                                  "sbp.9", "dbp.9", "pulse.9", "height.10", "height.sit.10", "waist.10", "weight.10", "sbp.10",
                                  "dbp.10", "height.11", "height.sit.11", "waist.11", "hip.11", "weight.11", "sbp.11", "dbp.11",
                                  "pulse.11", "height.12", "height.sit.12", "waist.12", "sbp.12", "dbp.12", "pulse.12", 
                                  "height.13", "waist.13", "weight.13", "sbp.13", "dbp.13", "pulse.13", "height.15", "weight.15",
                                  "waist.15", "height.sit.15", "sbp.15", "dbp.15", "pulse.15", "height.17", "weight.17",
                                  "sbp.17", "dbp.17", "pulse.17")

#######################################################################
#
#  Calculate the variable 'BMI' 
#
#######################################################################

random.data.frame1$bmi.7 <- random.data.frame1$weight.7 / (random.data.frame1$height.7/100)^2
random.data.frame1$bmi.9 <- random.data.frame1$weight.9 / (random.data.frame1$height.9/100)^2
random.data.frame1$bmi.10 <- random.data.frame1$weight.10 / (random.data.frame1$height.10/100)^2
random.data.frame1$bmi.11 <- random.data.frame1$weight.11 / (random.data.frame1$height.11/100)^2
random.data.frame1$bmi.17 <- random.data.frame1$weight.17 / (random.data.frame1$height.17/100)^2

random.data.frame2$bmi.7 <- random.data.frame2$weight.7 / (random.data.frame2$height.7/100)^2
random.data.frame2$bmi.9 <- random.data.frame2$weight.9 / (random.data.frame2$height.9/100)^2
random.data.frame2$bmi.10 <- random.data.frame2$weight.10 / (random.data.frame2$height.10/100)^2
random.data.frame2$bmi.11 <- random.data.frame2$weight.11 / (random.data.frame2$height.11/100)^2
random.data.frame2$bmi.17 <- random.data.frame2$weight.17 / (random.data.frame2$height.17/100)^2

random.data.frame3$bmi.7 <- random.data.frame3$weight.7 / (random.data.frame3$height.7/100)^2
random.data.frame3$bmi.9 <- random.data.frame3$weight.9 / (random.data.frame3$height.9/100)^2
random.data.frame3$bmi.10 <- random.data.frame3$weight.10 / (random.data.frame3$height.10/100)^2
random.data.frame3$bmi.11 <- random.data.frame3$weight.11 / (random.data.frame3$height.11/100)^2
random.data.frame3$bmi.17 <- random.data.frame3$weight.17 / (random.data.frame3$height.17/100)^2


#######################################################################
#
#  Add the simulated gender variable to the dataframes
#
#######################################################################

random.data.frame1$sex <- sex1
random.data.frame2$sex <- sex2
random.data.frame3$sex <- sex3


#######################################################################
#
#  Simulate the age at the different clinics assuming normal distribution
#
#######################################################################

names(E1)

age.7 <- E1$f7003c/12
hist(age.7)
mean(age.7)
sd(age.7)
random.data.frame1$age.7 <- rnorm(15500, mean(age.7), sd(age.7))
random.data.frame1$age.7
random.data.frame2$age.7 <- rnorm(155000, mean(age.7), sd(age.7))
random.data.frame3$age.7 <- rnorm(1550000, mean(age.7), sd(age.7))


age.8 <- E1$f8003c/12
hist(age.8)
mean(age.8)
sd(age.8)
random.data.frame1$age.8 <- rnorm(15500, mean(age.8), sd(age.8))
random.data.frame2$age.8 <- rnorm(155000, mean(age.8), sd(age.8))
random.data.frame3$age.8 <- rnorm(1550000, mean(age.8), sd(age.8))


age.9 <- E1$f9003c/12
hist(age.9)
mean(age.9)
sd(age.9)
random.data.frame1$age.9 <- rnorm(15500, mean(age.9), sd(age.9))
random.data.frame2$age.9 <- rnorm(155000, mean(age.9), sd(age.9))
random.data.frame3$age.9 <- rnorm(1550000, mean(age.9), sd(age.9))


age.10 <- E1$fd003c/12
hist(age.10)
mean(age.10)
sd(age.10)
random.data.frame1$age.10 <- rnorm(15500, mean(age.10), sd(age.10))
random.data.frame2$age.10 <- rnorm(155000, mean(age.10), sd(age.10))
random.data.frame3$age.10 <- rnorm(1550000, mean(age.10), sd(age.10))


age.11 <- E1$fe003c/12
hist(age.11)
mean(age.11)
sd(age.11)
random.data.frame1$age.11 <- rnorm(15500, mean(age.11), sd(age.11))
random.data.frame2$age.11 <- rnorm(155000, mean(age.11), sd(age.11))
random.data.frame3$age.11 <- rnorm(1550000, mean(age.11), sd(age.11))


age.12 <- E1$ff0011a/12
hist(age.12)
mean(age.12)
sd(age.12)
random.data.frame1$age.12 <- rnorm(15500, mean(age.12), sd(age.12))
random.data.frame2$age.12 <- rnorm(155000, mean(age.12), sd(age.12))
random.data.frame3$age.12 <- rnorm(1550000, mean(age.12), sd(age.12))


age.13 <- E1$fg0011a/12
hist(age.13)
mean(age.13)
sd(age.13)
random.data.frame1$age.13 <- rnorm(15500, mean(age.13), sd(age.13))
random.data.frame2$age.13 <- rnorm(155000, mean(age.13), sd(age.13))
random.data.frame3$age.13 <- rnorm(1550000, mean(age.13), sd(age.13))


age.15 <- E1$fh0011a/12
hist(age.15)
mean(age.15)
sd(age.15)
random.data.frame1$age.15 <- rnorm(15500, mean(age.15), sd(age.15))
random.data.frame2$age.15 <- rnorm(155000, mean(age.15), sd(age.15))
random.data.frame3$age.15 <- rnorm(1550000, mean(age.15), sd(age.15))


age.17 <- E1$FJ003a/12
hist(age.17)
mean(age.17)
sd(age.17)
random.data.frame1$age.17 <- rnorm(15500, mean(age.17), sd(age.17))
random.data.frame2$age.17 <- rnorm(155000, mean(age.17), sd(age.17))
random.data.frame3$age.17 <- rnorm(1550000, mean(age.17), sd(age.17))


#######################################################################
#
#  Save the data to .csv files
#
#######################################################################

setwd("[FILEPATH]")

names(random.data.frame1)
dim(random.data.frame1)
simulated.data.1 <- random.data.frame1[, c(62,63,1:8,57,64,9:10,65,11:18,58,66,19:24,59,67,25:32,60,68,33:38,69,39:44,70,45:51,71,52:56,61)]
names(simulated.data.1)
dim(simulated.data.1)

write.csv(simulated.data.1, file="simulated.data.1.csv")

names(random.data.frame2)
dim(random.data.frame2)
simulated.data.2 <- random.data.frame2[, c(62,63,1:8,57,64,9:10,65,11:18,58,66,19:24,59,67,25:32,60,68,33:38,69,39:44,70,45:51,71,52:56,61)]
names(simulated.data.2)
dim(simulated.data.2)

write.csv(simulated.data.2, file="simulated.data.2.csv")

names(random.data.frame3)
dim(random.data.frame3)
simulated.data.3 <- random.data.frame3[, c(62,63,1:8,57,64,9:10,65,11:18,58,66,19:24,59,67,25:32,60,68,33:38,69,39:44,70,45:51,71,52:56,61)]
names(simulated.data.3)
dim(simulated.data.3)

write.csv(simulated.data.3, file="simulated.data.3.csv")

#######################################################################
#
#  Compare original with simulated data
#
#######################################################################

names(simulated.data.2)
mean.simulated.data.2 <- c()
var.simulated.data.2 <- c()
for (i in 1:dim(simulated.data.2)[2]){
  mean.simulated.data.2[i] <- mean(simulated.data.2[,i])
  var.simulated.data.2[i] <- var(simulated.data.2[,i])
} 
cbind(names(simulated.data.2), mean.simulated.data.2, var.simulated.data.2)

names(D4)
mean.D4 <- c()
var.D4 <- c()
for (i in 1:dim(D4)[2]){
  mean.D4[i] <- mean(D4[,i])
  var.D4[i] <- var(D4[,i])
} 
cbind(names(D4), mean.D4, var.D4)

newD <- D4[, c(1,11,2:6,8:10,7,14,12:13,24,15:19,21:23,20,32,25:28,30:31,29,42,33:37,39:41,38,49,43:45,46:48,56,50:52,53:55,64,57:60,61:63,71,65:66,67:70)]
names(newD)
mean.newD <- c()
var.newD <- c()
for (i in 1:dim(newD)[2]){
  mean.newD[i] <- mean(newD[,i])
  var.newD[i] <- var(newD[,i])
} 
cbind(names(simulated.data.2), mean.newD, var.newD)

# Convert ages in years to ages in months
simulated.data.2$age.7 <- simulated.data.2$age.7*12
simulated.data.2$age.8 <- simulated.data.2$age.8*12
simulated.data.2$age.9 <- simulated.data.2$age.9*12
simulated.data.2$age.10 <- simulated.data.2$age.10*12
simulated.data.2$age.11 <- simulated.data.2$age.11*12
simulated.data.2$age.12 <- simulated.data.2$age.12*12
simulated.data.2$age.13 <- simulated.data.2$age.13*12
simulated.data.2$age.15 <- simulated.data.2$age.15*12
simulated.data.2$age.17 <- simulated.data.2$age.17*12

# Convert gender 0-1 data to 1-2 data
simulated.data.2$sex <- simulated.data.2$sex+1

# Compare the means of the variables
names(simulated.data.2)
mean.simulated.data.2 <- c()
var.simulated.data.2 <- c()
for (i in 1:dim(simulated.data.2)[2]){
  mean.simulated.data.2[i] <- mean(simulated.data.2[,i])
  var.simulated.data.2[i] <- var(simulated.data.2[,i])
} 
cbind(names(simulated.data.2), mean.simulated.data.2, var.simulated.data.2)
mean <- cbind(names(simulated.data.2), mean.newD, mean.simulated.data.2)
write.csv(mean, file="mean.csv")

# Compare the variances of the variables
vars <- cbind(names(simulated.data.2), var.newD, var.simulated.data.2)
write.csv(vars, file="variance.csv")

# Compare the covariance matrices of the data
sim.cov <- cov(simulated.data.2)
write.csv(sim.cov, file="covariance_simulated_155000_data.csv")
real.cov <- cov(newD)
write.csv(real.cov, file="covariance_real_1593_data.csv")




