library(haven)
library(randomizeR)
library(blockTools)
library(samplingbook)
library(ggplot2)
library(plotrix)
library(survival)
library(blockrand)
library(tibble)
library(ATE)

set.seed(42)

data <- read_dta("randomization_2021.dta")

data <- na.omit(data) 

aggregate(data$y0,by=list(data$year), FUN=mean)
aggregate(data$y1,by=list(data$year), FUN=mean)

aggregate(data$y0,by=list(data$black), FUN=mean)
aggregate(data$y1,by=list(data$black), FUN=mean)

#Creating stratum
data$stratum <- NA
data$stratum <- ifelse (data$black==1 & data$year==2005, 1,
                        ifelse(data$black==0 & data$year ==2005, 2,
                               ifelse(data$black==1 & data$year == 2006, 3,
                                      ifelse(data$black==0 & data$year == 2006, 4, NA)))) 

#Random treatment assignment - Blocked and stratified ===== Olivia needs to finish!





stratum1 <- blockrand(n=28, stratum='1')
stratum2 <- blockrand(n=24, stratum='2')
stratum3 <- blockrand(n=33, stratum='3')
stratum4 <- blockrand(n=10, stratum='4')

strata <- rbind(stratum1, stratum2, stratum3, stratum4)

#Recording observed outcome based on treatment assignment
data$observedoutcome <- NA

data$observedoutcome <- ifelse (data$treated ==1, data$y1,
                        ifelse(data$treated ==0, data$y0,
                               NA))

averages <- tapply(data$observedoutcome,data$treated,mean)
average <- as.data.frame(averages)

#Estimating ATE
ATE <- average[2,]-average[1,]
print(ATE)

#ATE derived from regression
lm <- lm(observedoutcome ~ treated + birthday, data=data)
summary(lm)












