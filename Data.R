library(shiny)
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
library(vtable)
library(tinytex)

set.seed(43)

data <- read_dta("randomization_2021.dta")

data <- na.omit(data) 

########################### Question C, implement randomization

#Difference in potential outcomes as per blacks and years 
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

#Random treatment assignment - Blocked and stratified 

stratum1 <- blockrand(n=28, block.sizes=1, stratum='1')
stratum2 <- blockrand(n=24, block.sizes=1, stratum='2')
stratum3 <- blockrand(n=33, block.sizes=1, stratum='3')
stratum4 <- blockrand(n=10, block.sizes=1, stratum='4')

data$id <- NA

data$id[data$stratum == 1] <- seq(1:sum(data$stratum == 1))
data$id[data$stratum == 2] <- seq(1:sum(data$stratum == 2))
data$id[data$stratum == 3] <- seq(1:sum(data$stratum == 3))
data$id[data$stratum == 4] <- seq(1:sum(data$stratum == 4))

data$treatment <- NA

data$treatment[data$stratum ==1] <- stratum1$treatment[match(stratum1$id, data$id[data$stratum==1])]
data$treatment[data$stratum ==2] <- stratum2$treatment[match(stratum2$id, data$id[data$stratum==2])]
data$treatment[data$stratum ==3] <- stratum3$treatment[match(stratum3$id, data$id[data$stratum==3])]
data$treatment[data$stratum ==4] <- stratum4$treatment[match(stratum4$id, data$id[data$stratum==4])]

data$treatment <- as.factor(data$treatment)

sumtable(data, group="treatment", group.test = TRUE) #Checking whether randomization worked  

########################### Question E, estimate the ATE

#Recording observed outcome based on treatment assignment

data$observedoutcome <- NA

data$observedoutcome <- ifelse (data$treatment ==2, data$y1,
                        ifelse(data$treatment ==1, data$y0,
                               NA))

#Estimating ATE

mean <- tapply(data$observedoutcome , data$treatment, mean)
mean <- as.data.frame(mean)

alphaestimate <- mean[1,]
print(alphaestimate)

betaestimate <- mean[2,]-mean[1,]
print(betaestimate)

######################### Question F, estimate the ATE using LM 


#Simple regression of outcome on treatment
lm <- lm(observedoutcome ~ treatment, data=data)
summary(lm)

#Inclusion of birthday and stratum control variables
lm <- lm(observedoutcome ~ treatment + stratum + birthday, data=data)
summary(lm)









