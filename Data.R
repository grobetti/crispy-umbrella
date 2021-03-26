
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(shiny)
library(xfun)
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
library(plm)
library(lmtest)

```

Problem Set 3 Analysis of Experimental Data: Project STAR

1) Summary statistics
```{r}
data <- read_dta("STAR.dta")
data$girl <- as.factor(data$girl)
data$freelunk <- as.factor(data$freelunk)
data$sck <- as.factor(data$sck)
summary(data)
aggregate(tscorek ~ sck, mean, data=data)
max(data$totexpk_m)
```
There are 2795 girls and 2954 boys in the dataset.
The mean outcome for all students (testscore) is 461.2 and the median is 457.5.
The mean for the treated is 466 and for the control 459.1.
The most experienced teacher has 324 months of experience, thats equal to 27 years of teaching.


2) OLS Regression
```{r}

lm <- lm(data$tscorek ~ data$sck, data = data)
summary(lm)

plm <- plm(data$tscorek ~ data$sck, model = "within", index = c("schidkn"), data = data)
summary(plm)

#AG#
fit1 <- lm(tscorek ~ sck, data = data) #fit linear regression
par(mfrow = c(2,2))
plot(fit1, which = 2:5) #diagnostic plots
par(mfrow = c(1,1))
summary(fit1)
fit2 <- plm(tscorek ~ sck, index = c("schidkn"), model = "within", data = data) #fit linear regression with school fixed effects
summary(fit2)
fitcheck <- lmer(tscorek ~ 1 + sck + (1|schidkn), data = data) #same with another package
summary(fitcheck)
sqrt(var(data$tscorek)) #get standard deviation
#AG#

```

3) Standard Errors
```{r}

coeftest(plm, vcov. = vcovHC, type = "HC3")

```

4) Testing whether randomization worked (using Ws)
```{r}

data$sck <- as.numeric(data$sck)

lmgirl <- lm(data$sck ~ data$girl, data = data)
summary(lmgirl)

lmfreelunk <- lm(data$sck ~ data$freelunk, data = data)
summary(lmfreelunk)

lmtotexp <- lm(data$sck ~ data$totexpk_m, data = data)
summary(lmtotexp)

totallm <- lm(data$sck ~ data$girl + data$freelunk + data$totexpk_m, data = data)
anova(totallm)

lmfrom2a <- lm(data$tscorek ~ data$sck + data$girl + data$freelunk + data$totexpk_m, data = data)
summary(lmfrom2a)

```

5) Heterogeneity in Treatment Effects
```{r}



```





