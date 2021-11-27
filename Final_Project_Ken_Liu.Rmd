---
title: "Final_Project_MD_Ken_Liu"
author: "Ken Liu"
date: "3/14/2021"
output: pdf_document
---

```{r setup, include=FALSE}
install.packages("janitor")
library(janitor)
library(ggplot2)
library(mi)
library(mice)
tt <- read.csv("~/Desktop/Titanic.csv")
```

# 1. Creating missingness
Since the original dataset contain only two variables with missingness, we want to create more because we would like to see how each method performs on the missing data and their accuracies. 

```{r}
# fare
x1 <- tt$Fare
miss <- sample(1:891, 190)
t_fare <- tt$Fare
t_fare[miss] <-NA
# gender
x2 <- as.factor(tt$Sex)
miss2 <- sample(1:891, 180)
t_sex <- x2
t_sex[miss2] <- NA
# form the dataset with variables with missing values
tt_new <- data.frame(t_sex, t_fare, tt$PassengerId, tt$Survived, tt$Pclass, tt$Age)
```

# Method 1: mean imputation

## Mean imputation (numeric variables)

```{r}
# function of mean imputation
mean.imp <- function (a)
  {
  missing <- is.na(a)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- mean(a.obs)
  # Output the imputed vector
  return (imputed)
}
# calculate mean of Age with missing values
head(mean.imp(tt$Age))
Age_Mean <- mean.imp(tt$Age)
hist(Age_Mean)
# calculate mean of Fare with missing values
head(mean.imp(t_fare))
Fare_Mean <- mean.imp(t_fare)
hist(Fare_Mean)
```

# Method 2: Mode impuatation (categorical variables)
```{r}
# function of mode imputation setup
mode = function(b)
{
 ta = table(b)
 tam = max(ta)
 if (all(ta == tam))
 mod = NA
 else
 mod = names(ta)[ta == tam]
 return(mod)
}

mode.imp <- function (c)
{
 missing <- is.na(c)
 c.obs <- c[!missing]
 imputed <- c
 imputed[missing] <- mode(c.obs)
 # Output the imputed vector
 return (imputed)
}
head(mode(t_sex))
mode_imp_sex <- mode.imp(t_sex)
head(mode_imp_sex)
```

## Compare to original data

```{r}
# survival rate by age
# original data
age_effect1 <- lm(Survived~Age,data=tt)
coef(age_effect1)
# survived = 0.483753 -0.002613*Age
# mean imputation
age_effect2 <- lm(tt.Survived~Age_Mean,data=tt_new)
coef(age_effect2)
# tt.Survived = 0.461429 -0.002613*Age_Mean
# survival rate by gender
# original data
tt$Sex <- as.factor(tt$Sex)
sex_effect1 <- lm(Survived~as.factor(Sex),data=tt)
coef(sex_effect1)
# survived = 0.74204 -0.55313*Sex
# mode imputation
sex_effect2 <- lm(tt.Survived~mode_imp_sex,data=tt_new)
coef(sex_effect2)
# tt.Survived = 0.73725 -0.49512*mode_imp_sex
# the average % change in all regression coefficients of mean imputation is apprioximately
# 2%
mean(abs(age_effect1$coef - coef(age_effect2))/abs(age_effect1$coef))
# the average % change in all regression coefficients of mode imputation is apprioximately
# 7%
mean(abs(sex_effect1$coef - coef(sex_effect2))/abs(sex_effect1$coef))
# average % change of standard error 
mean(abs(0.4903 - 0.4857)/abs(0.4903)) #age
mean(abs(0.4087 - 0.4361)/abs(0.4087)) #sex
```

```{r}
# survival rate by gender (original data)
ggplot(tt, aes(x = as.factor(Sex), fill = as.factor(Survived))) +
  geom_bar(alpha = 0.8) + 
  labs(y = "Number of Passengers", title = "Titanic Survival Rate by Gender") 
```

```{r}
# impute method setup
m2 <- data.frame(mode_imp_sex, tt_new$tt.Survived)
```

```{r}
# survival rate by gender (imputed method)
ggplot(tt_new,aes(x = as.factor(mode_imp_sex), fill = as.factor(tt.Survived))) +
  geom_bar(alpha = 0.8) + 
  labs(y = "Number of Passengers", title = "Titanic Survival Rate by Gender") 

```

```{r}
# exact percentage
# original data
library(janitor)
tabyl(tt, Sex, Survived) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
```

We are trying to get survival rate by gender. From the comparisons of linear regressions, by using the imputation medthod, the standard error for age is 0.4857 and for sex is 0.4087, where the standard error from original dataset are 0.4903 and 0.4323. As we can see from the graph and percentage table above, comparing with complete case method, the impute method provides us a closer result to the original data. 

# Method 3: regression imputation

```{r}
# regression imputation for numeric variable age
fit_age <- lm(tt.Age ~ tt.Survived + tt.Pclass + tt.PassengerId, data = tt_new)
pred_age <- predict(fit_age, newdata = ic(tt_new))
tt_new$tt.Age[is.na(tt_new$tt.Age)] <- pred_age
# regression imputation for numeric variable fare
fit_far <- lm(t_fare ~ tt.Survived + tt.Pclass + tt.PassengerId, data = tt_new)
pred_far <- predict(fit_far, newdata = ic(tt_new))
tt_new$t_far[is.na(tt_new$t_far)] <- pred_far
# check whether a missing value still remains
which(is.na(tt_new$tt.Age))
which(is.na(tt_new$t_far))
```

## listwise deletion
```{r}
gl <- na.omit(tt_new)
```

## comparisons
```{r}
# survival rate by age
# original data
age_effect3 <- lm(Survived~Age,data=tt)
coef(age_effect3)
# survived = 0.483753 -0.002613*Age
# reg
age_effect4 <- lm(tt.Survived~tt.Age,data=gl)
coef(age_effect4)
# tt.Survived = 0.424776 -0.001377*tt.Age
# survival rate by gender
# original data
tt$Sex <- as.factor(tt$Sex)
sex_effect5 <- lm(Survived~Sex,data=tt)
coef(sex_effect5)
# survived = 0.74204 -0.55313*Age
# reg
tt_new$t_sex <- as.factor(tt_new$t_sex)
sex_effect6 <- lm(tt.Survived~t_sex,data=gl)
coef(sex_effect6)
# tt.Survived = 0.72165 -0.51779*tt.Age
# the average % change in all regression coefficients of regression imputation is apprioximately
# 6%
mean(abs(age_effect3$coef - coef(age_effect4))/abs(age_effect3$coef))
# the average % change in all regression coefficients of regression imputation is apprioximately
# 2.6%
mean(abs(sex_effect5$coef - coef(sex_effect6))/abs(sex_effect5$coef))
# % change of standard error
mean(abs(0.4903 - 0.4942)/abs(0.4903)) #age
mean(abs(0.4087 - 0.4175)/abs(0.4087)) #sex
```

We are interested in finding the survival rate by age/gender. Since it's hard to find differences from plots, we used linear regressions to complete the analysis. The standard error are 0.4903 and 0.4087 by using original data and are 0.487 and 0.42 by using the regression imputation method. Based on the analysis above, the regression imputation method produces slightly larger coefficients and smaller intercept value from the analysis by using original data. Then we also conduct the same analysis over survival rate and sex and we got the same conclusion. Therefore, the analysis of both methods creates results with few discrepancies. 

# Method 4: Regression imputation with noise

```{r}
set.seed(10101)
# age regression imputation with noise
age_imp <- tt_new$tt.Age
Ra <- as.numeric(!is.na(age_imp))
# create complete case dataframe and dataframe with data dropped
age_cc <- data.frame(tt$Survived,tt$Pclass,tt$PassengerId,age_imp)[Ra == 1, ]
age_drop <- data.frame(tt$Survived,tt$Pclass,tt$PassengerId,age_imp)[Ra == 0, ]
# regression imputation
age_reg <- lm(age_imp ~ tt.Survived+tt.Pclass+tt.PassengerId, data = age_cc)
# predict
age_imputation <- predict(age_reg, newdata = age_drop)
# reg with noise
age_noise = rnorm(length(age_imputation), 0, summary(age_reg)$sigma)
# add the random normal draw
age_imp1 = age_imputation + age_noise
age_imp[Ra == 0] = age_imp1
head(age_imp)


# fare regression imputation with noise
fare_imp <- tt_new$t_fare
Rf <- as.numeric(!is.na(fare_imp))
# create complete case dataframe and dataframe with data dropped
fare_cc <- data.frame(tt$Survived,tt$Pclass,tt$PassengerId,fare_imp)[Rf == 1, ]
fare_drop <- data.frame(tt$Survived,tt$Pclass,tt$PassengerId,fare_imp)[Rf == 0, ]
# regression imputation
fare_reg = lm(fare_imp ~ tt.Survived+tt.Pclass+tt.PassengerId, data = fare_cc)
# predict
fare_imputation <- predict(fare_reg, newdata = fare_drop)
# reg with noise
fare_noise <- rnorm(length(fare_imputation), 0, summary(fare_reg)$sigma)
# add the random normal draw
fare_imp1 <- fare_imputation + fare_noise
fare_imp[Rf == 0] <- fare_imp1
head(fare_imp)
```

# Method 5: Logistic model 

```{r, include=FALSE}
library(readr)
tt <- read_csv("~/Desktop/Titanic.csv")
# create missingness
# fare
x1 <- tt$Fare
miss <- sample(1:891, 190)
t_fare <- tt$Fare
t_fare[miss] <-NA
# gender
x2 <- tt$Sex
miss2 <- sample(1:891, 180)
t_sex <- x2
t_sex[miss2] <- NA
# form the dataset with variables with missing values
tt_new <- data.frame(t_sex, t_fare, tt$PassengerId, tt$Survived, tt$Pclass, tt$Age)
```

```{r}
tt_new$t_sex[tt_new$t_sex=="male"] <- "1"
tt_new$t_sex[tt_new$t_sex=="female"] <- "0"                          
sex_imp <- tt_new$t_sex
Rs <- as.numeric(!is.na(sex_imp))
sex_cc <- data.frame(tt$PassengerId, tt$Survived, tt$Pclass,sex_imp)[Rs == 1, ]
sex_drop <- data.frame(tt$PassengerId, tt$Survived, tt$Pclass,sex_imp)[Rs == 0, ]
#logistic model:
logi <- glm(factor(sex_imp) ~ tt.Survived+tt.Pclass+tt.PassengerId, data = sex_cc, family = "binomial")
summary(logi)
#predict
sex_imputation <- predict(logi, newdata = sex_drop, type = "response")

sex_imp[Rs == 0] = rbinom(sum(Rs==0), 1, sex_imputation)
head(sex_imp)

# number of missing data in sex = 180
sum(Rs==0)
# the number of male is 580 and the number of female is 311
sum(sex_imp==1)
sum(sex_imp==0)
```


## Compare with original data
```{r}
# survival rate by age
# original data
age_effect7 <- lm(Survived~Age,data=tt)
coef(age_effect7)
# Survived = 0.483753 -0.002613*Age
# regression imputation with noise
age_effect8 <- lm(tt.Survived~age_imp,data=tt_new)
coef(age_effect8)
# tt.Survived = 0.457855 -0.002516*age_imp
# survival rate by gender
# original data
sex_effect9 <- lm(Survived~factor(Sex),data=tt)
coef(sex_effect9)
# survived = 0.74204 -0.55313*Age
# logistic model 
sex_effect10 <- lm(tt.Survived~factor(sex_imp),data=tt_new)
coef(sex_effect10)
# tt.Survived = 0.72871 -0.53533*sex_imp
# the average % change in all regression coefficients of regression imputation is approximately
# 9%
mean(abs(age_effect7$coef - coef(age_effect8))/abs(age_effect7$coef))
# the average % change in all regression coefficients of logistic model is approximately
# 1.6%
mean(abs(sex_effect9$coef - coef(sex_effect10))/abs(sex_effect10$coef))
# average pervent change of standard error
mean(abs(0.4903 - 0.4848)/abs(0.4903)) #age
mean(abs(0.4087 - 0.4097)/abs(0.4087)) #sex
```

In this case, we are interested in finding the survival rate by age/gender. Since it's hard to find differences from plots, we used linear regressions to complete the analysis. The standard error are 0.4903 and 0.4087 by using the original data where the standard error are 0.4857 and 0.4138 by using the regression imputation with noise and logistic model. For age variable, we found that the coefficient produced by complete case is much closer to zero than the coefficient produced by regression imputation method, which means that survival rate is not affected by age under complete case method. However, based on the analysis above, for sex~survival rate analysis, the regression imputation method produces slightly larger intercept value and smaller coefficient from the analysis by using complete case data, showing that sex is negatively correlated with survival rate. 

# Method 6: Imputation with mi package
```{r}
set.seed(10101)
mdf1 <- missing_data.frame(tt_new)
```

```{r,include=FALSE}
imputations1 <- mi(mdf1, parallel = F)
imputations1 <- mi(mdf1, n.iter = 50, n.chains = 5)
```


```{r, include=FALSE}
plot(imputations1)
```

## traceplot 
```{r}
par(ask=F)
par(mfrow = c(1,1))
converged1 <- mi2BUGS(imputations1)
mean_tt.Age <- converged1[, , 4]
# Traceplot of mean imputed age
ts.plot(mean_tt.Age[,1], col=1, ylim = c(0.47,0.485))
lines(mean_tt.Age[,2], col= 2)
lines(mean_tt.Age[,3], col= 3)
lines(mean_tt.Age [,4], col= 4)
lines(mean_tt.Age [,5], col= 5)
mean_t_fare <- converged1[, , 3]
# Traceplot of mean imputed fare
ts.plot(mean_t_fare[,1], col=1, ylim = c(-0.03,0.013))
lines(mean_t_fare[,2], col= 2)
lines(mean_t_fare[,3], col= 3)
lines(mean_t_fare[,4], col= 4)
lines(mean_t_fare[,5], col= 5)
Rhats(imputations1)
# rhat
#  mean_t_sex mean_t_fare mean_tt.Age    sd_t_sex   sd_t_fare   sd_tt.Age 
#  0.9931363   1.0100409   1.0033854   0.9932699   1.0231926   1.0047141 
```

## Pool the results and report the estimated equation
```{r}
analysis1 <- mi::pool(tt.Survived ~ factor(t_sex) + t_fare +  factor(tt.Pclass) + tt.Age, imputations1, m=5)
display(analysis1)
# tt.Survived = 2.34 - 2.48*factor(t_sex) - 1.67*factor(tt.Pclass) - 0.03*tt.Age
```

## Compare with original data
```{r}
# Analysis of original data
cc <- na.omit(tt_new)
mm <- glm(formula = Survived ~ factor(Sex) + Fare +  factor(Pclass) + Age, family = gaussian, data = tt)
summary(mm)
# tt.Survived = 1.118 + -4.788e-01*factor(t_sex) + 6.052e-05*t_fare -2.035e-01*factor(tt.Pclass) + -5.433e-03*tt.Age
# the average % change in all regression coefficients of imputation is approximately
# 168.8%
mean(abs(coef(analysis1) - mm$coefficients)/abs(coef(analysis1)))
```

# Method 7: Hot Deck
```{r, include=FALSE}
# This plot gives the frequencies for different combination of missing variables
install.packages("VIM")
library(VIM)
tt_aggr = aggr(tt_new, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(nhanes), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
```
```{r, include=FALSE}
# Missing patterns
matrixplot(tt_new)
```
# Hot deck imputation
```{r}
ht_tt <- hotdeck(tt_new)
```

## Compare with original dataset
```{r}
# survival rate by age
# original data
age_effect11 <- lm(Survived~Age,data=tt)
coef(age_effect11)
# Survived = 0.483753 -0.002613*Age
# regression imputation with noise
age_effect12 <- lm(tt.Survived~tt.Age_imp,data=ht_tt)
coef(age_effect12)
# tt.Survived = 0.40616 -0.11238*tt.Age_imp
# survival rate by gender
# original data
sex_effect13 <- lm(Survived~factor(Sex),data=tt)
coef(sex_effect13)
# survived = 0.74204 -0.55313*Age
# logistic model 
sex_effect14 <- lm(tt.Survived~factor(t_sex_imp),data=ht_tt)
coef(sex_effect14)
# tt.Survived = 0.38959 -0.02848*t_sex_imp
# the average % change in all regression coefficients of hotdeck imputation is apprioximately
# 2108.7%
mean(abs(age_effect11$coef - coef(age_effect12))/abs(age_effect11$coef))
# the average % change in all regression coefficients of hotdeck imputation is apprioximately
# 71.2%
mean(abs(sex_effect13$coef - coef(sex_effect14))/abs(sex_effect13$coef))
# average pervent change of standard error
mean(abs(0.4903 - 0.4848)/abs(0.4903)) #age
mean(abs(0.4087 - 0.48677)/abs(0.4087)) #sex
```



