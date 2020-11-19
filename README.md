---
title: "Visualizing the Ten-year Fatal Cardiovascular Risk Modelling among Malaysians using R"
author: "Dr. Che Muhammad Nur Hidayat bin Che Nawi"
date: "21 November 2020"
output: 
  rmdformats::readthedown:
    self_contained: true
    thumbnails: true
    lightbox: true
    highlight: kate
bibliography: C:/Users/admin/OneDrive - Universiti Sains Malaysia/R-RUG Conference/globo.bib
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r, out.width="600px", out.height="350px", echo=FALSE}

knitr::include_graphics("introductory slide.jpg")

```

# Motivation

Difficulty in model interpretation.

# Introduction

## Cardiovascular Disease

Globally, the cardiovascular disease (CVD) is a number one top killer. It is estimated that, 17.8 million death are due to CVD and it accounted for 31% of all global death @jagannathan2019global. Besides, about two thrid of the CVD related death occured in low and middle income countries. 

```{r, out.width="600px", out.height="350px", echo=FALSE}

knitr::include_graphics("WHF-CVD.jpg")

```

We encountered a similar problem with CVD in Malaysia, as it accounted for a quarter of all hospital deaths. The average age at which Malaysians getting CVD is 58.5 years. However, one in four CVD related hospital admissions is those under 50 years of age. This situation is worrying as those age range are the reproductive age group. Thus, it important for us to anticipate the problem by measuring the CVD risk Malaysian and administer early intervention. 

```{r, out.width="600px", out.height="350px", echo=FALSE}

knitr::include_graphics("malaysia CVD.jpg")

```

## Globorisk (@hajifathalian2015novel)

- It is one of the latest CVD calculator
- It can be specified to each country unlike other CVD calculator. Hence, it is superior compared to previously developed calculator.
- Developed by Public Health team from Harvard University in 2015

## Cardiovascular risk

- According to @hajifathalian2015novel and @ueda2017laboratory, the Globorisk calculator has two primary outcomes which are the **fatal Globorisk** risk and **non-fatal Globorisk** risk.
- The fatal Globorisk risk is the probability of having future fatal CVD events in relative to the fatal CVD events in the initial recalibration process for specific country applications

# Objectives

To model the Malaysians CVD risk scores as a function of age category adjusted with ethnicity in Malaysia

# Method

## Data source

The 2015 National Health and Morbidity Survey data

## Fatal Globorisk stratification (@ueda2017laboratory)

- Low risk as defined as < 10%
- High risk as defined as >= 10%
- High risk as defined as >= 20%
- High risk as defined as >= 30%

## Statistical method

- Statitical package used : *ordinal*, *tidyverse*, *haven*, *effects*, *gtsummary*, *forcats*, *dplyr* and *broom*.
- Ordinal logistic regression modeling was used to ascertain the relationship of age category to the ten-year fatal Globorisk score of Malaysians.

### Cumulative link models (Proportional Odds Models)

- Cumulative link models (CLMs) - *ordinal* package - are an excellent model class for ordinal data as it allows development of flexible regression framework for details analyses.

### polr 

- polr function from MASS package is used in combination with *effects* package to visualize the model through plot. 
 
# Analysis

## Preparation for analysis

```{r}
library(ordinal) #to perform model
library(tidyverse) #data wrangling
library(haven)
library(broom) #to generate more pleasant result
library(gtsummary) #presentation ready table
library(dplyr)
library(forcats) # factor reorder 
```
## Read data

Under this section, we will perform the following steps:
- Convert the data into object and name it as globorisk
- Briefly examine the data
- Summarize the data

```{r}
#Read data file form working directory

globorisk <- read.csv("dataRUGgloborisk.csv") 

glimpse(globorisk)

```
The globorisk dataset consist of 10 variables with 7660 samples.

Noted that most of the variables are in character form. We will transform the data class into factor. 

```{r}
summary(globorisk)
```
Summary of the data shows that there is no missing value. If any, the missing value should be dealt accordingly to prevent bias.

We will convert the variables to factors variables

```{r}
#checking the class of variable within data.frame
str(globorisk) 

# converting the dataframe character variables into factor variables and selecting intended variables for analysis

globorisk$ethnic <- as.factor(globorisk$ethnic)
globorisk$agecat <- as.factor(globorisk$agecat)
globorisk$fatal.cat <- as.factor(globorisk$fatal.cat)

globorisk2 <- globorisk %>% select(ethnic, agecat, fatal.cat)

str(globorisk2)
```
The outcome variable is fatal.cat which represent the categorical ten year fatal globorisk risk as mentioned above.

## Factor reorder

```{r}
# Reorder ethnic (Malay as reference group)
globorisk2 <- globorisk2 %>% mutate_at(vars(ethnic), ~fct_relevel(., c("Malay","Chinese",
                                                                       "Indian", 
                                                                       "Others Bumis", 
                                                                       "Others")))

# Reorder fatal.cat 
globorisk2 <- globorisk2 %>% mutate_at(vars(fatal.cat), ~fct_relevel(., c("Low 10%", "High 10%", "High 20%", "High 30%"
                                                                     )))
```

```{r}
summary(globorisk2)
```
Factor reorder is important to ensure the reference group is meaningful. 

## Estimation

Next, we will fit the model using *the Proportional Odds Model* or the *Cumulative Link Model*

In ordinal package we can use the function clm to run the maximum likelihood estimates.

Now run the clm() function to estimate:

1. regression coefficients
2. standard errors
3. Wald based p-values (testing the parameters against zero)

```{r}
globo1 <- clm(fatal.cat ~ agecat + ethnic, data = globorisk2)

summary(globo1)
```
Nice output

```{r}
tidy(globo1) #from broom package
```
Variable age category is positive in value and the value is increasing with older age. Meanwhile, variable ethnic is in negative value for Chinese, Indian and Others Bumis. This means

1. Older age and Others ethnic are associated with higher categories of outcome
2. Chinese, Indian and Others Bumis ethnicity are not associated with higher categories of outcome (protective)

Now lets estiamte the odds ratio and the 95% CI for both age category and gender variables

```{r}
tidy(globo1, exponentiate = TRUE, conf.int = TRUE)
```


This tells you that: 

1. Older age is associated with higher odds of having severe CVD risk, when adjusted with ethnic variable
2. When controlled for age category, the Chinese and Others Bumis have 0.51 and 0.49 times lower odds to have severe CVD risk respectively. 

After that, we will use tbl_regression function from *gtsummary* package to simplified the model for easier interpretation. The tbl_regression function is highly customizable and allows us to create a presentation ready table.

```{r}
globo1 %>% tbl_regression(exponentiate = TRUE, label = list(
                            ethnic~"Ethnicity", agecat~"Age Category")) %>% 
  modify_header(label = "**Variable**") %>% 
  bold_labels() 
```


# Inferences

Perform inferences by creating new model, and perform comparison with previous model using likelihood ratio test

```{r}
globo1b <- clm(fatal.cat ~ agecat , data = globorisk2)
anova(globo1b, globo1, test = 'Chisq')
```
The comparison can be done by comparing the AIC and looking at the significant of p value. 

Taking significant level at 5%, if the p value is less than 0.05 then the two model are significantly different. 

# Prediction

To predict the probabilities of *i-th* subject falls into which outcome category for model with a new data, we need to create a new data first. We will use expand.grid() to create a new data of class data.frame

This is the new data

```{r}
newData <- expand.grid(agecat= levels(globorisk2$agecat),
                       ethnic= levels(globorisk2$ethnic))

newData
```
Now, we will predict the probabilities for a new data. The predicted probabilities are the probabilities that the subject fall into each category of the outcome.

Remember the categories of the outcome are:

```{r}
levels(globorisk2$fatal.cat)
```
We will do the prediction using linear predictor.

```{r}
lp.newdata <- predict(globo1, newdata = newData, type = 'linear.predictor')
prob.newData <- predict(globo1, newdata = newData, type = 'prob')
lp.newdata ; prob.newData
```
We can use cbind() to amke a better results.

```{r}
cbind(newData, prob.newData)
```
The prediction allows us to predict the probability of the observations to fall in which outcome category.

Looking at the results, we can see that any ethnicity at younger age group will fall into low CVD risk group. While, any ethnicity at older age group will fall into high CVD risk group, in which Malay and Others ethnicity have comparable higher probability to have high CVD risk as compared to other ethnic counter parts. 

For those with visual learner type, looking at this huge amount of number can be trouble some.

Later I will show you on how to visualize the model to make the interpretation easier. 

# Checking proportional odds assumption (POS)

The one assumption you have to check is the proportional odds assumption using nominal_test under *ordinal* package.

Let us check if variable ETHNIC (which is a categorical var) fulfills the POS 


```{r}
nominal_test(globo1)
```
The function test ordinality one by one covariate. In this example, the ethnicity show that the assumption of proportional odds is presumed (p=0.06).

# Visualizing the Globorisk score

In this section we will use the polr() function form *MASS* package to fit model

Next, we will use Effect() function form *effects* package to plot the model.

## Fitting the model

```{r}
library(MASS)

m_polr <- polr(fatal.cat~agecat+ethnic, data = globorisk2)
summary(m_polr)
```
## Plotting the model

```{r}
library(effects)
#effect of age category
plot(Effect(focal.predictors = "agecat", m_polr))
#combine effect gender and age category
plot(Effect(focal.predictors = c("agecat", "ethnic"), m_polr))
```
On the effect of **age category**, older age is associated with higher risk of having high CVD risk of >=30%.

When combine between the effect of **age category and ethnic**, all ethnics have high risk of getting high CVD risk at older age group. 

# Conclusion

The model shows that older age groups are associated with severe CVD risk when adjusted with ethnicity. 

# Thank you

Email: drchehidayat@student.usm.my

github: https://github.com/drchehidayat/RUG-Conference-2020---Globorisk.git

# References
