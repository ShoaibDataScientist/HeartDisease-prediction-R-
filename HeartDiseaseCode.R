---
title: "Heart Disease Prediction MA334"
output:
  pdf_document: default
  html_notebook: default
  html_document:
    df_print: paged
---

 
Importing Libraries

psych: Procedures for Psychological, Psychometric, and Personality Research
```{r message=FALSE, warning=FALSE}
library(psych)
```
dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges
```{r message=FALSE, warning=FALSE}
library(dplyr)
```
plyr library used for splitting the data into parts
```{r message=FALSE, warning=FALSE}
library(plyr)
```
ggplot used for creation of graphics,visulization data etc 
```{r message=FALSE, warning=FALSE}
library(ggplot2)
```
corrplot used for visulization of Data matrix etc 
```{r message=FALSE, warning=FALSE}
library(corrplot)
```
multcom used for simultaneous and confidence intervals 
```{r message=FALSE, warning=FALSE}
library(multcomp)
```
rpivotTable used for creation of Data tables etc
```{r message=FALSE, warning=FALSE}
library(rpivotTable)
```
caret used for training of regression and classification model
```{r message=FALSE, warning=FALSE}
library(caret)
```
e1071 is also used for different Machine learning models
```{r message=FALSE, warning=FALSE}

library(e1071)
```
recursive partitioning for classification and regression 
```{r message=FALSE, warning=FALSE}
library(rpart)
```

```{r message=FALSE, warning=FALSE}
library(rpart.plot)
```
**Data Exploration**\
We have loaded the data in Rstudio and loading data and saving it in Heart_disease Variable
```{r warning=FALSE}
heart_disease <- read_csv("G:/FiverrProject/HeartDisease/heart_disease.csv")
```
There are 70000 Records in the data and total 13 columns in our data set
*Some initial insights of the Data*
```{r}
head(heart_disease)
```

*Some Detailed description of All attributes*
```{r}
describe(heart_disease)
```
We have total 70000 rows in the Heart disease data. The target variable is Disease in the data and there some other data attributes as well like height weight etc. We need Data preprocessing steps to clean the data and get meaningful information from the data  
**Data Reprocessing**\
*1:ID Column Removed*
This attributes is not meaningful for disease prediction so we are omitting this and saving data in 'heart' variable 
```{r}
heart <- heart_disease[, 2:13]
```
```{r}
head(heart)
```
*2.we can remove higher  blood pressure lower than lower  blood pressure*
```{r}
ap_cleaned <- heart %>% filter(heart$ap_hi > heart$ap_lo)
```
*3 Anomalies Reduction*

There are some records which are very less correlated and unrepeatable in our data analysis so we are going to remove that 
```{r}
height_cleaned <- ap_cleaned %>% filter(ap_cleaned$height >= 140 & ap_cleaned$height <= 220)
weight_cleaned <- height_cleaned %>% filter(height_cleaned$weight >= 30)
ap_cleaned2 <- weight_cleaned %>% filter(weight_cleaned$ap_lo >= 30 & weight_cleaned$ap_lo <= 170)
cleaned_cardio <- ap_cleaned2 %>% filter(ap_cleaned2$ap_hi >= 70 & ap_cleaned2$ap_hi < 250)

head(cleaned_cardio)
summary(cleaned_cardio)
```
**4.Conversion of continuous variables to categorical variables**

In our data set some values treated as continuous variable but in real those are categorical variables so we need to convert it
```{r}
cols = c("gender", "cholesterol", "gluc", "smoke", "alco", "active", "disease")
cleaned_cardio[cols] = lapply(cleaned_cardio[cols], factor)
summary(cleaned_cardio)
```
**5.Split the data for Training and Testing**\
we divided the data in to 3 parts Training ,validation and test set and the ration of splitting is 70,15,15% respectively 
```{r}
data <- sample_n(cleaned_cardio, 10000)
idx <- sample(seq(1, 3), size = nrow(data), replace = TRUE, prob = c(.7, .15, .15))
train <- data[idx == 1,]
test <- data[idx == 2,]
valid <- data[idx == 3,]
```
Assigning three different splitting data into variables 
```{r}
cols = c("gender", "cholesterol", "gluc", "smoke", "alco", "active", "disease")
```
```{r}
train[cols] = lapply(train[cols], factor)
test[cols] = lapply(test[cols], factor)
valid[cols] = lapply(valid[cols], factor)

```
**Some EDA on the Training Data**\
*1.Summary statistics*
```{r}
summary(train)
```
4518 observations belongs to females which indicate as a gender 1 and 2473 entries belongs to females which indicate gender 2 , and we can say that there is a pretty difference between both classes distributions so data is biased.
3520  class individuals has no  disease and in the same way 3471 individuals has disease \
**2.Graphical representation of the findings**

*Histogram and density plots are used to identify the distribution of continuous variables*
```{r}
ggplot(train, aes(x=age)) + geom_density()


ggplot(train, aes(x=height)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


ggplot(train, aes(x=weight)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
```
According to above graphs The variables are not normal distributed so we can say that the data has bias factor.so we have take care this factor as well while choosing the prediction model \
**3.Another way to plot the findings using scatter plot**

*Scatter plots and correlation plots can visualize and roughly identify the possible correlation between any two continuous variables.*
```{r}
train.corr <- cor(train[, c(1, 3, 4, 5, 6)])
train.corr
corrplot(train.corr, method = "circle")
```
This graph shows that ap_hi and ap_lo has big corelation as well as between height and weight there are also high corelations 

**Hypothesis Testing**

*H-1: we want to check weight are correlated with cholesterol*
```{r}
oneway.test(train$weight~train$cholesterol, var.equal = TRUE)
boxplot(train$weight~train$cholesterol)
```
from the above graph it is showing that weight and cholesterol variables are correlated 

*H-2: Correlation between height and weight?*
```{r}
corr.test(train$height, train$weight)
```
The correlation matrix values shows that its= is 0.31 so we can say that it is slightly correlated.\
*H-3: Are High blood pressure value and low blood pressure value is correlated?*
```{r}
corr.test(train$ap_hi, train$ap_lo)
```
we can see that the value of correlation matrix is 0.74 so this is strongly correlated.

*H- 4: Smoking habit effect any gender?*
```{r}
rpivotTable(train, rows = c("gender"), cols = c("smoke"))
```
```{r}
chisq.test(train$gender, train$smoke, correct=FALSE)
```
There is a correlation between gender and smoke 

**Transformation of variables**

*1.conversion of age in days to years*
```{r}
train$age_y <- NA
train$age_y <- train$age / 365

test$age_y <- NA
test$age_y <- test$age / 365

valid$age_y <- NA
valid$age_y <- valid$age / 365
```
*2.Replaced BMI variable with weight and Height*
```{r}
train$BMI <- NA
train$BMI <- (train$weight/ ((train$height/100)^2))

test$BMI <- NA
test$BMI <- (test$weight/ ((test$height/100)^2))

valid$BMI <- NA
valid$BMI <- (valid$weight/ ((valid$height/100)^2))
```
*3.Include correlated variables*

As we concluded that gender and smoking is correlated so we need to add that correlation findings
upper blood pressure and lower blood pressure is also strongly correlated so also need to add them

*4.Model selection*

So on the basis of our previous hypothesis we will select a best model for desease prediction
```{r}
full_model <- glm(disease~age_y + gender + BMI + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + (ap_hi*ap_lo) + (gender*smoke) + (cholesterol * BMI), data = train, family = "binomial")
step(full_model, direction="backward")
```
So we are going to include our most correlated variables in our model  and selected Logistic Regression Model
**Model Implementation**

*1. Logistic Regression Model*
```{r}
lm1 <- glm(disease~age_y + gender + BMI + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active + (ap_hi*ap_lo) + (gender*smoke), data = train, family = "binomial")
summary(lm1)
```

*Confusion matrix to find out the accuracy of the model*
```{r}
train$prob <- predict(lm1, train, type = "response")
train$pred <- NA
train$pred[train$prob >= 0.50] <- "Yes"
train$pred[train$prob < 0.50] <- "No"
table(train$pred, train$disease)

```
Our logistic regression model accuracy is  (2266 + 2764) / 6991 x 100% = 71.94%.


**Some Assumptions of the model**\
*Independence*
Binary diversity is considered independently distributed. As data is collected from medical facilities, respondents to the database are considered to be individuals, independent of each other. This means that cases are independent, which means that reasoning is valid.

*Linearity*
The standard line model assumes a correlation between the continuous independent variable and the logit of the variable result which means that each independent variant corresponds to the logit (p) = log (p / (1-p)) where the p is the predictable probability. This can be seen using the scatterplot between each continuous forecast and logit values.
```{r}
train$logitp <- log(train$prob/(1-train$prob))

ggplot(train, aes(BMI, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

ggplot(train, aes(age, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

ggplot(train, aes(ap_lo, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")

ggplot(train, aes(ap_hi, logitp))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess")
```
According to scatter plots all continuous independent variables of BMI, age, ap_hi and ap_low are highly correlated with the predicted line of 'cardio' on the logit scale. In ap_lo, it's almost the same as the average width in the middle, not the two edges overlapped. Therefore, the concept is satisfied.


