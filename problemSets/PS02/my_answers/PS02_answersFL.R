###########################
# Problem Set 2
# Name: Futong Li
# Student Id: 23341767
###########################

setwd("/Users/poisson/Documents/GitHub/Fork_Statsl Fall2023")
getwd()
rm(list=ls())
library(tidyverse)

# Question 1: Political Science

# (a) Calculate the  \texttt(χ^2 )test statistic by hand/manually

bribe_data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, ncol = 3, byrow = TRUE)
rownames(bribe_data) <- c("Upper class", "Lower class")
colnames(bribe_data) <- c("Not stopped", "Bribe requested", "Stopped")
bribe_data

r <- apply(bribe_data, 1, sum)
c <- apply(bribe_data, 2, sum)
r
c
fe_all <- numeric()
chisq <- 0
fe <- 0


for (i in seq(1:nrow(bribe_data))) {
  for (j in seq(1:ncol(bribe_data))) {
    fe <- (c[j] * r[i]) / sum(bribe_data)
    chisq <- chisq + ((bribe_data[i, j] - fe) ^ 2 / fe)
    fe_all <- c(fe_all, fe)
  }}
fe_all <- matrix(fe_all, nrow = 2, ncol = 3, byrow = TRUE)
print(fe_all)
print(chisq)

# check chi-squre value
chisq.test(bribe_data)
x_sq <- chisq.test(bribe_data)
chisq == x_sq$statistic

# (b) Now calculate the p-value from the test statistic you just created

p_value = pchisq(chisq, df = (nrow(bribe_data)-1) * (ncol(bribe_data)-1), lower.tail=FALSE)
p_value 
p_value <= 0.1

# (c) Calculate the standardized residuals for each cell a

z <- 0
z_all <- numeric()

for (i in seq(1:nrow(bribe_data))) {
  for (j in seq(1:ncol(bribe_data))) {
    z = (bribe_data[i,j] - fe_all[i, j]) / 
         (sqrt
          (fe_all[i,j] * (1- (r[i] / sum(bribe_data)))
            * ( 1- (c[j] / sum(bribe_data)))))
    z_all <- c(z_all, z)
  }}
z_all <- matrix(z_all, nrow = 2, ncol = 3, byrow = TRUE)
print(z_all)  

# check standardized residuals
x_sq$stdres
ls(x_sq)
chisq.test(bribe_data[])$stdres
?chisq.test

## Question 2: Economics

## (b) Run a bivariate regression to test this hypothesis

## Read data
df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
head(df)
View(df)
reserved <- factor(df$reserved)

## Scatter plot
scatter <- ggplot(data = df, 
                  mapping = aes(x = reserved, y = water)) + 
  geom_point()
scatter

## Fit model
model <- lm(water~reserved, data = df)
summary(model)


