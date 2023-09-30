#####################
# load libraries
# set wd
# clear global .envir
#####################


# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", 
                      "package:graphics",
                      "package:grDevices",
                      "package:utils",
                      "package:datasets", 
                      "package:methods",
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)


# have a first view of data
summary(y)
hist(y)

# 1.Find a 90% confidence interval for the average student IQ in the school.

# check the functions goona be used
length(y)
sd(y)
?qt
?t.test

# way 1: mathematical method
t_score <- qt(0.95, df=length(y)-1)
lower_90_y <- mean(y)-(t_score)*(sd(y)/sqrt(length(y)))
upper_90_y <- mean(y)+(t_score)*(sd(y)/sqrt(length(y)))

# combine two ends into a confident interval and print result in a formated way
ci_y <- c(round(lower_90_y, 2), round(upper_90_y, 2))    
ci_y
cat("The 90% confidence interval for the average student IQ in the school is:",
    paste(ci_y, collapse = "~"))


# way 2: encapsulated function, an easier method
T <- t.test(y, conf.level = 0.90, alternative = "two.sided")
str(T)
# subset the lower and upper end of the confident interval
T_lower_90_y <- T$conf.int[1]
T_upper_90_y <- T$conf.int[2]

# combine two ends into a confident interval and print result in a formated way
T_ci_y <- c(round(T_lower_90_y, 2), round(T_upper_90_y, 2))             
cat("The 90% confidence interval for the average student IQ in the school is:",
    paste(T_ci_y, collapse = "~"))

## 2. Whether the average student IQ in her school 
## is higher than the average IQ score (100) among all the schools in the country.

## null H: mean(y) = 100
## alternative H: mean(y) > 100
## α = 0.05  

mean(y)
?t.test

# run a one-sided t.test function with a significant level of 0.05
t.test(y, alternative = "greater", mu = 100, conf.level = 0.95)


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=TRUE)

# have a firsr view of data
str(expenditure)
head(expenditure)

# copy the original dataset with a shorter name
ex <- expenditure

# 1. plot the relationships among Y, X1, X2, and X3

# create scatterplot of Y and X1 
png(file="/Users/poisson/Documents/Trinity/PS01_Q2_scplot1.png")
plot(ex$X1,
     ex$Y,
     type = "p",
     col = "blue",
     pch = 16,
     xlab = "per capita personal income",
     ylab = "per capita expenditure on shelters",
     main = "The relationships between X1 and Y")
dev.off()

# create scatterplot of Y and X2
png(file="/Users/poisson/Documents/Trinity/PS01_Q2_scplot2.png")
par(las = 1)
plot(ex$X2,
     ex$Y,
     type = "p",
     col = "red",
     pch = 16,
     xlab = "per 100,000 'financially insecure' residents",
     ylab = "per capita expenditure on shelters",
     main = "The relationships between X2 and Y")
dev.off()

# create scatterplot of Y and X3
png(file="/Users/poisson/Documents/Trinity/PS01_Q2_scplot3.png")
plot(ex$X3,
     ex$Y,
     type = "p",
     col = "green",
     pch = 16,
     xlab = "per thousand residing in urban areas",
     ylab = "per capita expenditure on shelters",
     main = "The relationships between X3 and Y")
dev.off()

# create scatterplot of X1 and X2 
png(file="/Users/poisson/Documents/Trinity/PS01_Q2_scplot4.png")
plot(ex$X1,
     ex$X2,
     type = "p",
     col = "orange",
     pch = 16,
     xlab = "per capita personal income",
     ylab = "per 100,000 'financially insecure' residents",
     main = "The relationships between X1 and X2")
dev.off()

# create scatterplot of X1 and X3 
png(file="/Users/poisson/Documents/Trinity/PS01_Q2_scplot5.png")
plot(ex$X1,
     ex$X3,
     type = "p",
     col = "pink",
     pch = 16,
     xlab = "per capita personal income",
     ylab = "per thousand residing in urban areas",
     main = "The relationships between X1 and X3")
dev.off()

# create scatterplot of X2 and X3
png(file="/Users/poisson/Documents/Trinity/PS01_Q2_scplot6.png")
plot(ex$X2,
     ex$X3,
     type = "p",
     col = "purple",
     pch = 16,
     xlab = "per 100,000 'financially insecure' residents",
     ylab = "per thousand residing in urban areas",
     main = "The relationships between X2 and X3")
dev.off()

# create scatterplot of Y~X1~X2~X3
png(file='/Users/poisson/Documents/Trinity/PS01_Q2_scplot7.png')
pairs(ex[, c("Y", "X1", "X2", "X3")],
      main = "The relationships among Y~X1~X2~X3")
dev.off()


## 2. plot the relationship between Y and Region.

## have a first view of data
view(ex$Region)
typeof(ex$Region)
table(ex$Region)

## convert region data into factor variable
name_region <- c("Northeast", "North Central", "South", "West")
ex$Region <- factor(ex$Region, labels = name_region)
print(ex$Region)

## create boxplot of Y and Region
png(file='/Users/poisson/Documents/Trinity/PS01_Q2_scplot8.png')
plot(ex$Region,
     ex$Y,
     type = "b",
     xlab = "Region",
     ylab = "per capita expenditure on shelters",
     main = "The relationship between Y and Region")
dev.off()

### 3.Reproduce the Y~X1 graph including one more variable Region and display
### different regions with different types of symbols and colors.

### load the ggplot package
library(tidyverse)

### create scatterplot of Y and X1 by regions
ggplot(
  data = ex, 
  mapping = aes(x = X1, y = Y, color = Region, shape = Region)
  ) +
  geom_point(size = 3
  ) + 
  labs(
    title = "The relationship of X1 ~ Y by Region",
    x = "per capita personal income",
    y = "per capita expenditure on shelters"
  )+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file='/Users/poisson/Documents/Trinity/PS01_Q2_scplot9.png')
