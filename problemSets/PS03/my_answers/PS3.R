#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
# Helps clean up namespaces, prevent conflicts between packages, 
# or ensure you get the latest version when reloading a package.
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
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
lapply(c("stargazer", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
View(inc.sub)

###### Q1 #####

# check if value is na
inc.sub$voteshare[is.na(inc.sub$voteshare)]
inc.sub$difflog[is.na(inc.sub$difflog)]

# Fit models
model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model1)
nobs(model1)

inc.sub$presvote[is.na(inc.sub$presvote)]
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)
nobs(model2)

model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3)

model4<- lm(residual1 ~ residual2)
summary(model4)

model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model5)
nobs(model5)


# Make scatterplots
scatter1 <-
  ggplot(data = inc.sub, 
         mapping = aes(x = difflog,
                       y = voteshare)) + 
  geom_point(color = "blue") +
  geom_smooth(method='lm',col="black") # Add regression line
scatter1

scatter2 <-
  ggplot(data = inc.sub, 
         mapping = aes(x = difflog,
                       y = presvote)) + 
  geom_point(color = "yellow") +
  geom_smooth(method='lm',col="black") # Add regression line
scatter2

scatter3 <-
  ggplot(data = inc.sub, 
         mapping = aes(x = presvote,
                       y = voteshare)) + 
  geom_point(color = "lightblue") +
  geom_smooth(method='lm',col="black") # Add regression line
scatter3

scatter4 <-
  ggplot(
         mapping = aes(x = residual2,
                       y = residual1)) + 
  geom_point(color = "orange") +
  geom_smooth(method='lm',col="black") # Add regression line
scatter4




# Save residuals as objects
residual1 <- model1$residuals
rs<-resid(model1)

residual2 <- model2$residuals
residual2


head(residual2)


# Get Latex table
stargazer(model1)
stargazer(model2)
stargazer(model3)
stargazer(model4)
stargazer(model5)
