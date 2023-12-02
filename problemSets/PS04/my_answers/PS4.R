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
# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("stringr", 'car','texreg', 'stargazer'),  pkgTest)


# read in data
data(Prestige)
help(Prestige)
View(Prestige)

# set wd for current folder
setwd("/Users/poisson/Documents/GitHub/Fork_Statsl Fall2023")
getwd()

###### Q1 #####

#(a)
# check if value is na
Prestige$type[is.na(Prestige$type)]
Prestige$prestige[is.na(Prestige$prestige)]
Prestige$income[is.na(Prestige$income)]

# ignore missing values
Prestige <- Prestige[!is.na(Prestige[, 'type']), ]

# create dummy variables for type
unique(Prestige$type)
Prestige$professional <- ifelse(Prestige$type == 'prof', 1, 0)
Prestige$professional <- as.factor(Prestige$professional)



#(b) run a model
model <- lm(prestige ~ income + professional + income : professional, data = Prestige)
summary(model)
texreg(list(model), digits=3)


  
#(f) marginal effect of income when professional = 1
y_hat1 = 21.1422589 + 0.0031709 * 1000 + 37.7812800 * 1 - 0.0023257 * 1000 * 1
y_hat2 = 21.1422589 + 0.0031709 * 0 + 37.7812800 * 1 - 0.0023257 * 0 * 1
print(y_hat1-y_hat2)

#(g) marginal effect of income when income = 6000
y_hat3 = 21.1422589 + 0.0031709 * 6000 + 37.7812800 * 1 - 0.0023257 * 6000 * 1
y_hat4 = 21.1422589 + 0.0031709 * 6000 + 37.7812800 * 0 - 0.0023257 * 6000 * 0
print(y_hat3-y_hat4)

####### Q2 #####

#(a) Hypotheis test for coefficient of 'precinct assigned lawn signs'
b1 <- 0.042
se1 <- 0.016
n <- 131
k <- 2
p_b1 <- 2*pt((b1-0)/se1, n-k, lower.tail = F) # p_b1 = 0.009711646
print(p_b1 < 0.05) # True

#(b) Hypotheis test for coefficient of 'precinct adjacent to lawn signs'
b2 <- 0.042
se2 <- 0.013
n <- 131
k <- 2
p_b2 <- 2*pt((b2-0)/se2, n-k, lower.tail = F) # p_b2 = 0.001566685
print(p_b2 < 0.05) # True


#(d) the model 
# precinct assigned lawn signs：
# voteshare = 0.042 * 1 + 0.302

# precinct adjacent lawn signs
# voteshare = 0.042 * 1 + 0.302

# precinct far from lawn signs
# voteshare = 0.302


