#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
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
 lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

# Exploring the data frame and selected variables 
head(inc.sub, n=10)
summary(inc.sub)
summary(inc.sub)
sum(is.na(inc.sub))
sum(is.na(inc.sub))

# Question 1

# Regressing the selected variables and saving the model as an object
model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model1)

# Creating a table of the results 
#install.packages("stargazer")
library(stargazer)
stargazer(model1, type = "latex", title = "Linear Regression Results: Difference in campaign spending and vote share", 
          out = "regression_table_model1.tex")

# Creating a scatterplot 
library(ggplot2)
scatter1 <- ggplot(data = inc.sub, 
                  mapping = aes(x = difflog, 
                                y = voteshare)) + 
  geom_point(size=0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") + # Add regression line
  labs(x = "Difference in Log Values", # Add labels
       y = "Vote Share") + 
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position = c(0.85, 0.85)) # Change position of legend

# Printing and saving the scatterplot 
scatter1
ggsave(scatter1, file = "scatter_vs_dl.png")

# Saving the residuals as an object and summarizing 
residuals_voteshare_difflog <- residuals(model1)
summary(residuals_voteshare_difflog)

# Question 2

# Regressing the selected variables and saving the model as an object
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)

# Creating a table of the results 
stargazer(model2, type = "latex", title = "Linear Regression Results: Difference in campaign spending 
          and vote share of the presidential candidate", 
          out = "regression_table_model2.tex")




# Creating a scatterplot 
library(ggplot2)
scatter2 <- ggplot(data = inc.sub, 
                  mapping = aes(x = difflog, 
                                y = presvote)) + 
  geom_point(size=0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") + # Add regression line
  labs(x = "Difference in Log Values", # Add labels
       y = "Vote Share") + 
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position = c(0.85, 0.85)) # Change position of legend

# Printing and saving the scatterplot 
scatter2
ggsave(scatter2, file = "scatter_pv_dl.png")

# Saving the residuals as an object and summarizing 
residuals_presvote_difflog <- residuals(model2)
summary(residuals_presvote_difflog)


# Question 3

# Regressing the selected variables and saving the model as an object
model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3)

# Creating a table of the results 
stargazer(model3, type = "latex", title = "Linear Regression Results: Vote share of the presidential candidate and the incumbent", 
          out = "regression_table_model3.tex")

# Creating a scatterplot 
library(ggplot2)
scatter3 <- ggplot(data = inc.sub, 
                   mapping = aes(x = presvote, 
                                 y = voteshare)) + 
  geom_point(size=0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") + # Add regression line
  labs(x = "Presidential candidate's vote share", # Add labels
       y = "Incumbent's vote share") + 
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position = c(0.85, 0.85)) # Change position of legend

# Printing and saving the scatterplot 
scatter3
ggsave(scatter3, file = "scatter_vs_pv.png")


# Question 4

# Regressing the selected variables and saving the model as an object
model4 <- lm(residuals_voteshare_difflog ~ residuals_presvote_difflog, data = inc.sub)
summary(model4)

# Creating a table of the results 
stargazer(model4, type = "latex", title = "Linear Regression Results: Model residuals", 
          out = "regression_table_model4.tex")

# Creating a scatterplot 
library(ggplot2)
scatter4 <- ggplot(data = inc.sub, 
                   mapping = aes(x = residuals_presvote_difflog, 
                                 y = residuals_voteshare_difflog)) + 
  geom_point(size=0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") + # Add regression line
  labs(x = "Residuals (presvote and difflog)", # Add labels
       y = "Residuals (voteshare and difflog)") + 
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position = c(0.85, 0.85)) # Change position of legend

# Printing and saving the scatterplot 
scatter4
ggsave(scatter4, file = "scatter_res1_res2.png")


# Question 5

# Regressing the selected variables and saving the model as an object
model5 <- lm(voteshare ~ presvote + difflog, data = inc.sub)
summary(model5)

# Creating a table of the results 
stargazer(model5, type = "latex", title = "Multivariate Linear Regression Results", 
          out = "regression_table_model5.tex")





