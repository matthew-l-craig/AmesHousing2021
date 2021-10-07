################################################################################
# prepare the environment for reproducibility
################################################################################
# clear console
cat('\014')
​
# clear global environment
rm(list = ls())
​
# clear plots
tryCatch(
  dev.off(dev.list()['RStudioGD']),
  error = function(e) {
    print('no plots to clear')
  }
)
​
# clear packages
if (!is.null(names(sessionInfo()$otherPkgs))) {
  lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""),
         detach,
         character.only = TRUE,
         unload = TRUE)
}
​
################################################################################
# install and load libraries
################################################################################
# install.packages("car")
# library(car)
install.packages("ggpubr")
library(ggpubr)
install.packages("dplyr")
library(dplyr)
​
################################################################################
# set the seed for reproducibility
################################################################################
set.seed(41)
​
################################################################################
# load the file
################################################################################
ames_df <- read.table("AmesHousing.csv", sep = ",", header = TRUE, 
                      row.names = "PID")
​
################################################################################
# preliminary EDA
################################################################################
# reduce data to only needed columns 
desired_attrs <- c("SalePrice", "Bldg.Type", "Neighborhood")
attrs <- c(desired_attrs)
ames_df_1 <- ames_df[, attrs]
str(ames_df_1)
​
################################################################################
# Visualize data
################################################################################
print(ggboxplot(ames_df_1, x = "Bldg.Type", y = "SalePrice", 
                color = "Neighborhood"))
print(ggboxplot(ames_df_1, x = "Bldg.Type", y = "SalePrice", 
                #stat_bin(binwidth=200),
                binwidth = 200,
                color = "Neighborhood", add = c("mean_se", "dotplot")))
​
Sys.sleep(3)
​
print(ggboxplot(ames_df_1, x = "Neighborhood", y = "SalePrice", 
                color = "Bldg.Type"))
print(ggboxplot(ames_df_1, x = "Neighborhood", y = "SalePrice", 
                color = "Bldg.Type", add = c("mean_se", "dotplot")))
Sys.sleep(3)
​
# Box plot with two factor variables - view with Building Type as first
boxplot(SalePrice ~ Bldg.Type * Neighborhood, data = ames_df_1, frame = FALSE, 
        ylab = "SalePrice")
# Two-way interaction plot - view with Bldg.Type as x factor
interaction.plot(x.factor = ames_df_1$Bldg.Type, trace.factor = ames_df_1$Neighborhood, 
                 response = ames_df_1$SalePrice, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Bldg.Type", ylab = "SalePrice", pch = c(1, 19))
​
# Box plot with two factor variables - view with Neighborhood as first
boxplot(SalePrice ~ Neighborhood * Bldg.Type, data = ames_df_1, frame = FALSE, 
        ylab = "SalePrice")
# Two-way interaction plot - view with Garage type as x factor
interaction.plot(x.factor = ames_df_1$Neighborhood, trace.factor = ames_df_1$Bldg.Type, 
                 response = ames_df_1$SalePrice, fun = mean, type = "b", legend = TRUE, 
                 xlab = "Garage.Type", ylab = "SalePrice", pch = c(1, 19))
​
################################################################################
# Developing Hypothesis 
################################################################################
# Building Type Hypothesis
# H0: There is no difference in sales price of houses with different building types
# H1: There is a difference in sales price of houses with different building types
# Claim: H1
# 
# Neighborhood Hypothesis
# H0: There is no difference in sales price of houses with different neighborhoods
# H1: There is a difference in sales price of houses with different neighborhoods
# Claim: H1
# 
# Building Type / Neighborhood Hypothesis
# H0: There is no interaction effect between type of building and type of 
# neighborhood on sales price
# H1: There is an interaction effect between type of building and type of 
# neighborhood on sales price
# Claim: H1
​
################################################################################
# compute two-way anova
################################################################################
writeLines('\n****************************************************************')
writeLines('compute two-way anova:')
result <- aov(SalePrice ~ Bldg.Type + Neighborhood, data = ames_df_1)
summary(result)
result2 <- aov(SalePrice ~ Bldg.Type * Neighborhood, data = ames_df_1)
summary(result2)
​
################################################################################
# compute summary statistics
################################################################################
writeLines('\n****************************************************************')
writeLines('compute some summary statistics:')
options(pillar.sigfig = 4)  # print 4 sig figs
print(
  dplyr::group_by(ames_df_1, Bldg.Type, Neighborhood) 
  %>% 
    dplyr::summarise(count = n(), 
                     mean = mean(SalePrice, na.rm = TRUE), 
                     sd = sd(SalePrice, na.rm = TRUE)
    )
)
​
################################################################################
# Which pair of means are different
################################################################################
writeLines('\n****************************************************************')
writeLines('which pair of means are different:')
# print(TukeyHSD(result2, which = "Bldg.Type"))
# print(TukeyHSD(result2, which = "Neighborhood"))
#print(TukeyHSD(result2))
​
writeLines('\n****************************************************************')
writeLines('check anova assumptions:')
​
writeLines('\n************************')
writeLines('Homogeneity of variances')
plot(result2, 1)
print(leveneTest(SalePrice ~ Bldg.Type * Neighborhood, data = ames_df_1))
​
writeLines('\n************************')
writeLines('Normality')
plot(result2, 2)
# extract the residuals and run Shapiro-Wilk test
aov_residuals <- residuals(object = result2)
#print(shapiro.test(x = aov_residuals))
