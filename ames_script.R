################################################################################
# prepare the environment for reproducibility
################################################################################
# clear console
cat('\014')

# clear global environment
rm(list = ls())

# clear plots
tryCatch(
  dev.off(dev.list()['RStudioGD']),
  error = function(e) {
    print('no plots to clear')
  }
)

# clear packages
if (!is.null(names(sessionInfo()$otherPkgs))) {
  lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""),
         detach,
         character.only = TRUE,
         unload = TRUE)
}

################################################################################
# install and load libraries
################################################################################

library(car)
library(ggpubr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(RColorBrewer)
library(FSA)
library(hash)
library(VIM)
library(Metrics)
library(ISLR)
library(caret)  
library(gridExtra)
library(pROC)
library(scales)
library(stats)
################################################################################
# Helpful functions
################################################################################
process_corr_matrix<-function(corr_matrix,pos_corr_thresh, neg_corr_thresh) {
  
  # Get pairwise table
  corr_df<- as.data.frame(as.table(corr_matrix))
  
  if (nrow(corr_df) > 0) {
    
    #drop like/like corr
    corr_df<-corr_df[corr_df['Var1'] != corr_df['Var2'],]
    
    #threshold corr
    corr_df<-corr_df[corr_df['Freq']>= pos_corr_thresh|
                       corr_df['Freq']<=neg_corr_thresh,]
    
    #drop duplicate rows
    for (i in 1:nrow(corr_df)) {
      corr_df[i,c("Var1", "Var2")]= sort(corr_df[i,c("Var1", "Var2")])
    }
    corr_df<-corr_df[!duplicated(corr_df),]
    
    #sort by corr pairs and corr strength
    corr_df<-corr_df[order(corr_df$Var1, corr_df$Var2),]
    
    #rename rows
    rownames(corr_df)<- 1:nrow(corr_df)
    
  }
  
  #return
  corr_df
  
}



vec_freq_dist <- function(a_vec, a_vec_name = '', rel_freq = 'yes', 
                          freq = 'yes')
{
  
  a_table = table(a_vec)
  
  if (freq == 'yes') {
    writeLines(paste('\n', a_vec_name, ' categorical frequency distribution:\n', 
                     sep = ''))
    print(a_table)
  }
  
  if (rel_freq == 'yes') {
    writeLines(paste('\n', a_vec_name, 
                     ' relative categorical frequency distribution:\n', sep = ''))
    print(a_table / sum(a_table))
  }
}

classifier_roc_auc <- function(a_y_probs, a_y_true) {
  
  an_roc <- roc(a_y_true, a_y_probs)
  
  print(plot(an_roc, col = 'blue', xlab = 'FPR', 
             ylab = 'sensitivity (recall or TPR)'))
  grid(col = 'black')
  
  
  
  an_auc <- auc(an_roc)
  # writeLines(paste('ROC AUC: ', an_auc))
  #area under curve
  
}

classifier_performance <- 
  function(a_model, a_model_name = '', a_data_df, a_data_df_name = '', y_true,
           print_classification_report = TRUE) {
    
    writeLines('\n****************************************************************')
    writeLines(paste(a_data_df_name, ' set predictions using model ', 
                     a_model_name, '\n', sep = ''))
    
    y_probs <- predict(a_model, a_data_df, type = 'response')
    y_preds <- as.factor(ifelse(y_probs >= 0.50, 'Yes', 'No'))
    
    classification_report <- confusionMatrix(y_preds, y_true, positive = 'Yes')
    if (print_classification_report) {
      writeLines('\nsensitivity = recall')
      writeLines('positive predictive value = precision')
      print(classification_report)
    }
    
    classifier_roc_auc(y_probs, y_true) 
    
    return(list('y_probs' = y_probs, 'y_preds' = y_preds, 'y_true' = y_true, 
                'classification_report' = classification_report))
  }

log_reg_pseudo_r_square <- function(logistic) {
  # https://www.youtube.com/watch?v=C4N3_XJJ-jU
  
  ## Now calculate the overall "Pseudo R-squared" and its p-value
  
  ## NOTE: Since we are doing logistic regression...
  ## Null deviance = 2*(0 - LogLikelihood(null model))
  ##               = -2*LogLikihood(null model)
  ## Residual deviance = 2*(0 - LogLikelihood(proposed model))
  ##                   = -2*LogLikelihood(proposed model)
  ll.null <- logistic$null.deviance/-2
  ll.proposed <- logistic$deviance/-2
  
  ## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
  r_square <- (ll.null - ll.proposed) / ll.null
  
  ## chi-square value = 2*(LL(Proposed) - LL(Null))
  ## p-value = 1 - pchisq(chi-square value, df = 2-1)
  p_value <- 1 - pchisq(2 * (ll.proposed - ll.null), df = 1)
  
  return(list('r_square' = r_square, 'p_value' = p_value))
}
################################################################################ 
################################################################################
################################################################################

################################################################################
# set the seed for reproducibility
################################################################################
set.seed(44)

################################################################################
# load the file
################################################################################
ames_df <- read.table(paste('AmesHousing.csv', sep=''),
header = TRUE, sep=',',row.names = 'PID')

################################################################################
# Select attributes to use
################################################################################ 
continuous_attrs<-c('Lot.Area','Mas.Vnr.Area','Total.Bsmt.SF','Gr.Liv.Area',
                    'Garage.Area','Wood.Deck.SF','Open.Porch.SF',
                    'Enclosed.Porch','X3Ssn.Porch','Screen.Porch','Pool.Area',
                    'SalePrice')

discrete_attrs<- c('Year.Built','Year.Remod.Add','Full.Bath','Half.Bath','Bedroom.AbvGr',
                   'TotRms.AbvGrd','Fireplaces',
                   'Garage.Cars','Mo.Sold','Yr.Sold')
 
ordinal_attrs<-c('Overall.Qual','Overall.Cond','Kitchen.Qual')
               
nominal_attrs<-c('Neighborhood','Bldg.Type','Roof.Style',
                 'Roof.Matl','Mas.Vnr.Type','Foundation','Heating','Central.Air',
                 'Sale.Condition')

#attribute vector to use
attrs<-c(continuous_attrs,nominal_attrs,discrete_attrs,ordinal_attrs)

#only look at single family homes with normal sale conditions
ames_df_1<-filter(ames_df, ames_df$Bldg.Type=='1Fam', ames_df$Sale.Condition=='Normal')

ames_df_1 <- ames_df_1[colnames(ames_df_1) %in% attrs]
ames_df_1$Date<- as.Date(paste(ames_df_1$Yr.Sold,ames_df_1$Mo.Sold, "01",sep = "-"))
################################################################################
################################################################################
################################################################################

################################################################################
#Prepare attributes for regression by making them factors
################################################################################
#Ordinal
# ames_df_1$Overall.Qual<-factor(ames_df_1$Overall.Qual, ordered = TRUE)
# ames_df_1$Overall.Cond<-factor(ames_df_1$Overall.Cond,ordered = TRUE)
#ames_df_1$Kitchen.Qual<-factor(ames_df_1$Kitchen.Qual, ordered = TRUE,
                          #levels = c('Po','Fa','TA','Gd','Ex'))
ames_df_1$Kitchen.Qual<- recode(ames_df_1$Kitchen.Qual,
                                `Po`=1,`Fa`=2,`TA`=3,`Gd`=4,`Ex`=5)


#Nominal
ames_df_1$Neighborhood<-factor(ames_df_1$Neighborhood)
ames_df_1$Bldg.Type<-factor(ames_df_1$Bldg.Type)
ames_df_1$Roof.Style<-factor(ames_df_1$Roof.Style)
ames_df_1$Neighborhood<-factor(ames_df_1$Neighborhood)
ames_df_1$Roof.Matl<-factor(ames_df_1$Roof.Matl)  
ames_df_1$Mas.Vnr.Type<-factor(ames_df_1$Mas.Vnr.Type)
ames_df_1$Foundation<-factor(ames_df_1$Foundation)
ames_df_1$Heating<-factor(ames_df_1$Heating)
ames_df_1$Central.Air<-factor(ames_df_1$Central.Air)
ames_df_1$Sale.Condition<-factor(ames_df_1$Sale.Condition)

#Discrete
#ames_df_1$Year.Built<-factor(ames_df_1$Year.Built)
ames_df_1$Year.Remod.Add<-factor(ames_df_1$Year.Remod.Add)
ames_df_1$Full.Bath<-factor(ames_df_1$Full.Bath)
# ames_df_1$Half.Bath<-factor(ames_df_1$Half.Bath)
ames_df_1$Bedroom.AbvGr<-factor(ames_df_1$Bedroom.AbvGr)
# ames_df_1$TotRms.AbvGrd<-factor(ames_df_1$TotRms.AbvGrd)
# ames_df_1$Fireplaces<-factor(ames_df_1$Fireplaces)
# ames_df_1$Garage.Yr.Blt<-factor(ames_df_1$Garage.Yr.Blt)
# ames_df_1$Garage.Cars<-factor(ames_df_1$Garage.Cars)
# ames_df_1$Mo.Sold<-factor(ames_df_1$Mo.Sold)
# ames_df_1$Yr.Sold<-factor(ames_df_1$Yr.Sold)

ames_df_1

################################################################################
# preliminary EDA
################################################################################

str(ames_df_1)
summary(ames_df_1)

#Where are the houses?
ggplot(ames_df_1, aes(x=Neighborhood)) + geom_bar(color="darkblue",fill="blue",
                                                               alpha=0.5)+
  ggtitle("Exhibit 1:Histogram of Ames Housing Sales by Neighborhood" )+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#How expensive are the houses?
mu<- mean(ames_df_1$SalePrice)

ggplot(ames_df_1, aes(x=SalePrice)) + geom_histogram(color="darkblue",fill="blue",
                                                               alpha=0.5, bins = 50)+
  ggtitle("Exhibit 2:Histogram of Ames Housing Sale Prices" )+
  labs(x="Sale Price")+
  scale_x_continuous(labels=scales::dollar_format())+
  geom_vline(xintercept = mu, color= "red", linetype= "dashed")+
  geom_label(aes(x=mu,y=280, label="Mean Sales Price"), col="red")
  
  
#When were the houses built?
ggplot(ames_df_1, aes(x=Year.Built))+ geom_histogram(color="darkblue",fill="blue",
                                                               alpha=0.5, stat="count")+
  ggtitle("Exhibit 3:Histogram of the Year Homes were Built within Ames City Limits" )


#When were the houses sold?
ggplot(ames_df_1, aes(x=Yr.Sold)) + geom_histogram(color="darkblue",fill="blue",
                                                               alpha=0.5)+
  ggtitle("Exhibit x:Histogram of Ames Housing Sales by Year" )

ggplot(ames_df_1, aes(x=Mo.Sold)) + geom_histogram(color="darkblue",fill="blue",
                                                             alpha=0.5)+
  ggtitle("Exhibit x:Historgram of Ames Housing Sales by Month" )+
  scale_x_continuous(breaks = seq(1, 12, 1))

#trend 
ames_df_1 %>% count(Date) %>% 
  ggplot(aes(x=Date, y=n))+
  geom_line(color="blue",alpha=0.5)+
  ggtitle("Exhibit 1:Trend of Ames House Sales" )

#How big are the houses?
ggplot(ames_df_1, aes(x=ames_df_1$Gr.Liv.Area)) + geom_histogram(color="darkblue",fill="blue",
                                                             alpha=0.5)+
  ggtitle("Exhibit 4:Histogram of Home Sizes by Above Gr. SF" ) + xlab('Square Footage')

#How big are the lots?

#SF to acres = 43560 = 1 acre
acres<- ames_df_1$Lot.Area/43560

ggplot(ames_df_1, aes(x=acres)) + geom_histogram(color="darkblue",fill="blue",
                                                                 alpha=0.5,
                                                 bins = 50)+
  ggtitle("Exhibit x:Historgram of Ames Housing Lot Sizes (SF)")
################################################################################
################################################################################
################################################################################

################################################################################
# Explore missingness graphically
################################################################################
aggr(ames_df_1,prop=TRUE, numbers=TRUE)
matrixplot(ames_df_1)
################################################################################
################################################################################
################################################################################

################################################################################
#Explore missingness 
################################################################################
#Drop columns with missingness exceeding threshold
print('fractional NA report:')
miss_thresh = 0.25
keeps <- c()
drops <- c()
attr_vector <- c()
missingness_vector <- c()
missingness_dict <- hash()
for (attr_name in colnames(ames_df_1)) {
  missingness <- sum(is.na(ames_df_1[[attr_name]]))/nrow(ames_df_1)
  attr_vector <- c(attr_vector, attr_name)
  missingness_vector <- c(missingness_vector, missingness)
  missingness_dict[[attr_name]] <- missingness
  cat(sprintf("\"%s\"\"%f\"\n", attr_name, missingness))
  if (missingness <= miss_thresh) {
    keeps <- append(keeps, attr_name)
  } else {
    drops <- append(drops, attr_name)
  }
}
missingness_df <- data.frame(attr = attr_vector, frac_missingness=
                               missingness_vector)
print(keeps)
print(drops)
ames_df_1 <- ames_df_1[keeps]
################################################################################
################################################################################
################################################################################

################################################################################
# input "0" in the blank entries within Mas.Vnr.Type 
################################################################################

ames_df_1$Mas.Vnr.Area<-ifelse(is.na(ames_df_1$Mas.Vnr.Area),0,ames_df_1$Mas.Vnr.Area)
aggr(ames_df_1, prop = TRUE, numbers = TRUE)
################################################################################
################################################################################
################################################################################

################################################################################
# eda continued, mostly forloop for scatter plots
################################################################################
for (col_name in colnames(ames_df_1)) {
  x <- tryCatch(    #not necessarily needed
    {
      if (col_name != 'SalePrice') {
        scatterplot(ames_df_1$SalePrice ~ ames_df_1[, col_name], 
                    main= paste('ames_df_1$SalePrice ~', col_name),
                    xlab = col_name, id = list(n=0))
      }
    },
    error = function(e) {
      print(e)
      print(paste("col_name:", col_name))
      print(typeof(ames_df_1[, col_name]))
      print(cat("first 5 values of col_name:", ames_df_1[1:5, col_name], '\n\n'))
    }
  )
}

summary(ames_df_1)
tableview<-head(ames_df_1)
################################################################################
################################################################################
################################################################################

################################################################################
# Visualize data
################################################################################

options(scipen = 100)

ames_df_1$Yr.Sold.1 <- as.factor(ames_df_1$Yr.Sold.1)

ggboxplot(ames_df_1, x = "Kitchen.Qual.1", y = "SalePrice",
                color = "Yr.Sold.1", legend = 'right') + ggtitle('Exhibit 5: Sale Price Based on Kitchen Quality') + xlab('Kitchen Qual') + ylab('Sale Price') 

ggboxplot(ames_df_1, x = "Garage.Cars", y = "SalePrice",
                color = "Yr.Sold.1", legend = 'right') + ggtitle('Exhibit 6: Sale Price Based on Garage Size') + xlab('Car Bays') + ylab('Sale Price') 


Sys.sleep(3)

# # Two-way interaction plot - view with Garage type as x factor
# interaction.plot(x.factor = ames_df_1$Garage.Cars, trace.factor = ames_df_1$Neighborhood,
#                  response = ames_df_1$SalePrice, fun = mean, type = "b", legend = TRUE,
#                  xlab = "Garage.Type", ylab = "SalePrice", pch = c(1, 19))
# 
# # view with density on x axis
# print(ggboxplot(cp_df, x = "density", y = "yield", color = "fertilizer", 
#                 palette = c("#00AFBB", "#E7B800", "#FF0000")))
# print(ggline(cp_df, x = "density", y = "yield", color = "fertilizer",
#              add = c("mean_se"), palette = c("#00AFBB", "#E7B800", "#FF0000")))
# Sys.sleep(3)
# 
# # view with fertilizer on x axis
# print(ggboxplot(cp_df, x = "fertilizer", y = "yield", color = "density", 
#                 palette = c("#00AFBB", "#E7B800", "#FF0000")))
# print(ggline(cp_df, x = "fertilizer", y = "yield", color = "density",
#              add = c("mean_se"), palette = c("#00AFBB", "#E7B800", "#FF0000")))
# Sys.sleep(3)
# 
# # Box plot with two factor variables - view with density as first
# boxplot(yield ~ density * fertilizer, data = cp_df, frame = FALSE, 
#         ylab = "yield")
# # Two-way interaction plot - view with density as x factor
# interaction.plot(x.factor = cp_df$density, trace.factor = cp_df$fertilizer, 
#                  response = cp_df$yield, fun = mean, type = "b", legend = TRUE, 
#                  xlab = "density", ylab = "yield", pch = c(1, 19))
# 
# # Box plot with two factor variables - view with fertilizer as first
# boxplot(yield ~ fertilizer * density, data = cp_df, frame = FALSE, 
#         ylab = "yield")
# # Two-way interaction plot - view with density as x factor
# interaction.plot(x.factor = cp_df$fertilizer, trace.factor = cp_df$density, 
#                  response = cp_df$yield, fun = mean, type = "b", legend = TRUE, 
#                  xlab = "density", ylab = "yield", pch = c(1, 19))

################################################################################
# check out correlations
################################################################################
# check out correlations for numerical variables



corrs <- round(cor(ames_df_1[, unlist(lapply(ames_df_1, is.numeric))], use = "pairwise"),
               2)
corrplot(corrs, type = "upper", col = brewer.pal( n = 8, name = "RdYlBu"), title = "Exhibit 7: Correlation Plot of The Ames Housing Dataset",mar=c(0,0,1,0))

# find the strongest correlations
corr_df <- process_corr_matrix(corrs, 0.50, -0.50)
sale_price_corr_df = corr_df[corr_df['Var1'] == 'SalePrice'|
                               corr_df['Var2']== 'SalePrice',]
multi_corr_df <- corr_df[corr_df['Var1'] != 'SalePrice' &
                           corr_df['Var2'] != 'SalePrice',]
################################################################################
################################################################################
################################################################################

################################################################################
# Developing Hypothesis - One way anova
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

################################################################################
# # compute two-way anova
# ################################################################################
# writeLines('\n****************************************************************')
# writeLines('compute two-way anova:')
# result <- aov(SalePrice ~ Bldg.Type + Neighborhood, data = ames_df_1)
# summary(result)
# result2 <- aov(SalePrice ~ Bldg.Type * Neighborhood, data = ames_df_1)
# summary(result2)

################################################################################
# compute summary statistics
################################################################################
writeLines('\n****************************************************************')
writeLines('compute some summary statistics:')
options(pillar.sigfig = 4)  # print 4 sig figs
print(
  dplyr::group_by(ames_df_1, Neighborhood) 
  %>% 
    dplyr::summarise(count = n(), 
                     mean = mean(SalePrice, na.rm = TRUE), 
                     sd = sd(SalePrice, na.rm = TRUE)
    )
)

# ################################################################################
# # Which pair of means are different
# ################################################################################
# writeLines('\n****************************************************************')
# writeLines('which pair of means are different:')
# # print(TukeyHSD(result2, which = "Bldg.Type"))
# # print(TukeyHSD(result2, which = "Neighborhood"))
# #print(TukeyHSD(result2))
# 
# writeLines('\n****************************************************************')
# writeLines('check anova assumptions:')
# 
# writeLines('\n************************')
# writeLines('Homogeneity of variances')
# plot(result2, 1)
# print(leveneTest(SalePrice ~ Bldg.Type * Neighborhood, data = ames_df_1))
# 
# writeLines('\n************************')
# writeLines('Normality')
# plot(result2, 2)
# # extract the residuals and run Shapiro-Wilk test
# aov_residuals <- residuals(object = result2)
# #print(shapiro.test(x = aov_residuals))
# ################################################################################
# ################################################################################
# ################################################################################

################################################################################
# one-way ANNOVA
################################################################################
writeLines('\n****************************************************************')
writeLines('one-way ANNOVA:')

# one-way ANNOVA
writeLines('\nH0: The mean sale price for a normal sale of single family homes 
           across the neighborhood city limits of Ames is the same')
writeLines('H1: at least one of the neighborhoods has a higher or lower mean sale price than the other')
writeLines('claim: H1')

# perform test
# wants a data frame
alpha <- 0.05
anova <- aov(ames_df_1$SalePrice ~ ames_df_1$Neighborhood, data = ames_df_1)

writeLines('\n***************************')
writeLines('results of one-way anova')
print(summary(anova))


# visualize the results
boxplot(ames_df_1$SalePrice ~ ames_df_1$Neighborhood, data = ames_df_1)


# save summary to an object
anova_summary <- summary(anova)

# p-value
p_value <- anova_summary[[1]][1, 'Pr(>F)']

writeLines('\n***************************')
# compare the p-value to alpha and make decision
if (p_value > alpha) {
  decision = 'fail to reject H0'
} else {
  decision = 'reject h0'
}
writeLines(paste('decision: ', decision))

writeLines('\n***************************')
writeLines('use TukeyHSD to understand differences between the means\n')
# see differences
# diff: differences between the means
# lwr, upr: bounds of 95% confidence intervals for diff
# p adj: adjusted for multiple comparisons
print(TukeyHSD(anova))

################################################################################
# Developing Hypothesis - Two way anova
################################################################################
#Bedrooms
writeLines('H0: There is no difference in sales price of houses with different bedroom types')
writeLines('H1: There is a difference in sales price of houses with different bedroom types')
writeLines('Claim: H1')

# Bathrooms
WriteLines('H0: There is no difference in sales price of houses with different bathroom types')
WriteLines('H1: There is a difference in sales price of houses with different bathroom types')
WriteLines('Claim: H1')

#Bedrooms & Bathrooms Hypothesis
WriteLines('H0: There is no interaction effect between type of bedroom and type of bathroom on sales price')
WriteLines('H1: There is an interaction effect between type of bedroom and type of bathroom on sales price')
WriteLines('Claim: H1')

################################################################################
# # compute two-way anova
# ################################################################################
writeLines('\n****************************************************************')
writeLines('compute two-way anova:')
result2 <- aov(SalePrice ~ Bedroom.AbvGr * Full.Bath, data = ames_df_1)
summary(result2)

################################################################################
# compute summary statistics
################################################################################
writeLines('\n****************************************************************')
writeLines('compute some summary statistics:')
options(pillar.sigfig = 4)  # print 4 sig figs
print(
  dplyr::group_by(ames_df_1, Neighborhood) 
  %>% 
    dplyr::summarise(count = n(), 
                     mean = mean(SalePrice, na.rm = TRUE), 
                     sd = sd(SalePrice, na.rm = TRUE)
    )
)

###Tukey?????????



################################################################################
# fit a regression model - model 3 with factor attributes
################################################################################
fit_3 <- lm(formula = SalePrice ~ Gr.Liv.Area + Garage.Area + Overall.Qual +
              Kitchen.Qual, data = ames_df_1)


print(summary(fit_3))

par(mfrow = c(2,2))
plot(fit_3)
par(mfrow = c(1,1))

vif(fit_3)
################################################################################
################################################################################
################################################################################


################################################################################
#make a prediction using fit 1
################################################################################
# the_data <- df_1[, c('Gr.Liv.Area','Garage.Area', 'X1st.Flr.SF')]
# pred<- predict(fit_1, the_data)
# print(the_data[1,])
# print(pred[1])
# print(df_1[1, c('SalePrice')])
# ################################################################################
# ################################################################################
# ################################################################################






################################################################################
#Logistic Regression - Central Air Predictors?
################################################################################
################################################################################
################################################################################


################################################################################
#Plots
################################################################################
ggplot(ames_df_1) + geom_boxplot(aes(x=Central.Air, y= Year.Built),color="darkblue",fill="blue",
                                 alpha=0.5)+
  ggtitle("Exhibit 1:Boxplot of Central Air Homes vs Year Built" )+
  labs(x="Central Air")



ggplot(ames_df_1) + geom_boxplot(aes(x=Central.Air, y= SalePrice),color="darkblue",fill="blue",
                                 alpha=0.5)+
  ggtitle("Exhibit 2:Boxplot of Central Air Homes vs Sale Price" )+
  labs(x="Central Air")+
scale_y_continuous(labels=scales::dollar_format())


ggplot(ames_df_1) + geom_boxplot(aes(x=Central.Air, y= Gr.Liv.Area),color="darkblue",fill="blue",
                                 alpha=0.5)+
  ggtitle("Exhibit 3:Boxplot of Central Air Homes vs Above Ground Livable Area (SF)" )+
  labs(x="Central Air")


################################################################################
################################################################################
################################################################################
# split data into train and test sets before EDA - avoid data leakage through 
# the data scientist
################################################################################
writeLines('\n****************************************************************')
writeLines('split data into train and test sets\n')
trainIndex<- createDataPartition(ames_df_1$Central.Air, p=0.7, list= FALSE, times =1)
train_df<-ames_df_1[trainIndex,]
test_df<-ames_df_1[-trainIndex,]



writeLines('\n*********************')
writeLines('head(train_df):\n')
print(head(train_df))

writeLines('\n*********************')
writeLines('head(test_df):\n')
print(head(test_df))

writeLines('\n*********************')
vec_freq_dist(a_vec = train_df$Central.Air, a_vec_name = 'train_df$Central.Air')

writeLines('\n*********************')
vec_freq_dist(a_vec = test_df$Central.Air, a_vec_name = 'test_df$Central.Air')
################################################################################
################################################################################
################################################################################


################################################################################
# fit a logistic regression model
################################################################################
writeLines('\n****************************************************************')
writeLines('fit a logistic regression model\n')

writeLines('\n*********************')
writeLines('model_1\n')

model_1 <- glm(Central.Air ~ Year.Built+Gr.Liv.Area, data = train_df, family = binomial(link = 'logit'))
print(summary(model_1))
return_list <- log_reg_pseudo_r_square(model_1)
r_square_model_1 <- return_list['r_square'][[1]]
p_value_model_1 <- return_list['p_value'][[1]]

writeLines('\n*********************')
writeLines('McFadden\'s Pseudo R^2 and its p-value:\n')
writeLines(paste('McFadden\'s Pseudo R^2: ', r_square_model_1))
writeLines(paste('McFadden\'s Pseudo R^2 p-value: ', p_value_model_1))

writeLines('\n*********************')
writeLines('display regression coefficients (log-odds)\n')
print(coef(model_1))

writeLines('\n*********************')
writeLines('display regression coefficients (odds)\n')
print(exp(coef(model_1)))

################################################################################
################################################################################
################################################################################

################################################################################
# train set predictions
################################################################################
writeLines('\n****************************************************************')


return_list <- classifier_performance(model_1, a_model_name = 'model_1', 
                                      train_df, a_data_df_name = 'train_df', 
                                      y_true = train_df$Central.Air, FALSE)
title("Exitibit 7: Train Data ROC Curve Plot", line = 3)

y_probs_train <- return_list['y_probs'][[1]]
y_preds_train <- return_list['y_preds'][[1]]
y_true_train <- return_list['y_true'][[1]]
classification_report_train <- return_list['classification_report'][[1]]

writeLines('\nconfusion matrix (Reference is also known as actual or truth):\n')
print(classification_report_train$table)

writeLines('\naccuracy:')
print(unname(classification_report_train$overall['Accuracy']))

writeLines('\nsensitivity (also known as recall):')
print(unname(classification_report_train$byClass['Sensitivity']))

writeLines('\npositive predictive value (also known as precision):')
print(unname(classification_report_train$byClass['Pos Pred Value']))

writeLines('\nspecificity (also known as true negative rate):')
print(unname(classification_report_train$byClass['Specificity']))
################################################################################
################################################################################
################################################################################

################################################################################
# test set predictions
################################################################################
writeLines('\n****************************************************************')

return_list <- classifier_performance(model_1, a_model_name = 'model_1', 
                                      test_df, a_data_df_name = 'test_df', 
                                      y_true = test_df$Central.Air, FALSE)
title("Exitibit 8: Test Data ROC Curve Plot", line= 3)
y_probs_test <- return_list['y_probs'][[1]]
y_preds_test <- return_list['y_preds'][[1]]
y_true_test <- return_list['y_true'][[1]]
classification_report_test <- return_list['classification_report'][[1]]

writeLines('\nconfusion matrix (Reference is also known as actual or truth):\n')
print(classification_report_test$table)
writeLines('\naccuracy:')
print(unname(classification_report_test$overall['Accuracy']))
writeLines('\nsensitivity (also known as recall):')
print(unname(classification_report_test$byClass['Sensitivity']))
writeLines('\npositive predictive value (also known as precision):')
print(unname(classification_report_test$byClass['Pos Pred Value']))
################################################################################
################################################################################
################################################################################

writeLines('\n\n\n\n\n\n\n\n\n')


######TEST#



