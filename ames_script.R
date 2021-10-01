# Checking edit access to GitHub - Venkatesh
#Make library calls.
library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(car)
library(mice)
library(leaps)

#Load the Ames housing dataset.
Ames <- read.csv(file = 'AmesHousing.csv')
