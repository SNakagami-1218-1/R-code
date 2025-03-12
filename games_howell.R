library(ggplot2)
library(dplyr)
library(multcomp)
library(multcompView)
library(rstatix)  
library(tidyverse)
library(DescTools)
library(ggbeeswarm) 
library(car)
library(ggpubr)
library(car)
library(broom)
library(tidyverse)
library(rcompanion)
library(ggsignif)
library(openxlsx)
library(PMCMRplus)

data <- read.csv("9dag_ck_LR.csv", header = T)

data$X <- factor(data$X, levels = c("WT","sep","dou"))


#Y軸~X軸
levene_p <- levene_test( ~ , data = data)
levene_p
                                                                                                                                                                                                                                                                                       +     )                     