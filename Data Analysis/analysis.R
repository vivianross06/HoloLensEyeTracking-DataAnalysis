rm(list = ls())

library(plyr)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(rstatix)
library(emmeans)
library(phia)       #testInteractions
library(tidyr)      #spread
library(ARTool)     #art, artlm
library(coin)

#average errors: recalibrated with calibration task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_calibration_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

#plot dataframes for cosine error
ggplot(a_average_error, aes(x=taskname, y=cosineError)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean", fill='goldenrod') + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)')
ggplot(b_average_error, aes(x=taskname, y=cosineError)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean", fill='goldenrod') + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Cosine Error (degrees)')



#average errors: recalibrated with ssHeadConstrained task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_ssHeadConstrained_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))



#average errors: recalibrated with wsBodyConstrained task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_wsBodyConstrained_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))



#average errors: recalibrated with ssWalking task
average_error_data <- read.csv("/Users/vivianross/Documents/School/Research/EyeTracking/HoloLensEyeTracking-DataAnalysis/Data/recalibrated_ssWalking_average_error.csv", header = TRUE, fileEncoding = 'UTF-8-BOM')

#turn variables into factors
average_error_data$taskname = factor(average_error_data$taskname)
average_error_data$hololens = factor(average_error_data$hololens)

#separate between HoloLenses
a_average_error <- average_error_data[average_error_data$hololens=='A',]
b_average_error <- average_error_data[average_error_data$hololens=='B',]

#change dataframes into graphable format
long_a_average_error <- a_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')
long_b_average_error <- b_average_error |> pivot_longer(cols = c('euclideanError', 'recalibratedEuclideanError'), names_to = 'ErrorType', values_to = 'ErrorValue')

#plot dataframes for calibrated and uncalibrated euclidean error with 95% confidence interval
ggplot(long_a_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))
ggplot(long_b_average_error, aes(x=taskname, y=ErrorValue, fill=ErrorType)) + geom_bar(position=position_dodge(), stat = "summary", fun = "mean") + geom_errorbar(position=position_dodge(), stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96)) + scale_x_discrete(name = "", limits=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), breaks=c('calibration', 'ssHeadConstrained', 'wsBodyConstrained', 'ssWalking', 'wsWalking', 'hallway'), labels=c('Calibration', 'Head Constrained', 'Body Constrained', 'Screen Stabilized Walking', 'World Stabilized Walking', 'Hallway')) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ylab('Euclidean Error (degrees)') + scale_fill_discrete(name="Error Type", breaks=c('euclideanError', 'recalibratedEuclideanError'), labels=c('Uncalibrated', 'Recalibrated'))

