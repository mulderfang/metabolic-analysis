library(tidyverse)
library(ggplot2)
library(reshape2)
library(lattice)
library(epiR)
library(plotrix)
library(multcomp)

data <- read.csv("data/raw_data.csv")
# ----metabolic process----
#1 g_wc
table(is.na(data$g_wc))
data = data[complete.cases(data$g_wc),]

data$first <- 0
data[which(data$gender == 1 & data$g_wc >= 90) , "first"] <- 1
data[which(data$gender == 2 & data$g_wc >= 80) , "first"] <- 1
table(data$first)

#2 g_ss/g_ds
#deal with na g_ssl&g_ssr
data[which(is.na(data$g_ssl) == TRUE),which(names(data) == "g_ssl")] <- data[which(is.na(data$g_ssl) == TRUE),which(names(data) == "g_ssr")]
data[which(is.na(data$g_ssr) == TRUE),which(names(data) == "g_ssr")] <- data[which(is.na(data$g_ssr) == TRUE),which(names(data) == "g_ssl")]
data <- data %>% mutate(g_savg = (g_ssl + g_ssr)/2  )
#deal with na g_dsl&g_dsr
data[which(is.na(data$g_dsl) == TRUE),which(names(data) == "g_dsl")] <- data[which(is.na(data$g_dsl) == TRUE),which(names(data) == "g_dsr")]
data[which(is.na(data$g_dsr) == TRUE),which(names(data) == "g_dsr")] <- data[which(is.na(data$g_dsr) == TRUE),which(names(data) == "g_dsl")]
data <- data %>% mutate(g_davg = (g_dsl + g_dsr)/2)

table(is.na(data$g_davg));table(is.na(data$g_savg))
data = data[complete.cases(data$g_davg),]
data = data[complete.cases(data$g_savg),]
#filter
data <- data %>% mutate(second = ifelse(g_savg >= 130 | g_davg >= 85 , 1 , 0 ) )

#3 dm_fg
table(is.na(data$dm_fg))
data = data[complete.cases(data$dm_fg),]
data <- data %>% mutate(third =  ifelse(dm_fg >= 100,1,0))

#4 l_tg
table(is.na(data$l_tg))
data = data[complete.cases(data$l_tg),]
data <- data %>% mutate(fourth =  ifelse(l_tg >= 150,1,0) )

#5 l_hdlc
table(is.na(data$l_hdlc))
data = data[complete.cases(data$l_hdlc),]
data$fifth <- 0
data[which(data$gender == 1 & data$l_hdlc < 40) , "fifth"] <- 1
data[which(data$gender == 2 & data$l_hdlc < 50) , "fifth"] <- 1

data <- data %>% mutate(metabolic = first + second + third +fourth + fifth)
# data <- data[,-214:-215]

# make group 1-16 for diagnosis
index = 
  expand_grid(waistline = c(0,1),
              pressure  = c(0,1),
              glucose   = c(0,1),
              TG        = c(0,1),
              lipo      = c(0,1))

index = index %>% mutate(sum = waistline + pressure + glucose + TG + lipo ) %>% filter( sum >=3)
index = index[order(index$sum),]
index = index[,-6]

data$diagnosis <- 0
for(i in 1:16){
  data$diagnosis <- ifelse(data$first  == as.numeric(index[i,1]) & 
                             data$second == as.numeric(index[i,2]) &
                             data$third  == as.numeric(index[i,3]) &
                             data$fourth == as.numeric(index[i,4]) &
                             data$fifth  == as.numeric(index[i,5]) ,
                           i , data$diagnosis) }

# if metabolic or not
data = data %>% mutate( metabolic = ifelse(diagnosis > 0 , 1,0 ) )

#========== metabolic anaylsis plot================
## abdominal obesity
par(mfrow = c(2,3))
histStack( data$g_wc , data$gender ,
           col = c("navy" , "skyblue"),
           main = "Abdominal obesity",
           xlab = "g_wc",
           ylab = "Count")
legend( "right",c("male" , "female"),title = "Level" , title.col = "black",
        text.col = c("navy" , "skyblue") , text.font = 2 , cex = 1, bty = "n" , text.width = 30)
# male line
abline(v= 90 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext("Male (>= 90)", cex = 0.7 , line = -3 , adj = 0.7, col = "black")
#female line
abline(v= 80 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext("Female (>= 80)", cex = 0.7 , line = -1 , adj = 0.6 , col = "black")

## Systolic blood pressure s
hist( data$g_savg ,
           col = c("navy"),
           main = "Systolic blood pressure",
           xlab = "top number",
           ylab = "Count")
# savg line
abline(v= 130 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext(">= 130", cex = 0.7 , line = -3 , adj = 0.45, col = "black")

## Systolic blood pressure s
hist( data$g_davg ,
      col = c("navy"),
      main = "Systolic blood pressure",
      xlab = "bottom number",
      ylab = "Count")
# savg line
abline(v= 85 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext(">= 85", cex = 0.7 , line = -3 , adj = 0.5, col = "black")

## Fasting glucose
hist( data2$dm_fg ,
      col = c("navy"),
      main = "Fasting glucose",
      xlab = "dm_fg",
      ylab = "Count",
      xlim = c(50,250),
      breaks = 50)
# savg line
abline(v= 100 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext(">= 100", cex = 0.7 , line = -3 , adj = 0.35, col = "black")

## Triglyceride leve
hist( data$l_tg ,
      col = c("navy"),
      main = "Triglyceride level",
      xlab = "l_tg",
      ylab = "Count",
      xlim = c(0,500),
      breaks = 100
      )
# savg line
abline(v= 150 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext(">= 150", cex = 0.7 , line = -3 , adj = 0.4, col = "black")

## HDL cholesterol
histStack( data$l_hdlc , data$gender ,
           col = c("navy" , "skyblue"),
           main = "HDL cholesterol",
           xlab = "l_hdlc",
           ylab = "Count")
legend( "right",c("male" , "female"),title = "Level" , title.col = "black",
        text.col = c("navy" , "skyblue") , text.font = 2 , cex = 1, bty = "n" , text.width = 45)
# male line
abline(v= 40 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext("Male (<= 40)", cex = 0.7 , line = -3 , adj = 0.03, col = "black")
#female line
abline(v= 50 , lty= 3 , col='gray47', size = 2 , lwd = 1)
mtext("Female (<= 50)", cex = 0.7 , line = 0.2 , adj = 0.3 , col = "black")

# ====statistic====
library(plotrix)
library(multcomp)
data_p = data[which(data$diagnosis != 0),]
data_p$diagnosis = as.character(data_p$diagnosis)
data_p$yr = as.character(data_p$yr)

#plot it
type = as.character(seq(1:16))
color = c("navy","skyblue" , "firebrick1" , "azure" , "antiquewhite")
#by year
par(bg = "gray100", fg = "gray22",
    col.axis = "gray22", mar = c(5,5,5,2),
    cex = .65, las = 1)

value = rbind(table(data_p[which(data_p$yr == 2005),"diagnosis"]),
              table(data_p[which(data_p$yr == 2008),"diagnosis"]),
              table(data_p[which(data_p$yr == 2011),"diagnosis"]),
              table(data_p[which(data_p$yr == 2014),"diagnosis"]),
              table(data_p[which(data_p$yr == 2017),"diagnosis"]))
barplot(value, names.arg = type,
          col= color,
          main = "Diagnosis by year",
          col.main = "gray22", font.main = 2, cex.main = 2,
          xlab = "Diagnosis Type", ylab = "Count",
          col.lab = "gray22", font.lab = 2, cex.lab = 1)
legend("topright",c("2005","2008","2011","2014","2017"), cex = 1, fill = color)

#by gender
value = rbind(table(data_p[which(data_p$gender == 1),"diagnosis"]),
              table(data_p[which(data_p$gender == 2),"diagnosis"]))
barplot(value, names.arg = type,
        col= color,
        main = "Diagnosis by gender",
        col.main = "gray22", font.main = 2, cex.main = 2,
        xlab = "type", ylab = "Count",
        col.lab = "gray22", font.lab = 2, cex.lab = 1)
legend("topright",c("Male","Female"), cex = 1 , fill = color)

#by age
par(bg = "gray100", fg = "white",
    col.axis = "gray47", mar = c(5,5,5,2),
    cex = .8, las = 1)
boxplot(data_p$age ~ data_p$diagnosis , horizontal = TRUE,
        border = "cadetblue",
        main= "Diagnosis in age",
        col.main = "peachpuff4", font.main = 2, cex.main = 1.5,
        col = c("deepskyblue","peachpuff4"),
        xlab = "Age",
        ylab = "Type",
        col.lab = "peachpuff4", font.lab = 2, cex.lab = 1.5,
        names = type,
        cex.names = .6)
axis(1, col = "cadetblue", at = seq(10,90,10))


#education 
bwplot( education ~ yr | diagnosis , data_p ,
           index.cond = list(c(5,6,7,8,16,2,3,4,12,13,14,15,1,9,10,11)),
           strip = strip.custom( bg="lightgrey",
           par.strip.text = list(color="black", cex=.8, font=2)),
           main = "Education & year by 16 diagnosis",
           xlab = "Year",
           ylab = "Education")

# weight
bwplot( g_wei ~ yr | diagnosis ,data_p ,
        index.cond = list(c(5,6,7,8,16,2,3,4,12,13,14,15,1,9,10,11)),
        strip = strip.custom( bg="lightgrey",
                              par.strip.text = list(color="black", cex=.8, font=2)),
        main = "weight & year by 16 diagnosis")
        # xlab = "Year",
        # ylab = "Education")

# count diagnosis number
value = table(data$diagnosis)
barplot(value[-1], names.arg = type,
        col= "maroon",
        main = "Diagnosis number",
        col.main = "gray22", font.main = 2, cex.main = 2,
        xlab = "type", ylab = "Count",
        col.lab = "gray22", font.lab = 2, cex.lab = 1)





  