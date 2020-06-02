#setwd("~/Desktop")
library(tidyverse)
library(ggplot2)
library(Cairo)
library(epiR)
library(car)
library(caret)
library(InformationValue)
library(ResourceSelection)
library(ROCR)
library(MatchIt)
#read
dataraw <- read.csv("/Users/mac/Desktop/美兆/data/dataclear.csv")
dataclear <- dataraw

y1_raw <- dataraw  %>% filter(yr == 2005 )
y2_raw <- dataraw  %>% filter(yr == 2008 )
y3_raw <- dataraw  %>% filter(yr == 2011 )
y4_raw <- dataraw  %>% filter(yr == 2014 )
y5_raw <- dataraw  %>% filter(yr == 2017 )
#---data clear step --------------

#education 1 大學以下 2 大學以上
table(y1_raw$education)
y1_raw <- y1_raw %>% mutate(education_level = ifelse(education <= 5 ,1 , 2) )

# occupation
table(y1_raw$occupation)
#9 -> 11 or 12 : 162 = 7 4284 = 1 11842 = 1 
y1_raw[162,16] <- 12 ; y1_raw[4284,16] <- 11 ; y1_raw[11842,16] <- 11
#2 -> 18
y1_raw[which(y1_raw$occupation == 2),16] <- 18
#3 -> 13 
y1_raw[which(y1_raw$occupation == 3),16] <- 13
#5 -> 19 
y1_raw[which(y1_raw$occupation == 5),16] <- 19

#1 軍人 2 (11,12)學生 3 (6 7 13 14 15 16 17 18 21 22 ) 工商管理 4 (19 20)勞動型 5 (8 10 23 24 25 26 )無 
y1_raw <- y1_raw %>% mutate(occupation_level = ifelse(occupation ==11 | occupation ==12 , 2 , occupation))
y1_raw$occupation_level <- ifelse( y1_raw$occupation_level >= 13 & y1_raw$occupation_level <= 18 ,3 , y1_raw$occupation_level)
y1_raw$occupation_level <- ifelse( y1_raw$occupation_level >= 21 & y1_raw$occupation_level <= 22 ,3 , y1_raw$occupation_level)
y1_raw$occupation_level <- ifelse( y1_raw$occupation_level >= 6  & y1_raw$occupation_level <= 7  ,3 , y1_raw$occupation_level)

y1_raw$occupation_level <- ifelse( y1_raw$occupation_level >= 19 & y1_raw$occupation_level <= 20  ,4 , y1_raw$occupation_level)

y1_raw$occupation_level <- ifelse( y1_raw$occupation_level >= 23 & y1_raw$occupation_level <= 26  ,5 , y1_raw$occupation_level)
y1_raw$occupation_level <- ifelse( y1_raw$occupation_level >= 8  & y1_raw$occupation_level <= 10  ,5 , y1_raw$occupation_level)

table(y1_raw$occupation_level)

#fincome

# bad habbie
y1_raw <- y1_raw %>% mutate(bad_habbie = smokeornot_03 + drinkornot_98 + cocohabit_98)
# #smokeornot_03 pass temporally
# junk food eat
y1_raw <- y1_raw %>% mutate(bad_food =  food19_98 + food20_98 + food07_98 + food06_98 + food21_98)
y1_raw <- y1_raw %>% mutate(rsick =  rsick09 + rsick10 + rsick11 + rsick12 )

# sex relationship yes = 1 , no = 0
y1_raw <- y1_raw %>% mutate(relationship = ifelse(is.na(y1_raw$relate51b) == TRUE , 0 , 1) )

#mdrug01
#-----------------無法查出因果
#肝膽功能檢查
y1_raw$lf_tb  <- ifelse( y1_raw$lf_tb >= 0.2 & y1_raw$lf_tb <= 1.5 , 0 , 1)
y1_raw$lf_alp <- ifelse( y1_raw$lf_alp >= 35 & y1_raw$lf_alp <= 104 , 0 , 1)
y1_raw$lf_got <- ifelse( y1_raw$lf_got >= 10 & y1_raw$lf_got <= 27 , 0 , 1)
y1_raw$lf_gpt <- ifelse( y1_raw$lf_gpt >= 5 & y1_raw$lf_gpt <= 33 , 0 , 1)
y1_raw$lf_ggt <- ifelse( y1_raw$lf_ggt < 50 & y1_raw$gender == 1 , 0 ,  y1_raw$lf_ggt)
y1_raw$lf_ggt <- ifelse( y1_raw$lf_ggt > 39 & y1_raw$gender == 2 , 0 , 1)
y1_raw$lf_ldh <- ifelse( y1_raw$lf_ldh  >= 135 & y1_raw$lf_ldh  <= 214 , 0 , 1)

y1_raw <- y1_raw %>% mutate(liver = lf_tb + lf_alp + lf_got + lf_gpt + lf_ggt + lf_ldh )

#-----------


lm_logit_1 <- glm(formula = metabolic ~ 
                       education + occupation + fincome + bad_habbie + nutrino + vegetarian + 
                       bad_food + sportornot_98 + sleeptime_96 + sleeptype + 
                       rsick +  allergydrug +  relationship + mdrug01 +
                       g_bmi + g_pul + g_fat , 
                     data = y1_raw ,family = binomial(link = "logit"))

summary(lm_logit_1)

y1_omit <- y1_raw %>% select( pid, nn , yr , metabolic , gender , age , 
                              education , occupation , fincome , bad_habbie , nutrino , vegetarian , 
                                bad_food , sportornot_98 , sleeptime_96 , sleeptype , 
                                rsick ,  allergydrug ,  relationship , mdrug01 ,
                                g_bmi , g_pul , g_fat )

y1 <- na.omit(y1_omit)
y1$group <- ifelse(y1$metabolic == 0 , FALSE , TRUE )

data_match1 <- matchit( group ~ gender + age , data = y1 , method = "nearest" , ratio = 1)
data_y11 <- y1[which(data_match1$weights==1),]

plot(data_match1 , type = "hist")

#--------------------------
# logit regression
mis <- c()
pre <- c()
recall <- c()
spe <- c()
acu <- c()
set.seed(123)
datarandom<- data_y11[sample(nrow(data_y11)),]
folds <- cut(seq(1,nrow(datarandom)),breaks=10,labels=FALSE)
#--------------------------------------------------------
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_data1 <- datarandom[testIndexes, ]
  train_data1 <- datarandom[-testIndexes, ]
  
  model_logit <- glm(formula = metabolic ~ 
                       education + occupation + fincome + bad_habbie + nutrino + vegetarian + 
                       bad_food + sportornot_98 + sleeptime_96 + sleeptype + 
                       rsick +  allergydrug +  relationship + mdrug01 +
                       g_bmi + g_pul + g_fat ,
                     data = train_data1 ,family = binomial(link = "logit"))
  prob_logit <- predict(model_logit , test_data1 , type="response" )
  optCutOff <- optimalCutoff(actuals = test_data1$metabolic , predictedScores = prob_logit)[1]
  acu[i] <-  1-misClassError(test_data1$metabolic ,  prob_logit,optCutOff)
  pre[i] <-  precision(test_data1$metabolic ,  prob_logit ,optCutOff)
  recall[i] <-  sensitivity(test_data1$metabolic ,  prob_logit ,optCutOff)
  spe[i] <-  specificity(test_data1$metabolic ,  prob_logit ,optCutOff)  
}
output_logit <- data.frame(
  accuracy = acu,
  precision = pre,
  sensitivity = recall,
  specificity = spe
)
avg_output_logit<- data.frame(
  accuracy = mean(acu),
  precision = mean(pre),
  sensitivity = mean(recall),
  specificity = mean(spe)
)
round(avg_output_logit,4)
plotROC(test_data1$metabolic , prob_logit)

#--------------
#RandomForest
library(randomForest)
datarandom<- data_y11[sample(nrow(data_y11)),]
folds <- cut(seq(1,nrow(datarandom)),breaks=10,labels=FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_data2 <- datarandom[testIndexes, ]
  train_data2 <- datarandom[-testIndexes, ]
  
  model_rd <- randomForest(metabolic ~ 
                             education + occupation + fincome + bad_habbie + nutrino + vegetarian + 
                             bad_food + sportornot_98 + sleeptime_96 + sleeptype + 
                             rsick +  allergydrug +  relationship + mdrug01 +
                             g_bmi + g_pul + g_fat , 
                           data = train_data2, n_tree = 500 )
  prob_rd <- predict(model_rd , test_data2 , type="response" )
  
  optCutOff <- optimalCutoff(actuals = test_data2$metabolic, predictedScores = prob_rd )[1]
  acu[i] <-  1-misClassError(test_data2$metabolic , prob_rd  ,optCutOff)
  pre[i] <-  precision(test_data2$metabolic , prob_rd  ,optCutOff)
  recall[i] <-  sensitivity(test_data2$metabolic , prob_rd  ,optCutOff)
  spe[i] <-  specificity(test_data2$metabolic , prob_rd  ,optCutOff)  
}
output_rd <- data.frame(
  accuracy = acu,
  precision = pre,
  sensitivity = recall,
  specificity = spe
)
avg_output_rd <- data.frame(
  accuracy = mean(acu),
  precision = mean(pre),
  sensitivity = mean(recall),
  specificity = mean(spe)
)
avg_output_rd

#--------------
#決策樹CART
library(rpart)
datarandom<- data_y34[sample(nrow(data_y34)),]
folds <- cut(seq(1,nrow(datarandom)),breaks=10,labels=FALSE)

for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_data3 <- datarandom[testIndexes, ]
  train_data3 <- datarandom[-testIndexes, ]
  model_tree <- rpart(formula = metabolic ~ 
                        smokeornot_03 + cocohabit_98 + drinkornot_98 + 
                        food11_98 +
                        sport +
                        rsick09 + 
                        g_pul + g_bmi + g_fat,
                      data = train_data3, method = "class",cp = 1e-3)
  
  prob_tree <- as.numeric(predict(object = model_tree , newdata = test_data3, type = "class"))-1
  optCutOff <- optimalCutoff(actuals = test_data3$metabolic, predictedScores = prob_tree)[1]
  acu[i] <-  1-misClassError(test_data3$metabolic , prob_tree  ,optCutOff)
  pre[i] <-  precision(test_data3$metabolic , prob_tree  ,optCutOff)
  recall[i] <-  sensitivity(test_data3$metabolic , prob_tree  ,optCutOff)
  spe[i] <-  specificity(test_data3$metabolic , prob_tree  ,optCutOff) 
}
output_tree <- data.frame(
  accuracy = acu,
  precision = pre,
  sensitivity = recall,
  specificity = spe
)
avg_output_tree <- data.frame(
  accuracy = mean(acu),
  precision = mean(pre),
  sensitivity = mean(recall),
  specificity = mean(spe)
)
avg_output_tree

#--------------
#Support Vector Machine(SVM)
library(e1071)
datarandom<- data_omit2[sample(nrow(data_omit2)),]
folds <- cut(seq(1,nrow(datarandom)),breaks=10,labels=FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_data4 <- datarandom[testIndexes, ]
  train_data4 <- datarandom[-testIndexes, ]
  
  model_svm <- svm(formula = metabolic ~ 
                     smokeornot_03 + cocohabit_98 + drinkornot_98 + 
                     food11_98 +
                     sport +
                     rsick09 + 
                     g_pul + g_bmi + g_fat,
                   data = train_data4)
  prob_svm <- as.numeric(predict(object = model_svm , newdata = test_data4, type = "response"))-1
  optCutOff <- optimalCutoff(actuals = test_data4$metabolic, predictedScores = prob_svm)[1]
  acu[i] <-  1-misClassError(test_data4$metabolic , prob_svm  ,optCutOff)
  pre[i] <-  precision(test_data4$metabolic , prob_svm  ,optCutOff)
  recall[i] <-  sensitivity(test_data4$metabolic , prob_svm  ,optCutOff)
  spe[i] <-  specificity(test_data4$metabolic , prob_svm  ,optCutOff) 
}
output_svm <- data.frame(
  accuracy = acu,
  precision = pre,
  sensitivity = recall,
  specificity = spe
)
avg_output_svm <- data.frame(
  accuracy = mean(acu),
  precision = mean(pre),
  sensitivity = mean(recall),
  specificity = mean(spe)
)
avg_output_svm

#--------------
#Neural Netwoarks(NN)
library(nnet)
datarandom<- data_omit2[sample(nrow(data_omit2)),]
folds <- cut(seq(1,nrow(datarandom)),breaks=10,labels=FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test_data5 <- datarandom[testIndexes, ]
  train_data5 <- datarandom[-testIndexes, ]
  
  model_nn <- nnet(formula = metabolic ~ 
                     smokeornot_03 + cocohabit_98 + drinkornot_98 + 
                     food11_98 +
                     sport +
                     rsick09 + 
                     g_pul + g_bmi + g_fat,
                   data = train_data5 , size = 50 , maxit = 500) 
  
  prob_nn <- predict(object = model_nn , newdata = test_data5 , type = "raw")
  optCutOff <- optimalCutoff(actuals = test_data5$metabolic, predictedScores = prob_nn)[1]
  acu[i] <-  1-misClassError(test_data5$metabolic , prob_nn  ,optCutOff)
  pre[i] <-  precision(test_data5$metabolic , prob_nn  ,optCutOff)
  recall[i] <-  sensitivity(test_data5$metabolic , prob_nn  ,optCutOff)
  spe[i] <-  specificity(test_data5$metabolic , prob_nn  ,optCutOff) 
}
output_nn <- data.frame(
  accuracy = acu,
  precision = pre,
  sensitivity = recall,
  specificity = spe
)
avg_output_nn <- data.frame(
  accuracy = mean(acu),
  precision = mean(pre),
  sensitivity = mean(recall),
  specificity = mean(spe)
)
avg_output_nn





pr_logit <- prediction(predictions = prob_logit, labels = test_data1$metabolic)
prf_logit <- performance(prediction.obj = pr_logit , measure = "tpr", x.measure = 'fpr')
dd_logit <- data.frame(FP = prf_logit@x.values[[1]], TP = prf_logit@y.values[[1]])

pr_rd <- prediction(predictions = prob_rd , labels = test_data2$metabolic)
prf_rd <- performance(prediction.obj = pr_rd , measure = "tpr", x.measure = 'fpr')
dd_rd <- data.frame(FP = prf_rd@x.values[[1]], TP = prf_rd@y.values[[1]])

pr_tree <- prediction(predictions = prob_tree , labels = test_data3$metabolic)
prf_tree <- performance(prediction.obj = pr_tree , measure = "tpr", x.measure = 'fpr')
dd_tree <- data.frame(FP = prf_tree@x.values[[1]], TP = prf_tree@y.values[[1]])

pr_svm <- prediction(predictions = prob_svm , labels = test_data4$metabolic)
prf_svm <- performance(prediction.obj = pr_svm , measure = "tpr", x.measure = 'fpr')
dd_svm <- data.frame(FP = prf_svm@x.values[[1]], TP = prf_svm@y.values[[1]])

pr_nn <- prediction(predictions = prob_nn , labels = test_data5$metabolic)
prf_nn <- performance(prediction.obj = pr_nn , measure = "tpr", x.measure = 'fpr')
dd_nn <- data.frame(FP = prf_nn@x.values[[1]], TP = prf_nn@y.values[[1]])

g <- 
  ggplot() +
  geom_line(data = dd_logit , mapping = aes(x = FP, y = TP, color = 'Logistic Regression')) +
  geom_line(data = dd_nn , mapping = aes(x = FP, y = TP, color = 'Neural Networks')) +
  geom_line(data = dd_tree , mapping = aes(x = FP, y = TP, color = 'CART')) +
  geom_line(data = dd_rd , mapping = aes(x = FP, y = TP, color = 'Random Forest')) +
  geom_line(data = dd_svm,mapping = aes(x = FP, y = TP, color = 'Support Vector Machine')) +
  geom_segment(mapping = aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle(label = "ROC Curve") +
  labs(x = "False Positive Rate", y = "True Positive Rate")
g +
  scale_color_manual(name = "classifier",values = c('Logistic Regression'='#E69F00', 
                                                    'CART'='#009E73',
                                                    'Neural Networks'='#56B4E9',
                                                    'Random Forest'='#D55E00',
                                                    'Support Vector Machine'='#0072B2'))
