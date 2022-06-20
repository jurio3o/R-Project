setwd('C:/Users/wkddm/Desktop/통계분석실습')
library(dplyr)
library(ggplot2)
library(fastDummies)
library(tidyverse)
library(prediction)
library(caret)
library(MLmetrics)
library(doParallel)
library(parallel)
library(foreach)
library(cvTools)
library(plyr)
library(C50)
library(printr)

raw_train <- read.csv('trainset.csv')
raw_test <- read.csv('testset.csv')

#### train

train_c <- raw_train[,(5:63)]
train_b <- raw_train[,c(64:122)]
diff_bc <- train_c - train_b

col_list <- c()
for (i in 1:ncol(diff_bc)){
  col_name <- paste0('diff',i)
  col_list <- c(col_list, col_name)
}

colnames(diff_bc) <- col_list

train <- cbind(raw_train, diff_bc)

train <-  subset(train, select = -c(Id, Y_c, yy_c, mm_c, yy_b, mm_b)) %>% 
  dummy_cols(select_columns = 'Y_b')   # 141 obs
train$Y_b <- NULL

##### test 

raw_test$Y_b <- factor(raw_test$Y_b, levels = c('A','A-','A+',
                                                'AA','AA-','AA+',
                                                'AAA','B','B-','B+',
                                                'BB','BB-','BB+','BBB',
                                                'BBB-','BBB+','C','CC','CCC',
                                                'CCC-','D'))

test_c <- raw_test[,(3:61)]
test_b <- raw_test[,(62:120)]
diff_bc_test <- test_c - test_b

colnames(diff_bc_test) <- col_list

test <- cbind(raw_test,diff_bc_test)

test <-  subset(test, select = -c(Id, yy_c, mm_c, yy_b, mm_b))  %>% 
  dummy_cols(select_columns = 'Y_b')   # 140 obs
test$Y_b <- NULL

############################# 데이터 분할 ##################################

set.seed(123)
train_idx <- createDataPartition(train$C, p = 0.8, list = F)
train.data <- train[train_idx,]
valid.data <- train[-train_idx,]


train.data$C <- as.factor(train.data$C)
valid.data$C <- as.factor(valid.data$C)

K = 5
R = 1

set.seed(123)
cv = cvFolds(NROW(train.data), K = K , R = R)

grid <-  expand.grid(trials = c(1,5,10,20,30),
                     rules = c(T,F))

result <- foreach(g=1:NROW(grid), .combine=rbind) %do% {
  foreach(r=1:R, .combine=rbind) %do% {
    foreach(k=1:K, .combine=rbind) %do% {
      validation_idx <- cv$subsets[which(cv$which == k), r]
      train <- train.data[-validation_idx, ]
      validation <- train.data[validation_idx, ]
      
      # 모델 훈련
      m <- C5.0(C ~., data=train ,control=C5.0Control(winnow = F),
                trials=grid[g, "trials"],
                rules=grid[g, "rules"])
      
      # 예측
      predicted <- predict(m, newdata=validation)
      
      # 성능 평가
      F_score <- F1_Score(factor(predicted), factor(validation$C)) 
      return(data.frame(g=g, F_score=F_score))
    }
  }
}

ddply(result, .(g), summarize, mean_Fscore = mean(F_score)) %>% arrange(desc(mean_Fscore))
## 5번째 파라미터 조합의 valid F1-score가 가장 크다. 
grid[5,]  # 30 T

m <- C5.0(C ~., data=train.data ,control=C5.0Control(winnow = F),
          trials= 30,
          rules= T)

pred_valid_class_c5 = predict(m, valid.data, type = 'class')
pred_valid_prob_c5 = data.frame(predict(m, valid.data, type = 'prob'))
confusionMatrix(factor(pred_valid_class_c5), factor(valid.data$C))
F1_Score(factor(pred_valid_class_c5), factor(valid.data$C))
colnames(pred_valid_prob_c5) <- c(-1,0,1)

pred_test_class_c5 <- predict(m, test, type = 'class')
pred_test_prob_c5 <- data.frame(predict(m, test, type = 'prob'))
colnames(pred_test_prob_c5) <- c(-1,0,1)

table(pred_test_class_c5)


sample <- read.csv('sample_submission.csv')
sample$Predicted <- pred_test_class_c5 
write.csv(sample, 'sample_submission_c5_tune_차이변수.csv', row.names =  F)

