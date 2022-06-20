setwd("C:/Users/user/Desktop/통계분석실습/기말프로젝트")

train_set<-read.csv(file="trainset.csv")
test_set<-read.csv(file="testset.csv")


train_data<-subset(train_set, select = -c(Id, Y_c, yy_c, mm_c, yy_b, mm_b))
train_data$C<-as.factor(train_data$C)
#train_data$Y_b<-as.factor(train_data$Y_b)
test_data<-subset(test_set, select = -c(Id, yy_c, mm_c, yy_b, mm_b))
#test_data$Y_b<-factor(test_data$Y_b,levels=c("A","A-","A+","AA","AA-","AA+","AAA","B","B-","B+","BB","BB-","BB+","BBB","BBB-","BBB+","C","CC","CCC","CCC-","D"))
#knn
set.seed(123)
class_pred_knn<-knn(train_data[-1],test_data,cl=train_data$C,k=10)
summary(class_pred_knn)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_knn
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/knn_sample_submission.csv",row.names=F)

#의사결정나무
library(rpart)
tree <- rpart(C ~ ., data = train_data,control = rpart.control(cp = 0))
cp_opt<-tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree_pruned <- prune(tree, cp = cp_opt )

class_pred_tree <- predict(tree_pruned, newdata = test_data,type="class")
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_tree
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/tree_sample_submission.csv",row.names=F)


#svm 
library(e1071)
data_svm<-svm(C~.,data=train_data)
class_pred_svm<-predict(data_svm,test_data)
summary(class_pred_svm)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_svm
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/svm_sample_submission.csv",row.names=F)

#cross validation ; method확인필요함> svmRadial보다 나음
set.seed(123) # **처음에 시드설정안해서 f1score다시 확인해보기
data_svm<-train(C~.,
      data=train_data,method="lssvmRadial",
      trControl=trainControl("cv",number=5))
class_pred_svm<-data_svm %>% predict(test_data)
summary(class_pred_svm)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_svm
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/lssvm_sample_submission.csv",row.names=F)

#tuning svm -시간이 너무 오래걸림 ;이것저것해봤는데 디폴트가 낫거나 비슷
# best parameters:
#  gamma cost
#   0    0.1   
tune.svm(C~.,data=train_data, gamma =0:2, cost=0.1:1)
library(e1071)
data_svm<-svm(C~.,data=train_data)
class_pred_svm<-predict(data_svm,test_data,gamma=0,cost=0.1)
summary(class_pred_svm)


#c5
library(C50)
library(printr)
model <- C5.0(C ~., data=train_data)
class_pred_c5 <- predict(object=model, newdata=test_data, type="class")
summary(class_pred_c5)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_c5
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/c5_sample_submission2.csv",row.names=F)




library(caret)
c50 <- train(C ~. , data =train_data  , method="C5.0")
pred_c50<- predict(object=c50, newdata=test_data)
summary(pred_c50)


#c5
library(C50)
library(printr)
model20 <- C5.0(C ~., data=train_data,trials=20)
class_pred_c520 <- predict(object=model20, newdata=test_data, type="class")
summary(class_pred_c520)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_c5
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/c5_sample_submission2.csv",row.names=F)


library(C50)
library(printr)
m <- C5.0(C ~., data=train_data,winnow=T,trials=20)
class_pred_m <- predict(object=m, newdata=test_data, type="class")
summary(class_pred_m)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_m
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/m_sample_submission.csv",row.names=F)


#navieBayes
library(e1071)
nv<-naiveBayes(C ~., data=train_data)
class_pred_nv <- predict(object=nv, newdata=test_data, type="class")
summary(class_pred_nv )
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_nv
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/nv_sample_submission.csv",row.names=F)


#차이변수만들기
train_c <- train_data[,(3:61)]
train_b <- train_data[,c(62:120)]
diff_bc_train <- -(train_b - train_c)

etc_df <- train_data[,c(1,2)]
diff_new_train <- cbind(train_c,diff_bc_train, etc_df)

test_c <- test_data[,(2:60)]
test_b <- test_data[,(61:119)]
diff_bc_test <- -(test_b - test_c)
Y_b <- test_data[,1]

diff_new_test <- cbind(test_c, diff_bc_test, Y_b)
#차이변수c50
library(C50)
library(printr)
m1 <- C5.0(C ~., data=diff_new_train,control=C5.0Control(winnow = F),trials=20,rules=F)
class_pred_m1 <- predict(object=m1, newdata=diff_new_test, type="class")
table(class_pred_m1)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_m1
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/againm1_sample_submission.csv",row.names=F)

m1 <- C5.0(C ~., data=diff_new_train,control=C5.0Control(winnow = F),trials=20,rules=F)
class_pred_m1 <- predict(object=m1, newdata=diff_new_test, type="prob")
summary(class_pred_m1)
m2<-
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_m1
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/prob_sample_submission.csv",row.names=F)


#trials=10>m4 ; trials=15 >m3
m4 <- C5.0(C ~., data=diff_new_train,control=C5.0Control(winnow = F),trials=10,rules=F)
class_pred_m4 <- predict(object=m4, newdata=diff_new_test, type="class")
summary(class_pred_m4)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_m4
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/m4_sample_submission.csv",row.names=F)


#차이변수navieBayes
library(e1071)
nv<-naiveBayes(C ~., data=diff_new_train)
class_pred_nv <- predict(object=nv, newdata=diff_new_test, type="class")
summary(class_pred_nv )
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_nv
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/nv_sample_submission.csv",row.names=F)




#차이변수 c50 튜닝
library(caret)
c50 <- train(C ~. , data =diff_new_train  , method="C5.0")
pred_c50<- predict(object=c50, newdata=diff_new_test)
summary(pred_c50)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-pred_c50
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/tunec50_sample_submission.csv",row.names=F)


#이전등급 더미화

library(fastDummies)
dummy_df_train <- dummy_cols(diff_new_train, select_columns = 'Y_b')
head(dummy_df_train)
dummy_df_train$Y_b <- NULL

diff_new_train$Y_b<-as.factor(diff_new_train$Y_b) #비교용 그냥
diff_new_test$Y_b <- factor(diff_new_test$Y_b, levels = c('A','A-','A+',
                                        'AA','AA-','AA+',
                                        'AAA','B','B-','B+',
                                        'BB','BB-','BB+','BBB',
                                        'BBB-','BBB+','C','CC','CCC',
                                        'CCC-','D'))

dummy_df_test <- dummy_cols(diff_new_test, select_columns = 'Y_b')
dummy_df_test$Y_b <- NULL

#다시 c5.0
library(C50)
library(printr)
m2 <- C5.0(C ~., data=dummy_df_train,winnow=T,trials=20)
class_pred_m2 <- predict(object=m2, newdata=dummy_df_test, type="class")
summary(class_pred_m2)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_m2
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/m2_sample_submission.csv",row.names=F)

#변수스케일링
diff_new_train[,-c(119,120)]<-scale(diff_new_train[,-c(119,120)],center=T,scale=T)
diff_new_test[,-119]<-scale(diff_new_test[,-119],center=T,scale=T)
m2 <- C5.0(C ~., data=diff_new_train,control=C5.0Control(winnow = F),trials=20,rules=F)
class_pred_m2 <- predict(object=m2, newdata=diff_new_test, type="prob")
a<-(class_pred_m1+class_pred_m2)/2

newpred<-c()
for (i in 1:nrow(a)){
  newpred<- c(newpred,names(which.max(a[i,])))
}
newpred<-data.frame(as.numeric(newpred))
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-newpred
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/stk_sample_submission.csv",row.names=F)


#가장 좋았던 c5모델에서 중요하지않은 변수제거하고 다시해보기

m1 <- C5.0(C ~., data=diff_new_train,control=C5.0Control(winnow = F),trials=20,rules=F)
class_pred_m1 <- predict(object=m1, newdata=diff_new_test, type="class")
summary(class_pred_m1)
diff_delete_train<-subset(diff_new_train,select=-c(X27,BX27))
diff_delete_test<-subset(diff_new_test,select=-c(X27,BX27))
newm1 <- C5.0(C ~., data=diff_delete_train,control=C5.0Control(winnow = F),trials=20,rules=F)
class_pred_newm1 <- predict(object=newm1, newdata=diff_delete_test, type="class")
summary(class_pred_newm1)
sample_submission<-read.csv(file="sample_submission.csv")
sample_submission[2]<-class_pred_newm1
write.csv(sample_submission,"C:/Users/user/Desktop/통계분석실습/기말프로젝트/newm1_sample_submission.csv",row.names=F)








#stacking
