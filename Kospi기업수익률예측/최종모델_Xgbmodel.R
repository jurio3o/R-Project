#2021 통계분석실습 중간 프로젝트          
#   
#팀 이름 : 1조
#
#팀원 이름: 이정민, 장은조, 홍주리
#
#평균 Sharpe ratio : 14.68 (seed 설정으로 1번만 돌림)
#
#평균 코드 실행 시간 : 1.091461 mins (튜닝 시간 9.719883 mins)      
#....................................................


########10번 돌려서 얻은 평균 Sharpe ratio와 평균 실행 시간을 반환하는 코드

rm(list=ls(all=TRUE))
setwd("D:/finance/final/data/")
load("dm1.RData")

# import packages
library(dplyr)
library(MASS)
library(xgboost)


# SR_all <- c() # Sharpe ratio 저장 공간
# time<-c() # 실행 시간 저장 공간

# for ( r in 1:10) {
Start <-Sys.time() #코드 실행 시작 시간

  
#........아래부터 sample data analysis입니다. 개인 코드로 변경해주세요...............
# train period
train_period <- 60 # 5 years    
  
# t_cur
t_cur_min = 1+12*15
t_cur_max1 = ((max(dm1$yy)-train_period/12)-min(dm1$yy))*12+1 
t_cur_vec1<-seq(t_cur_min,t_cur_max1,by=12)


# create hyperparameter grid for one train data set

tune_start <- Sys.time()

t_cur <- t_cur_vec1[1]

t_start <- t_cur  
t_end <- (t_cur-1) + train_period   
test_start <- t_end + 1  
test_end <- t_end + 12 

# training data
dm1_train<- dm1[(dm1$tp>=t_start)&(dm1$tp<=t_end),-c(1,2,3,78)]


hyper_grid <- expand.grid(max_depth = seq(3, 6, 1), eta = c(0.01,0.05,0.1),
                          booster = c('gbtree','gblinear'),
                          colsample_bytree = c(0.5, 0.7,1))  

hyper_grid$booster <- as.character(hyper_grid$booster)

xgb_test_rmse <- NULL


for (j in 1:nrow(hyper_grid)) {
  set.seed(123)
  m_xgb_untuned <- xgb.cv(
    booster = hyper_grid$booster[j],
    data = as.matrix(dm1_train[,-1]),
    label =  as.matrix(dm1_train[, 1]),
    nrounds = 30,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    nfold = 5,
    max_depth = hyper_grid$max_depth[j],
    eta = hyper_grid$eta[j],
    colsample_bytree = hyper_grid$colsample_bytree[j]
  )
  
  xgb_test_rmse[j] <- m_xgb_untuned$evaluation_log$test_rmse_mean[m_xgb_untuned$best_iteration]
  
  cat(j, "\n")
}    

tune_end <- Sys.time()

best <- hyper_grid[which.min(xgb_test_rmse), ]



##### KOSPI ANALYSIS ##### 
pred_1 <- c()
ref_1 <- c()

# Performance  
Portfolio_Pred_vw_1<-c()
Portfolio_Avg_vw_1<-c()

model_start <- Sys.time()

# analysis  ## 예제는 년도별로 한꺼번에 예측함, 월별로 따로 안하고 
for (t_cur in t_cur_vec1){ 
  
  ### t_cur가 1년 단위로 옮겨짐 181, 193, 205, 217,...,313
  cat("t_cur = ", t_cur, "\n")
  
  t_start <- t_cur    ### train data의 시작 tp
  t_end <- (t_cur-1) + train_period   ### train data의 마지막 tp (60개월)
  test_start <- t_end + 1  ###  test data의 시작 tp
  test_end <- t_end + 12 ### test data의 마지막 tp (one year)
  ### 만약, one month이면 test data의 start,end tp는 똑같을 것 
  
  # training data
  dm1_train<- dm1[(dm1$tp>=t_start)&(dm1$tp<=t_end),]
  
  # test data
  dm1_test <-dm1[(dm1$tp>=test_start)&(dm1$tp<=test_end),]
  
  #### fit a given learner & predict #######
  
  set.seed(123)
  xgb_model <- xgboost(
    data = as.matrix(dm1_train[,-c(1,2,3,78)]),
    label =  as.matrix(dm1_train[, 4]),
    nrounds = 50,
    booster = best$booster,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = best$max_depth,
    eta = best$eta,
    colsample_bytree = best$colsample_bytree
  )
  
  pred <- predict(xgb_model, as.matrix(dm1_test[,-c(1,2,3,78)]))
  
  ### 학습 모델로 test data를 예측, 1년치 월별 수익률 예측값이 있음 
  
  ### 예측 수익률
  ret <- dm1_test$ret
  ### 실제 수익률
  
  ref_1 <- c(ref_1,ret)
  pred_1 <- c(pred_1,pred)
  
  
  # Performance
  temp_Pred_vw <- c()
  temp_Avg_vw<-c()
  
  # value vector
  val<-exp(dm1_test$mv1m)
  ### mv1m변수의 exp취하면 해당 시점의 각 기업의 가치
  
  for (month in 1:12)  # 1년치 통채로 예측, 그래도 수익률은 월별 기준으로 해야하니 한달씩 정리하기 
  {
    pred_m<-pred[dm1_test$mm==month]
    ret_m<-ret[dm1_test$mm==month]
    
    port_m <- data.frame(pred_m,ret_m) %>% mutate(decile=ntile(pred_m, 10),value=val[dm1_test$mm==month])
    ### 해당 월의 예측수익률과 실제수익률을 같이 넣고
    ### 예측 수익률 기반으로 10개의 그룹으로 쪼개고, decile변수를 만들어서, 수익률이 높을수록 10에 가까움
    
    summa_m_vw <- group_by(port_m, decile) %>% 
      summarise(Pred = weighted.mean(pred_m,value),Avg=weighted.mean(ret_m,value)) %>%  as.data.frame
    ### 변수 decile, pred, avg
    ### 각 그룹별 예측, 실제 평균 수익률 
    ### 그리고 전략을 짜야지. 1과 10을 서로 바꾼다던지 
    ### 예측수익률은 필요없고, 실제 수익률을 계속 누적해서 저장할 것 
    
    temp_Avg_vw<-cbind(temp_Avg_vw,summa_m_vw[,3])
  }  # 만약 예측을 월별로 하고 싶다면, 수정필요
  
  Portfolio_Avg_vw_1 <- cbind(Portfolio_Avg_vw_1,temp_Avg_vw)  # 월별 수익률 저장 
  
}

model_end <- Sys.time()
  
# performance of value weighted portfolios
Avg_vw<-round(mean(apply(Portfolio_Avg_vw_1[c(1,10),]*100,2,diff)),2)
Std_vw<-round(sd(apply(Portfolio_Avg_vw_1[c(1,10),]*100,2,diff)),2)
SR_vw<-round((12*Avg_vw)/(Std_vw*sqrt(12)),2)
  
profit<-apply(Portfolio_Avg_vw_1[c(1,10),]*100,2,diff)
profit_ts<-ts(profit, frequency = 12, start = c(2007, 1)) 
plot(profit_ts)
abline(h=mean(profit),col=2,lty=2)

result<-data.frame(Avg=Avg_vw,Std=Std_vw,SR=SR_vw)
result
end <- Sys.time()

end - start # 처음부터 끝까지 걸린 시간 
tune_end - tune_start  # 튜닝하는게 걸린 시간 
model_end - model_start  # 모델 돌리는데 걸린 시간 