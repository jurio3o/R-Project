# envir setting
rm(list=ls(all=TRUE))
setwd("C:/Users/user/Desktop/통계분석실습")

# import packages
library(dplyr)
library(MASS)
library(ggplot2)

load("dm1.RData")
starttime<-Sys.time()
# train period
train_period <- 60 # 5 years    

# t_cur
t_cur_min = 1+12*15
t_cur_max1 = ((max(dm1$yy)-train_period/12)-min(dm1$yy))*12+1 
t_cur_vec1<-seq(t_cur_min,t_cur_max1,by=12)

# Performance
Portfolio_Avg_vw_1<-c()

# analysis
for (t_cur in t_cur_vec1){ 
  cat("t_cur = ", t_cur, "\n")
  
  t_start <- t_cur
  t_end <- (t_cur-1) + train_period
  test_start <- t_end + 1
  test_end <- t_end + 12# one year 
  
  # training data
  dm1_train <- dm1[(dm1$tp>=t_start)&(dm1$tp<=t_end),]
  
  # test data
  dm1_test<-dm1[(dm1$tp>=test_start)&(dm1$tp<=test_end),]

  #knn일때 predict 
  library(FNN)
  pred<- knn.reg(as.matrix(subset(dm1_train,select=-c(code,tp,yy,mm))), test = as.matrix(subset(dm1_test,select=-c(code,tp,yy,mm))), dm1_train$ret, k =14)$pred 

  
  ret <- dm1_test$ret
  
  
  # Performance
  temp_Avg_vw<-c()
  
  # value vector
  val<-exp(dm1_test$mv1m)
  
  for (month in 1:12)
  {
    pred_m<-pred[dm1_test$mm==month]
    ret_m<-ret[dm1_test$mm==month]
    
    port_m <- data.frame(pred_m,ret_m) %>% mutate(decile=ntile(pred_m, 10),value=val[dm1_test$mm==month])
    
    summa_m_vw <- group_by(port_m, decile) %>% 
      summarise(Avg=weighted.mean(ret_m,value)) %>%  as.data.frame
    
    temp_Avg_vw<-cbind(temp_Avg_vw,summa_m_vw[,2])
  }
  
  Portfolio_Avg_vw_1 <- cbind(Portfolio_Avg_vw_1,temp_Avg_vw)
  
}

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

#10개의 그룹으로 쪼갰을 때 (k=14일때)  sr=0.92
#20개의 그룹으로 쪼갰을 때 (k=23일때)  sr=0.98
end<-Sys.time()
end-starttime #총 실행 시간 5.494469초