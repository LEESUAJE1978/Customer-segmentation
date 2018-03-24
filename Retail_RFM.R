library(lubridate)
library(xlsx)
library(dplyr)
library(doBy)

# 데이터 출ㅊhttps://www.kaggle.com/regivm/retailtransactiondata
Retail_Data_R<-read.csv("Retail_Data_Response.csv")
Retail_Data_T<-read.csv("Retail_Data_Transactions.csv")
c(str(Retail_Data_R),str(Retail_Data_T), dim(Retail_Data_R), dim(Retail_Data_T))

#날짜 데이터 형식 변경하기
Retail_Data_T$trans_date<-dmy(Retail_Data_T$trans_date)

#고객수 확인
length(Retail_Data_R$customer_id)
length(unique(Retail_Data_T$customer_id))

#구매기간 확인
max(Retail_Data_T$trans_date)
min(Retail_Data_T$trans_date)


#고객별 구매금액의 합계, 평균, 중위수, 구매횟수
#데이터에 NA 값이 있을 때는 na.rm=T 사용,  
summary(is.na(Retail_Data_T$tran_amount))
cust_sum<-summaryBy(tran_amount~customer_id, data = Retail_Data_T, FUN = c(sum, mean, median, length))

#계산 값의 분포 보기
Retail_Data_Sum<-summaryBy(tran_amount~customer_id, data = Retail_Data_T, FUN = c(sum, mean, median, length))
hist(Retail_Data_Sum$tran_amount.sum)
hist(Retail_Data_Sum$tran_amount.mean)
hist(Retail_Data_Sum$tran_amount.median)
hist(Retail_Data_Sum$tran_amount.length)

#구매 데이터를 사용해 고객의 구매 주기를 계산
#구매데이터 고객별, 날짜별로 정렬
Retail_Data_T<-Retail_Data_T[order(Retail_Data_T$customer_id, Retail_Data_T$trans_date),]
head(Retail_Data_T)

#간격계산: A와 B 고객 간에 값을
Retail_Data_T$trans_date_lag<-shift(Retail_Data_T$trans_date, n=1, fill=NA, type = "lag")
Retail_Data_T$trans_date_diff<-Retail_Data_T$trans_date - Retail_Data_T$trans_date_lag
Retail_Data_T$trans_date_final<-ifelse(Retail_Data_T$customer_id == shift(Retail_Data_T$customer_id, n=1, fill = NA, type = "lag"),
       Retail_Data_T$trans_date-shift(Retail_Data_T$trans_date, n=1, fill=NA, type = "lag"),
       NA)

#구매간격의 평균을 고객별로 계산
cust_pattern<-summaryBy(trans_date_final~customer_id, data = Retail_Data_T, FUN= mean, na.rm =TRUE)

Retail_Data_T_shift<-shift(Retail_Data_T, n=1, fill=NA, type ="lag")
Retail_Data_T_shift
str(Retail_Data_T_shift)

#Retail_Data_R 데이터 병합
cust_master<-merge((merge(cust_sum,cust_pattern,all.x=T,by="customer_id")),
                   Retail_Data_R,all=T,by="customer_id")

plot(cust_master[,2:length(cust_master)])

#
head(cust_master)
q1<-quantile(cust_master$tran_amount.sum)
q1[1]
q1[2]
q1[4]
cust_master$tran_amount.sum_group<-ifelse(cust_master$tran_amount.sum<q1[2],"L",
                                          ifelse(cust_master$tran_amount.sum<q1[4],"M","H"))
