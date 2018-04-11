library(lubridate)
library(xlsx)
library(dplyr)
library(doBy)

## 데이터 출처 https://www.kaggle.com/regivm/retailtransactiondata

### 1. 데이터 불러오기
Retail_Data_R<-read.csv("Retail_Data_Response.csv")
Retail_Data_T<-read.csv("Retail_Data_Transactions.csv")

### 2. 데이터 구조 확인
c(str(Retail_Data_R),str(Retail_Data_T), dim(Retail_Data_R), dim(Retail_Data_T))

### 3. 날짜 데이터 형식 변경하기: factor를 date형식으로
Retail_Data_T$trans_date<-dmy(Retail_Data_T$trans_date)

### 4.고객수 확인
c(length(Retail_Data_R$customer_id),length(unique(Retail_Data_T$customer_id)))

###5.구매기간 확인
c(max(Retail_Data_T$trans_date),min(Retail_Data_T$trans_date))


###5.고객별 구매금액의 합계, 평균, 중위수, 구매횟수(*데이터에 NA 값이 있을 때는 na.rm=T 사용, length에는 적용 안됨)  
Cust_Sum<-summaryBy(tran_amount~customer_id, data = Retail_Data_T, 
                    FUN = c(sum, mean, median, length))
head(Cust_Sum)

###6.컬럼 이름 변경: 컬럼 이름 축약해서 보기 편하게 하기 위해
Cust_Sum<-rename(Cust_Sum, sum = tran_amount.sum, mean = tran_amount.mean,
                             median= tran_amount.median, length=tran_amount.length)

###7.1 데이터 분포 보기-sum
hist(Cust_Sum$sum)

###7.2 데이터분포 보기-mean
hist(Cust_Sum$mean)

###7.3 데이터 분포보기- median
hist(Cust_Sum$median)

###7.4 데이터 분포보기- length
hist(Cust_Sum$length)

###8. 구매 데이터를 사용해 고객의 구매 주기를 계산
###8.1. 구매데이터 고객별, 날짜별로 정렬
Retail_Data_T<-Retail_Data_T[order(Retail_Data_T$customer_id, Retail_Data_T$trans_date),]
head(Retail_Data_T)

###8.2. 각 고객의 구매일간 주기를 계산(첫번째 구매일과 두 번째 구매일간의 차이)
Retail_Data_T$trans_date_lag<-shift(Retail_Data_T$trans_date, n=1, fill=NA, type = "lag")
Retail_Data_T$trans_date_diff<-Retail_Data_T$trans_date - Retail_Data_T$trans_date_lag
Retail_Data_T$trans_date_final<-ifelse(Retail_Data_T$customer_id == shift(Retail_Data_T$customer_id, n=1, fill = NA, type = "lag"),
       Retail_Data_T$trans_date-shift(Retail_Data_T$trans_date, n=1, fill=NA, type = "lag"),
       NA)
head(Retail_Data_T)

###9. 고객별 구매주기 평균 계산
Cust_Pattern<-summaryBy(trans_date_final~customer_id, data = Retail_Data_T, FUN= mean, na.rm =TRUE)
head(Cust_Pattern)

###10.Retail_Data_R 데이터 병합
Cust_Master<-merge((merge(Cust_Sum,Cust_Pattern,all.x=T,by="customer_id")),
                   Retail_Data_R,all=T,by="customer_id")

plot(cust_master[,2:length(Cust_Master)])


#RFM기분에 따른 고객 분류

###11. 구매주기기준 고객 분류
q1<-quantile(Cust_Master$trans_date_final.mean)
Cust_Master$Recency<-ifelse(Cust_Master$trans_date_final.mean<q1[2],"H",
                              ifelse(Cust_Master$trans_date_final.mean<q1[4],"M","L"))
head(Cust_Master)

###12.구매횟수에 따른 고객 분류_H,M,L
q2<-quantile(Cust_Master$length)
Cust_Master$Frequency<-ifelse(Cust_Master$length<q2[2],"L",
                              ifelse(Cust_Master$length<q2[4],"M","H"))
head(Cust_Master)


###13. 구매 금액 기준 고객 분류_H, M, L: 4분위 기준으로 분류
q3<-quantile(Cust_Master$sum)
q3
Cust_Master$Monetary<-ifelse(Cust_Master$sum<q3[2],"L",
                                          ifelse(Cust_Master$sum<q3[4],"M","H"))
dim(Cust_Master)

names(Cust_Master)
###14.RFM 기준으로 각 값의 평균과 횟수 구하기
Result<-summaryBy(.~Recency+Frequency+Monetary, data=Cust_Master, FUN=c(mean,length))
Result
head(Result)

###15.분석에 필요한 컬럼만 선택하여 컬럼명과 순서 변경: 결과 인식을 용이하게 하기 위해
names(Result)
Result_F<-select(Result,-(contains("median")), -(starts_with("response")),-(starts_with("mean")))
Result_F<-rename(Result_F, Monetary.mean=sum.mean, Frequency.mean=length.mean, 
                 Recency.mean=trans_date_final.mean.mean, Monetary.length=sum.length,
                 Frequency.length=length.length, Recency.length=trans_date_final.mean.length)

Result_F<-Result_F[,c(1,2,3,6,5,4,9,8,7)]
head(Result_F)

###16. 엑셀 파일로 추출하여 값 확인하기
write.xlsx(Result_F, "RFM_Result.xlsx")

