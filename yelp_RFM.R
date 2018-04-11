# 데이터 출처 https://www.kaggle.com/yelp-dataset/yelp-dataset
#https://goo.gl/s1ZWo2

#필요 라이브러리
require(data.table) #data read
require(readr) ; # data read
library(tidyverse) #  data manipulation and graphs
library(stringr) #  string manipulation
library(lubridate) #  date manipulation
library('wordcloud') #  wordcloud
library(tidytext) # tidy implementation of NLP methods
library(DT)       # table format display of data
library(leaflet) # maps
library(igraph) #  graphs
library(ggraph) #  graphs
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(textcat)
library(doBy)
library(reshape)

###Yelp’s mission is to connect people with great local businesses

### Analysis Purpose: what factors influence the best restaurant selected?
### 체크인수가 레스토랑 평점과의 관계, 영업일과 평점과의 관계, 카테고리와 평점과의 관계, 팁과 평점과의 관계, 리뷰와 평점과의 관계
### influentiable factors
###0. initialize
rm(list = ls())
options(digits=2,scipen =100)
gc()

#1. 데이터 불러오기: fread 함수사용 대용량 데이터 불러오기
User<-fread("yelp_user.csv")
Biz<-fread("yelp_business.csv")
Biz_Att<-fread("yelp_business_attributes.csv")
Biz_Hour<-data.table(read_csv("yelp_business_hours.csv"))
Check_in<-fread("yelp_checkin.csv")
Tip<-fread("yelp_tip.csv")
Review<-data.table(read_csv("yelp_review.csv"))


###############################User Data########################################################################
glimpse(User)#유저수 1,326,100명, 22개 Column으로 되어 있고.  
#review_count, yelping_since, useful,funny, cool, fans, elite, average_stars선택하고 활동년도와 elete선정 년도 추가 
User$Act_Duration<-round(as.numeric(Sys.Date()-as.Date(User_R$yelping_since))/365)#활동년도 추가
User$elite_Act<-ifelse(str_count(User$elite,",")>0, str_count(User$elite,",")+1,0)#Yelp Elite 선정 년도 추가

## 관심 변수만으로 User_R 테이블 생성
User_R<-
  User %>% 
  select(user_id, review_count,yelping_since, useful,funny,fans, elite, elite_Act, Act_Duration,average_stars)#관심 값 추출

plot(User_sum)
###Character 변수형태를 Numeric으로  변환 
glimpse(User_R)
User_R$review_count<-as.numeric(User_R$review_count)
User_R$useful<-as.numeric(User_R$useful)
User_R$funny<-as.numeric(User_R$funny)
User_R$fans<-as.numeric(User_R$fans)
User_R$average_stars<-as.numeric(User_R$average_stars)


quantile(User_R$Act_Duration)
mean(User_R$Act_Duration) # 유저들의 평균 활동 기간은 4.77년

User_sum<-User_R %>% 
  group_by(Act_Duration) %>% 
  summarise(review_sum = sum(review_count), useful_sum=sum(useful), funny_sum=sum(funny), fans_sum=sum(fans))

ggplot(data = User_sum)+
  geom_bar(aes(x=Act_Duration, y=review_sum), stat = "identity") 

ggplot(data = User_sum)+
  geom_bar(aes(x=Act_Duration, y=useful_sum), stat = "identity") 

ggplot(data = User_sum)+
  geom_bar(aes(x=Act_Duration, y=funny_sum), stat = "identity") 

ggplot(data = User_sum)+
  geom_bar(aes(x=Act_Duration, y=fans_sum), stat = "identity") 

#######################################################################################################3
User_sum_E<-User_R %>% 
  group_by(elite_Act) %>% 
  summarise(review_sum = sum(review_count), useful_sum=sum(useful), funny_sum=sum(funny), fans_sum=sum(fans))

ggplot(data = User_sum_E)+
  geom_bar(aes(x=elite_Act, y=review_sum), stat = "identity") 

ggplot(data = User_sum_E)+
  geom_bar(aes(x=elite_Act, y=useful_sum), stat = "identity") 

ggplot(data = User_sum_E)+
  geom_bar(aes(x=elite_Act, y=funny_sum), stat = "identity") 
  
ggplot(data = User_sum_E)+
  geom_bar(aes(x=elite_Act, y=fans_sum), stat = "identity") 

#활동기간이 5~10년 사이의 유저의 활동 지수가 높게 나타남 (yelp전성기로 보임)




nchar(User$elite[20:3])
ncol(User$elite)

#################################Business Data##############################################################################
###Biz 
glimpse(Biz)##총사업자수 174,567

###비즈니스 카테고리 확인 
Biz_Category<-str_split(Biz$categories,";")#사업자의 사업 영역 추출
Biz_Category<-as.data.frame(unlist(Biz_Category)) #데이터 프레임으로 변환
colnames(Biz_Category)=c("Name")  #열 이름 변경, 총 66,8027개의 사업명 존재

####상위 20개 카테고리를 확인 이 중 7개의 사업영역 선택
Biz_Category %>% 
  group_by(Name) %>%
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% 
  head(20)  #Restaurants,Food(3),Bars(10),Sandwiches(14),Fast Food(15), American (Traditional)(17), 
            #Pizza (18),Coffee & Tea (19) 

Restaurant<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Restaurants") %>% 
  arrange(desc(review_count,stars))

Food<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Food") %>% 
  arrange(desc(review_count,stars))

Bars<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Bars") %>% 
  arrange(desc(review_count,stars)) 

Sandwich<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Sandwiches") %>% 
  arrange(desc(review_count,stars))

FastFood<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Fast Food") %>% 
  arrange(desc(review_count,stars)) 

American<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "American") %>% 
  arrange(desc(review_count,stars)) 

Pizza<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Pizza") %>% 
  arrange(desc(review_count,stars)) 

Coffee_Tea<-
  Biz %>% 
  select(business_id, review_count,stars, categories) %>%
  filter(categories %like% "Coffee & Tea") %>% 
  arrange(desc(review_count,stars))


#Tip: REVIEW 숫자가 일정숫자 이상인 사업자의 평점만 보기   
########################################Check_in Data#################################################################

### 요일별  체크인 수
glimpse(Check_in)
Check_in_R<-as.data.table(dcast(data = data.frame(Check_in), business_id~weekday, value.var = "checkins", sum, margins=T))
names(Check_in_R)[9]<-c("Total")#컬럼 명 바꾸기
Check_in_R<-arrange(Check_in_R, desc(Check_in_R$Total))

#######################################Biz_Hours#####################################################################
##영업일수 확인
glimpse(Biz_Hour)
Biz_Hour[,.(.N),by=c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")][order(-N)]
Biz_Hour_Bi<-data.frame(apply(Biz_Hour[,2:length(Biz_Hour)],2,function(x) str_count(x,"-")))
names(Biz_Hour_Bi)<-paste(names(Biz_Hour_Bi),1:length(Biz_Hour_Bi),sep="_")
Biz_Hour1<-cbind(Biz_Hour,Biz_Hour_Bi)
Biz_Hour1$open_days<-rowSums(Biz_Hour1[,9:15])
hist(Biz_Hour1$open_days)
100*table(Biz_Hour1$open_days)/sum(table(Biz_Hour1$open_days))

############################################Biz Data master data set##################################################
names(Biz)
names(Biz_Hour1)
names(Check_in_R)

setkey(Biz,"business_id")
setkey(Biz_Hour1,"business_id")
setkey(Check_in_R,"business_id")
Master<-Biz[Biz_Hour1[Check_in_R]]
names(Master)
Master_S<-Master[,c(1:2,10:11,28,36)]


######################################Review Data###################################################################
glimpse(Review)
#review_id, user_id 차이
Review_Master<-Review[,c(3,6)]


Star5_Review<-
Review %>% 
  filter(stars == 5) %>% 
  group_by(business_id) %>% 
  summarise(Count=n()) %>% 
  arrange(desc(Count)) %>% 
  ungroup() %>% 
  mutate(BusinessID=reorder(business_id,Count)) %>% 
  head(20)

Star5_Review1<-inner_join(Star5_Review, Biz)
Star5_Review1 %>% 
  mutate(name=reorder(name, Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = "orange")+
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold')+
  labs(x = 'Name of the Business', 
       y = 'Count', 
       title = 'Name of the Business and Count')+
  coord_flip()+
  theme_bw()


names(Review)  
Review_Text<-Review[business_id=="4JNXUYY8wbaaDmk3BPzlWw",list(business_id,text)]

Review_Text_1<-
  Review%>% 
  filter(business_id=="4JNXUYY8wbaaDmk3BPzlWw") %>% 
  select(business_id, text)


Review_Text %>%
  unnest_tokens(word, text) %>% 
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(30) %>%
  with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))


createWordCloud <- function(wordcloud)
{
  Review_Text %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(30) %>%
    
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}

createWordCloud(Review %>%
                  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))

positiveWordsBarGraph <- function(positive) {
  contributions <- Review %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(Review %>%
                        filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))


calculate_sentiment <- function(Sentiment_Cal)
 {
     sentiment_lines  =  Review %>%
     filter(textcat(text) == "english") %>%  # considering only English text
     unnest_tokens(word, text) %>%
     inner_join(get_sentiments("afinn"), by = "word") %>%
     group_by(user_id) %>%
     summarize(sentiment = mean(score),words = n()) %>%
     ungroup() %>%
     filter(words >= 5) 
       
     return(sentiment_lines)
       
   }  

sentiment_lines = calculate_sentiment(mon_ami_gabi_reviews)

mon_ami_gabi_reviews = Review %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw")

head(sentiment_lines)

names(Review)




setkey(Biz,"business_id")
setkey(Review_Master,"business_id")

Master<-Biz[Review_Master]
names(Master)