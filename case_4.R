setwd("C:/Users/kanis/OneDrive/Documents/sem3/Applied statistics and R programming/R/Package")
data<-read.csv(file.choose())
data

library(dplyr)
#What are the least and most expensive cities for pizza?
groupel<-data%>%
  group_by(city)%>%
  filter(!any(is.na(priceRangeMax)))%>%
  filter(!any(is.na(priceRangeMin)))%>%
  summarise(Price_r=(sum(priceRangeMax) - sum(priceRangeMin)))
print(groupel)
View(groupel)
mini<-groupel$Price_r[1]
maxi<-groupel$Price_r[1]
index=1
ind=1
count=2

a1<-c(groupel$Price_r)
a1
for(i in groupel$city)
{
  if(a1[count]<mini)
  {
    mini=a1[count]
    index=count
  }
  count=count+1
}
count=2
for(i in groupel)
{
  if(a1[count]>maxi)
  {
    maxi=a1[count]
    ind=count
  }
  count=count+1
}
mini
maxi
cat("The least expensive city for pizza:",groupel$city[index])
cat("The most expensive city for pizza:",groupel$city[ind])

#What is the number of restaurants serving pizza per capita (100,000 residents) across the U.S.?
groupe<-data%>%
  group_by(name)%>%
  filter(!any(is.na(menus.currency)))%>%
  summarise(Menu=(sum(menus.amountMax)-sum(menus.amountMin)))
print(groupe)
View(groupe)
cat("The number of restaurants serving pizza per capita:",length(groupe$Menu))

#What is the median price of a large plain pizza across the U.S.?

count=1
count1=0
library(stringr)
a2<-c(groupe$name)
a2
l=0
for(i in groupe$name)
{
  if(str_detect(a2[count],"Pizza"))
  {
    l=l+a1[count]
    count1=count1+1
  }
  count=count+1
}
if(count1%%2==0)
{
  count1=count1/2
  val=a1[count1]
}else{
  count1=(count1+1)/2
  val=(a1[count1]+a1[count1-1])/2
}
cat("Median is ",val)

#Which cities have the most restaurants serving pizza per capita (100,000 residents)?
groupei<-data%>%
  group_by(city)%>%
  filter(!any(is.na(menus.amountMax)))%>%
  filter(!any(is.na(menus.amountMin)))%>%
  summarise(vall=(sum(menus.amountMax)-sum(menus.amountMin)))
print(groupei)
View(groupei)
a3<-groupei$city
count=0
cat("The cities are:")
for(i in groupei$vall)
{
  cat(a3[count])
  count=count+1
  cat("\n")
}
