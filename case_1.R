setwd("C:/Users/kanis/OneDrive/Documents/sem3/Applied statistics and R programming/R/Package")
data<-read.csv(file.choose())

#How many calories does the average McDonald's value meal contain?
a<-mean(data$Calories)
print("Average calories of a mcdonald meal is ")
print(a)

#How much do beverages,like soda or coffee, contribute to the overall caloric intake?
library(dplyr)

grouped<-data%>%
  group_by(Category)%>%
  summarise(Total_Calorie_Content=sum(Calories))
print(grouped)
l<-0
a1<-c(grouped$Total_Calorie_Content)
a1
c=1
for(i in grouped$Category)
{
  if(i=="Coffee & Tea")
  {
    print(a1[c])
    l=(a1[c])/sum(data$Calories)
    print(l)
  }
  c=c+1
}
print(l*100)

#Does ordered grilled chicken instead of crispy increase a sandwich's nutritional value?
library(stringr)

count<-1
#Gr-grilled
Gr<-0
#Cr-Crispy
Cr<-0
for (i in data$Category)
{
  if (i=="Chicken & Fish" && str_detect(data$Item[count], "Sandwich"))
  {
    if (str_detect(data$Item[count], "Grilled"))
    {
      Gr<-Gr+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }
    if (str_detect(data$Item[count], "Crispy"))
    {
      
      Cr<-Cr+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }
  }
  count<-count+1
}
print(Gr)
print(Cr)
if (Gr>Cr)
{
  print("Grilled is nutritious")
}else{
  print("Crispy is nutritious")
}

#What about ordering egg whites instead of whole eggs?
count<-1
white<-0
egg<-0
for (i in data$Category)
{
  if (i=="Breakfast" && str_detect(data$Item[count], "Egg"))
  {
    if (str_detect(data$Item[count], "White"))
    {
      white<-white+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }else{
      egg<-egg+data$Calories[count]+data$Total.Fat[count]+data$Sodium[count]+data$Carbohydrates[count]+data$Dietary.Fiber[count]+data$Protein[count]
    }
  }
  count<-count+1
}
print(white)
print(egg)
if (white>egg)
{
  print("Egg White is nutritionous")
}else{
  print("Whole eggs are nutritionous")
}

#What is the least number of items could you order from the menu to meet one day's nutritional requirements?
#5)
