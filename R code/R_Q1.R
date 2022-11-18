---
title: "coursework_Question1_R"
output: pdf_document
---

Question1. When is the best time of day, day of week, and time of year to minimise delays?

```{r}
library(knitr)
```

Set working directory
```{r, setup, include=FALSE}
knitr::opts_knit$set(root.dir = 'C:/Users/juhyu/Desktop/Programming ST2195')
```

```{r}
# checking working directory
getwd()
```

Read dataset 
```{r}
df2003<-read.csv("2003.csv.bz2")
df2004<-read.csv("2004.csv.bz2")
df2005<-read.csv("2005.csv.bz2")
df1<-rbind(df2003,df2004,df2005)
```

Extract even rows & selecting rows with flights not cancelled
```{r}
row_odd<-seq_len(nrow(df1)) %% 2
df1<-df1[row_odd==0,]
df1<-subset(df1,df1$Cancelled==0)
```

Getting the column names
```{r}
names(df1)
```


1) best time of day 
Extracting hours
```{r}
class(df1$DepTime)
df1$DepHour<-floor(df1$DepTime/100)
df1<-subset(df1,df1$DepHour<=24)
class(df1$DepHour)
```

assigning timing (morn,afternoon,night)
```{r}
df1$DepHour<-as.numeric(df1$DepHour)
timing<-list()
i=1
for (x in df1$DepHour){
  x<-as.numeric(x)
  if (x>=4 && x<12){
    timing[i]<-'morning'
  }
  else if (x>24){
    timing[i]<-'morning'
  }
  else if (x>=12 && x<20){
    timing[i]<-'afternoon'
  }
  else{
    timing[i]<-'night'
  }
  i<-i+1
}
```

Add timing column to df1
```{r}
#add timing column into df1
timing<- data.frame(matrix(unlist(timing), nrow=length(timing), byrow=TRUE))
df1$timing<-timing
```

Plotting bar chart for distribution of timing
```{r}
names(df1)
depdelay1<-list()
arrdelay1<-list()

for (x in sort(unique(unlist(df1$timing)))) {
  mean_depdelay1<-colMeans(df1[df1$timing==x,]['DepDelay'],na.rm=TRUE)
  depdelay1<-append(depdelay1,mean_depdelay1)
  mean_arrdelay1<-colMeans(df1[df1$timing==x,]['ArrDelay'],na.rm=TRUE)
  arrdelay1<-append(arrdelay1,mean_arrdelay1)
}
head(depdelay1,5) #checking
head(arrdelay1,5) #checking
length(depdelay1)

deparr_delay1<-rbind(depdelay1,arrdelay1)
colnames(deparr_delay1)<-c('afternoon','morning','night')
deparr_delay1<-subset(deparr_delay1,select=c('afternoon','morning','night'))

Timing<-unique(unlist(df1$timing))
barplot(deparr_delay1,main='Distribution of delay over a day',xlab='Timing',ylab='Delay(min)',col=c('#1B4D80', '#F46664'))

legend(x="top",inset=0.05,cex=0.6,
       legend = c('departure delay','arrival delay'),
       col=1:2,
       pch=16,
       border = "black") # Color of the border of the squares
```


2) Best day of week 
```{r}
days<-sort(unique(df1$DayOfWeek))
days
#1:Monday, 7:Sunday

#resetting depdelay1&arrdelay1
depdelay1<-list()
arrdelay1<-list()

for (x in 1:7) {
  mean_depdelay1<-colMeans(df1[df1$DayOfWeek==x,]['DepDelay'],na.rm=TRUE)
  depdelay1<-append(depdelay1,mean_depdelay1)
  mean_arrdelay1<-colMeans(df1[df1$DayOfWeek==x,]['ArrDelay'],na.rm=TRUE)
  arrdelay1<-append(arrdelay1,mean_arrdelay1)
}

depdelay1
arrdelay1
#creating deparr_delay1 by rbinding
deparr_delay1<-rbind(depdelay1,arrdelay1)
deparr_delay1
deparr_delay1<-t(deparr_delay1)

colnames(deparr_delay1)<-c('depdelay','arrdelay')
str(deparr_delay1)

#line plot for distribution of delay per days of a week
matplot(days,deparr_delay1,type='l',lty=1, lwd=2,main='Distribution of delay by days of a week',xlab='Day of Week',ylab='Delay(min)',
     xlim=c(1,7),col=c('#1B4D80', '#F46664'))

legend(x="topright",inset=0.05,cex=0.6,
       legend = c('departure delay','arrival delay'),
       col=1:2,
       pch=16,
       border = "black") # Color of the border of the squares
```

3) Best time of year to minimise delay - Quarters
#Q1: Jan,Feb,March
#Q2: Apr,May,June
#Q3: July,Aug,Sep
#Q4: Oct,Nov,Dec

```{r}
#assigning quarter (Q1,Q2,Q3,Q4)

Quarter<-list()
i=1
for (x in df1$Month){
  x<-as.numeric(x)
  if (x>=1 && x<=3){
    Quarter[i]<-'Q1'
  }
  else if (x>=4 && x<=6){
    Quarter[i]<-'Q2'
  }
  else if (x>=7 && x<=9){
    Quarter[i]<-'Q3'
  }
  else {
    Quarter[i]<-'Q4'
  }
  i<-i+1
}

#add timing column into df1
Quarter<- data.frame(matrix(unlist(Quarter), nrow=length(Quarter), byrow=TRUE))
df1$Quarter<-Quarter

depdelay1<-list()
arrdelay1<-list()

for (x in sort(unique(unlist(df1$Quarter)))) {
  mean_depdelay1<-colMeans(df1[df1$Quarter==x,]['DepDelay'],na.rm=TRUE)
  depdelay1<-append(depdelay1,mean_depdelay1)
  mean_arrdelay1<-colMeans(df1[df1$Quarter==x,]['ArrDelay'],na.rm=TRUE)
  arrdelay1<-append(arrdelay1,mean_arrdelay1)
}
head(depdelay1,5) #checking
head(arrdelay1,5) #checking

deparr_delay1<-rbind(depdelay1,arrdelay1)
colnames(deparr_delay1)<-c('Q1','Q2','Q3','Q4')
deparr_delay1<-subset(deparr_delay1,select=c('Q1','Q2','Q3','Q4'))

Timing<-unique(unlist(df1$timing))

#Bar plot for distribution of delay over quarters in a year

barplot(deparr_delay1,main='Distribution of delay over quarters in a year',xlab='Quarter of a Year',ylab='Delay(min)',col=c('#1B4D80', '#F46664'))

legend(x="topright",inset=0.1,cex=0.6,
       legend = c('departure delay','arrival delay'),
       col=1:2,
       pch=16,
       border = "black") # Color of the border of the squares
```
