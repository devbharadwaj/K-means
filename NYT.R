#R script
#Needs internet connection
#It will take some time to run
#And generate plots

data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))
head(data1)
data1$agecat <- cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
summary(data1)
library(doBy)
siterange <- function(x){c(length(x),min(x),mean(x),max(x))}
summaryBy(Age~agecat, data=data1, FUN=siterange)
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data=data1)
library(ggplot2)
ggplot(data1,aes(x=Impressions,file=agecat)) + geom_histogram(binwidth=1)
ggplot(data1,aes(x=agecat, y=Impressions, fill=agecat))+geom_boxplot()
data1$hasimps <- cut(data1$Impressions,c(-Inf,0,Inf))
summaryBy(Clicks~hasimps, data=data1, FUN=siterange)
ggplot(subset(data1,Impressions>0),aes(x=Clicks/Impressions,color=agecat))+geom_density()
ggplot(subset(data1,Clicks>0),aes(x=Clicks/Impressions,color=agecat))+ geom_density()
ggplot(subset(data1,Clicks>0),aes(x=agecat,y=Clicks,fill=agecat))+geom_boxplot()
ggplot(subset(data1,Clicks>0),aes(x=Clicks,color=agecat))+geom_density()
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks > 0] <- "Clicks"
data1$scode <- factor(data1$scode)
head(data1)
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+agecat,data=data1,FUN=clen)


data1 <- data1[data1$Signed_In == 1,]

mainframe <- aggregate(cbind(data1$Click,data1$Impressions)~agecat,data=data1,sum,na.rm=TRUE)
colnames(mainframe) <- c("agecat","Clicks","Impressions")
mainframe$day <- "1st"

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt2.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "2nd"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt3.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "3rd"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt4.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "4th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt5.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "5th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt6.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "6th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt7.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "7th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt8.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "8th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt9.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "9th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt10.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "10th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt11.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "11th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt12.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "12th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt13.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "13th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt14.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "14th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt15.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "15th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt16.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "16th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt17.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "17th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt18.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "18th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt19.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "19th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt20.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "20th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt21.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "21st"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt22.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "22nd"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt23.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "23rd"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt24.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "24th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt25.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "25th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt26.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "26th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt27.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "27th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt28.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "28th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)


data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt29.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "29th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt30.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "30th"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

data2 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt31.csv"))
data2$agecat <-cut(data2$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
tempframe <- aggregate(cbind(data2$Click,data2$Impressions)~agecat,data=data2,sum,na.rm=TRUE)
colnames(tempframe) <- c("agecat","Clicks","Impressions")
tempframe$day <- "31st"
mainframe <- rbind(mainframe,tempframe)
rm(data2)

mainframe <- mainframe[mainframe$agecat!="(-Inf,0]",]


age1 <- mainframe[mainframe$agecat=="(0,18]",]
age2 <- mainframe[mainframe$agecat=="(18,24]",]
age3 <- mainframe[mainframe$agecat=="(24,34]",]
age4 <- mainframe[mainframe$agecat=="(34,44]",]
age5 <- mainframe[mainframe$agecat=="(44,54]",]
age6 <- mainframe[mainframe$agecat=="(54,64]",]
age7 <- mainframe[mainframe$agecat=="(64, Inf]",]
library(plyr)
age1 <- arrange(age1,desc(age1$Clicks))
age2 <- arrange(age2,desc(age2$Clicks))
age3 <- arrange(age3,desc(age3$Clicks))
age4 <- arrange(age4,desc(age4$Clicks))
age5 <- arrange(age5,desc(age5$Clicks))
age6 <- arrange(age6,desc(age6$Clicks))
age7 <- arrange(age7,desc(age7$Clicks))

ggplot(head(age1,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()
ggplot(head(age2,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()
ggplot(head(age3,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()
ggplot(head(age4,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()
ggplot(head(age5,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()
ggplot(head(age6,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()
ggplot(head(age7,10),aes(x=day,y=Clicks,fill=day)) + geom_bar()


age1 <- arrange(age1,desc(age1$Clicks/age1$Impressions))
age2 <- arrange(age2,desc(age2$Clicks/age2$Impressions))
age3 <- arrange(age3,desc(age3$Clicks/age3$Impressions))
age4 <- arrange(age4,desc(age4$Clicks/age4$Impressions))
age5 <- arrange(age5,desc(age5$Clicks/age5$Impressions))
age6 <- arrange(age6,desc(age6$Clicks/age6$Impressions))
age7 <- arrange(age7,desc(age7$Clicks/age7$Impressions))


ggplot(head(age1,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()
ggplot(head(age2,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()
ggplot(head(age3,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()
ggplot(head(age4,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()
ggplot(head(age5,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()
ggplot(head(age6,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()
ggplot(head(age7,10),aes(x=day,y=(Clicks/Impressions),fill=day)) + geom_bar()


day6 <- mainframe[mainframe$day=="6th",]
day13 <- mainframe[mainframe$day=="13th",]

ggplot(day13,aes(x=agecat,y=Clicks,fill=agecat)) + geom_bar()
ggplot(day13,aes(x=agecat,y=Impressions,fill=agecat)) + geom_bar()
ggplot(day6,aes(x=agecat,y=Clicks,fill=agecat)) + geom_bar()
ggplot(day6,aes(x=agecat,y=Impressions,fill=agecat)) + geom_bar()
