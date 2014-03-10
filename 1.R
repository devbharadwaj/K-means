
# Source code

ratings<-read.csv("/home/subhendu/Desktop/ml-1m/ratings.csv")
movies<-read.csv("/home/subhendu/Desktop/ml-1m/movies.csv")
avgratings<-round(aggregate(ratings$Rating,by=list(ratings$MovieID),FUN=mean),digits=1)
names(avgratings)<-c("MovieID","AvgRating")
newmovies<-merge(x=movies,y=avgratings)
colors=c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan")
hist(newmovies$AvgRating,col=colors,xlab="Average Ratings",ylab="Number of Movies",main="Distribution of Ratings from All Users")

#

dt1<-data.table(users,key="UserID")
colnames(ratings)[1]<-"UserID"
newratings<-merge(ratings,dt1,by="UserID")
maleraters<-subset(newratings,newratings[,"Gender"]=="M")
avgratings<-round(aggregate(maleraters$Rating,by=list(maleraters$MovieID),FUN=mean),digits=1)
names(avgratings)<-c("MovieID","AvgRating")
newmoviesmale<-merge(x=movies,y=avgratings)
colors=c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan")
hist(newmoviesmale$AvgRating,col=colors,xlab="Average Ratings",ylab="Number of Movies",main="Distribution of Ratings from Male Users")


#

dt1<-data.table(users,key="UserID")
colnames(ratings)[1]<-"UserID"
newratings<-merge(ratings,dt1,by="UserID")
femaleraters<-subset(newratings,newratings[,"Gender"]=="F")
avgratings<-round(aggregate(femaleraters$Rating,by=list(femaleraters$MovieID),FUN=mean),digits=1)
names(avgratings)<-c("MovieID","AvgRating")
newmoviesfemale<-merge(x=movies,y=avgratings)
colors=c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan")
hist(newmoviesfemale$AvgRating,col=colors,xlab="Average Ratings",ylab="Number of Movies",main="Distribution of Ratings from Female Users")

# Repeat this by changing values of Age to 1,18,25,45,50,56

dt1<-data.table(users,key="UserID")
colnames(ratings)[1]<-"UserID"
newratings<-merge(ratings,dt1,by="UserID")
newraters<-subset(newratings,newratings[,"Age"]==56)
avgratings<-round(aggregate(newraters$Rating,by=list(newraters$MovieID),FUN=mean),digits=1)
names(avgratings)<-c("MovieID","AvgRating")
newmoviesage<-merge(x=movies,y=avgratings)
colors=c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan")
hist(newmoviesage$AvgRating,col=colors,xlab="Average Ratings",ylab="Number of Movies",main="Distribution of Ratings from 56+ Age Group")

# Repeat this by changing values of Occupation to different values in README

dt1<-data.table(users,key="UserID")
colnames(ratings)[1]<-"UserID"
newratings<-merge(ratings,dt1,by="UserID")
newraters<-subset(newratings,newratings[,"Occupation"]==2|newratings[,"Occupation"]==20|newratings[,"Occupation"]==1|newratings[,"Occupation"]==6)
avgratings<-round(aggregate(newraters$Rating,by=list(newraters$MovieID),FUN=mean),digits=1)
names(avgratings)<-c("MovieID","AvgRating")
newmoviesage<-merge(x=movies,y=avgratings)
colors=c("red", "yellow", "green", "violet", "orange","blue", "pink", "cyan")
hist(newmoviesage$AvgRating,col=colors,xlab="Average Ratings",ylab="Number of Movies",main="Distribution of Ratings from Artists/Writers/etc")

#
