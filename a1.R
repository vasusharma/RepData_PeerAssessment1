library(data.table)
activity <- read.csv("C:/Users/arpita/Desktop/coursera/reproducible_research/RepData_PeerAssessment1/activity.csv")  
activity2<-subset(activity,complete.cases(activity)==TRUE)  
activity3 <- as.data.table(activity2)           # convert data.frame to data.table
total_steps_day <- activity3[,sum(steps,na.rm= TRUE),by=date]
hist(total_steps_day$V1,col="red",main="Hist Total Steps per Day", xlab="Number of Steps")
mean_steps <- mean(total_steps_day$V1)
median_steps <- median(total_steps_day$V1) 
avg_interval <- activity3[,mean(steps,na.rm= TRUE),by=interval]
plot(avg_interval, type="l", main="5 minute Interval Time Series Plot", ylab="Average Number of Steps", xlab="Interval index", col="blue")
avg_interval[V1==max(avg_interval$V1)]$interval
nrow(subset(activity,complete.cases(activity)==FALSE))
activity4 <- activity
for(i in 1:nrow(activity))
{ 
  if(is.na(activity[i,]$steps)== TRUE)
  {
    activity4[i,]$steps=round(avg_interval[interval==activity[i,]$interval]$V1)
  }
}

activity5 <- as.data.table(activity4)           # convert data.frame to data.table
total_steps_day2 <- activity5[,sum(steps,na.rm= TRUE),by=date]
hist(total_steps_day2$V1,col="red",main="Hist Total Steps per Day", xlab="Number of Steps")
mean_steps2 <- mean(total_steps_day2$V1)
median_steps2 <- median(total_steps_day2$V1) 

activity6 <- activity4
activity6$date <- as.Date(strptime(activity6$date, format="%Y-%m-%d"))  
activity6$day <- weekdays(activity6$date)                                
for (i in 1:nrow(activity6)) 
  {                                      
  if (activity6[i,]$day %in% c("Saturday","Sunday")) 
    {            
    activity6[i,]$day<-"weekend"                                
     }
  else
    {
    activity6[i,]$day<-"weekday"                                
    }
  }


stepsByDay <- aggregate(activity6$steps ~ activity6$interval + activity6$day, activity6, mean)
names(stepsByDay) <- c("interval", "day", "steps")
par(mfrow=c(1,1))  
with(stepsByDay, plot(steps ~ interval, type="n", main="Weekday vs. Weekend Avg."))  
with(stepsByDay[stepsByDay$day == "weekday",], lines(steps ~ interval, type="l", col="blue"))  
with(stepsByDay[stepsByDay$day == "weekend",], lines(steps ~ interval, type="l", col="orange" ))  
legend("topright", lty=c(1,1), col = c("blue", "orange"), legend = c("weekday", "weekend"), seg.len=3)