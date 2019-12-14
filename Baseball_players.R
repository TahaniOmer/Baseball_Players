#Schmidt/Luzinski 1975 Home Run Race

#Download our data
events <- read.csv("events.csv",header = FALSE)
days <- read.csv("days.csv",header = FALSE)
head(events)

#specify the data frame 
colnames(events)<-c("gameid", "batterid","result")
head(events)
colnames(days)<- c("gameid","date")
head(days)

library(dplyr)

#join the two data frame(by gameid)
#add a column gives 1 if event is homerun gives 0 for else (23 is the code for home run )
#break the date up into three column
#combine the date as R date format

bdat<-inner_join(events,days,by= c("gameid"))%>% # continuation operator
      mutate(HR=as.numeric(result==23))%>%
      mutate(year= substr(date,1,4))%>%
      mutate(month= substr(date,5,6))%>%
      mutate(day= substr(date,7,8))%>%
      mutate(Rdate=as.Date(paste(year ,month,day,sep="-")))
             
head(bdat)

#Greg Luzinski data 

luzinski <- bdat%>%
             filter(batterid == "luzig001")%>%
             select(HR,Rdate)%>%
             arrange(Rdate)%>%  # put the date in order
             mutate(tot_HR=cumsum(HR))%>% #accumulate home run
             mutate(name="Greg Luzinski")

head(luzinski)

# Mike Schmidt Data 

schmidt <- bdat%>%
   filter(batterid == "schmm001")%>%
   select(HR,Rdate)%>%
   arrange(Rdate)%>%  # put the date in order
   mutate(tot_HR=cumsum(HR))%>% #accumulate home run
   mutate(name="Mike Schmidt")

head(schmidt)

library(ggplot2)

#Cumulative Home Run Plot
#add colors,legent and title
ggplot()+
   geom_line(data= luzinski, aes(x=Rdate, y=tot_HR, color= name))+
   geom_line(data= schmidt, aes(x=Rdate, y=tot_HR,color= name))+
   xlab("date")+
   ylab("home runs")+
   ggtitle("Schmidt/Luzinski 1975 Home Run Race")



























