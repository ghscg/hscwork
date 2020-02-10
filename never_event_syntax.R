
#################
#               #
#      Name     #
#               #
#################

# Gerard Gallagher developed the script
# Mark McCann modified he randomisation inference element

#############
#  Purpose  #
#############

##############
#            #
#    Notes   #
#            #
##############

#########################
#                       #
#  Outstanding actions  #
#                       #
#########################

#The code produces warning xes in Rstudio if the variables touch the + symbols
#      Add a space to tidy the code 


#########################
#                       #
#    Load packages      #
#                       #
#########################
library(dplyr)
library(foreign)
library(ggplot2)
library(zoo)
library(tidyverse)
library(writexl)
library(qicharts2)
library("RColorBrewer") #This is for a nice colour palet:to see pallets display.brewer.all()
library(dslabs)
library(ggthemes) # Load
library(purrr)
library(pdftools)
library(lubridate)
library(readxl)

library(fastDummies)


#########################
#                       #
#     Load functions    #
#                       #
#########################


#########################
#                       #
#  Main body of script  #
#                       #
#########################



setwd("P:/Gerard/Data/Never Events/2019_12_11_never_events")

#Loading packages 

setwd("P:/Gerard/Data/Never Events")

#Importing data set 
      never_events <- read_excel("2019_12_11_never_events/never_events.xlsx", 
                                 col_types = c("numeric", "date", "numeric"))
      View(never_events)
#Adding another df: 
never1<- never_events
      
#Adding Labels 
      
never1$trust <- ordered(never1$trust,
                                 levels = c(1,2,3,4,5),
                                 labels = c("SEHSCT", "WHSCT", "BHSCT", "PCARE", "SHSCT"))

never1$sub_category <- ordered(never1$sub_category,
                           levels = c(1,2,3),
                           labels = c("Wrong Implant/Prosthesis", "Retained Object", "Wrong site surgery"))
#adding a date var as backup 
never1$date1 <- as.Date(never1$date,
                                  format = "%Y/%m/%d")

#### Table of events 
table(never1$trust,never1$sub_category

#Barcharts 

#Trust & Event type 
t_e<- ggplot(never1) +
  geom_bar(aes(x = trust, fill =  sub_category)) + 
  scale_fill_brewer("Participant Role", palette = "Pastel1") +
  ggtitle("Never Event By Trust % Type") +
  xlab("Trust") + ylab("Never Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

  
#trust event count  
t1<-  ggplot(never1) +
  geom_bar(aes(x = trust), fill = "#B3CDE3") +
  ggtitle("Trust") +
  xlab("Trust") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

#event type   
e1<-ggplot(never1) +
geom_bar(aes(x = sub_category ), fill = "#B3CDE3") +
  ggtitle("Event Type") +
  xlab("Event Type") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1)) 
 
t_e + scale_y_continuous(name="Number of Events", limits=c(0, 15))

#creating a vector of time between events 

events <- sort(sample(never1$date1, 21) )
d <- c(NA, diff(events))
 

#SPC charts 
qic(d,
    chart = 't',
    x = never1$date1,
    title   = 'Days Between Events Overall',
    ylab  = 'Events',
    xlab  = 'Number of Events.') 

qic(d,
    chart = 't',
    title   = 'Days Between Events Overall',
    ylab  = 'Days between Events (Higher is Better)',
    xlab  = 'Number of Events.') 

#Creating a subset for each trust then a qic 

#SEHSCT 
sehsct<-subset(never1, `trust` =="SEHSCT")
sehsctevents <- sort(sample(sehsct$date1, 3))
d1 <- c(NA, diff(sehsctevents))

qic(d1,
    chart = 't',
    x = sehsct$date1,
    title   = 'Days Between Events SEHSCT',
    ylab  = ' Events',
    xlab  = 'Number of Events.') 

##WHSCT
whsct<-subset(never1, `trust` =="WHSCT")
whsctevents <- sort(sample(whsct$date1, 3))
d2 <- c(NA, diff(whsctevents))

qic(d2,
    chart = 't',
    x = whsct$date1,
    title   = 'Days Between Events WHSCT',
    ylab  = 'Events',
    xlab  = 'Number of Events.') 




##bhsct

bhsct<-subset(never1, `trust` =="BHSCT")
bhsctevents <- sort(sample(bhsct$date1, 12))
d3 <- c(NA, diff(bhsctevents))

qic(d3,
    chart = 't',
    x = bhsct$date1,
    title   = 'Days Between Events BHSCT',
    ylab  = 'Events',
    xlab  = 'Number of Events.') 


###No Point running this only 1 event 
#pcare<-subset(never1, `trust` =="PCARE")
#events <- sort(sample(never1$date1, 21))
#d <- c(NA, diff(events))

###SHSCT : Not much point doing this either. 
#shsct<-subset(never1, `trust` =="SHSCT")
#shsctevents <- sort(sample(bhsct$date1, 2))
#d4 <- c(NA, diff(shsctevents))
#qic(d4,
 #   chart = 't',
  #  title   = 'Days Between Events SHSCT',
   # ylab  = 'Events',
    #xlab  = 'Number of Events.') 

#Bar Charts for time!
#This function creates a new variable for month/year etc. 
never1$month1<- strftime(never1$date1, "%B") #%B = month name, %m = month num

# ordering days and months by calendar year 
never1<- never1 %>% mutate(Day1 = recode (day, 'Mon' = "1",
                                          'Tue' = "2",
                                          'Wed' = "3",
                                          "Thu" = "4",
                                          "Fri" = "5",
                                          "Sat"= "6",
                                          "Sun" = "7"))

# adding labels 
never1$Day1 <- ordered(never1$Day1,
                       levels = c(1,2,3,4,5,6,7),
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))  
##

never1<- never1 %>% mutate(month2 = recode (month1, 'January' = "1",
                                          'February' = "2",
                                          'March' = "3",
                                          "April" = "4",
                                          "May" = "5",
                                          "June"= "6",
                                          "July" = "7",
                                          "August" = "8",
                                          "September" = "9",
                                          "October"= "10",
                                          "November" = "11", 
                                          "December" = "12"))

# adding labels 
never1$month2 <- ordered(never1$month2,
                       levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) 



#event type x month
neverordermonth <-never1[order(never1$month2),]
ggplot(neverordermonth) +
  geom_bar(aes(x = month2 ), fill = "#B3CDE3") +
  ggtitle("Events by Month") +
  xlab("Month") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1)) 

#event type by day 
neverordered <-never1[order(never1$Day1),]

ggplot(neverordered) +
  geom_bar(aes(x = Day1 ), fill = "#B3CDE3") +
  ggtitle("Events by Day") +
  xlab("Month") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1)) 


never1$yemon<- strftime(never1$date1, "%B%Y")

never1$day<- strftime(never1$date1, "%a") 
ggplot(never1) +
  geom_bar(aes(x = yemon ), fill = "#B3CDE3") +
  ggtitle("Events by Day") +
  xlab("Month") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1)) 

##scatter plot over time 

#making a dummy: 
  
  results <- fastDummies::dummy_cols(never1, select_columns = "sub_category")
knitr::kable(results)

ggplot(results) +
  geom_line(aes(x = date, y=sub_category), fill = "#B3CDE3") +
  ggtitle("Never Events") +
  xlab("Date") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

ggplot(results) +
  geom_line(aes(x = date, y=results$`sub_category_Wrong Implant/Prosthesis`), fill = "#B3CDE3") +
  ggtitle("Never Events: Wrong Implant Over time") +
  xlab("Date") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

ggplot(results) +
  geom_line(aes(x = date, y=results$`sub_category_Retained Object`), fill = "#B3CDE3") +
  ggtitle("Never Events: Retained Object") +
  xlab("Date") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

ggplot(results) +
  geom_line(aes(x = date, y=results$`sub_category_Wrong site surgery`), fill = "#B3CDE3") +
  ggtitle("Never Events: Retained Object") +
  xlab("Date") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))


#Mark wants some TS line charts. 

#Added a new var that is effectively a binary: incident 1= yes. 
New1<- never1 %>% mutate(incident = recode (sub_category, '1' = "1",
                                            '2' = "1",
                                            '3' = "1"))

#imputting missing dates. 
#Creates new var of all dates in range.
full_dates <- seq(min(New1$date), max(New1$date), 
                  by = "1 day")
#changes vector to a df
full_dates <- data.frame(date = full_dates)
#merges df together. 
my_complete_data <- merge(full_dates, New1, by = "date", 
                          
                          all.x = TRUE)
#Converting NAs to 0 
# first 'converting new incident var to numeric'

my_complete_data$incident1 <- as.numeric(my_complete_data$incident)

#changing NA to 0 for numeric values. 
my_complete_data<- my_complete_data %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#creating a month var 
my_complete_data$month1<- strftime(my_complete_data$date, "%B")
#creating a yday
my_complete_data$yda<- yday(my_complete_data$date)
#creadting a yearmon
ymon<- as.yearmon(my_complete_data$date, format="%Y/%m/%d") 

#Storing ggplot object Below is line plot  for incidents over time.
q<- ggplot(my_complete_data) +
  geom_line(aes(x = ymon, y=my_complete_data$incident1), fill = "#B3CDE3") +
  ggtitle("Frequency of Never Events Over Time") +
  xlab("Date") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

#Storing ggplot object Below is line plot  for incidents over time.
r<- ggplot(my_complete_data) +
  geom_point(aes(x = ymon, y=my_complete_data$incident1), fill = "#B3CDE3") +
  ggtitle("Frequency of Never Events Over Time") +
  xlab("Date") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1))

#limiting y axis 0 - 2. 
r +scale_y_continuous(limits = c(0, 2))
q +scale_y_continuous(limits = c(0, 2))
#################################################

#Performing analysis dropping wrong tooth extracton
never2<- never1 %>% slice(1:4, 6:18,20)
neverordered2<- neverordered%>% slice(1:4, 6:18,20)
neverorrderredmonth2<- neverordermonth %>% slice(1:4, 6:18,20)
#QIC 

neve <- sort(sample(never2$date1, 18))
n2 <- c(NA, diff(events))

qic(n2,
    chart = 't',
    title   = 'Days Between Events Excl Wrong Tooth',
    ylab  = 'Days between Events (Higher is Better) ',
    xlab  = 'Number of Event.') 

#break @ 8 
qic(n2,
    chart = 't',
    part     = 8,
    title   = 'Days Between Events Across Region excl Wrong tooth (Event 8 Break)',
    part.labels = c('Mar 2016 - Mar 2018', 'Apr 2018 - Oct 2019'),
    ylab  = 'Days Between Events (Higher is better)',
    xlab  = 'Event Number')  

#events by day excl tooth 
neverordered2 <-neverordered2[order(neverordered2$Day1),]
ggplot(neverordered2) +
  geom_bar(aes(x = Day1 ), fill = "#B3CDE3") +
  ggtitle("Events by Day Excl Wrong Tooth") +
  xlab("Month") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1)) 


#event type x month 
neverorrderredmonth2 <- neverordermonth[order(neverordermonth$month2),]
ggplot(neverorrderredmonth2) +
  geom_bar(aes(x = month2 ), fill = "#B3CDE3") +
  ggtitle("Events by Month") +
  xlab("Month") + ylab("Number of Events") +
  theme(axis.text.x = element_text(angle = 70,hjust = 1)) 




############################################################################
############################################################################
############################################################################
################## Randomisation inference for events ######################
############################################################################
############################################################################

#changes vector to a df
#creating new df of random variables. 

# create blank data frame
dat <- as.data.frame( matrix(0, nr=1303, nc = 1000) )

#Generate random numbers
for (i in 1:1000){
    x<- runif(1303) 
    dat[,i] <-x
}

 
#Split random numbers into zero and ones at the desired prevalence
dat[,] <- ifelse(dat[,] < "0.01611665", 1, ifelse(dat[,] > "0.01611665", 0, 1 ) )

length(dat[,1])

plot.df <- as.data.frame(matrix(0, nr = 1000, nc = 2))
colnames(plot.df) <- c("Mean","Sd")

#Check the run length encoding

  runs <- rle(dat[,i])
    #Check the length of runs
    runs$lengths
    #Remove the incident days and leave the length of 'no incident' runs
    noinc.days   <- runs$lengths[which(runs$lengths > 1)]
    plot.df[i,1] <- mean(noinc.days)
    plot.df[i,2] <- sd(noinc.days)
}

##take lines 427 - 433 and run them on the observed data. Take the mean(noinc.days) and put that in place of the 70 below

ggplot(plot.df, aes(Mean))  + geom_density() + geom_vline(aes(xintercept=70, color = "red")) + theme(legend.position = "none")

ggplot(plot.df, aes(Sd))  + geom_density() + geom_vline(aes(xintercept=70, color = "red")) + theme(legend.position = "none")


