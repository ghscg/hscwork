###Creates a vector of difference in time between events from events.csv
d <- c(NA, diff(events))
      
#Produces an SPC chart  for time between events     
      qic(d,
          chart = 't',
          title   = 'Days Between Events Overall',
          ylab  = 'Days between Events (Higher is Better)',
          xlab  = 'Number of Events.')

#converts d vector to df, then slice to drop NA. Then mean, median and SD. 

d<-as.data.frame(d)   
d1<-d%>%slice(2:21)
mean(d1$d)            
median(d1$d)
sd(d1$d)
      
