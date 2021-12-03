# ETX2550  ASSIGNMENT 2

####
rm(list=ls())
library(tidyverse)
ewd<-read_csv('electricity_weather_data.csv')
View(ewd)

library(ggplot2)
library(GGally)
ggparcoord(ewd)


# 1.Boxplot

 
ggplot(ewd, aes(x=Day, y=Demand, col=State)) + 
  geom_boxplot() + 
  facet_wrap(~State,nrow=1) + scale_y_log10() +
  ggtitle("Boxplots of electricity demand in 5 states for each day of the week") +
  xlab("Boxplots for each state") +
  ylab("Logtransform Total demand for electricity at 3PM")

# 2. seasonality
ewd%>%
  filter(State %in% c('NSW','QLD','SA','TAS','VIC'))%>%
ggplot(aes(x=Date,y=Demand,col=State))+
  geom_line()+facet_wrap(~State,nrow=5,scale='free_y') + 
  ggtitle("The daily demand time series plots for 5 states") +
  xlab("Time (day)") + ylab("Total demand for electricity at 3PM (Mwh)")

# 3. weather component
filter(ewd,Day %in% c('Sat','Sun'))->wend
wend

filter(ewd, !(Day%in% c('Sat','Sun'))) ->weekd
weekd

ggplot(wend,
       aes(x=Temp,
           y=Demand)) + #Country mapped to label
  geom_point(aes(col=State,size=WindSpeed)) + 
  facet_wrap(~State,nrow=3,scale='free_y')

ggplot(weekd,aes(x=Temp,
                 y=Demand)) + 
  geom_point(aes(col=State,size=WindSpeed)) + 
  geom_point(data=wend,colour='black') + 
  geom_smooth() + facet_wrap(~State,nrow=5,scale='free_y') + 
  scale_x_log10() + scale_y_log10()+
  ggtitle("Scatter plot of Tempature against Demand in 5 states") +
  xlab("Tempature at 3PM (ºC) ") + ylab("Total demand for electricity at 3PM (Mwh)")

# 4. Netexport with price voltalitiy

ewd%>%
  ggplot(aes(x=Demand,y=Price,col=NetExport))+ geom_point() + 
  scale_x_log10()+scale_y_log10() + 
  scale_color_viridis_c(option='C') +
  facet_wrap(~State,nrow=2,scale='free_y') +
  ggtitle("The influence of demand on NetExport and price level for each state at 3PM") +
  xlab("Total demand for electricity at 3PM (Mwh)") +
  ylab("Wholesale price of electricity at 3PM ($ per Mwh)")




# 5.GGpair

library(GGally)


ewd %>% select(-WindDir) ->ewd2
ggpairs(ewd2)



###  draft  1.0



#############################
ewd%>%
  ggplot(aes(x=Demand,y=Price,col=NetExport))+ geom_point() + 
  scale_x_log10()+scale_y_log10() + 
  scale_color_viridis_c(option='C') +
  facet_wrap(~State,nrow=2,scale='free_y') +
  ggtitle("The influence of demand on NetExport and price level for each state at 3PM") +
  xlab("Total demand for electricity at 3PM (Mwh)") + ylab("Wholesale price of electricity at 3PM ($ per Mwh)")

ggplot(ewd) + geom_point(aes(x=Date,y=Demand,col=NetExport)) + scale_color_viridis_c(option='C') +
  facet_wrap(~State,nrow=5,scale='free_y','free_x') +
  ggtitle("Relationship of daily NetExports with demand for each state") +
  xlab("Time (day)") + ylab("Total demand for electricity at 3PM (Mwh)")


ggplot(ewd) + geom_point(aes(x=Demand,y=Price,col=NetExport)) + scale_color_viridis_c(option='C') +
  facet_wrap(~State,nrow=2,scales = 'free_y') +
  ggtitle("Relationship of daily NetExports with demand for each state") +
  xlab("Time (day)") + ylab("Total demand for electricity at 3PM (Mwh)")
#Convert to long form
ewdlong<-gather(data = ewd,
                  key = WindSpeed, #Column names become variable
                  value = Temp, #All numbers are exports
                  -Date,-Day) #Do not gather these variables
ewdlong
#Select Germany and US spread
ewdlong%>%
  spread(Country,YearlyExports)->
  Exports_DE_US
#Finally make plot
ggplot(Exports_DE_US,aes(x=DE,y=US,col=Year))+
  geom_path()+scale_color_viridis_c()

ggplot(ewd, aes(x=Temp, y=WindSpeed)) + geom_()+ scale_color_viridis_c()+facet_wrap(~WindDir,nrow=4)

ggplot(ewd, aes(x=Demand, y=Price,col=NetExport)) + geom_path() +
  scale_color_viridis_c()+facet_wrap(~WindDir,nrow=)

ggplot(ewd, aes(x=WindSpeed, y=Temp,col=Date)) +
  geom_point(position='jitter')+ facet_wrap(~WindDir,nrow=4,scale='free_y')


data(Titanic)
titanic <- as.data.frame(Titanic)
titanic
library(ggmosaic)
ggplot(ewd) +
  geom_mosaic(aes(x=product(Temp,WindSpeed)))

# sample 1
ggplot(ewd,
       aes(x=Temp,
           y=Demand,col=Day)) + #Country mapped to label
  geom_point() +
  geom_text(aes(label=WindDir,size=1)) + facet_wrap(~State,nrow=3,scale='free_y')

# sample 2
ggplot(ewd,
       aes(x=Temp,
           y=Demand)) + #Country mapped to label
  geom_point(aes(col=State,size=WindSpeed)) +
  facet_wrap(~State,nrow=3,scale='free_y')


ggplot(USState,
       aes(x=Income,
           y=`Life Exp`))+ #Country mapped to label
  geom_point(aes(col=Region,
                 size=Population))+ #color only in point
  geom_text(aes(label=Abbreviation))
# sample 3

ggplot(ewd,
       aes(x=Temp,
           y=Demand,col=Day)) + #Country mapped to label
  geom_point() +
  facet_wrap(~State,nrow=3,scale='free_y')

ewd%>%
  filter(Day %in% c('Sat','Sun'))%>% #Filter years
  spread(Day,Demand)->ewd11 #Need to spread

ewd%>%
  rename(Day %in% c('Sat','Sun'))
###############################################################
filter(ewd,Day %in% c('Sat','Sun'))->wend
wend

filter(ewd, !(Day%in% c('Sat','Sun'))) ->weekd
weekd

ggplot(wend,
       aes(x=Temp,
           y=Demand)) + #Country mapped to label
  geom_point(aes(col=State,size=WindSpeed)) + 
  facet_wrap(~State,nrow=3,scale='free_y')

ggplot(weekd,aes(x=Temp,
                y=Demand)) + 
  geom_point(aes(col=State,size=WindSpeed)) + 
  geom_point(data=wend,colour='black') + 
  geom_smooth() + facet_wrap(~State,nrow=5,scale='free_y') + 
  ggtitle("Scatter plot of Tempature against Demand in 5 states") +
  xlab("Tempature at 3PM (ºC) ") + ylab("Total demand for electricity at 3PM (Mwh)")

###############################################################
View(wend)
ewd %>%
  select(-Day) %>%
  group_by(,demand)->ewd111
ewd111
#################
ewd%>%
  filter(Day %in% c('Sat','Sun'))%>% #Filter years
  spread(Day,Day)->ewd11 #Need to spread
ewd11

ewd_long1<-gather(data = ewd11,
                  key = Weekend, #Column names become variable
                  value = Day, #All numbers are exports
                  -Date,-State,-Price,-Demand,-NetExport,-Temp,-WindDir,-WindSpeed) #Do not gather these variables
View(ewd_long1)
##########################
elec
SwissLong%>% #This is much easier with long data
  select(-Date)%>% #Eventually cannot aggregate dates
  group_by(Year,Country)%>%
  summarise(YearlyExports=sum(Exports))->SwissYearly
# filter variable
filter(diamonds,(price>1000))

# And operator, if two statements need to be satisfied use &
filter(diamonds,
       (price>1000)&(cut=='Ideal'))

# Or operator , if either one or the other statement needs to be satisfied use 
filter(diamonds,(cut=='Ideal')|color=='E')

# In operator , another useful operator is %in%
filter(diamonds,(cut %in% c ('Ideal','Fair')))

# Not operator. us ! as not
filter(diamonds,
       !(cut %in% c ('Ideal','Fair')))


library(GGally)


ewd %>% select(-WindDir,-Date) ->ewd2
ggpairs(ewd2)
StateRed<-readRDS('USStateRed.rds')
StateRed
ewd2
ewd %>% select(Temp,WindSpeed,WindDir,State,NetExport) -> ewd3
ewd3
ggpairs(ewd3)
ggplot(ewd,mapping=aes(x=))

#
elec<-read_csv('electricity.csv')
library(plotly)
g<-ggplot(ewd,aes(x=Temp,
                   y=Demand,
                   col=State,
                   text=Date))+geom_point()
ggplotly(g)
