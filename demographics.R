query = "select * from PARTICIPANTS_COMPLETE WHERE ERROR_RATE BETWEEN 0 AND 25;"

complete = dbGetQuery(conn, query)

library(ggplot2)
library(dplyr)
library(rworldmap)

country_parts = aggregate(complete$PARTICIPANT_ID, by = list(complete$COUNTRY), FUN = length)
#country_parts = country_parts[country_parts[,1] != "AQ",]
n <- joinCountryData2Map(country_parts, joinCode="ISO2", nameJoinColumn="Group.1")

png(filename= "/home/vvkd/Desktop/RTypingMaster/plots/map_participants.png", height = 1200, width = 1200)
mapParams = mapCountryData(n, nameColumnToPlot="x", mapTitle="Participants by countries", 
                           colourPalette = "terrain", addLegend = FALSE, catMethod = "logFixedWidth", numCats = 7)
do.call( addMapLegend, c(mapParams, legendWidth=0.8, legendMar = 8, legendLabels = "all", legendShrink=0.3, 
                         labelFontSize=1, legendIntervals = "page", sigFigs = 1))

dev.off()

png(filename= "/home/vvkd/Desktop/RTypingMaster/plots/demographics_gender.png")
barplot(table(complete$GENDER), main = "Distribution of Participants by gender")
dev.off()


dfing = as.data.frame(complete$FINGERS)
fingers_summary = dfing %>% group_by(complete$FINGERS) %>%summarise(count = n())
ggplot(fingers_summary, aes(x=c("0-2","2-4","4-6","6-8","8-10"), y = count)) +
  geom_bar(data = fingers_summary[-1,],stat = 'identity') + 
  ggtitle("Distribution of Participants by number of fingers used in typing") +
  labs(x = "no of fingers used", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14),axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_fingers.png")


#keyboard type
dev.off()
dfkbdtype = as.data.frame(complete$KEYBOARD_TYPE)
kbdtype_summary = dfkbdtype %>% group_by(complete$KEYBOARD_TYPE) %>%summarise(count = n())
ggplot(kbdtype_summary, aes(x=c("full", "laptop", "on-screen", "small"), y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by keyboard type used for the test") +
  labs(x = "keyboard type", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14),axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_kbdtype.png")


#keyboard layout
dev.off()
dflayout = as.data.frame(complete$LAYOUT)
layout_summary = dflayout %>% group_by(complete$LAYOUT) %>%summarise(count = n())
ggplot(layout_summary, aes(x=layout_summary$'complete$LAYOUT', y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by keyboard layout used for the test") +
  labs(x = "keyboard layout", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_layout.png")


#OS type
dev.off()
dfos = as.data.frame(complete$OS)
os_summary = dfos %>% group_by(complete$OS) %>%summarise(count = n())
ggplot(os_summary, aes(x=os_summary$'complete$OS', y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution of Participants by OS used for the test") +
  labs(x = "OS used", y = "no of participants") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_OS.png")


#BROWSER
dev.off()
dfbrowser = as.data.frame(complete$BROWSER)
browser_summary = dfbrowser %>% group_by(complete$BROWSER) %>%summarise(count = n())
ggplot(browser_summary, aes(x=browser_summary$'complete$BROWSER', y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution of Participants by Browser used for the test") +
  labs(x = "Browser used", y = "no of participants") +
  theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5, size = 14), axis.title = element_text(size = 14) , plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_browser.png")


#typing course taken or not
dev.off()
dfcourse = as.data.frame(complete$HAS_TAKEN_TYPING_COURSE)
training_summary = dfcourse %>% group_by(complete$HAS_TAKEN_TYPING_COURSE) %>%summarise(count = n())
ggplot(training_summary, aes(x=c("Has taken typing course","not taken a course"), y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by whether typing course taken") +
  labs(x = "", y = "no of participants") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_training.png")


#hours of typing each day
dev.off()
dfhoursp = transform(complete$TIME_SPENT_TYPING, hours_group = cut(as.numeric(as.character(complete$TIME_SPENT_TYPING)), 
                                                                   breaks=c(0.0,2.0,5.0,10.0,15.0,20.0,24.0), 
                                                                   labels=c('0-2','3-5','6-10','11-15','16-20','21-24'), include.lowest = TRUE))
hours_summary = dfhoursp %>% group_by(hours_group) %>%summarise(count = n())
ggplot(na.omit(hours_summary), aes(x=na.omit(hours_group), y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by no of hours spent typing daily") +
  labs(x = "No of hours", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_hours.png")


dev.off()
dfage = transform(complete$AGE, 
                  age_group = cut(as.numeric(as.character(complete$AGE)), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf), 
                        labels=c('0-5','6-10','11-15','16-20','21-25','26-30','31-35','36-40','41-45','46-50',
                                 '51-55','56-60','61-65','66-70','71-75','76-80','80+'), 
                        include.lowest = TRUE))

age_summary = dfage %>% group_by(age_group) %>%summarise(count = n())
ggplot(na.omit(age_summary), aes(x=age_group, y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution of Participants by age") +
  labs(x = "age groups", y = "no of participants") +
  theme(axis.text=element_text(size = 9), axis.title = element_text(size = 14), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_age.png")

#age-group summary by country and gender


library(gridExtra)

query2 = "select COUNTRY, count(PARTICIPANT_ID) AS NUMBER from PARTICIPANTS_COMPLETE 
          where ERROR_RATE between 0 and 25
          group by COUNTRY
          order by NUMBER desc
          LIMIT 10;"

top_countries = dbGetQuery(conn, query2)
png(filename= "/home/vvkd/Desktop/RTypingMaster/plots/top_countries.png", height = 500, width = 300)
grid.table(top_countries)
dev.off()


grouped_countries = read.csv("/home/vvkd/Desktop/RTypingMaster/plots/grouped_countries.csv")

png(filename= "/home/vvkd/Desktop/RTypingMaster/plots/grouped_countries.png")
grid.table(grouped_countries)
dev.off()
