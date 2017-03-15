#parameters used

errorLim = 25
numTS = 15
#====================================================================================================================

library(data.table)
library(RMySQL)
conn <- dbConnect(MySQL(), user = 'vvkdhkl', 
                  password = .rs.askForPassword("password: "), host = '127.0.0.1', dbname = 'typingmasterreaddb')


query1 = "select * from TS_IKI_KP;"
computed = as.data.table(dbGetQuery(conn, query1))

query2 = "select * from PARTICIPANTS_COMPLETE where ERROR_RATE BETWEEN 0 AND 25;"
complete = as.data.table(dbGetQuery(conn, query2))

query_allP = "select count(*) from PARTICIPANTS;"
allP = dbGetQuery(conn, query_allP)
write(paste("totalP,",allP),"variables.dat")
write(paste("completeErrorP,",nrow(complete)),"variables.dat", append = TRUE)
write(paste("errorLim,",errorLim),"variables.dat", append = TRUE)
write(paste("numTS,",numTS),"variables.dat", append = TRUE)


query3 = "select TEST_SECTION_ID, PARTICIPANT_ID, UPDATED_INPUT_TIME, ERROR_RATE, INPUT_LENGTH, UPDATED_WPM FROM TEST_SECTIONS_COMPLETE;"
testsec = as.data.table(dbGetQuery(conn, query3))

#library(data.table)

merged = data.table()
merged = merge(testsec, computed, by="TEST_SECTION_ID", all.y = TRUE)
merged = merged[KSCOUNT>=INPUT_LENGTH]
mergedg = data.table()
mergedg = merged[, list(AVG_IKI = sum(AVG_IKI * (INPUT_LENGTH-1))/sum(INPUT_LENGTH-1), 
                        AVG_KEYPRESS = sum(AVG_KEYPRESS * (INPUT_LENGTH))/sum(INPUT_LENGTH), 
                        INPUT_LENGTH = sum(INPUT_LENGTH), TSCOUNT = sum(TEST_SECTION_ID>0),
                        KSCOUNT = sum(KSCOUNT), ERR_CORR = sum(ERR_CORR)), by = PARTICIPANT_ID]
mergedg$KSPC = mergedg$KSCOUNT/mergedg$INPUT_LENGTH
mergedg$ECPC = mergedg$ERR_CORR/mergedg$INPUT_LENGTH
merged = merge(complete, mergedg, by="PARTICIPANT_ID", all.x = TRUE)

write(paste("withTS,",nrow(merged)),"variables.dat", append = TRUE)

merged = as.data.frame(merged[TSCOUNT>=numTS])
write(paste("withCompleteTS,",nrow(merged)),"variables.dat", append = TRUE)

iki = merged$AVG_IKI
keypress = merged$AVG_KEYPRESS

library(ggplot2)
library(dplyr)
library(rworldmap)

country_parts = aggregate(merged$PARTICIPANT_ID, by = list(merged$COUNTRY), FUN = length)
#country_parts = country_parts[country_parts[,1] != "AQ",]
n <- joinCountryData2Map(country_parts, joinCode="ISO2", nameJoinColumn="Group.1")

jpeg(filename= "/home/vvkd/Desktop/RTypingMaster/plots/map_participants.jpg", height = 1200, width = 1200)
mapParams = mapCountryData(n, nameColumnToPlot="x", mapTitle="Participants by countries", 
                           colourPalette = "terrain", addLegend = FALSE, catMethod = "logFixedWidth", numCats = 7)
do.call( addMapLegend, c(mapParams, legendWidth=0.8, legendMar = 8, legendLabels = "all", legendShrink=0.3, 
                         labelFontSize=1, legendIntervals = "page", sigFigs = 1))

dev.off()

jpeg(filename= "/home/vvkd/Desktop/RTypingMaster/plots/demographics_gender.jpg")
barplot(table(merged$GENDER), main = "Distribution of Participants by gender")
dev.off()


dfing = as.data.frame(merged$FINGERS)
fingers_summary = dfing %>% group_by(merged$FINGERS) %>%dplyr::summarise(count = n())
ggplot(fingers_summary, aes(x=c("1-2","3-4","5-6","7-8","9-10"), y = count)) +
  geom_bar(data = fingers_summary[ fingers_summary$`merged$FINGERS` %in% c("1-2","3-4","5-6","7-8","9-10"),],stat = 'identity') + 
  ggtitle("Distribution by number of fingers used in typing") +
  labs(x = "no of fingers used", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14),axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_fingers.jpg")


#keyboard type
dev.off()
dfkbdtype = as.data.frame(merged$KEYBOARD_TYPE)
kbdtype_summary = dfkbdtype %>% group_by(merged$KEYBOARD_TYPE) %>%dplyr::summarise(count = n())
ggplot(kbdtype_summary, aes(x=c("full", "laptop", "on-screen", "small"), y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by keyboard type used for the test") +
  labs(x = "keyboard type", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14),axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_kbdtype.png")


#keyboard layout
dev.off()
dflayout = as.data.frame(merged$LAYOUT)
layout_summary = dflayout %>% group_by(merged$LAYOUT) %>%dplyr::summarise(count = n())
ggplot(layout_summary, aes(x=layout_summary$'merged$LAYOUT', y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by keyboard layout used for the test") +
  labs(x = "keyboard layout", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_layout.png")


#OS type
dev.off()
dfos = as.data.frame(merged$OS)
os_summary = dfos %>% group_by(merged$OS) %>%dplyr::summarise(count = n())
ggplot(os_summary, aes(x=os_summary$'merged$OS', y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution of Participants by OS used for the test") +
  labs(x = "OS used", y = "no of participants") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5, size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_OS.png")


#BROWSER
dev.off()
dfbrowser = as.data.frame(merged$BROWSER)
browser_summary = dfbrowser %>% group_by(merged$BROWSER) %>%dplyr::summarise(count = n())
ggplot(browser_summary, aes(x=browser_summary$'merged$BROWSER', y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution of Participants by Browser used for the test") +
  labs(x = "Browser used", y = "no of participants") +
  theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5, size = 14), axis.title = element_text(size = 14) , plot.title = element_text(size = 16))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_browser.png")


#typing course taken or not
dev.off()
dfcourse = as.data.frame(merged$HAS_TAKEN_TYPING_COURSE)
training_summary = dfcourse %>% group_by(merged$HAS_TAKEN_TYPING_COURSE) %>%dplyr::summarise(count = n())
ggplot(training_summary, aes(x=c("not taken typing course","has taken a course"), y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by whether typing course taken") +
  labs(x = "", y = "no of participants") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_training.png")


#hours of typing each day
dev.off()
dfhoursp = transform(merged$TIME_SPENT_TYPING, hours_group = cut(as.numeric(as.character(merged$TIME_SPENT_TYPING)), 
                                                                   breaks=c(0.0,2.0,5.0,10.0,15.0,20.0,24.0), 
                                                                   labels=c('0-2','3-5','6-10','11-15','16-20','21-24'), include.lowest = TRUE))
hours_summary = dfhoursp %>% group_by(hours_group) %>%dplyr::summarise(count = n())
ggplot(na.omit(hours_summary), aes(x=na.omit(hours_group), y = count)) +
  geom_bar(stat = 'identity') + 
  ggtitle("Distribution by no of hours spent typing daily") +
  labs(x = "No of hours", y = "no of participants") +
  theme(axis.text=element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/demographics_hours.png")


dev.off()
dfage = transform(merged$AGE, 
                  age_group = cut(as.numeric(as.character(merged$AGE)), breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf), 
                                  labels=c('0-5','6-10','11-15','16-20','21-25','26-30','31-35','36-40','41-45','46-50',
                                           '51-55','56-60','61-65','66-70','71-75','76-80','80+'), 
                                  include.lowest = TRUE))

age_summary = dfage %>% group_by(age_group) %>%dplyr::summarise(count = n())
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

#=======================================================================================
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#=======================================================



#form query to load data into R.Use the COMPLETE table that has rows with
#ERROR_RATE < 25, and all demographic information are complete.

basics = merged[,c(25,20)]
colnames(basics) = c("WPM","ERROR_RATE")
#boxplots of the WPM and ERROR_RATE measures


png(filename="/home/vvkd/Desktop/RTypingMaster/plots/avgWPM.png")
boxplot(as.matrix(basics[1]), 
        ylab="average Words Per Minute speed in the typing test for a participant", 
        outpch=3, 
        main = "Distribution of WPM values across participants")
dev.off()

png(filename="/home/vvkd/Desktop/RTypingMaster/plots/avgWPM_hist.png")
hist(basics[,1], main = "Histogram of WPM distribution by no of participants", breaks = 30, xlab = "WPM", ylab = "no of participants")
dev.off()

png(filename="/home/vvkd/Desktop/RTypingMaster/plots/avgWPM_density.png")
hist(basics[,1], main = "Histogram of WPM distribution by participants",  breaks = 30, xlab = "WPM", ylab = "probability density", prob = TRUE)
lines(density(basics[,1]))
dev.off()

png(filename="/home/vvkd/Desktop/RTypingMaster/plots/avgERR.png")
boxplot(as.matrix(basics[2]), ylab = "average ERROR_RATE (%) in the typing test for a participant ", outpch=3, 
        main = "Distribution of Error Rates (%) across Participants")
dev.off()

png(filename="/home/vvkd/Desktop/RTypingMaster/plots/avgerr_hist.png")
hist(basics[,2], main = "Histogram of Error distribution by no of participants", xlab = "Error Rate (%)", ylab = "no of participants")
dev.off()

png(filename="/home/vvkd/Desktop/RTypingMaster/plots/avgerr_density.png")
hist(basics[,2], main = "Histogram of Error distribution by participants", xlab = "Error Rate (%)", ylab = "probability density", prob = TRUE)
lines(density(basics[,2]))
dev.off()

ggplot(basics, aes(x = WPM, y = ERROR_RATE)) + geom_point(alpha=1/20) + ggtitle("WPM vs Error Rate(%) across all participants") + 
  labs(x = "WPM", y = "Error Rate  (%)")
ggsave(file="/home/vvkd/Desktop/RTypingMaster/plots/wpmerr.png")

#dev.new()
ggplot(basics, aes(x = WPM, y = ERROR_RATE)) + 
  geom_smooth() + 
  ggtitle("WPM vs Error Rate across all participants with confidence region") + labs(x = "WPM", y = "Error Rate  (%)")
ggsave(file="/home/vvkd/Desktop/RTypingMaster/plots/wpmerrconf.png")

#View(merged[1:10,])
#colour <- ifelse(merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1, "blue", 
#                 (ifelse((merged$FINGERS=="1-2" | merged$FINGERS=="3-4" | merged$FINGERS=="5-6") & merged$HAS_TAKEN_TYPING_COURSE == 0,"red", "white")))

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/touchtype_perf.png", height = 2400, width = 1800, pointsize = 30)
par(mfrow= c(4, 2), mar = c(4,4,7,1))
for (wpmv in c(0,60,80,100)) {
  plot(merged[merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1 & merged$AVG_WPM_15>wpmv,"AVG_IKI"], 
       merged[merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1 & merged$AVG_WPM_15>wpmv,"ECPC"], col = alpha("blue", 0.1), 
       xlim = c(80,800+0.074*wpmv^2-14*wpmv), ylim = c(0,0.5-wpmv/250), pch = 'o', cex = 1,
       main = paste("mean IKI and ECPC distributions for WPM > ",as.character(wpmv)), xlab = "mean IKI (ms)", ylab = "ECPC")
  plot(merged[merged$FINGERS != "9-10" & merged$HAS_TAKEN_TYPING_COURSE == 0 & merged$AVG_WPM_15>wpmv,"AVG_IKI"],
       merged[merged$FINGERS != "9-10" & merged$HAS_TAKEN_TYPING_COURSE == 0 & merged$AVG_WPM_15>wpmv,"ECPC"], col = alpha("red", 0.1), 
       xlim = c(80,800+0.074*wpmv^2-14*wpmv), ylim = c(0,0.5-wpmv/250), pch = 'o', cex = 1,
       main = paste("mean IKI and ECPC distributions for WPM > ",as.character(wpmv)), xlab = "mean IKI (ms)", ylab = "ECPC")
}
mtext("Comparative study of touch vs non-touch typists by varying WPM speeds", line = 80, at = 65, cex = 1.5)
dev.off()

#colour2 <- ifelse(merged$HAS_TAKEN_TYPING_COURSE == 1, "blue", "red")

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/typingcourse_perf.png", height = 2400, width = 1800, pointsize = 30)
par(mfrow= c(4, 2), mar = c(4,4,7,1))
for (wpmv in c(0,60,80,100)) {
  plot(merged[merged$HAS_TAKEN_TYPING_COURSE == 1 & merged$AVG_WPM_15>wpmv,"AVG_IKI"], 
       merged[merged$HAS_TAKEN_TYPING_COURSE == 1 & merged$AVG_WPM_15>wpmv,"ECPC"], col = alpha("blue", 0.1), 
       xlim = c(80,800+0.074*wpmv^2-14*wpmv), ylim = c(0,0.5-wpmv/300), pch = 'o', 
       main = paste("mean IKI and ECPC distributions for WPM > ",as.character(wpmv)), xlab = "mean IKI (ms)", ylab = "ECPC")
  plot(merged[merged$HAS_TAKEN_TYPING_COURSE == 0 & merged$AVG_WPM_15>wpmv,"AVG_IKI"],
       merged[merged$HAS_TAKEN_TYPING_COURSE == 0 & merged$AVG_WPM_15>wpmv,"ECPC"], col = alpha("red", 0.1), 
       xlim = c(80,800+0.074*wpmv^2-14*wpmv), ylim = c(0,0.5-wpmv/300), pch = 'o', 
       main = paste("mean IKI and ECPC distributions for WPM > ",as.character(wpmv)), xlab = "mean IKI (ms)", ylab = "ECPC")
}
mtext("Comparative study of participants based on typing course by varying WPM speeds", line = 80, at = 65, cex = 1.5)
dev.off()

ggplot(basics, aes(x = WPM, y = ERROR_RATE)) + 
  geom_smooth() + 
  coord_cartesian(xlim=c(20,80)) + 
  ggtitle("Error Rate variation by WPM values between [20,80] interval") + labs(x = "WPM", y = "Error Rate  (%)") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 ))
ggsave(file="/home/vvkd/Desktop/RTypingMaster/plots/wpmerrconf_zoom.png")


#======================================================================================

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/mean_ikis.png")
plot(density(iki), xlim = c(0,1000) , xlab = "mean IKI (ms)", main = "mean IKI distribution by participants: density plot")
dev.off()

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/mean_keypress.png")
plot(density(keypress), xlim = c(50,250) , xlab = "mean Key-press duration (ms)", 
     main = "mean Key-press duration distribution by participants: density plot")
dev.off()


ggplot(merged, aes(x = AVG_WPM_15 , y = KSPC)) + geom_smooth() + 
  ggtitle("WPM vs KSPC across all participants with confidence region") +
  labs(x = "WPM", y = "Keystrokes per character (KSPC)")+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_kspc.png")

ggplot(merged, aes(x = AVG_WPM_15 , y = ECPC)) + geom_smooth() + 
  ggtitle("WPM vs Error Corrections (per Character presented)(ECPC) with confidence region") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 )) +
  labs(x = "WPM", y = "Error Corrections per Character (ECPC)")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_ecpc.png")

#library(raster)
myCol = colorRampPalette(c("dark red","red","orange","yellow","green","light blue","dark blue","violet"))(max(merged$AVG_WPM_15))

colour <- ifelse(merged$AVG_WPM_15>=80, "blue", 
                 (ifelse(merged$AVG_WPM_15 <=40,"red", "green")))
colour2 = myCol[merged$AVG_WPM_15]
png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/wpmgrps.png", height = 900, width = 900, pointsize = 18)
layout(matrix(c(1,2,3,4,4,4,4,4,4), 3, 3, byrow = TRUE))
plot(merged[merged["AVG_WPM_15"]>=80,c("AVG_IKI","ECPC")], pch='.', col = "blue",xlim = c(50,800), ylim = c(0,0.3))
ticks<-seq(0,800,100)
axis(1,at=ticks,labels=ticks)
plot(merged[merged["AVG_WPM_15"]>40 & merged["AVG_WPM_15"]<80,c("AVG_IKI","ECPC")], pch='.', col = "green",xlim = c(50,800), ylim = c(0,0.3))
ticks<-seq(0,800,100)
axis(1,at=ticks,labels=ticks)
plot(merged[merged["AVG_WPM_15"]<=40,c("AVG_IKI","ECPC")], pch='.', col = "red",xlim = c(50,800), ylim = c(0,0.3))
ticks<-seq(0,800,100)
axis(1,at=ticks,labels=ticks)
plot(merged[,c("AVG_IKI","ECPC")], col = alpha(colour2,0.2),xlim = c(50,800), ylim = c(0,0.3), 
     main = "speed heatmap: blue = fastest, red = slowest",
     xlab = "mean IKI (ms)", ylab = "No of Error Corrections per character (ECPC)", pch='.',cex=2)
ticks<-seq(0,800,100)
axis(1,at=ticks,labels=ticks)
dev.off()

ggplot(merged, aes(x = AVG_IKI , y = ECPC)) + geom_smooth() + 
  ggtitle("mean IKI(ms) vs Error Corrections (per Character presented) with confidence region") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/iki_ecpc.png")

wpm_fing = summarySE(merged, c("AVG_WPM_15"), c("FINGERS"))
ggplot(data = wpm_fing[wpm_fing$FINGERS %in% c("1-2","3-4","5-6","7-8","9-10"),], aes(y = AVG_WPM_15, x = FINGERS)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = AVG_WPM_15-ci, ymax = AVG_WPM_15+ci)) +
  coord_cartesian(ylim = c(30,70)) +
  ggtitle("Average WPM values by no. of fingers used in typing") + 
  labs(x = "No of Fingers", y = "WPM values with 95% confidence interval") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_fingers.png")

iki_fing = summarySE(merged, c("AVG_IKI"), c("FINGERS"))
ggplot(data = iki_fing[iki_fing$FINGERS %in% c("1-2","3-4","5-6","7-8","9-10"),], aes(y = AVG_IKI, x = FINGERS)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = AVG_IKI-ci, ymax = AVG_IKI+ci)) + 
  coord_cartesian(ylim = c(150,400)) +
  ggtitle("Average IKI values by no. of fingers used in typing") + 
  labs(x = "No of Fingers", y = "IKI(ms) values with 95% confidence interval") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/iki_fingers.png")

wpm_kbdtype = summarySE(merged, c("AVG_WPM_15"), c("KEYBOARD_TYPE"))
ggplot(data = wpm_kbdtype, aes(y = AVG_WPM_15, x = KEYBOARD_TYPE)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = AVG_WPM_15-ci, ymax = AVG_WPM_15+ci)) + 
  ggtitle("Average WPM values by keyboard size used in typing") + 
  labs(x = "Keyboard size", y = "WPM values with 95% confidence interval")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_kbdtype.png")

iki_kbdtype = summarySE(merged, c("AVG_IKI"), c("KEYBOARD_TYPE"))
ggplot(data = iki_kbdtype, aes(y = AVG_IKI, x = KEYBOARD_TYPE)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = AVG_IKI-ci, ymax = AVG_IKI+ci)) + 
  ggtitle("Average IKI values by no. of fingers used in typing") + 
  labs(x = "Keyboard size", y = "IKI(ms) values with 95% confidence interval")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/iki_kbdtype.png")
#*************************************************************
#correlation matrix of numerical features

#ikis of specific bigrams
library(reshape2)

load("/home/vvkd/Desktop/RTypingMaster/bigram_data.Rda")



library(stringr)
bigram_iki_c = as.data.frame(bigram_iki_c)
let_space = bigram_iki_c[,colnames(bigram_iki_c)[str_sub(colnames(bigram_iki_c),-2,-1)=="32"]][,-1]
space_let = bigram_iki_c[,3:28]

hf_bigram = bigram_iki_c[,c("72 69","84 72","73 78","69 82","65 78","82 69","78 68","83 76","70 82","85 80","67 76","65 80")]
bigrams = c("he", "th", "in", 
            "er", "an", "re", "nd", "sl", "fr","up", "cl","ap")
names(hf_bigram) = bigrams
hf_bigram_m = na.omit(melt(hf_bigram))
hf_bigram_s = summarySE(hf_bigram_m, "value", "variable")

ggplot(data = hf_bigram_s, aes(y = value, x = variable)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = value-ci, ymax = value+ci)) + 
  geom_rect(aes(xmin = 0, xmax = 7.5, ymin = -Inf, ymax = Inf), fill = "blue", alpha = 0.02) +
  geom_rect(aes(xmin = 7.5, xmax = 12.5, ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.02) +
  ggtitle("Average IKI values for high(in blue) / low(in red) frequency bigrams") + 
  labs(x = "bigrams", y = "IKI(ms) values with 95% confidence interval") +
  
  ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/fqbigrams.png")


names(let_space) = letters
let_space_m = na.omit(melt(let_space))
let_space_s = summarySE(let_space_m, "value", "variable")

ggplot(data = let_space_s, aes(y = value, x = variable)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = value-ci, ymax = value+ci)) + 
  ggtitle("Average IKI values for letter-spacebar pairs") + 
  labs(x = "letter", y = "IKI(ms) values with 95% confidence interval")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/letter_space.png")

names(space_let) = letters
space_let_m = na.omit(melt(space_let))
space_let_s = summarySE(space_let_m, "value", "variable")

ggplot(data = space_let_s, aes(y = value, x = variable)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = value-ci, ymax = value+ci)) + 
  ggtitle("Average IKI values for spacebar-letter pairs") + 
  labs(x = "letter", y = "IKI(ms) values with 95% confidence interval")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/space_letter.png")

hf_bigram_med = apply(hf_bigram, 2, function(x) median(na.omit(x)))
png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/fqbigram_med.png")
barplot(hf_bigram_med, names.arg = bigrams, xlab = "bigrams", ylab = "median IKI (ms)", 
        main = "Median IKIs of high / low frequency bigrams")
dev.off()

let_space_med = apply(let_space, 2, function(x) median(na.omit(x)))
png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/letter_space_med.png", height = 800, width = 800, pointsize = 15)
barplot(let_space_med, names.arg = letters, xlab = "letter", ylab = "median IKI (ms)", 
        main = "Median IKIs of letter-spacebar pairs")
dev.off()

space_let_med = apply(space_let, 2, function(x) median(na.omit(x)))
png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/space_letter_med.png", height = 800, width = 800, pointsize = 15)
barplot(space_let_med, names.arg = letters, xlab = "letter", ylab = "median IKI (ms)", 
        main = "Median IKIs of spacebar-letter pairs")
dev.off()

#rollover times and correlation plot

library(corrplot)


merged = merge(merged, bigram_iki_c[,c("PARTICIPANT_ID","65 65","67 67", "76 76", "65 76", "83 80", "65 84", "83 84", "65 78", "71 72", "82 84","65 32", "84 32", "80 32","65 83","76 75")], 
               by="PARTICIPANT_ID", all.x = TRUE)
colnames(merged)[36:50] <- c("aa","cc","ll","al","sp","at","st","an","gh","rt", "a_", "t_","p_","as","lk")



load("/home/vvkd/Desktop/RTypingMaster/rollover_data.Rda")

ggplot(rollover_time, aes(ROR)) + 
  geom_histogram(aes(y = ..density..),binwidth = 0.03, col = "red",fill = "black", alpha = 0.2) + 
  geom_density(col = 4) +
  labs(x = "rollover ratio", y = "density") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 )) +
  ggtitle("density distribution of rollover ratio over participants")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/rollover_density.png")

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/rollover_hist.png")
hist(rollover_time$ROR, xlab = "rollover ratio", 
     ylab = "Number of participants", main  = "distribution of rollover ratio among participants")
dev.off()

merged = merge(merged, rollover_time, all.x = TRUE, by = "PARTICIPANT_ID")
ggplot(data = merged, aes(x = merged$ROR, y = merged$AVG_WPM_15)) + geom_smooth() +
  ggtitle("WPM speeds vs rollover ratio") +
  labs(x = "rollover ratio", y = "WPM")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_rollover.png")

rot_s0_wpm = summarySE(data = merged[merged[,"ROR"] < 0.05 & merged[,"FINGERS"]!="10+",] , measurevar = "AVG_WPM_15", groupvars = "FINGERS" )
rot_s20_wpm = summarySE(data = merged[merged[,"ROR"] < 0.2 & merged[,"ROR"] > 0.05 & merged[,"FINGERS"]!="10+",] , 
                        measurevar = "AVG_WPM_15", groupvars = "FINGERS" )
rot_s50_wpm = summarySE(data = merged[merged[,"ROR"] < 0.4 & merged[,"ROR"]> 0.2 & merged[,"FINGERS"]!="10+" ,] , 
                        measurevar = "AVG_WPM_15", groupvars = "FINGERS" )
rot_s100_wpm = summarySE(data = merged[merged[,"ROR"] > 0.5 & merged[,"FINGERS"]!="10+",] , measurevar = "AVG_WPM_15", groupvars = "FINGERS" )
mergedrot = rbind(merge(rot_s0_wpm,"<0.05"),merge(rot_s20_wpm,"0.05-0.2"),
                  merge(rot_s50_wpm,"0.2-0.4"),merge(rot_s100_wpm,">0.5"))

ggplot(mergedrot, aes(factor(FINGERS), AVG_WPM_15, fill = y)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin = AVG_WPM_15-ci, ymax = AVG_WPM_15+ci), position = "dodge") +
  ggtitle("WPM vs rollover ratio, grouped by no of fingers used in typing") +
  labs(y = "WPM", x = "no of fingers used (y = rollover ratio)") +
  scale_fill_brewer(palette = "Set1")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_rollover_fingers.png")

ggplot(mergedrot, aes(factor(y), AVG_WPM_15, fill = FINGERS)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin = AVG_WPM_15-ci, ymax = AVG_WPM_15+ci), position = "dodge") +
  ggtitle("WPM vs no of fingers used in typing, grouped by rollover ratio") +
  labs(y = "WPM", x = "rollover ratio") +
  scale_fill_brewer(palette = "Set1")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_fingers_rollover.png")

colnames(merged)[53] <- "R"
colnames(merged)[20] <- "Err"
colnames(merged)[29] <- "Kp"
colnames(merged)[28] <- "IKI"
colnames(merged)[25] <- "WPM15"

corr = cor(merged[sapply(merged, is.numeric)], use="pairwise.complete.obs")
rownames(corr)[7] = "WPM"
colnames(corr)[7] = "WPM"
rownames(corr)[14] = "KS"
colnames(corr)[14] = "KS"
jpeg(filename = "/home/vvkd/Desktop/RTypingMaster/plots/corrplot_num2.jpg", height = 1200, width = 1200  , pointsize = 15)
corrplot.mixed(corr[c(2,6,7,10,11,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,35),
                    c(2,6,7,10,11,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,35)], 
               number.cex = 1.2, lower = "square", upper = "number", 
               title = "Correlation chart of variables", mar=c(0,0,1,0))
dev.off()
#merged2 = merge(merged, bigram_iki_c, by = "PARTICIPANT_ID", all.x = TRUE)

char_pairs_l = c("aa","cc","ll","al","sp","at","st","an","gh","rt", "a_", "t_","p_","as","lk")
char_pairs = c("65 65","67 67", "76 76", "65 76", "83 80", "65 84", "83 84", "65 78", "71 72", "82 84","65 32", "84 32", "80 32","65 83","76 75")

for (cp in char_pairs_l) {
  png(filename = gsub(" ","",paste("/home/vvkd/Desktop/RTypingMaster/plots/iki_fst_",cp,".png"), 
                      fixe=TRUE), pointsize = 11)
  
  set.seed(42)
  hist(merged[merged[cp]<500 & merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE==1 & merged$WPM15 >=80,c(cp)], 
       prob = TRUE, breaks = 100, 
       main = paste("Slow & Fast touch typists (red <=40 and blue >=80wpm), letter pair = ",cp), 
       xlab = "IKI (ms)",xlim = c(0,500), ylim=c(0,0.025),col='skyblue',border=F)
  hist(merged[merged[cp]<500 & merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE==1 & merged$WPM15 <=40,c(cp)], 
       prob = TRUE, breaks = 100, 
       xlab = "IKI (ms)", ylim=c(0,0.025),add=T,col=scales::alpha('red',.5),border=F)
  dev.off()
}

for (cp in char_pairs_l) {
  png(filename = gsub(" ","",paste("/home/vvkd/Desktop/RTypingMaster/plots/iki_tn_",cp,".png"), 
                      fixed=TRUE), pointsize = 12)
  set.seed(42)
  hist(merged[merged[cp]<500 & merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE==1,c(cp)], 
       prob = TRUE, breaks = 100, 
       main = paste(" touch (blue) vs non-touch (red) typists , letter pair = ",cp), 
       xlab = "IKI (ms)", xlim = c(0,500), ylim=c(0,0.02),col='skyblue',border=F)
  hist(merged[merged[cp]<500 & merged$FINGERS != "9-10" & merged$HAS_TAKEN_TYPING_COURSE==0,c(cp)], 
       prob = TRUE, breaks = 100, 
       xlab = "IKI (ms)", add = T,col=scales::alpha('red',.5), ylim=c(0,0.025),border=F)
  dev.off()
}

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/ror_fs.png", height = 1000, width = 1000, pointsize = 20)
par(mfrow= c(2,1))
hist(merged$R[merged$WPM15>=80],
     main = "Distribution of rollover ratio in Fast typists (WPM >= 80)", xlab = "rollover ratio")
hist(merged$R[merged$WPM15<=40],
     main = "Distribution of rollover ratio in Slow typists (WPM <= 40)", xlab = "rollover ratio")
dev.off()





hist(merged$R[merged$FINGERS=="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1],
     main = "All Touch typists", xlab = "rollover ratio")
mtext("Distribution of rollover ratio", side = 3, line=3, at = 1, cex = 1.25)
hist(merged$R[merged$FINGERS!="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 0],
     main = "All Non-Touch typists", xlab = "rollover ratio")
hist(merged$R[merged$FINGERS=="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1 & merged$WPM15>=80],
     main = "Fast Touch typists (WPM >= 80)", xlab = "rollover ratio")
hist(merged$R[merged$FINGERS!="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 0 & merged$WPM15>=80],
     main = "Slow Touch typists (WPM >= 80)", xlab = "rollover ratio")

dev.off()


#============================= clustering ==================================================
