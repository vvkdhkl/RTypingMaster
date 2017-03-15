#form query to load data into R.Use the COMPLETE table that has rows with
#ERROR_RATE < 25, and all demographic information are complete.

query = "select WPM, ERROR_RATE from PARTICIPANTS_COMPLETE where WPM>0 AND ERROR_RATE BETWEEN 0 AND 25;"
basics = dbGetQuery(conn, query)

#boxplots of the WPM and ERROR_RATE measures
#dev.new()
library(ggplot2)
#dev.off()
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

colour <- ifelse(merged$FINGERS == "9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1, "blue", 
                 (ifelse((merged$FINGERS=="1-2" | merged$FINGERS=="3-4" | merged$FINGERS=="5-6") & merged$HAS_TAKEN_TYPING_COURSE == 0,"red", "white")))

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/touchtype_perf.png", height = 2100, width = 2000, pointsize = 35)
par(mfrow= c(2, 2), mar = c(4,4,7,1))
for (wpmv in c(0,60,80,100)) {
  plot(merged[merged$WPM>wpmv,"AVG_IKI"], merged[merged$WPM>wpmv,"ECPC"], col = alpha(colour, 0.5), 
       xlim = c(80,800-6.5*wpmv), ylim = c(0,0.5-wpmv/250), pch = 8, 
       main = paste("mean IKI and ECPC distributions for WPM > ",as.character(wpmv)), xlab = "mean IKI (ms)", ylab = "ECPC")
}
mtext("Comparative study of touch vs non-touch typists by varying WPM speeds", line = 35, at = 65, cex = 1.5)
dev.off()

colour2 <- ifelse(merged$HAS_TAKEN_TYPING_COURSE == 1, "blue", "red")
                 
png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/typingcourse_perf.png", height = 2000, width = 2000, pointsize = 35)
par(mfrow= c(2, 2), mar = c(4,4,7,1))
for (wpmv in c(0,60,80,100)) {
  plot(merged[merged$WPM>wpmv,"AVG_IKI"], merged[merged$WPM>wpmv,"ECPC"], col = alpha(colour2, 0.5), 
       xlim = c(80,900-6*wpmv), ylim = c(0,0.5-wpmv/300), pch = 8, 
       main = paste("mean IKI and ECPC distributions for WPM > ",as.character(wpmv)), xlab = "mean IKI (ms)", ylab = "ECPC")
}
mtext("Comparative study of participants based on typing course by varying WPM speeds", line = 33, at = 40, cex = 1.5)
dev.off()l

ggplot(basics, aes(x = WPM, y = ERROR_RATE)) + 
  geom_smooth() + 
  coord_cartesian(xlim=c(20,80)) + 
  ggtitle("Error Rate variation by WPM values between [20,80] interval") + labs(x = "WPM", y = "Error Rate  (%)") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 ))
ggsave(file="/home/vvkd/Desktop/RTypingMaster/plots/wpmerrconf_zoom.png")
