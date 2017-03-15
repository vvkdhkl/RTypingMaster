#function to generate summary statistics

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
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#******************************************************
library(data.table)

query1 = "select * from TS_IKI_KP;"
computed = as.data.table(dbGetQuery(conn, query1))

query2 = "select * from PARTICIPANTS_COMPLETE where ERROR_RATE BETWEEN 0 AND 25;"
complete = as.data.table(dbGetQuery(conn, query2))

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
merged = as.data.frame(merged[TSCOUNT>=15])

iki = merged$AVG_IKI
keypress = merged$AVG_KEYPRESS

library(ggplot2)


png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/mean_ikis.png")
plot(density(iki), xlim = c(0,1000) , xlab = "mean IKI (ms)", main = "mean IKI distribution by participants: density plot")
dev.off()

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/mean_keypress.png")
plot(density(keypress), xlim = c(50,250) , xlab = "mean Key-press duration (ms)", 
     main = "mean Key-press duration distribution by participants: density plot")
dev.off()


ggplot(merged, aes(x = AVG_WPM_15 , y = KSPC)) + geom_smooth() + 
  ggtitle("WPM vs KSPC across all participants with confidence region") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_kspc.png")

ggplot(merged, aes(x = AVG_WPM_15 , y = ECPC)) + geom_smooth() + 
  ggtitle("WPM vs Error Corrections (per Character presented)(ECPC) with confidence region") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_ecpc.png")

colour <- ifelse(merged$AVG_WPM_15>=80, "blue", 
                 (ifelse(merged$AVG_WPM_15 <=40,"red", "green")))

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
plot(merged[,c("AVG_IKI","ECPC")], col = alpha(colour,0.2),xlim = c(50,800), ylim = c(0,0.3), 
     main = "Groups by WPM speeds, blue: >=80wpm, green: 40-80wpm, red: <=40wpm",
     xlab = "mean IKI (ms)", ylab = "No of Error Corrections per character (ECPC)", pch='.',cex=2)
ticks<-seq(0,800,100)
axis(1,at=ticks,labels=ticks)
dev.off()

ggplot(merged, aes(x = AVG_IKI , y = ECPC)) + geom_smooth() + 
  ggtitle("mean IKI(ms) vs Error Corrections (per Character presented) with confidence region") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 16 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/iki_ecpc.png")

wpm_fing = summarySE(merged, c("AVG_WPM_15"), c("FINGERS"))
ggplot(data = wpm_fing, aes(y = AVG_WPM_15, x = FINGERS)) + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = AVG_WPM_15-ci, ymax = AVG_WPM_15+ci)) +
  coord_cartesian(ylim = c(30,70)) +
  ggtitle("Average WPM values by no. of fingers used in typing") + 
  labs(x = "No of Fingers", y = "WPM values with 95% confidence interval") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 ))
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_fingers.png")

iki_fing = summarySE(merged, c("AVG_IKI"), c("FINGERS"))
ggplot(data = iki_fing, aes(y = AVG_IKI, x = FINGERS)) + 
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
  labs(x = "proportion of prevalence of overlapped keys", y = "density") +
  theme(axis.text=element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 18 )) +
  ggtitle("density distribution of rollover typing characteritic over participants")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/rollover_density.png")

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/rollover_hist.png")
hist(rollover_time$ROR, xlab = "proportion of prevalence of overlapped keys", 
     ylab = "Number of participants", main  = "distribution of rollover typing characteritic over participants")
dev.off()

merged = merge(merged, rollover_time, all.x = TRUE, by = "PARTICIPANT_ID")
ggplot(data = merged, aes(x = merged$ROR, y = merged$AVG_WPM_15)) + geom_smooth() +
  ggtitle("WPM speeds vs prevalence of rollover typing") +
  labs(x = "ratio of consecutive keypairs that have overlaps between them", y = "WPM")
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
  ggtitle("WPM vs ratio of rollover keypairs, grouped by no of fingers used in typing") +
  labs(y = "WPM", x = "no of fingers used (y = ratio of rollover keypairs)") +
  scale_fill_brewer(palette = "Set1")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_rollover_fingers.png")

ggplot(mergedrot, aes(factor(y), AVG_WPM_15, fill = FINGERS)) + 
  geom_bar(stat="identity", position = "dodge") +
  geom_errorbar(aes(ymin = AVG_WPM_15-ci, ymax = AVG_WPM_15+ci), position = "dodge") +
  ggtitle("WPM vs no of fingers used in typing, grouped by ratio of rollover keypairs") +
  labs(y = "WPM", x = "ratio of rollover keypairs") +
  scale_fill_brewer(palette = "Set1")
ggsave(file = "/home/vvkd/Desktop/RTypingMaster/plots/wpm_fingers_rollover.png")

colnames(merged)[53] <- "R"
colnames(merged)[20] <- "Err"
colnames(merged)[29] <- "Kp"
colnames(merged)[28] <- "IKI"
colnames(merged)[25] <- "WPM15"

corr = cor(merged[sapply(merged, is.numeric)], use="pairwise.complete.obs")
png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/corrplot_num2.png", height = 1200, width = 1200  , pointsize = 15)
corrplot.mixed(corr[c(2,6,7,10,11,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,35),
                    c(2,6,7,10,11,14,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,35)], 
               number.cex = 1.2, lower = "square", upper = "number", 
               title = "Correlation chart of variables", mar=c(0,0,1,0))
dev.off()
merged2 = merge(merged, bigram_iki_c, by = "PARTICIPANT_ID", all.x = TRUE)

char_pairs_l = c("aa","cc","ll","al","sp","at","st","an","gh","rt", "a_", "t_","p_","as","lk")
char_pairs = c("65 65","67 67", "76 76", "65 76", "83 80", "65 84", "83 84", "65 78", "71 72", "82 84","65 32", "84 32", "80 32","65 83","76 75")

for (cp in char_pairs) {
  png(filename = gsub(" ","",paste("/home/vvkd/Desktop/RTypingMaster/plots/iki_fst_",char_pairs_l[match(cp, char_pairs)],".png"), 
                      fixe=TRUE), pointsize = 12)
  
  set.seed(42)
  hist(merged2[merged2[cp]<500 & merged2$FINGERS == "9-10" & merged2$HAS_TAKEN_TYPING_COURSE==1 & merged2$WPM15 <=40,c(cp)], 
       prob = TRUE, breaks = 100, 
       main = paste("Slow touch typists (<=40wpm), letter pair = ",char_pairs_l[match(cp, char_pairs)]), 
       xlab = "IKI (ms)", ylim=c(0,0.025),col='skyblue',border=F)
  hist(merged2[merged2[cp]<500 & merged2$FINGERS == "9-10" & merged2$HAS_TAKEN_TYPING_COURSE==1 & merged2$WPM15 >=80,c(cp)], 
       prob = TRUE, breaks = 100, 
       main = paste("Fast touch typists (>=80wpm), letter pair = ",char_pairs_l[match(cp, char_pairs)]), 
       xlab = "IKI (ms)", ylim=c(0,0.025),add=T,col=scales::alpha('red',.5),border=F)
  dev.off()
}

for (cp in char_pairs) {
  png(filename = gsub(" ","",paste("/home/vvkd/Desktop/RTypingMaster/plots/iki_tn_",char_pairs_l[match(cp, char_pairs)],".png"), 
                      fixed=TRUE), pointsize = 12)
  set.seed(42)
  hist(merged2[merged2[cp]<500 & merged2$FINGERS == "9-10" & merged2$HAS_TAKEN_TYPING_COURSE==1,c(cp)], 
       prob = TRUE, breaks = 100, 
       main = paste(" touch typists , letter pair = ",char_pairs_l[match(cp, char_pairs)]), 
       xlab = "IKI (ms)", ylim=c(0,0.02),col='skyblue',border=F)
  hist(merged2[merged2[cp]<500 & merged2$FINGERS != "9-10" & merged2$HAS_TAKEN_TYPING_COURSE==0,c(cp)], 
       prob = TRUE, breaks = 100, 
       main = paste("Non touch typists , letter pair = ",char_pairs_l[match(cp, char_pairs)]), xlab = "IKI (ms)"
       , add = T,col=scales::alpha('red',.5), ylim=c(0,0.025),border=F)
  dev.off()
}

png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/ror_fs.png", height = 1000, width = 1000, pointsize = 20)
par(mfrow= c(2,1))
hist(merged$R[merged$WPM15>=80],
     main = "Distribution of rollover prevalence ratio in Fast typists (WPM >= 80)", xlab = "rollover proportion")
hist(merged$R[merged$WPM15<=40],
     main = "Distribution of rollover prevalence ratio in Slow typists (WPM <= 40)", xlab = "rollover proportion")
dev.off()



png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/ror_nt.png", height = 1200, width = 1000, pointsize = 25)
par(mfrow= c(3,1))
hist(merged$R[merged$FINGERS=="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1],
     main = "Distribution of rollover prevalence ratio in Touch typists", xlab = "rollover proportion")
hist(merged$R[merged$FINGERS!="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 0],
     main = "Distribution of rollover prevalence ratio in Non-Touch typists", xlab = "rollover proportion")
hist(merged$R[merged$FINGERS=="9-10" & merged$HAS_TAKEN_TYPING_COURSE == 1 & merged$WPM15>=80],
     main = "Distribution of rollover prevalence ratio in  Fast Touch typists (WPM >= 80)", xlab = "rollover proportion")
dev.off()