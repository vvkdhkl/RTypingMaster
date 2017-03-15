ffload("/home/vvkd/Desktop/RTypingMaster/ff_data/savedff/bigramff", overwrite = TRUE)

bigrams = c("he", "th", "in", 
            "er", "an", "re", "nd", "at", "on", "nt", "ha", "es", "st", "en")

mean_bigram_IKI = aggregate(IKI ~ KEYCODE_PAIR, bigram_iki, mean)

median_bigram_IKI = aggregate(IKI ~ KEYCODE_PAIR, bigram_iki, median)

count_bigram_IKI = aggregate(IKI ~ KEYCODE_PAIR, bigram_iki, length)

cor(mean_bigram_IKI[2], count_bigram_IKI[2])

dev.off
barplot(as.matrix(mean_bigram_IKI[,2]), 
        beside=TRUE, 
        names.arg = bigrams, 
        xlab="bigrams", 
        ylab = "mean IKI (ms) between pressdown events", 
        main = "mean IKI between bigram pairs")
ggsave(file="plots/meanIKIbigrams.png")
#dev.new()

barplot(as.matrix(median_bigram_IKI[,2]), 
        beside=TRUE, 
        names.arg = bigrams, 
        xlab="bigrams", 
        ylab = "median IKI (ms) between pressdown events", 
        main = "median IKI between bigram pairs")
ggsave(file="plots/medianIKIbigrams.png")
#dev.new()

boxplot(formula = IKI ~ KEYCODE_PAIR, 
        data = bigram_iki, 
        names = bigrams, 
        xlab="bigrams", 
        ylab = "IKI (ms) between pressdown events", 
        main = "Distributions of bigram IKIs",  
        outpch = 3, outcex = .4)
ggsave(file="plots/distIKIbigrams.png")