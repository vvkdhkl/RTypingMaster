#letter_space = dbGetQuery(conn, query)
ffload("/home/vvkd/Desktop/RTypingMaster/ff_data/savedff/lsff", overwrite = TRUE)

#mean IKI matrix for all letters
mean_lat = ffdfdply(x = letter_space , split = as.character(x$TEST_SECTION_ID), 
                    FUN=function(df) {
                      df = data.table(df)
                      df
                      as.data.table(df)
                    },trace=T)
#mean_lat = aggregate(LATENCY ~ KEYCODE, letter_space, mean)

#count of each letter in letter-spacebar pairs
count_lat = aggregate(LATENCY ~ KEYCODE, letter_space, length)

#correlate means with counts
cor(mean_lat[2], count_lat[2])

#median IKI matrix for all letters
median_lat = aggregate(LATENCY ~ KEYCODE, letter_space, median)

#plot distributions, mean and median
letters = c('a','b','c','d','e','f','g','h','i',
            'j','k','l','m','n','o','p','q','r',
            's','t','u','v','w','x','y','z')
dev.off()
barplot(as.matrix(mean_lat[,2]), 
        beside=TRUE, 
        names.arg = letters,
        xlab="letters in pairs with spacebar", 
        ylab = "mean IKI (ms) between pressdown events", 
        main = "mean IKI between letter-spacebar pairs")
ggsave(file="plots/meanIKIls.png")
#dev.new()
barplot(as.matrix(median_lat[,2]), 
        beside=TRUE, 
        names.arg = letters, 
        xlab="letters in pairs with spacebar",
        ylab = "median IKI (ms) between pressdown events", 
        main = "median IKI between letter-spacebar pairs")
ggsave(file="plots/medianIKIls.png")

#dev.new()
boxplot(formula = LATENCY ~ KEYCODE, 
        data = letter_space, 
        names = letters, 
        xlab="letters in pairs with spacebar", 
        ylab = "IKI (ms) between pressdown events", 
        main = "Distributions of letter-spacebar IKI",  
        outpch = 3, outcex = .4)
ggsave(file="plots/distIKIls.png")
