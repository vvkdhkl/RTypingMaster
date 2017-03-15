#This script assumes a connection to the database has been defined as conn
#Refer to dbconnect.R script for that

ffload("/home/vvkd/Desktop/RTypingMaster/ff_data/savedff/lsff", overwrite = TRUE)

#mean IKI matrix for all letters
mean_lat_2 = aggregate(LATENCY ~ KEYCODE, space_letter, mean)

#count of each letter in letter-spacebar pairs
count_lat_2 = aggregate(LATENCY ~ KEYCODE, space_letter, length)

#correlate means with counts
cor(mean_lat_2[2], count_lat_2[2])

#median IKI matrix for all letters
median_lat_2 = aggregate(LATENCY ~ KEYCODE, space_letter, median)

dev.off()
#plot distributions, mean and median
letters = c('a','b','c','d','e','f','g','h',
            'i','j','k','l','m','n','o','p',
            'q','r','s','t','u','v','w','x','y','z')

barplot(as.matrix(mean_lat_2[,2]), 
        beside=TRUE, 
        names.arg = letters, 
        xlab="letters in pairs with and followed by spacebar", 
        ylab = "mean IKI (ms) between pressdown events", 
        main = "mean IKI from spacebar to letterkey in ms")
ggsave(file="plots/meanIKIsl.png")
#dev.new()
barplot(as.matrix(median_lat_2[,2]), 
        beside=TRUE, 
        names.arg = letters, 
        xlab="letters in pairs with and followed by spacebar", 
        ylab = "median IKI (ms) between pressdown events", 
        main = "median IKI from spacebar to letter key in ms")
ggsave(file="plots/medianIKIsl.png")

#dev.new()
boxplot(formula = LATENCY ~ KEYCODE, 
        data = space_letter, 
        names = letters, 
        xlab="letters in pairs with and followed by spacebar", 
        ylab = "IKI (ms) between pressdown events", 
        main = "Distributions of spacebar-letter IKI",  
        outpch = 3, outcex = .4)
ggsave(file="plots/distIKIsl.png")
