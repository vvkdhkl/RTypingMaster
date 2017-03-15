library(mclust)
library(gridExtra)
library(gplots)
library(ggplot2)




t = as.data.frame(na.omit(ds[,c(2:77)]))

plots_folder = "/home/vvkd/Desktop/RTypingMaster/plots/clustering_clara/"
mod2 = Mclust(t[,13:77], modelNames = "VEI")
#mod2 = Mclust(t[,5:92], modelNames = "VVI")
a = summary(mod2)

for (n in c(5:11)){
  png(filename = gsub(" ","",paste(plots_folder,"clusters_info",n,".png")), height = 1200, width = 1000, pointsize = 20)
  par(mfrow= c(3,3))
  
  for (k in c(1:9)) {
    cc = t[mod2$classification==k,]
    hist(cc[,n], probability = TRUE, breaks=20, main = paste("distribution of", 
                colnames(t)[n],"in cluster",k), xlim = quantile(t[,n], c(.01,.99)), xlab = colnames(t)[n])
    lines(density(cc[,n]))
    abline(v = mean(cc[,n]),
           col = "royalblue",
           lwd = 2)
    abline(v = median(cc[,n]),
           col = "red",
           lwd = 2)
  }
  dev.off()
}


for (k in c(1:9)) {
  png(filename = gsub(" ","",paste(plots_folder,"cluster_detail",k,".png")), height = 1200, width = 1000, pointsize = 20)
  par(mfrow= c(3,3))
  tt = data.frame(a$mean[1:7,k])
  textplot(tt)
  for (n in c(5:11)){
    cc = t[mod2$classification==k,]
    hist(cc[,n], probability = TRUE, breaks=20, main = paste("distribution of", colnames(t)[n],"in cluster",k), 
         xlab = colnames(t)[n])
    lines(density(cc[,n]))
    abline(v = mean(cc[,n]),
           col = "royalblue",
           lwd = 2)
    abline(v = median(cc[,n]),
           col = "red",
           lwd = 2)
  }
  dev.off()
}

ikibigrams = c("72 69","84 72","73 78","69 82","65 78","82 69","78 68")
bigrams = c("he", "th", "in", "er", "an", "re", "nd")

for (n in ikibigrams) {
  png(filename = gsub(" ","",paste(plots_folder,"cluster_bigrams",bigrams[ikibigrams==n],".png")), height = 1200, width = 1000, pointsize = 20)
  par(mfrow= c(3,3))
  for (k in c(1:9)){
    cc = t[mod2$classification==k,]
    hist(cc[,n], probability = TRUE, breaks=20, main = paste("distribution in cluster",k), xlab = colnames(t)[n])
    lines(density(cc[,n]))
    abline(v = mean(cc[,n]),
           col = "royalblue",
           lwd = 2)
    abline(v = median(cc[,n]),
           col = "red",
           lwd = 2)
  }
  dev.off()
}


myggplots = list()
 
  for (k in c(1:9)){
    cc = t[mod2$classification==k,]
    
    myggplots[[k]] <- ggplot(cc, aes(x = WPM15, y = ECPC), main = paste("distribution in cluster",k), 
           xlim = quantile(t[,WPM15], c(.01,.99))) +  geom_smooth() + ggtitle(paste("cluster", k)) + 
          coord_cartesian(xlim = quantile(t$WPM15, c(.01,.99)))
    
  }
  ggsave(gsub(" ","",paste(plots_folder,"clusters_wpm_ecpc.png")), arrangeGrob(grobs = myggplots))

  
  myggplots2 = list()
 
  for (k in c(1:9)){
    cc = t[mod2$classification==k,]
    
    myggplots2[[k]] <- ggplot(cc, aes(x = WPM15, y = KSPC), main = paste("distribution in cluster",k), 
                  xlim = quantile(t[,WPM15], c(.01,.99))) +  geom_smooth() + ggtitle(paste("cluster", k))
    
  }
  ggsave(gsub(" ","",paste(plots_folder,"clusters_wpm_kspc.png")), arrangeGrob(grobs = myggplots2))
  
  
  sc <- spark_connect(master = "local")
  tsc = copy_to(sc, t)
 kmeans8 = tsc %>% ml_kmeans(centers=8, iter.max = 50, features = tbl_vars(tsc)[12:76])

 
 plots_folder = "/home/vvkd/Desktop/RTypingMaster/plots/clustering_kmeans_norm_all/"
 
 for (n in c(5:11)){
   png(filename = gsub(" ","",paste(plots_folder,"clusters_info",n,".png")), height = 1200, width = 1000, pointsize = 20)
   par(mfrow= c(3,3))
   
   for (k in c(0:7)) {
     cc = t[predict(kmeans9)==k,]
     hist(cc[,n], probability = TRUE,  main = paste("distribution of", colnames(t)[n],"in cluster",k), xlim = quantile(t[,n], c(.01,.99)),  xlab = colnames(t)[n])
     lines(density(cc[,n]))
     abline(v = mean(cc[,n]),
            col = "royalblue",
            lwd = 2)
     abline(v = median(cc[,n]),
            col = "red",
            lwd = 2)
   }
   dev.off()
 }
 
 
 for (k in c(1:7)) {
   png(filename = gsub(" ","",paste(plots_folder,"cluster_detail",k,".png")), height = 1200, width = 1000, pointsize = 20)
   par(mfrow= c(3,3))
   #tt = data.frame(kmeans8$centers[k])
   #textplot(tt)
   for (n in c(5:11)){
     cc = t[predict(kmeans8)==k,]
     hist(cc[,n], probability = TRUE, main = paste("distribution of", colnames(t)[n],"in cluster",k), 
          xlab = colnames(t)[n])
     lines(density(cc[,n]))
     abline(v = mean(cc[,n]),
            col = "royalblue",
            lwd = 2)
     abline(v = median(cc[,n]),
            col = "red",
            lwd = 2)
   }
   dev.off()
 }
 
 ikibigrams = c("72 69","84 72","73 78","69 82","65 78","82 69","78 68")
 bigrams = c("he", "th", "in", "er", "an", "re", "nd")
 
 for (n in ikibigrams) {
   png(filename = gsub(" ","",paste(plots_folder,"cluster_bigrams",bigrams[ikibigrams==n],".png")), height = 1200, width = 1000, pointsize = 20)
   par(mfrow= c(3,3))
   for (k in c(1:4)){
     cc = t[1:4000,][mod$classification==k,]
     hist(cc[,n], probability = TRUE, main = paste("distribution in cluster",k), xlab = colnames(t)[n])
     lines(density(cc[,n]))
     abline(v = mean(cc[,n]),
            col = "royalblue",
            lwd = 2)
     abline(v = median(cc[,n]),
            col = "red",
            lwd = 2)
   }
   dev.off()
 }
 
 library(ggplot2)
 myggplots = list()
 
 for (k in c(1:7)){
   cc = t[predict(kmeans8)==k,]
   
   myggplots[[k]] <- ggplot(cc, aes(x = WPM15, y = ECPC), main = paste("distribution in cluster",k), 
                            xlim = quantile(t[,WPM15], c(.01,.99))) +  geom_smooth() + ggtitle(paste("cluster", k)) + 
     coord_cartesian(xlim = quantile(t$WPM15, c(.01,.99)))
   
 }
 ggsave(gsub(" ","",paste(plots_folder,"clusters_wpm_ecpc.png")), arrangeGrob(grobs = myggplots))
 
 
 myggplots2 = list()
 
 for (k in c(1:7)){
   cc = t[predict(kmeans8)==k,]
   
   myggplots2[[k]] <- ggplot(cc, aes(x = WPM15, y = KSPC), main = paste("distribution in cluster",k), 
                             xlim = quantile(t[,WPM15], c(.01,.99))) +  geom_smooth() + ggtitle(paste("cluster", k))
   
 }
 ggsave(gsub(" ","",paste(plots_folder,"clusters_wpm_kspc.png")), arrangeGrob(grobs = myggplots2))
 
 
 #-------------------------------------
 distri3 = dbGetQuery(conn, "SELECT a.*, b.PARTICIPANT_ID FROM (SELECT child.TEST_SECTION_ID AS TSID,
              child.KEYCODE AS KEYCODE, parent.KEYCODE AS nxKEYCODE,
              CASE WHEN parent.PRESS_TIME-child.PRESS_TIME BETWEEN 0 AND 5000 THEN parent.PRESS_TIME-child.PRESS_TIME ELSE NULL END as IKI  
                      from KEYSTROKES_COMPLETE child,      KEYSTROKES_COMPLETE parent        
              WHERE child.ORDERID=parent.ORDERID-1 AND child.TEST_SECTION_ID=parent.TEST_SECTION_ID        
                      AND (parent.KEYCODE=32 or parent.KEYCODE between 65 and 90)        
                      and (child.KEYCODE=32 or child.KEYCODE between 65 and 90)) a, TEST_SECTIONS_COMPLETE b 
              WHERE a.TSID=b.TEST_SECTION_ID AND b.PARTICIPANT_ID BETWEEN 100000 AND 110000 ORDER BY a.KEYCODE, a.nxKEYCODE;")
 
 df_grp = split(distri3, distri3$PARTICIPANT_ID)
 
 plot(density(df_grp$`100001`$IKI/mean(df_grp$`100001`$IKI)), lwd = 0.06, ylim=c(0,2))
 for (x in df_grp){
   
   lines(density(na.omit(x$IKI/mean(na.omit(x$IKI)))),col="black", lwd=0.06)
 }
 
 library(fitdistrplus)
 
 hwt = read.csv("/home/vvkd/Downloads/summary4.csv")
 
 descdist(hwt$mean_iki, discrete = FALSE)
 fitWei1 = fitdist(hwt$mean_iki, "weibull")
 fitExp1 = fitdist(hwt$mean_iki, "exp")
 
 fitExp2 = fitdist(ds$IKI, "exp")
 fitWei2 = fitdist(ds$IKI, "weibull")
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/distfit_iki_hwtwei.png", height = 2000, width = 2000, pointsize = 30)
 par(mar = c(4,4,7,1))
 plot(fitWei1) #, main = "Fitting the How-We-Type IKIs as a Weibull distribution")
 mtext("Fitting the How-We-Type IKIs as a Weibull distribution", line = 5, at = 0.5, cex = 1.5)
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/distfit_iki_tmwei.png", height = 2000, width = 2000, pointsize = 30)
 par(mar = c(4,4,7,1))
 plot(fitWei2) #, main = "Fitting the TypingMaster IKIs as a Weibull distribution")
 mtext("Fitting the TypingMaster IKIs as a Weibull distribution", line = 5, at = 0.5, cex = 1.5)
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/distfit_iki_hwtexp.png", height = 2000, width = 2000, pointsize = 30)
 par(mar = c(4,4,7,1))
 plot(fitExp1) #, main = "Fitting the How-We-Type IKIs as a Weibull distribution")
 mtext("Fitting the How-We-Type IKIs as an Exponential distribution", line = 5, at = 0.5, cex = 1.5)
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/distfit_iki_tmexp.png", height = 2000, width = 2000, pointsize = 30)
 par(mar = c(4,4,7,1))
 plot(fitExp2) #, main = "Fitting the How-We-Type IKIs as a Weibull distribution")
 mtext("Fitting the TypingMaster IKIs as an Exponential distribution", line = 5, at = 0.5, cex = 1.5)
 dev.off()

 exletters = c(letters, "_") 
 a = data.frame()
 t = hwt_mg4[hwt_mg4$PARTICIPANT_ID %in% hwt_mg$PARTICIPANT_ID[grps4==4],]
 for (l in 1:27) {
   for (m in 1:27) {
     if (l==27 && m==27 && ("__" %in% colnames(t))) {
       a[l,m][[1]] = list(t[,"__"])
     } else if (l==27 && (gsub(" ","",paste("_", exletters[m])) %in% colnames(t))){
       a[l,m][[1]] = list(t[,gsub(" ","",paste("_", exletters[m]))])
     } else if (m==27 && (gsub(" ","",paste(exletters[l],"_")) %in% colnames(t))){
       a[l,m][[1]] = list(t[,gsub(" ","",paste(exletters[l],"_"))])
     } else if (gsub(" ","",paste(exletters[l], exletters[m])) %in% colnames(t))
       a[l,m][[1]] = list(t[,gsub(" ","",paste(exletters[l], exletters[m]))])
     else
       a[l,m][[1]] = list(array(0,nrow(t)))
   }
 }
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/heatmap_dist.png", height = 4000, width = 4000)
 par(mfrow= c(27, 27))
 mediki = data.frame()
 sdiki = data.frame()
 meaniki = data.frame()
 
 
 for (i in 1:27){
   for (j in 1:27) {
     if (sum(!(is.na(a[i,j][[1]])))>1) {
       plot(density(a[i,j][[1]], na.rm = TRUE), main = paste(exletters[i], exletters[j]))
     } 
     mediki[i,j] = median(a[i,j][[1]], na.rm = TRUE)
     sdiki[i,j] = sd(a[i,j][[1]], na.rm = TRUE)
     meaniki[i,j] = mean(a[i,j][[1]], na.rm = TRUE)
   }
 }
 mtext("Heatmap distributions of IKI by bigrams", line = 80, at = 65, cex = 1.5)
 dev.off()
 
 
 heatmap(as.matrix(mediki),NA,NA, labRow = exletters[strtoi(rownames(a))], labCol = exletters[strtoi(rownames(a))])

 #---------------------------- 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/heatmap_mean.png", height = 2000, width = 2000, pointsize = 30)
 heatmap(as.matrix(meaniki), labRow = exletters[strtoi(rownames(a))], labCol = exletters[strtoi(rownames(a))], 
         main = "heatmap: mean bigram IKI (blue: slower, red: faster)",  xlab = "second character in bigram", ylab = "first character in bigram", col = hmcol)
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/heatmap_median.png", height = 2000, width = 2000, pointsize = 30)
 heatmap(as.matrix(mediki), labRow = exletters[strtoi(rownames(a))], labCol = exletters[strtoi(rownames(a))], 
         main = "heatmap: median bigram IKI (blue: slower, red: faster)",  xlab = "second character in bigram", ylab = "first character in bigram", col = hmcol)
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/heatmap_sd.png", height = 2000, width = 2000, pointsize = 30)
 heatmap(as.matrix(sdiki), labRow = exletters[strtoi(rownames(a))], labCol = exletters[strtoi(rownames(a))], 
         main = "heatmap: Standard Deviation of bigram IKI (blue: high SD, red: low SD)",  xlab = "second character in bigram", ylab = "first character in bigram", col = hmcol)
 dev.off()
 
 
 #---------------------------
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/bigramiki_dist_hwt.png")
 hist(as.numeric(hwt2$mean_iki), probability = TRUE, main = "average IKIs of all bigrams typed by all participants", xlab = "avg IKI (ms)")
 lines(density(hwt2$mean_iki))
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/bigramiki_onepart_hwt.png")
 hist(as.numeric(hwt2$mean_iki[hwt2$Part_id==12772]), probability = TRUE, main = "average IKIs of all bigrams typed by all participants", xlab = "avg IKI (ms)")
 lines(density(hwt2$mean_iki))
 dev.off()
 
 png(filename = "/home/vvkd/Desktop/RTypingMaster/plots/bigramiki_onepart_tm.png")
 hist(as.numeric(ds[ds$PARTICIPANT_ID==100575,13:165]), probability = TRUE, main = "avg IKIs of unique bigrams for one participant in TypingMaster", xlab = "avg IKI (ms)")
 lines(density(na.omit(as.numeric(ds[ds$PARTICIPANT_ID==100575,13:165]))))
 dev.off()
 
 