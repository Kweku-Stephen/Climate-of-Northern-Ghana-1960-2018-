df<-read.csv("spel.csv",header = TRUE, sep = ",")


library(imputeTS)
#na.interpolation(df$X3)
Bole<-na.kalman(df$B_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Bolga<-na.kalman(df$BO_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Damango<-na.kalman(df$DA_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Navorongo<-na.kalman(df$NA_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Tamale<-na.kalman(df$TA_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Wa<-na.kalman(df$W_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Yendi<-na.kalman(df$YE_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Zualungu<-na.kalman(df$ZU_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Vea<-na.kalman(df$VE_SPEL, model = "StructTS", smooth = TRUE, nit = -1)
Babile<-na.kalman(df$BA_SPEL, model = "StructTS", smooth = TRUE, nit = -1)

write.csv(rbind(Bole,Bolga,Damango,Navorongo,Tamale,Wa,Yendi,Zualungu,Vea,Babile), "spell.csv")

library(ggplot2)
#tiff("B_len.tiff")
ggplot(df, aes(x=df$Years, y = Babile)) + geom_line(color = "blue", size = 1) + theme_minimal()+ stat_smooth(color = "red", fill = "gray", method = "loess")+  labs(x = "Years")+ labs(y = "Number of Days") + labs(title = "Longest Dry for Babile")

#dev.off()
