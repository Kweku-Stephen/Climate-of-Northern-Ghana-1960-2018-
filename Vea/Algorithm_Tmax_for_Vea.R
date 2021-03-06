
#Maximum Temperature

head(data1)
Tmax <- data.frame(data1$dates, data1$Tmax)
names(Tmax) <- c("date","Tmax")
head(Tmax)

#......................................................................................................#

#Mean Annual Temperature
Tmax.xts <- xts(Tmax$Tmax,Tmax$date)
Tmax.ann.xts <- apply.yearly(Tmax.xts, mean, na.rm=T)
head(Tmax.ann.xts)
colSums(is.na(Tmax.ann.xts))

#conversion to d.f
Tmax.ann.df <- data.frame(
  date=index(Tmax.ann.xts), 
  coredata(Tmax.ann.xts)
)

Tmax.ann.df$date <- as.Date(
  Tmax.ann.df$date, 
  format="%Y-%m-%d"
)

names(Tmax.ann.df)[2] <- "Tmax"
#estimating values for missing cases in this dataset (2014 - 2015)
Tmax.ann.df$Tmax <- na_kalman(
  Tmax.ann.df$Tmax, 
  model = "StructTS",
  smooth=T, nit = -1
)

head(Tmax.ann.df)
colSums(is.na(Tmax.ann.df))

MK <- MannKendall(Tmax.ann.df[,2])
summary(MK)

write.csv(Tmax.ann.df,file = "Annual Max Tmpof Vea(1972-2017).csv")

ggplot(Tmax.ann.df, aes(x=date, y=Tmax)) + geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm) +  
  labs(title="Maximum Temperature (1972-2017)\n Vea", 
       x="Year", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "4 years", date_labels = "%Y") + 
  annotate("text",x=Tmax.ann.df[Tmax.ann.df[,1] == "1990-12-31", ]$date,
           cex=5, label="P-value = 1.5736e-05", y=36.5) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Annual Max Tmmp of Vea (1972-2017).png",
         width = 1000, height = 750)
dev.off()

sumx <- summary(Tmax.ann.df)
write.csv(sumx,
          file="summary stats for annual max Tmp of Vea.csv")
#or
sumn <- data.frame(Tmax.ann.df[Tmax.ann.df[,2] == max(Tmax.ann.df[,2]), ],
                   Tmax.ann.df[Tmax.ann.df[,2] == min(Tmax.ann.df[,2]), ])

sumn[2,c(1,2)] <- sumn[1, c(3,4)] 
sumn[-c(3,4)] -> sumn
#sumxn[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumn) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn,
          file = "summary stats for ann Max Tmp (1960-2017).csv")




#Mean Monthly Temperature
head(Tmax.xts)
Tmax.mn.xts <- apply.monthly(Tmax.xts, mean,na.rm=T)

#conversion (d.f)
Tmax.mn.df <- data.frame(
  date=index(Tmax.mn.xts),
  coredata(Tmax.mn.xts)
)

names(Tmax.mn.df)[2] <- "Tmax"
head(Tmax.mn.df)
Tmax.mn.df$date <- as.Date(
  Tmax.mn.df$date,
  format="%Y-%m-%d"
)

#Estimating missing cases
#subsetting all individual months
Janu <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="01")
#Janu$Tmax <- na.kalman(Janu$Tmax, model="StructTS",smooth = T, nit = -1)
#
Febr <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="02")
#Febr$Tmax <- na.kalman(Febr$Tmax, model="StructTS",smooth = T, nit = -1)
#
Marc <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="03")
#Marc$Tmax <- na.kalman(Marc$Tmax, model="StructTS",smooth = T, nit = -1)
#
Apri <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="04")
#Apri$Tmax <- na.kalman(Apri$Tmax, model="StructTS",smooth = T, nit = -1)
#4 missing
Mayy <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="05")
#Mayy$Tmax <- na.kalman(Mayy$Tmax, model="StructTS",smooth = T, nit = -1)
#
Junee <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="06")
#Junee$Tmax <- na.kalman(Junee$Tmax, model="StructTS",smooth = T, nit = -1)
#
Julyy <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="07")
#Julyy$Tmax <- na.kalman(Julyy$Tmax, model="StructTS",smooth = T, nit = -1)
#
Augus <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="08")
#Augus$Tmax <- na.kalman(Augus$Tmax, model="StructTS",smooth = T, nit = -1)

Septe <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="09")
#Septe$Tmax <- na.kalman(Septe$Tmax, model="StructTS",smooth = T, nit = -1)

Octo <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="10")
#Octo$Tmax <- na.kalman(Octo$Tmax, model="StructTS",smooth = T, nit = -1)

Novem <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="11")
#Novem$Tmax <- na.kalman(Novem$Tmax, model="StructTS",smooth = T, nit = -1)

Decem <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="12")
#Decem$Tmax <- na.kalman(Decem$Tmax, model="StructTS",smooth = T, nit = -1)

#creating a dataframe with the above

#Tmax.mn.df.new <- data.frame(Janu$date, Janu$Tmax, Febr$Tmax, Marc$Tmax,
#                             Apri$Tmax,Mayy$Tmax,Junee$Tmax,Julyy$Tmax,
#                              Augus$Tmax,Septe$Tmax,Octo$Tmax,
#                             Novem$Tmax,Decem$Tmax)


#Averaging
J.x <- mean(Janu$Tmax, na.rm=T)
F.x <- mean(Febr$Tmax, na.rm=T)
M.x <- mean(Marc$Tmax, na.rm=T)
A.x <- mean(Apri$Tmax, na.rm=T)
Ma.x <- mean(Mayy$Tmax, na.rm=T)
Ju.x <- mean(Junee$Tmax, na.rm=T)
Jl.x <- mean(Julyy$Tmax, na.rm=T)
Au.x <- mean(Augus$Tmax, na.rm=T)
S.x <- mean(Septe$Tmax, na.rm=T)
O.x <- mean(Octo$Tmax, na.rm=T)
N.x <- mean(Novem$Tmax, na.rm=T)
D.x <- mean(Decem$Tmax, na.rm=T)

Mn.m.tx.v <- c(J.x, F.x,M.x,A.x,Ma.x,Ju.x,Jl.x,
               Au.x,S.x,O.x,N.x,D.x)

date.mn
Mn.mn.tmx <- data.frame(date.mn, Mn.m.tx.v)
write.csv(Mn.mn.tmx, file = "Mean Monthly Tmax of Vea.csv")

ggplot(Mn.mn.tmx, aes(x=date.mn, y=Mn.m.tx.v)) +
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Maximum Temperature (1972-2017)\n Vea",
       x="Month", y=expression("Temperature("*~degree*c*")")) +
  scale_x_date(date_breaks= "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Max Tmmp of Vea (1972-2017).png",
         width = 1000, height = 750)
dev.off()

sumx1 <- summary(Mn.mn.tmx)
write.csv(sumx1,
          file="summary stats for mean monthly max Tmp of Vea.csv")
#or
sumn1 <- data.frame(Mn.mn.tmx[Mn.mn.tmx[,2] == max(Mn.mn.tmx[,2]), ],
                    Mn.mn.tmx[Mn.mn.tmx[,2] == min(Mn.mn.tmx[,2]), ])

sumn1[2,c(1,2)] <- sumn1[1, c(3,4)] 
sumn1[-c(3,4)] -> sumn1
sumn1[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn1) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn1,
          file = "summary stats for Monthly Max Tmp (1960-2017).csv")




# seasonal Cycle of Max Tmp of Tamale
head(Tmax)
data <- Tmax
names(data) <- c("dates","Rain")
head(data)

mean_dailytmx_Vea<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                      Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                      Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,
                      Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                      Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,
                      Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,
                      Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,
                      Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,
                      Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,
                      Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,
                      Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Veatmx <-data.frame(
  c(1:366),
  mean_dailytmx_Vea
)

write.csv(Seasonal_cycle_Veatmx, 
          file = "data_for_seasonal_cycle_of_Max tmp of Vea (1972-2017).csv")

t <- ksmooth(
  c(1:366),
  mean_dailytmx_Vea, bandwidth = 15
)

t.1 <- data.frame(t) # converting to a dataframe
ggplot(Seasonal_cycle_Veatmx, aes(x=c(1:366), y=mean_dailytmx_Vea)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Maximum Temperature (1972-2017)\n Vea",
       x="Day", y=yyy) + 
  geom_line(data = t.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Mean Max Tmmp of Vea (1972-2017).png",
         width = 1000, height = 750)
dev.off()

sumx2 <- summary(Seasonal_cycle_Veatmx)
write.csv(sumx2,
          file="summary stats for seaonal cycle of max Tmp of Vea.csv")



#Indexing of Max tmp of Tamale (1960-2016)
lng.mn.Tmax <- mean(Tmax.ann.df$Tmax)
sd.Tmax <- sd(Tmax.ann.df$Tmax)

Tmax.ann.df.anom.v <- (Tmax.ann.df$Tmax - lng.mn.Tmax)/
  sd.Tmax

Tmax.ann.df.anom <- data.frame(
  Tmax.ann.df$date, 
  Tmax.ann.df.anom.v
)

write.csv(Tmax.ann.df.anom, 
          file="Index of Annual Max Tmp of Vea (1972-2017).csv")

ggplot(Tmax.ann.df.anom, aes(Tmax.ann.df$date, Tmax.ann.df.anom.v, ymin = 0,
                             ymax = Tmax.ann.df.anom.v)) + 
  geom_linerange(data = Tmax.ann.df.anom, aes(colour = ifelse(Tmax.ann.df.anom.v >0,
  "Positive", "Negative")),stat = "identity", position = "identity",size=2) +
  theme(legend.title = element_text(size = 2, colour = F)) + 
  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title="Maximum Temperature Anomaly (1972-2017)\n Vea",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Index of Annual MAX tMP OF vEA.png",
         width=1000, height=750)
dev.off()

sumn2 <- summary(Tmin.ann.df.anom)
#or
sumn2 <- data.frame(Tmax.ann.df.anom[Tmax.ann.df.anom[,2] == max(Tmax.ann.df.anom[,2]), ],
                    Tmax.ann.df.anom[Tmax.ann.df.anom[,2] == min(Tmax.ann.df.anom[,2]), ])

sumn2[2,c(1,2)] <- sumn2[1, c(3,4)] 
sumn2[-c(3,4)] -> sumn2
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn2) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn2,
          file = "summary stats for Ann Max Tmp  Anomaly(1960-2017).csv")






#Dividing Into two halves
#Dividing Tmax into two halves
#Dividing daily Tmax
head(Tmax)
Tmax.fh <- Tmax[(Tmax$date >= "1972-01-01" &
                   Tmax$date <= "1994-12-31"), ]

Tmax.sh <- Tmax[(Tmax$date >= "1995-01-01" & 
                   Tmax$date <= "2017-12-31"), ]
#Dividing annual Prcp
Tmax_fh <- Tmax.ann.df[(Tmax.ann.df$date >= "1972-12-31" & 
                          Tmax.ann.df$date <= "1994-12-31"), ]

Tmax_sh <- Tmax.ann.df[(Tmax.ann.df$date >= "1995-12-31" & 
                          Tmax.ann.df$date <= "2017-12-31"), ]


#Annual Tmax first half (1975-1996)
Tmax_fh
write.csv(Tmax_fh,
          file="Annual Max.Tmp of Vea (1972-1994).csv")

ggplot(Tmax_fh, aes(x=Tmax_fh$date, y=Tmax_fh$Tmax)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + scale_x_date(date_breaks = "3 years",
                                                      date_labels = "%Y") +
  labs(title='Maximum Temperature (1972-1994)\n Vea', x="Year", y=yyy) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Annual Max Tmmp of Vea (1972-1994).png",
         width = 1000, height = 750)
dev.off()

sumx3 <- summary(Tmax_fh)
write.csv(sumx3,
          file="summary Stats for Annual Mx Tmp of Vea (1972-1994).csv")
#or
sumx33 <- data.frame(Tmax_fh[Tmax_fh[,2] == max(Tmax_fh[,2]), ],
                     Tmax_fh[Tmax_fh[,2] == min(Tmax_fh[,2]), ])

sumx33[2,c(1,2)] <- sumx33[1, c(3,4)] 
sumx33[-c(3,4)] -> sumx33
#sumx33[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumx33) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumx33,
          file = "summary stats for ann Max Tmp (1960-1988).csv")



#Mean Monthly Tmax
head(Tmax.mn.df)
Tmax.mn.dffh <- Tmax.mn.df[(Tmax.mn.df$date >= "1972-01-01" &
                                  Tmax.mn.df$date <= "1994-12-31"), ]

Tmax.mn.dfsh <- Tmax.mn.df[(Tmax.mn.df$date >= "1995-01-01" & 
                                  Tmax.mn.df$date <= "2017-12-31"), ]

#First Half
head(Tmax.mn.dffh)

#Averaging all months
Janx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="01")$Tmax, na.rm = T
  )
Febx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="02")$Tmax, na.rm = T
)
Marchx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="03")$Tmax, na.rm = T
)
Aprilx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="04")$Tmax, na.rm = T
)
Mayx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="05")$Tmax, na.rm = T
)
Junex.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="06")$Tmax, na.rm = T
)
Julyx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="07")$Tmax, na.rm = T
  )
Augustx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="08")$Tmax, na.rm = T
)
Septemberx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="09")$Tmax, na.rm = T
)
Octoberx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="10")$Tmax, na.rm = T
)
Novemberx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="11")$Tmax, na.rm = T
)
Decemberx.fh <- mean(subset(Tmax.mn.dffh,format.Date(
  Tmax.mn.dffh[,1], "%m")=="12")$Tmax, na.rm = T
)

#creating a vector
mean.mn.Tmaxfh.v <- c(Janx.fh,Febx.fh,Marchx.fh,Aprilx.fh,Mayx.fh,Junex.fh,
                      Julyx.fh,Augustx.fh,Septemberx.fh,Octoberx.fh,
                      Novemberx.fh,Decemberx.fh)

date.mn 
mean.mn.Tmaxfh <- data.frame(
  date.mn, 
  mean.mn.Tmaxfh.v
)

write.csv(mean.mn.Tmaxfh,file = "Mean Maax Tmp of Vea (1972-1994).csv")

ggplot(mean.mn.Tmaxfh, aes(x=date.mn, y=mean.mn.Tmaxfh.v)) + 
  geom_line(col="firebrick", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Maximum Temperature (1972-1994)\n Vea",
       x="Month",y=yyy) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Max Tmp of Vea(1972-1994).png",
         width = 1000, height=750)
dev.off()

sumx4 <- summary(mean.mn.Tmaxfh)
write.csv(sumx4,
          file="summary Stats for mean monthly Mx Tmp of Vea (1972-1994).csv")
#or
sumx44 <- data.frame(mean.mn.Tmaxfh[mean.mn.Tmaxfh[,2] == max(mean.mn.Tmaxfh[,2]), ],
                     mean.mn.Tmaxfh[mean.mn.Tmaxfh[,2] == min(mean.mn.Tmaxfh[,2]), ])

sumx44[2,c(1,2)] <- sumx44[1, c(3,4)] 
sumx44[-c(3,4)] -> sumx44
sumx44[,1] <- data.frame(month.name[c(04,08)])
dimnames(sumx44) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumx44,
          file = "summary stats for monthly Max Tmp (1960-1988).csv")




#Annual Tmax second half (1995 - 2017)
head(Tmax_sh)
MK <- MannKendall(Tmax_sh[,2])
summary(MK)

write.csv(Tmax_sh, 
          file="Annual Tmax of Vea (1995-2017).csv")

ggplot(Tmax_sh, aes(x=Tmax_sh$date, y=Tmax_sh$Tmax)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title='Maximum Temperature (1995-2017)\n Vea',
       x="Year", y=yyy) + 
  annotate("text", x=Tmax_sh[Tmax_sh[,1] == "2011-12-31", ]$date,cex=5,
           y=35.9,label="P-value=0.7917") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Max Tmp of Vea(1995-2017).png",
         width = 1000, height=750)
dev.off()

sumx5 <- summary(Tmax_sh)
write.csv(sumx5,
          file="summary Stats for Annual Mx Tmp of Vea (1995-2017).csv")
#or
sumx55 <- data.frame(Tmax_sh[Tmax_sh[,2] == max(Tmax_sh[,2]), ],
                     Tmax_sh[Tmax_sh[,2] == min(Tmax_sh[,2]), ])

sumx55[2,c(1,2)] <- sumx55[1, c(3,4)] 
sumx55[-c(3,4)] -> sumx55
#sumx55[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumx55) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumx55,
          file = "summary stats for Ann Max Tmp (1989-2017).csv")




#second Half
head(Tmax.mn.dfsh)

#Averaging all months
Janx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="01")$Tmax, na.rm = T
)
Febx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="02")$Tmax, na.rm = T
)
Marchx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="03")$Tmax, na.rm = T
)
Aprilx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="04")$Tmax, na.rm = T
)
Mayx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="05")$Tmax, na.rm = T
)
Junex.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="06")$Tmax, na.rm = T
)
Julyx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="07")$Tmax, na.rm = T
)
Augustx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="08")$Tmax, na.rm = T
)
Septemberx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="09")$Tmax, na.rm = T
)
Octoberx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="10")$Tmax, na.rm = T
)
Novemberx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="11")$Tmax, na.rm = T
)
Decemberx.sh <- mean(subset(Tmax.mn.dfsh,format.Date(
  Tmax.mn.dfsh[,1], "%m")=="12")$Tmax, na.rm = T
)


#creating a vector
mean.mn.Tmaxsh.v <- c(Janx.sh,Febx.sh,Marchx.sh,Aprilx.sh,Mayx.sh,Junex.sh,Julyx.sh,
                      Augustx.sh,Septemberx.sh,Octoberx.sh,Novemberx.sh,Decemberx.sh)

date.mn 
mean.mn.Tmaxsh <- data.frame(
  date.mn, 
  mean.mn.Tmaxsh.v
)

write.csv(mean.mn.Tmaxsh,file = "Mean Maax Tmp of Vea (1995-2017).csv")


ggplot(mean.mn.Tmaxsh, aes(x=date.mn, y=mean.mn.Tmaxsh.v)) +
  geom_line(col="firebrick", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Maaximum Temperature (1995-2017)\n Vea",
       x="Month",y=yyy) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png,
         filename="Mean mOnthly Rainfall (1995-2017).png",
         width = 1000, height= 750)
dev.off()


sumx6 <- summary(mean.mn.Tmaxsh)
write.csv(sumx6,
          file="summary Stats for mean monthly Mx Tmp of Vea (1995-2017).csv")
#or
sumx66 <- data.frame(mean.mn.Tmaxsh[mean.mn.Tmaxsh[,2] == max(mean.mn.Tmaxsh[,2]), ],
                     mean.mn.Tmaxsh[mean.mn.Tmaxsh[,2] == min(mean.mn.Tmaxsh[,2]), ])

sumx66[2,c(1,2)] <- sumx66[1, c(3,4)] 
sumx66[-c(3,4)] -> sumx66
sumx66[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumx66) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumx66,
          file = "summary stats for monthly Max Tmp (1989-2017).csv")



#plot(mean.mn.Tmaxfh, type="l", lwd=3, col="darkblue", 
#     main="Mean Monthly Max Tmp of Tamale- 1960-2016", 
#     ylim=c(30,40), xlab="Months", ylab=yyy)

#lines(mean.mn.Tmaxsh, col="red", lwd=3, type="l")
#legend("topright", legend = c("1960-1988", "1989-2016"), 
#       col=c("darkblue","red"), bty = "n", lty = 1,lwd=3)


ggplot(mean.mn.Tmaxfh, aes(x=mean.mn.Tmaxfh[,1])) +
  geom_line(aes(y=mean.mn.Tmaxfh[,2],col="1972-1994"), lwd=2) +
  geom_line(aes(y=mean.mn.Tmaxsh[,2],col="1995-2017"), lwd=2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_colour_manual("",
                      values = c("1972-1994"="darkblue","1995-2017"="red")) +
  labs(x="Month",y=yyy, title="Mean Monthly Maximum Temperature (1972-2017)\n Vea") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png,filename="Mean Monthly Max Tmp of Vea (1972-94-2917).png",
         width = 1000, height=750)
dev.off()
  



#Max Tmp Change between two halves
Tmpx_change <- mean(Tmax_sh[,2], na.rm = T) - mean(Tmax_fh[,2], na.rm = T)
change <- expression("0.3363875(" ~ degree * c * ")")


#Monthly difference in Max Tmp b/n two halves
head(mean.mn.Tmaxfh)
head(mean.mn.Tmaxsh)

#First Half
storage <- numeric(12)
m <- c(mean.mn.Tmaxfh[,2])

for (i in 1:12) {
  storage[i] <- m[i]
}

#Second Half
storage1 <- numeric(12)
n <- c(mean.mn.Tmaxsh[,2])

for (i in 1:12) {
  storage1[i] <- n[i]
}


change_in_Max_Temp <- data.frame(
  Month=month.name,
  Change=storage1-storage)


#Percentage Change in Monthly Tmax 
storage4 <- numeric(12)
r <- c(change_in_Max_Temp[,2])

for (i in 1:12) {
  storage4[i] <- ((r*100)/mean.mn.Tmaxfh[,2])[i]
}

Percen_change_tmax <- storage4




#seasonal Cycle 
#(first half)
head(Tmax.fh)
data <- Tmax.fh
names(data) <- c('dates','Rain')
head(data)

mean_dailytmx_Veafh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                          Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                          Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,
                          Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                          Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,
                          Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,
                          Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,
                          Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,
                          Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,
                          Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,
                          Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Veatmxfh <-data.frame(
  c(1:366),
  mean_dailytmx_Veafh)

write.csv(Seasonal_cycle_Veatmxfh, 
          file = "data_for_seasonal_cycle_of_Max tmp of Vea(1972-1994).csv")

aaa<- ksmooth(
  c(1:366), 
  mean_dailytmx_Veafh, 
  bandwidth = 15)

aaa.1<- data.frame(aaa)# converting to a dataframe
ggplot(Seasonal_cycle_Veatmxfh, aes(x=c(1:366), 
                                      y=mean_dailytmx_Veafh)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of maximum Temperature (1972-1994)\n Vea",
       x="Day", y=yyy) +
  geom_line(data = aaa.1,aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Rainfall Cycle of Vea (1972-1994).png", 
         width = 1000, height=750) +
dev.off()

sum6 <- summary(Seasonal_cycle_Veatmxfh)
write.csv(sum6,
          file="Seasonal Cycle of Mean Max Tmp of Vea (1972-1994).csv")




#Second Half
head(Tmax.sh)
data <- Tmax.sh
names(data) <- c("dates","Rain")
head(data)

mean_dailytmx_Veash<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                          Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                          Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,
                          Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                          Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,
                          Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,
                          Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,
                          Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,
                          Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,
                          Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,
                          Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,
                          Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,
                          Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,
                          Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Veatmxsh <-data.frame(
  c(1:366),
  mean_dailytmx_Veash)

write.csv(Seasonal_cycle_Veatmxsh,
          file = "data_for_seasonal_cycle_of_Max tmp of Vea(1995-2017).csv")

nnn<-ksmooth(
  c(1:366), 
  mean_dailytmx_Veash, 
  bandwidth = 15)

nnn.1<- data.frame(nnn)# converting to a dataframe
ggplot(Seasonal_cycle_Veatmxsh, aes(x=c(1:366), 
                                      y=mean_dailytmx_Veash)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Maximum Temperature (1995-2017)\n Vea",
   x="Day", y=yyy) + 
  geom_line(data = nnn.1,aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename='Seaonal Cycle of Max Tmp of Vea (1995-2017).png',
         width =1000, height=750)
dev.off()



sum7<- summary(Seasonal_cycle_Veatmxsh)
write.csv(sum7,
          file="Seasonal Cycle of Mean Max Tmp of Vea (1995-2017).csv")




#lot(Seasonal_cycle_Tmaletmxfh, lwd=1, type="l", 
#    col="darkblue", main="Seasonal Max Tmp Cycle of Tamale- 1960-2017", 
#    xlab="Days", ylab=yyy)
#ines(aa.1, col="blue", lwd=3)
#ines(Seasonal_cycle_Tmaletmxsh, type="l", col="darkblue")
#ines(nn.1, lwd=3, col="red")

#egend("topright", col=c("blue","red"), 
#      legend = c("1960-1988","1989-2017"),
#      bty = "n", lty = 1, lwd=3)



ggplot(Seasonal_cycle_Veatmxfh, aes(x=Seasonal_cycle_Veatmxfh[,1])) + 
  geom_line(aes(y=Seasonal_cycle_Veatmxfh[,2]),lwd=.25) +
  geom_line(aes(y=aaa.1[,2], colour="1972-1994"), lwd=1.5) + 
  geom_line(aes(y=Seasonal_cycle_Veatmxsh[,2]),lwd=.25) +
  geom_line(aes(y=nnn.1[,2], colour="1995-2017"), lwd=1.5) +
  scale_colour_manual("",
                      values = c("1972-1994"="darkblue","1995-2017"="red")) +
  xlab("Days") + ylab(yyy) + 
  ggtitle("Seaonal Cycle of Max Tmp of Vea \n (1972-2017)") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Max Tmp of Va(1972-94-2017).png", 
         width = 1000, height = 750)
dev.off()



#Seasonal Max Tmp on Monthly Basis
head(Tmax)
Ap2 <- mean(subset(Tmax,format.Date(date, "%m")=="04")$Tmax, na.rm=T)
Ma2 <- mean(subset(Tmax,format.Date(date, "%m")=="05")$Tmax, na.rm=T)
Ju2 <- mean(subset(Tmax,format.Date(date, "%m")=="06")$Tmax, na.rm=T)
Jl2 <- mean(subset(Tmax,format.Date(date, "%m")=="07")$Tmax, na.rm=T)
Au2 <- mean(subset(Tmax,format.Date(date, "%m")=="08")$Tmax, na.rm=T)
Se2 <- mean(subset(Tmax,format.Date(date, "%m")=="09")$Tmax, na.rm=T)
Oc2 <- mean(subset(Tmax,format.Date(date, "%m")=="10")$Tmax, na.rm=T)

Seas2 <- data.frame(
  dates=Mn.mn.tmx[4:10, 1],
  Tmax=c(Ap2,Ma2,Ju2,Jl2,Au2,Se2,Oc2)
)

require(ggplot2)
ggplot(data=Seas2,
       aes(x=dates,y=Tmax)) +
  geom_line(col="firebrick", lwd=2) +
  ggtitle(" Seasonal Maximum Temperature (1972-2017)\n Vea") + xlab("Month") +
  ylab(yyy) + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Tmax.png",
         width = 1000, height = 650)
dev.off()





#Indexing
#first half
head(Tmax_fh)
lng.mn.Tmaxfh <- mean(Tmax_fh$Tmax)
sd.Tmaxfh <- sd(Tmax_fh$Tmax)

Tmax_fh_anom.v <- (Tmax_fh$Tmax - lng.mn.Tmaxfh)/
  sd.Tmaxfh

Tmax_fh_anom <- data.frame(
  Tmax_fh$date,
  Tmax_fh_anom.v
)

ggplot(Tmax_fh_anom, aes(Tmax_fh$date, Tmax_fh_anom.v, 
                         ymin = 0,ymax = Tmax_fh_anom.v)) + 
  geom_linerange(data = Tmax_fh_anom, aes(colour = ifelse(Tmax_fh_anom.v >0,
                 "Positive","Negative")),stat = "identity",position="identity",size=2) +
  theme(legend.title = element_text(size = 2, colour = F)) + 
  (scale_x_date(date_breaks="4 year",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title="Index of Annual Maximum Tmp of Vea \n (1972-1994)", 
       x="Years", y="Index")
dev.copy(png, filename="Seasonal Cycle of Annual MAX tMP OF Vea(1972-1994.png",
         width=1000, height=750)
dev.off()


#Second half
head(Tmax_sh)
lng.mn.Tmaxsh <- mean(Tmax_sh$Tmax)
sd.Tmaxsh <- sd(Tmax_sh$Tmax)

Tmax_sh_anom.v <- (Tmax_sh$Tmax - lng.mn.Tmaxsh)/
  sd.Tmaxsh

Tmax_sh_anom <- data.frame(
  Tmax_sh$date, 
  Tmax_sh_anom.v
)

ggplot(Tmax_sh_anom, aes(Tmax_sh$date, Tmax_sh_anom.v, ymin = 0,
                         ymax = Tmax_sh_anom.v)) + 
  geom_linerange(data = Tmax_sh_anom, aes(colour = ifelse(Tmax_sh_anom.v >0,
                 "Positive","Negative")),stat="identity",position="identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = F)) +
  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title="Index of Annual Maximum Tmp of Vea \n (1995-2017)",
       x="Years", y="Index")

dev.copy(png, filename="Seasonal Cycle of Annual MAX tMP OF Vea(1995-2017).png",
         width=1000, height=750)
dev.off()
