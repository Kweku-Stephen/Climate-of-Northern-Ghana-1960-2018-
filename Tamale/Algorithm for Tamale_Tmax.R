###############################################################################################

head(data1)

Tmax <- data.frame(
   data1$dates, 
   data1$Tmax
   )

names(Tmax) <- c("date","Tmax")
head(Tmax)

#......................................................................................................#

#Mean Annual Temperature
Tmax.xts <- xts(
   Tmax$Tmax,
   Tmax$date
   )

Tmax.ann.xts <- apply.yearly(
   Tmax.xts, 
   mean, 
   na.rm=T
   )

head(Tmax.ann.xts)
colSums(is.na(Tmax.ann.xts))

#conversion to d.f
Tmax.ann.df <- data.frame(
   date=index(Tmax.ann.xts), 
   coredata(Tmax.ann.xts
            )
   )

Tmax.ann.df$date <- as.Date(
   Tmax.ann.df$date, 
   format="%Y-%m-%d"
   )

names(Tmax.ann.df)[2] <- "Tmax"

#estimating values for missing cases in this dataset (2014 - 2015)
Tmax.ann.df$Tmax <- na.kalman(
   Tmax.ann.df$Tmax, 
   model = "StructTS",
   smooth=T, 
   nit = -1
   )

head(Tmax.ann.df)
MK <- MannKendall(Tmax.ann.df[,2])
summary(MK)


write.csv(Tmax.ann.df, 
          file = "Mean ANNUAL Max Tmp of Tamale(1960-2016).csv")

ggplot(Tmax.ann.df, aes(x=date, y=Tmax)) +
   geom_line(col="firebrick", lwd=2) + 
   geom_smooth(method = lm) +  
   labs(title="Maximum Temperature (1960-2016)\n Tamale", 
        x="Year", y=expression("Temperature("*~degree*c*")")) + 
   scale_x_date(date_breaks= "4 years", date_labels = "%Y") + 
   annotate("text", x=Tmax.ann.df[Tmax.ann.df[,1] == "1982-12-31", ]$date, 
            y = 35.3, label="P-value = <2.22e-16", cex = 5) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Annual Maximum Tmp of Tamale (1960-2016).png", 
         width=1000, height = 750)
dev.off()
sumx <- summary(Tmax.ann.df)
write.csv(sumx,
          file = "summary stats for annual max tmp of Tamle.csv")

sumxx <- data.frame(Tmax.ann.df[Tmax.ann.df[,2] == max(Tmax.ann.df[,2]), ],
                    Tmax.ann.df[Tmax.ann.df[,2] == min(Tmax.ann.df[,2]), ])

sumxx[2,c(1,2)] <- sumxx[1, c(3,4)] 
sumxx[-c(3,4)] -> sumxx
#sumxx[,1] <- data.frame(month.name[c(09,01)])
dimnames(sumxx) <- list(
   c("Max:","Min:"), 
   c("Year","Tmax")
)  

write.csv(sumxx,
          file = "summary stats for Max Tmp (1960-2016).csv")




#Mean Monthly Temperature
head(Tmax.xts)
Tmax.mn.xts <- apply.monthly(
   Tmax.xts, 
   mean,na.rm=T
   )

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
      
      Tmax.mn.df.new <- data.frame(Janu$date, Janu$Tmax, Febr$Tmax, Marc$Tmax,Apri$Tmax,
                                   Mayy$Tmax,
                     Junee$Tmax,Julyy$Tmax,Augus$Tmax,Septe$Tmax,Octo$Tmax,
                     Novem$Tmax,Decem$Tmax)

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

Mn.m.tx.v <- c(J.x, F.x,M.x,A.x,Ma.x,Ju.x,Jl.x,Au.x,S.x,O.x,N.x,D.x)

date.mn

Mn.mn.tmx <- data.frame(
   date.mn, 
   Mn.m.tx.v
   )

write.csv(Mn.mn.tmx, 
          file="Mean Monthly Tmax of Tamale(1960-2016).csv")

ggplot(Mn.mn.tmx, aes(x=date.mn, y=Mn.m.tx.v)) + 
   geom_line(col="firebrick", lwd=2) +
   labs(title="Mean Monthly Maximum Temperature (1960-2016)\n Tamale",
        x="Month", y=expression("Temperature("*~degree*c*")")) + 
   scale_x_date(date_breaks= "1 month", date_labels = "%b") +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Maximum Tmp of Tamale (1960-2016).png", 
         width=1000, height = 750)
dev.off()
sumx1 <- summary(Mn.mn.tmx)
write.csv(sumx1,
          file = "summary stats for mean mnthly tmp of Tamale.csv")


sumxx1 <- data.frame(Mn.mn.tmx[Mn.mn.tmx[,2] == max(Mn.mn.tmx[,2]), ],
                     Mn.mn.tmx[Mn.mn.tmx[,2] == min(Mn.mn.tmx[,2]), ])

sumxx1[2,c(1,2)] <- sumxx1[1, c(3,4)] 
sumxx1[-c(3,4)] -> sumxx1
sumxx1[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumxx1) <- list(
   c("Max:","Min:"), 
   c("Month","Tmax")
)  

write.csv(sumxx1,
          file = "summary stats for Max Tmp (1960-2017).csv")




# seasonal Cycle of Max Tmp of Tamale
head(Tmax)
data <- Tmax
names(data) <- c("dates","Rain")
head(data)

mean_dailytmx_Tmale<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,Jan_11,
                        Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,Jan_20,Jan_21,
                        Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,
                        Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,
                        Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,
                        Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, 
                        Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,
                        Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,
                        Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,
                        Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,
                        Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Tmaletmx <-data.frame(
   c(1:366),
   mean_dailytmx_Tmale
   )

write.csv(Seasonal_cycle_Tmaletmx, 
          file = "data_for_seasonal_cycle_of_Max tmp of Tamale(1960-2016).csv")

t <- ksmooth(
   c(1:366), 
   mean_dailytmx_Tmale, 
   bandwidth = 15
   )

t.1 <- data.frame(t) # converting to a dataframe

ggplot(Seasonal_cycle_Tmaletmx, aes(x=c(1:366), y=mean_dailytmx_Tmale)) + 
   geom_line(col="firebrick", lwd=1) +
   labs(title="Seasonal Maximum Temperature (1960-2016)\n Tamale",
        x="Day", y=yyy) + 
   geom_line(data = t.1, aes(x=x, y=y), col="blue", lwd=1) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Maximum Tmp of Tamale (1960-2016).png",
         width=1000, height = 750)
dev.off()

sumx2 



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
          file="Index of Annual Max Tmp of Tamale (1960-2016).csv")

ggplot(Tmax.ann.df.anom, aes(Tmax.ann.df$date, Tmax.ann.df.anom.v, ymin = 0,
                             ymax = Tmax.ann.df.anom.v)) + 
   geom_linerange(data = Tmax.ann.df.anom, aes(colour = ifelse(Tmax.ann.df.anom.v >0,
      "Positive", "Negative")),stat = "identity",
                                                position = "identity",size=2) + 
   theme(legend.title = element_text(size = 2, colour = F)) + 
   (scale_x_date(date_breaks="5 years",date_labels="%Y"))  +
   geom_hline(yintercept=0) + 
   labs(title="Annual Rainfall Total Anomaly (1960-2016)\n Tamale",
        x="Year", y="Anomaly") +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Index of Maximum Tmp of Tamale (1960-2016).png", 
         width=1000, height = 750)
dev.off()


sumx2 <- summary(Tmax.ann.df.anom)
#0r
sumx22 <- data.frame(Tmax.ann.df.anom[Tmax.ann.df.anom[,2] == max(Tmax.ann.df.anom[,2]), ],
                     Tmax.ann.df.anom[Tmax.ann.df.anom[,2] == min(Tmax.ann.df.anom[,2]), ])

sumx22[2,c(1,2)] <- sumx22[1, c(3,4)] 
sumx22[-c(3,4)] -> sumx22
#sumx22[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumx22) <- list(
   c("Max:","Min:"), 
   c("Year","Tmax")
)  

write.csv(sumx22,
          file = "summary stats for Ann. Max Tmp Anomaly(1960-2016).csv")





#Dividing Into two halves
#Dividing Tmax into two halves
#Dividing daily Tmax
head(Tmax)
Tmax.fh <- Tmax[(Tmax$date >= "1960-01-01" & Tmax$date <= "1988-12-31"), ]
Tmax.sh <- Tmax[(Tmax$date >= "1989-01-01" & Tmax$date <= "2016-12-31"), ]

#Dividing annual Prcp
Tmax_fh <- Tmax.ann.df[(Tmax.ann.df$date >= "1960-12-31" &
                           Tmax.ann.df$date <= "1988-12-31"), ]

Tmax_sh <- Tmax.ann.df[(Tmax.ann.df$date >= "1989-12-31" &
                           Tmax.ann.df$date <= "2016-12-31"), ]



#Annual Tmax first half (1975-1996)
Tmax_fh
MK <- MannKendall(Tmax_fh[,2])
summary(MK)


write.csv(Tmax_fh, 
          file="Annual Max.Tmp of Tamale (1975-1996).csv")

ggplot(Tmax_fh, aes(x=Tmax_fh$date, y=Tmax_fh$Tmax)) + 
   geom_line(col="firebrick", lwd=2) + 
   geom_smooth(method = lm, col="blue") + 
   labs(title='Maximum Temperature (1960-1988)\n Tamale',
        x="Year", y=yyy) + 
   scale_x_date(date_breaks = "3 years", date_labels = "%Y") + 
   annotate("text", x = Tmax_fh[Tmax_fh[,1] == "1965-12-31", ]$date ,
            y = 34.8, label = "P-value = 0.0011739", cex = 5) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="AnnuAl Maximum Tmp of Tamale (1960-1988).png",
         width=1000, height = 750)
dev.off()


sumx3 <- summary(Tmax_fh)

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
head(Tmax.mn.df.new)
Tmax.mn.dffh <- Tmax.mn.df.new[(Tmax.mn.df.new$Janu.date >= "1960-01-31" &
                                   Tmax.mn.df.new$Janu.date <= "1988-12-31"), ]

Tmax.mn.dfsh <- Tmax.mn.df.new[(Tmax.mn.df.new$Janu.date >= "1989-01-31" &
                                   Tmax.mn.df.new$Janu.date <= "2016-12-31"), ]


#First Half
head(Tmax.mn.dffh)

#Averaging all months
Janx.fh <- mean(Tmax.mn.dffh$Janu.Tmax, na.rm = T)
Febx.fh <- mean(Tmax.mn.dffh$Febr.Tmax, na.rm = T)
Marchx.fh <- mean(Tmax.mn.dffh$Marc.Tmax, na.rm = T)
Aprilx.fh <- mean(Tmax.mn.dffh$Apri.Tmax, na.rm = T)
Mayx.fh <- mean(Tmax.mn.dffh$Mayy.Tmax, na.rm = T)
Junex.fh <- mean(Tmax.mn.dffh$Junee.Tmax, na.rm = T)
Julyx.fh <- mean(Tmax.mn.dffh$Julyy.Tmax, na.rm = T)
Augustx.fh <- mean(Tmax.mn.dffh$Augus.Tmax, na.rm = T)
Septemberx.fh <- mean(Tmax.mn.dffh$Septe.Tmax, na.rm = T)
Octoberx.fh <- mean(Tmax.mn.dffh$Octo.Tmax, na.rm = T)
Novemberx.fh <- mean(Tmax.mn.dffh$Novem.Tmax, na.rm = T)
Decemberx.fh <- mean(Tmax.mn.dffh$Decem.Tmax, na.rm = T)
#creating a vector
mean.mn.Tmaxfh.v <- c(Janx.fh,Febx.fh,Marchx.fh,Aprilx.fh,Mayx.fh,
                      Junex.fh,Julyx.fh,Augustx.fh,Septemberx.fh,Octoberx.fh,
                      Novemberx.fh,Decemberx.fh)

date.mn 
mean.mn.Tmaxfh <- data.frame(
   date.mn,
   mean.mn.Tmaxfh.v
   )

write.csv(mean.mn.Tmaxfh, 
          file="Mean Monthly Max Tmp of Tamle (1960-1988).csv")

ggplot(mean.mn.Tmaxfh, aes(x=date.mn, y=mean.mn.Tmaxfh.v)) + 
   geom_line(col="firebrick", lwd=2) + 
   scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
   labs(title="Mean Monthly Maximum Temperature (1960-1988)\n Tamale",
        x="Month",y=yyy) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Maximum Tmp of Tamale (1960-1988).png",
         width=1000, height = 750)
dev.off()


sumx4 <- summary(mean.mn.Tmaxfh)

sumx44 <- data.frame(mean.mn.Tmaxfh[mean.mn.Tmaxfh[,2] == max(mean.mn.Tmaxfh[,2]), ],
                     mean.mn.Tmaxfh[mean.mn.Tmaxfh[,2] == min(mean.mn.Tmaxfh[,2]), ])

sumx44[2,c(1,2)] <- sumx44[1, c(3,4)] 
sumx44[-c(3,4)] -> sumx44
sumx44[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumx44) <- list(
   c("Max:","Min:"), 
   c("Month","Tmax")
)  

write.csv(sumx44,
          file = "summary stats for monthly Max Tmp (1960-1988).csv")





#Annual Tmax second half (1989 - 2016)
head(Tmax_sh)
MK <- MannKendall(Tmax_sh[,2])
summary(MK)

write.csv(Tmax_sh, 
          file="Annual Tmax of Bolga (1989-2016).csv")

ggplot(Tmax_sh, aes(x=Tmax_sh$date, y=Tmax_sh$Tmax)) + geom_line(col="firebrick", lwd=2) + 
   geom_smooth(method = lm, col="blue") + 
   scale_x_date(date_breaks = "3 years", date_labels = "%Y") + 
   labs(title='Maximum Temperature (1989-2016)\n Tamale',
        x="Year", y=yyy) + 
   annotate("text", x = Tmax_sh[Tmax_sh[,1] == "1996-12-31", ]$date,
            y = 35.1, label='P-value = 0.025583', cex = 5) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Annual Maximum Tmp of Tamale (1989-2016).png",
         width=1000, height = 750)
dev.off()


sumx5 <- summary(Tmax_sh)

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
          file = "summary stats for Ann Max Tmp (1989-2016).csv")



#second Half
head(Tmax.mn.dfsh)

#Averaging all months
Janx.sh <- mean(Tmax.mn.dfsh$Janu.Tmax, na.rm = T)
Febx.sh <- mean(Tmax.mn.dfsh$Febr.Tmax, na.rm = T)
Marchx.sh <- mean(Tmax.mn.dfsh$Marc.Tmax, na.rm = T)
Aprilx.sh <- mean(Tmax.mn.dfsh$Apri.Tmax, na.rm = T)
Mayx.sh <- mean(Tmax.mn.dfsh$Mayy.Tmax, na.rm = T)
Junex.sh <- mean(Tmax.mn.dfsh$Junee.Tmax, na.rm = T)
Julyx.sh <- mean(Tmax.mn.dfsh$Julyy.Tmax, na.rm = T)
Augustx.sh <- mean(Tmax.mn.dfsh$Augus.Tmax, na.rm = T)
Septemberx.sh <- mean(Tmax.mn.dfsh$Septe.Tmax, na.rm = T)
Octoberx.sh <- mean(Tmax.mn.dfsh$Octo.Tmax, na.rm = T)
Novemberx.sh <- mean(Tmax.mn.dfsh$Novem.Tmax, na.rm = T)
Decemberx.sh <- mean(Tmax.mn.dfsh$Decem.Tmax, na.rm = T)

#creating a vector
mean.mn.Tmaxsh.v <- c(Janx.sh,Febx.sh,Marchx.sh,Aprilx.sh,Mayx.sh,
                      Junex.sh,Julyx.sh,Augustx.sh,Septemberx.sh,Octoberx.sh,
                      Novemberx.sh,Decemberx.sh)

date.mn 

mean.mn.Tmaxsh <- data.frame(
   date.mn, 
   mean.mn.Tmaxsh.v
   )

write.csv(mean.mn.Tmaxsh,
          file="Mean Monthly Max Tmp of Tamale 91989-2016).csv")

ggplot(mean.mn.Tmaxsh, aes(x=date.mn, y=mean.mn.Tmaxsh.v)) + 
   geom_line(col="firebrick", lwd=2) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
   labs(title="Mean Monthly Maximum Temperature (1989-2016)\n Tamale",
        x="Month",y=yyy) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Maximum Tmp of Tamale (1989-2016).png",
         width=1000, height = 750)
dev.off()


sumx6 <- summary(mean.mn.Tmaxsh)

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
          file = "summary stats for monthly Max Tmp (1989-2016).csv")



#plot(mean.mn.Tmaxfh, type="l", lwd=3, col="darkblue", 
#     main="Mean Monthly Max Tmp of Tamale- 1960-2016", 
#     ylim=c(30,40), xlab="Months", ylab=yyy)

#lines(mean.mn.Tmaxsh, col="red", lwd=3, type="l")

#legend("topright", legend = c("1960-1988", "1989-2016"),
#       col=c("darkblue","red"), 
#       bty = "n", lty = 1,lwd=3)


ggplot(mean.mn.Tmaxfh,aes(x=mean.mn.Tmaxfh[,1])) + 
   geom_line(aes(y=mean.mn.Tmaxfh[,2], col="1960-1988"), lwd = 2) +
   geom_line(aes(y=mean.mn.Tmaxsh[,2], col ="1989-2016"),lwd=2) +
   scale_colour_manual("",
                       values=c("1960-1988"="darkblue", "1989-2016"="red")) +
   labs(title="Mean Monthly Maximum Temperature (1960-2016)\n Tamale", x="Month",
        y=yyy) + scale_x_date(date_labels = "%b", date_breaks = "1 month") +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename = "Mean Monthly Max Tmp of Tamale (1960-88-2016.png",
         width = 1000, height = 750)
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
write.csv(change,
          file="Percentage change in Mnthly Mx tMP between the two halves.csv ")


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

mean_dailytmx_Tmalefh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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
                          Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Tmaletmxfh <-data.frame(
   c(1:366),
   mean_dailytmx_Tmalefh
   )

write.csv(Seasonal_cycle_Tmaletmxfh, 
          file = "data_for_seasonal_cycle_of_Max tmp of Tamale(1960-1988).csv")

aa <- ksmooth(
   c(1:366),
   mean_dailytmx_Tmalefh, bandwidth = 15
   )

aa.1 <- data.frame(aa) # converting to a dataframe

ggplot(Seasonal_cycle_Tmaletmxfh, aes(x=c(1:366), y=mean_dailytmx_Tmalefh)) + 
   geom_line(col="firebrick", lwd=1) + 
   labs(title="Seasonal Cycle of Maximum Temperature (1960-1988)\n Tamale", 
        x="Day", y=yyy) + 
   geom_line(data = aa.1, aes(x=x, y=y), col="blue", lwd=1) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Maximum Tmp of Tamale (1960-1988).png",
         width=1000, height = 750)
dev.off()



#Second Half
head(Tmax.sh)
data <- Tmax.sh
names(data) <- c("dates","Rain")
head(data)

mean_dailytmx_Tmalesh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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

Seasonal_cycle_Tmaletmxsh <-data.frame(
   c(1:366),
   mean_dailytmx_Tmalesh
   )

write.csv(Seasonal_cycle_Tmaletmxsh, 
          file = "data_for_seasonal_cycle_of_Max tmp of Tamale(1989-2016).csv")

bbb <- ksmooth(
   c(1:366), 
   mean_dailytmx_Tmalesh,
   bandwidth = 15
   )

bbb.1 <- data.frame(bbb) # converting to a dataframe

ggplot(Seasonal_cycle_Tmaletmxsh, aes(x=c(1:366), y=mean_dailytmx_Tmalesh)) + 
   geom_line(col="firebrick", lwd=1) + 
   labs(title="Seasonal Cycle of Maximum Temperature (1989-2016)\n Tamale",
        x="Day", y=yyy) + 
   geom_line(data = bbb.1, aes(x=x, y=y), col="blue", lwd=1) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Maximum Tmp of Tamale (1989-2016).png",
         width=1000, height = 750)
dev.off()



#plot(Seasonal_cycle_Tmaletmxfh, lwd=1, type="l", col="darkblue", 
#     main="Seasonal Max Tmp Cycle of Tamale- 1960-2017",
#     xlab="Days", ylab=yyy)

#lines(aa.1, col="blue", lwd=3)
#lines(Seasonal_cycle_Tmaletmxsh, type="l", col="darkblue")
#lines(bbb.1, lwd=3, col="red")

#legend("topright", col=c("blue","red"), 
#       legend = c("1960-1988","1989-2017"),
#       bty = "n", lty = 1, lwd=3)


ggplot(Seasonal_cycle_Tmaletmxfh, aes(x=Seasonal_cycle_Tmaletmxfh[,1])) +
   geom_line(aes(y=Seasonal_cycle_Tmaletmxfh[,2]), lwd=.25) +
   geom_line(aes(y=aa.1[,2],col="1960-1988"),lwd=1.25) +
   geom_line(aes(y=Seasonal_cycle_Tmaletmxsh[,2]), lwd=.25) +
   geom_line(aes(y=bbb.1[,2],col="1989-2016"), lwd=1.25) +
   scale_colour_manual("Legend",
                       values = c("1960-1988"="darkblue", "1989-2016"="red")) +
   ggtitle("Seasonal Cycle of Maximum Temperature (1960-2016)\n Tamale") + 
   xlab("Day") +  ylab(yyy) +
   theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Max Tmp of Tamale(1960-88-2016).png",
         width = 1000, height=750)
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

ggplot(data=Seas2,
       aes(x=dates,y=Tmax)) +
   geom_line(col="firebrick", lwd=2) +
   ggtitle(" Seasonal Maximum Temperature (1960-2016)\n Tamale") + xlab("Month") +
   ylab(yyy) + 
   theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
         axis.text = element_text(size = 13),
         axis.title = element_text(size = 13)) +
   scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Tmax.png",
         width = 1000, height = 650
)
dev.off()




#Monthly Seasonal Cycle of Tmax
head(Tmax)
sjx <- subset(Tmax, format.Date(dates,  "%m")=="01")
sfx <- subset(Tmax, format.Date(dates, "%m")=="02")
smx <- subset(Tmax, format.Date(dates, "%m")=="03")
sax <- subset(Tmax, format.Date(dates, "%m")=="04")
smax <- subset(Tmax, format.Date(dates, "%m")=="05")
sjux <- subset(Tmax, format.Date(dates, "%m")=="06")
sjlx <- subset(Tmax, format.Date(dates, "%m")=="07")
saux <- subset(Tmax, format.Date(dates, "%m")=="08")
ssx <- subset(Tmax, format.Date(dates, "%m")=="09")
sox <- subset(Tmax, format.Date(dates, "%m")=="10")
snx <- subset(Tmax, format.Date(dates, "%m")=="11")
sdx <- subset(Tmax, format.Date(dates, "%m")=="12")





#Indexing
#first half
#head(Tmax_fh)
#lng.mn.Tmaxfh <- mean(Tmax_fh$Tmax)
#sd.Tmaxfh <- sd(Tmax_fh$Tmax)
#Tmax_fh_anom.v <- (Tmax_fh$Tmax - lng.mn.Tmaxfh)/sd.Tmaxfh
#Tmax_fh_anom <- data.frame(Tmax_fh$date, Tmax_fh_anom.v)
#write.csv(Tmax_fh_anom,
#          file="Index of Meam Max Tmp of Tamale (1960-1988).csv")

#ggplot(Tmax_fh_anom, aes(Tmax_fh$date, Tmax_fh_anom.v, ymin = 0,ymax = Tmax_fh_anom.v)) + 
#   geom_linerange(data = Tmax_fh_anom, aes(colour = ifelse(Tmax_fh_anom.v >0,
#   "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#   theme(legend.title = element_text(size = 2, colour = F)) + 
#   (scale_x_date(date_breaks="4 year",date_labels="%Y"))  +
#   geom_hline(yintercept=0) + 
#   labs(title="Index of Annual Maximum Tmp of Tamale \n (1960-1988)",
#        x="Years", y="Index")

#dev.copy(png, filename="Index of Maximum Tmp of Tamale (1960-1988).png", 
#         width=1000, height = 750)
#dev.off()



#Second half
#head(Tmax_sh)
#lng.mn.Tmaxsh <- mean(Tmax_sh$Tmax)
#sd.Tmaxsh <- sd(Tmax_sh$Tmax)

#Tmax_sh_anom.v <- (Tmax_sh$Tmax - lng.mn.Tmaxsh)/
#   sd.Tmaxsh

#Tmax_sh_anom <- data.frame(
#   Tmax_sh$date,
#   Tmax_sh_anom.v
#   )

#write.csv(Tmax_sh_anom,
#          file="Index of Max Tmp of Tamale (1989-2016).csv")

#ggplot(Tmax_sh_anom, aes(Tmax_sh$date, Tmax_sh_anom.v, ymin = 0,ymax = Tmax_sh_anom.v)) + 
#   geom_linerange(data = Tmax_sh_anom, aes(colour = ifelse(Tmax_sh_anom.v >0,
#      "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#   theme(legend.title = element_text(size = 2, colour = F)) +
#   (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#   geom_hline(yintercept=0) + 
#   labs(title="Index of Annual Maximum Tmp of Tamale \n (1989-2016)", 
#        x="Years", y="Index")

#dev.copy(png, filename="Index of Maximum Tmp of Tamale (1989-2016).png",
#         width=1000, height = 750)
#dev.off()
