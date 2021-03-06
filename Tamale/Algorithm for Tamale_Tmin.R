###############################################################################################

head(data1)

Tmin <- data.frame(
  data1$dates,
  data1$Tmin
  )

names(Tmin) <- c("date","Tmin")
head(Tmin)

#................................................................................................................................#

#Annual Minimum Temperature
head(Tmin)
Tmin.xts <- xts(
  Tmin$Tmin,
  Tmin$date
  )

Tmin.ann.xts <- apply.yearly(
  Tmin.xts,
  mean,
  na.rm=T
  )

head(Tmin.ann.xts)

#xonversion to dataframe
Tmin.ann.df <- data.frame(
  date=index(Tmin.ann.xts),
  coredata(Tmin.ann.xts
           )
  )

names(Tmin.ann.df)[2] <- "Tmin"

#date conversion
Tmin.ann.df$date <- as.Date(
  Tmin.ann.df$date,
  format="%Y-%m-%d"
  )

head(Tmin.ann.df)
MK <- MannKendall(Tmin.ann.df[,2])
summary(MK)

write.csv(Tmin.ann.df, 
          file="Annual Minimum Temperature of Tamale (1960-2016).csv")

head(Tmin.ann.df)
ggplot(Tmin.ann.df, aes(x=date, y=Tmin)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  labs(title="Minimum Temperature (1960-2016)\n Tamale",
       x="Year", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "6 years", date_labels = "%Y") + 
  annotate("text", x = Tmin.ann.df[Tmin.ann.df[,1] == "1970-12-31", ]$date,
           y = 23.5, label="P-value = 1.1921e-07", cex = 5) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="AAnnuAl inimum Tmp of Tamale (1960-2016).png",
         width=1000, height=750)
dev.off()

sumn <- summary(Tmin.ann.df)

sumn <- data.frame(Tmin.ann.df[Tmin.ann.df[,2] == max(Tmin.ann.df[,2]), ],
                   Tmin.ann.df[Tmin.ann.df[,2] == min(Tmin.ann.df[,2]), ])

sumn[2,c(1,2)] <- sumn[1, c(3,4)] 
sumn[-c(3,4)] -> sumn
#sumxn[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumn) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn,
          file = "summary stats for ann Min Tmp (1960-2016).csv")




#Mean Monthly Temperature
head(Tmin.xts)

Tmin.mn.xts <- apply.monthly(
  Tmin.xts,
  mean,na.rm=T
  )

#conversion (d.f)
Tmin.mn.df <- data.frame(
  date=index(Tmin.mn.xts),
  coredata(Tmin.mn.xts
           )
  )

names(Tmin.mn.df)[2] <- "Tmin"
head(Tmin.mn.df)

Tmin.mn.df$date <- as.Date(
  Tmin.mn.df$date,
  format="%Y-%m-%d"
  )

#Monthly Subsetting
J.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="01")$Tmin, na.rm=T)
F.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="02")$Tmin, na.rm=T)
M.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="03")$Tmin, na.rm=T)
A.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="04")$Tmin, na.rm=T)
Ma.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="05")$Tmin, na.rm=T)
Ju.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="06")$Tmin, na.rm=T)
Jl.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="07")$Tmin, na.rm=T)
Au.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="08")$Tmin, na.rm=T)
S.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="09")$Tmin, na.rm=T)
O.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="10")$Tmin, na.rm=T)
N.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="11")$Tmin, na.rm=T)
D.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="12")$Tmin, na.rm=T)

Mn.m.tn.v <- c(J.n, F.n,M.n,A.n,Ma.n,
               Ju.n,Jl.n,Au.n,S.n,O.n,N.n,D.n)

date.mn
Mn.mn.tmn <- data.frame(
  date.mn, 
  Mn.m.tn.v
  )

write.csv(Mn.mn.tmn,
          file="Mean Monthly Tmin of Tamale (1960-2107).csv")

ggplot(Mn.mn.tmn, aes(x=date.mn, y=Mn.m.tn.v)) +
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Minimum Temperature (1960-2016)\n Tamale",
       x="Month", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Minimum Tmp of Tamale (1960-2016).png",
         width=1000, height=750)
dev.off()

sumn1 <- summary(Mn.mn.tmn)
#or
sumn1 <- data.frame(Mn.mn.tmn[Mn.mn.tmn[,2] == max(Mn.mn.tmn[,2]), ],
                    Mn.mn.tmn[Mn.mn.tmn[,2] == min(Mn.mn.tmn[,2]), ])

sumn1[2,c(1,2)] <- sumn1[1, c(3,4)] 
sumn1[-c(3,4)] -> sumn1
sumn1[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn1) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn1,
          file = "summary stats for Monthly Min Tmp (1960-2016).csv")



# seasonal Cycle of Min Tmp of Tamale
head(Tmin)
data <- Tmin
names(data) <- c("dates","Rain")
head(data)

mean_dailytmn_Tmale<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,Jan_11,
                        Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,Jan_20,Jan_21,
                        Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,
                        Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,
                        Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,
                        Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,
                        Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,
                        Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,
                        Mar_80,Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,
                        Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Tmaletmn <-data.frame(
  c(1:366),
  mean_dailytmn_Tmale
  )

write.csv(Seasonal_cycle_Tmaletmn,
          file = "data_for_seasonal_cycle_of_Min tmp of Tamale(1960-2016).csv")

oo <- ksmooth(
  c(1:366), 
  mean_dailytmn_Tmale,
  bandwidth = 15
  )

oo.1 <- data.frame(oo) # converting to a dataframe

ggplot(Seasonal_cycle_Tmaletmn, aes(x=c(1:366), y=mean_dailytmn_Tmale)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1960-2016)\n Tamale",
       x="Day", y=yyy) + 
  geom_line(data = oo.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Minimum Tmp of Tamale (1960-2016).png",
         width=1000, height=750)
dev.off()



#Indexing of Min tmp of Tamale (1960-2016)
head(Tmin.ann.df)
lng.mn.Tmin <- mean(Tmin.ann.df$Tmin)
sd.Tmin <- sd(Tmin.ann.df$Tmin)

Tmin.ann.df.anom.v <- (Tmin.ann.df$Tmin - lng.mn.Tmin)/
  sd.Tmin

Tmin.ann.df.anom <- data.frame(
  Tmin.ann.df$date, 
  Tmin.ann.df.anom.v
  )

write.csv(Tmin.ann.df.anom,
          file="Index of Annual Min Tmp of Tamale (1960-2016).csv")

ggplot(Tmin.ann.df.anom, aes(Tmin.ann.df$date, Tmin.ann.df.anom.v, ymin = 0,
                             ymax = Tmin.ann.df.anom.v)) + 
  geom_linerange(data = Tmin.ann.df.anom, aes(colour = ifelse(Tmin.ann.df.anom.v >0, 
        "Positive", "Negative")),stat = "identity", position = "identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = F)) + 
  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title="Minimum Temperature Anomaly (1960-2016)\n Tamale",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Index of Minimum Tmp of Tamale (1960-2016).png",
         width=1000, height=750)
dev.off()

sumn2 <- summary(Tmin.ann.df.anom)
#or
sumn2 <- data.frame(Tmin.ann.df.anom[Tmin.ann.df.anom[,2] == max(Tmin.ann.df.anom[,2]), ],
                    Tmin.ann.df.anom[Tmin.ann.df.anom[,2] == min(Tmin.ann.df.anom[,2]), ])

sumn2[2,c(1,2)] <- sumn2[1, c(3,4)] 
sumn2[-c(3,4)] -> sumn2
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn2) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn2,
          file = "summary stats for Ann Min Tmp  Anomaly(1960-2016).csv")




#Dividing Into two halves
#Dividing Tmax into two halves
#Dividing daily Tmin
head(Tmin)
Tmin.fh <- Tmin[(Tmin$date >= "1960-01-01" & Tmin$date <= "1988-12-31"), ]
Tmin.sh <- Tmin[(Tmin$date >= "1989-01-01" & Tmin$date <= "2016-12-31"), ]

#Dividing annual Tmin
Tmin_fh <- Tmin.ann.df[(Tmin.ann.df$date >= "1960-12-31" & 
                          Tmin.ann.df$date <= "1988-12-31"), ]

Tmin_sh <- Tmin.ann.df[(Tmin.ann.df$date >= "1989-12-31" & 
                          Tmin.ann.df$date <= "2016-12-31"), ]


#Annual Tmax first half (1960-1988)
head(Tmin_fh)
MK <- MannKendall(Tmin_fh[,2])
summary(MK)

write.csv(Tmin_fh,
          file="Annual Min.Tmp of Tamale (1960-1988).csv")

ggplot(Tmin_fh, aes(x=Tmin_fh$date, y=Tmin_fh$Tmin)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") + 
  labs(title='Minimum Temperature (1960-1988)\n Tamale',
       x="Year", y=yyy) + 
  annotate("text", x = Tmin_fh[Tmin_fh$date == "1967-12-31", ]$date, 
           y = 22.9, label="P-value = 0.51148", cex = 5) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Annual Minimum Tmp of Tamale (1960-1988).png",
         width=1000, height=750)
dev.off()

sumn3 <- summary(Tmin_fh)
#or
sumn3 <- data.frame(Tmin_fh[Tmin_fh[,2] == max(Tmin_fh[,2]), ],
                    Tmin_fh[Tmin_fh[,2] == min(Tmin_fh[,2]), ])

sumn3[2,c(1,2)] <- sumn3[1, c(3,4)] 
sumn3[-c(3,4)] -> sumn3
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn3) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn3,
          file = "summary stats for Ann Min Tmp  (1960-1988).csv")



#Mean Monthly Tmin
head(Tmin.mn.df)

#Imputation of missing values for all months except september)
##subsetting all individual months
Janu.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="01")
#Janu.w$Tmin <- na.kalman(Janu.w$Tmin, model="StructTS",smooth = T, nit = -1)
#
Febr.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="02")
#Febr.w$Tmin <- na.kalman(Febr.w$Tmin, model="StructTS",smooth = T, nit = -1)
#
Marc.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="03")
#Marc.w$Tmin <- na.kalman(Marc.w$Tmin, model="StructTS",smooth = T, nit = -1)
#
Apri.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="04")
#Apri.w$Tmin <- na.kalman(Apri.w$Tmin, model="StructTS",smooth = T, nit = -1)
#4 missing
Mayy.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="05")
#Mayy.w$Tmin <- na.kalman(Mayy.w$Tmin, model="StructTS",smooth = T, nit = -1)
#
Junee.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="06")
#Junee.w$Tmin <- na.kalman(Junee.w$Tmin, model="StructTS",smooth = T, nit = -1)
#
Julyy.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="07")
#Julyy.w$Tmin <- na.kalman(Julyy.w$Tmin, model="StructTS",smooth = T, nit = -1)
#
Augus.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="08")
#Augus.w$Tmin <- na.kalman(Augus.w$Tmin, model="StructTS",smooth = T, nit = -1)

Septe.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="09")
#Septe$Tmax <- na.kalman(Septe$Tmax, model="StructTS",smooth = T, nit = -1)

Octo.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="10")
#Octo.w$Tmin <- na.kalman(Octo.w$Tmin, model="StructTS",smooth = T, nit = -1)

Novem.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="11")
#Novem.w$Tmin <- na.kalman(Novem.w$Tmin, model="StructTS",smooth = T, nit = -1)

Decem.w <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="12")
#Decem.w$Tmin <- na.kalman(Decem.w$Tmin, model="StructTS",smooth = T, nit = -1)

#creating a dataframe with the above
#Tmin.mn.df.new <- data.frame(Janu.w$date, Janu.w$Tmin, Febr.w$Tmin, Marc.w$Tmin,Apri.w$Tmin,Mayy.w$Tmin,Junee.w$Tmin,Julyy.w$Tmin,Augus.w$Tmin,Septe.w$Tmin,Octo.w$Tmin,Novem.w$Tmin,Decem.w$Tmin)

Tmin.mn.dffh <- Tmin.mn.df.new[(Tmin.mn.df.new$Janu.w.date >= "1960-01-31" &
                                  Tmin.mn.df.new$Janu.w.date <= "1988-12-31"), ]

Tmin.mn.dfsh <- Tmin.mn.df.new[(Tmin.mn.df.new$Janu.w.date >= "1989-01-31" & 
                                  Tmin.mn.df.new$Janu.w.date <= "2016-12-31"), ]
#First Half
head(Tmin.mn.dffh)

#Averaging all months
Jann.fh <- mean(Tmin.mn.dffh$Janu.w.Tmin, na.rm = T)
Febn.fh <- mean(Tmin.mn.dffh$Febr.w.Tmin, na.rm = T)
Marchn.fh <- mean(Tmin.mn.dffh$Marc.w.Tmin, na.rm = T)
Apriln.fh <- mean(Tmin.mn.dffh$Apri.w.Tmin, na.rm = T)
Mayn.fh <- mean(Tmin.mn.dffh$Mayy.w.Tmin, na.rm = T)
Junen.fh <- mean(Tmin.mn.dffh$Junee.w.Tmin, na.rm = T)
Julyn.fh <- mean(Tmin.mn.dffh$Julyy.w.Tmin, na.rm = T)
Augustn.fh <- mean(Tmin.mn.dffh$Augus.w.Tmin, na.rm = T)
Septembern.fh <- mean(Tmin.mn.dffh$Septe.w.Tmin, na.rm = T)
Octobern.fh <- mean(Tmin.mn.dffh$Octo.w.Tmin, na.rm = T)
Novembern.fh <- mean(Tmin.mn.dffh$Novem.w.Tmin, na.rm = T)
Decembern.fh <- mean(Tmin.mn.dffh$Decem.w.Tmin, na.rm = T)
#creating a vector
mean.mn.Tminfh.v <- c(Jann.fh,Febn.fh,Marchn.fh,Apriln.fh,Mayn.fh,
                      Junen.fh,Julyn.fh,Augustn.fh,Septembern.fh,
                      Octobern.fh,Novembern.fh,Decembern.fh)

date.mn 

mean.mn.Tminfh <- data.frame(
  date.mn, 
  mean.mn.Tminfh.v
  )

write.csv(mean.mn.Tminfh,
          file="Mean Monthly Tmin of Tamale (1960-1988).csv")

ggplot(mean.mn.Tminfh, aes(x=date.mn, y=mean.mn.Tminfh.v)) + 
  geom_line(col="firebrick", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Minimum Temperature (1960-1988)\n Tamale",
       x="Month",y=yyy) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Minimum Tmp of Tamale (1960-1988).png",
         width=1000, height=750)
dev.off()

sumn4 <- summary(mean.mn.Tminfh)

sumn4 <- data.frame(mean.mn.Tminfh[mean.mn.Tminfh[,2] == max(mean.mn.Tminfh[,2]), ],
                    mean.mn.Tminfh[mean.mn.Tminfh[,2] == min(mean.mn.Tminfh[,2]), ])

sumn4[2,c(1,2)] <- sumn4[1, c(3,4)] 
sumn4[-c(3,4)] -> sumn4
sumn4[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn4) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn4,
          file = "summary stats for mean month Min Tmp (1960-1988).csv")




#Annual Tmin second half (1989 - 2016)
head(Tmin_sh)
MK <-MannKendall(Tmin_sh[,2])
summary(MK)

write.csv(Tmin_sh, file="Annual Tmin of Tamale (1989-2016).csv")
ggplot(Tmin_sh, aes(x=Tmin_sh$date, y=Tmin_sh$Tmin)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  labs(title='Minimum Temperature (1989-2016)\n Tamale', x="Year", y=yyy) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  annotate("text", x = Tmin_sh[Tmin_sh$date == "1997-12-31", ]$date, 
           y = 23.8, label="P-value = 0.0085986", cex = 5) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Annual Minimum Tmp of Tamale (1989-2016).png",
         width=1000, height=750)
dev.off()

sumn5 <- summary(Tmin_sh)

sumn55 <- data.frame(Tmin_sh[Tmin_sh[,2] == max(Tmin_sh[,2]), ],
                     Tmin_sh[Tmin_sh[,2] == min(Tmin_sh[,2]), ])

sumn55[2,c(1,2)] <- sumn55[1, c(3,4)] 
sumn55[-c(3,4)] -> sumn55
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn55) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn55,
          file = "summary stats for Ann Min Tmp  Anomaly(1989-2016).csv")



#second Half
head(Tmin.mn.dfsh)

#Averaging all months

Jann.sh <- mean(Tmin.mn.dfsh$Janu.w.Tmin, na.rm = T)
Febn.sh <- mean(Tmin.mn.dfsh$Febr.w.Tmin,na.rm = T)
Marchn.sh <- mean(Tmin.mn.dfsh$Marc.w.Tmin,na.rm = T)
Apriln.sh <- mean(Tmin.mn.dfsh$Apri.w.Tmin,na.rm = T)
Mayn.sh <- mean(Tmin.mn.dfsh$Mayy.w.Tmin,na.rm = T)
Junen.sh <- mean(Tmin.mn.dfsh$Junee.w.Tmin,na.rm = T)
Julyn.sh <- mean(Tmin.mn.dfsh$Julyy.w.Tmin,na.rm = T)
Augustn.sh <- mean(Tmin.mn.dfsh$Augus.w.Tmin,na.rm = T)
Septembern.sh <- mean(Tmin.mn.dfsh$Septe.w.Tmin, na.rm = T)
Octobern.sh <- mean(Tmin.mn.dfsh$Octo.w.Tmin,na.rm = T)
Novembern.sh <- mean(Tmin.mn.dfsh$Novem.w.Tmin,na.rm = T)
Decembern.sh <- mean(Tmin.mn.dfsh$Decem.w.Tmin,na.rm = T)


#creating a vector
mean.mn.Tminsh.v <- c(Jann.sh,Febn.sh,Marchn.sh,Apriln.sh,Mayn.sh,Junen.sh,
                      Julyn.sh,Augustn.sh,Septembern.sh,Octobern.sh,
                      Novembern.sh,Decembern.sh)

date.mn 
mean.mn.Tminsh <- data.frame(
  date.mn, 
  mean.mn.Tminsh.v
)

write.csv(mean.mn.Tminsh, 
          file="Mean Monthly Tmin of Tamale (1989-2016).csv")

ggplot(mean.mn.Tminsh, aes(x=date.mn, y=mean.mn.Tminsh.v)) + 
  geom_line(col="firebrick",  lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Minimum Temperature (1989-2016)\n Tamale", 
       x="Month",y=yyy, ylim=c(18,26)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Minimum Tmp of Tamale (1989-2016).png", 
         width=1000, height=750)
dev.off()

sumn6 <- summary(mean.mn.Tminsh)

sumn66 <- data.frame(mean.mn.Tminsh[mean.mn.Tminsh[,2] == max(mean.mn.Tminsh[,2]), ],
                     mean.mn.Tminsh[mean.mn.Tminsh[,2] == min(mean.mn.Tminsh[,2]), ])

sumn66[2,c(1,2)] <- sumn66[1, c(3,4)] 
sumn66[-c(3,4)] -> sumn66
sumn66[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn66) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn66,
          file = "summary stats for Ann Min Tmp (1989-2016).csv")



#plot(mean.mn.Tminfh, type="l", lwd=3, col="darkblue", 
#     main="Mean Monthly Min Tmp of Tamale- 1960-2016", 
#     xlab="Months", ylab=yyy, ylim=c(18,26))

#lines(mean.mn.Tminsh, col="red", lwd=3, type="l")

#legend("topright", legend = c("1960-1988", "1989-2016"),
#       col=c("darkblue","red"), 
#       bty = "n", lty = 1,lwd=3)



ggplot(mean.mn.Tminfh,aes(x=mean.mn.Tminfh[,1])) + 
  geom_line(aes(y=mean.mn.Tminfh[,2],col="1960-1988"),lwd = 2) + 
  geom_line(aes(y=mean.mn.Tminsh[,2],col ="1989-2016"),lwd=2) +
  scale_colour_manual("",
                      values = c("1960-1988"="darkblue","1989-2016"="red")) +
  labs(title="Mean Monthly Minimum Temperature (1960-2016)\n Temperature", 
       x="Month",y=yyy) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename = "Mean Monthly Min Tmp of Tamale (1960-88-2016.png",
         width = 1000, height = 750)
dev.off()




#Max Tmp Change between two halves
Tmpn_change <- (mean(Tmin_sh[,2], na.rm = T) - mean(Tmin_fh[,2], na.rm = T))*100/
  mean(Tmin_fh[,2], na.rm = T)

change <- expression("0.8815479(" ~ degree * c * ")")


#Monthly difference in Min Tmp b/n two halves
head(mean.mn.Tminfh)
head(mean.mn.Tminsh)

#First Half
storage2 <- numeric(12)
o <- c(mean.mn.Tminfh[,2])

for (i in 1:12) {
  storage2[i] <- o[i]
}

#Second Half
storage3 <- numeric(12)
p <- c(mean.mn.Tminsh[,2])

for (i in 1:12) {
  storage3[i] <- p[i]
}

change_in_Min_Temp <- data.frame(
  Month=month.name,
  Change=storage3-storage2)

write.csv(change_in_Min_Temp,
          file = "Percentage Change in monthly Min Tmp of tAMALE.csv")


#Percentage Change in Monthly Tmin
storage5 <- numeric(12)
k <- c(change_in_Min_Temp[,2])

for (i in 1:12) {
  storage5[i] <- ((k*100)/mean.mn.Tminfh[,2])[i]
}

Percen_change_tmin <- storage5




#seasonal Cycle 
#(first half)
head(Tmin.fh)
data <- Tmin.fh
names(data) <- c('dates','Rain')
head(data)

mean_dailytmn_Tmalefh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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
                          Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,
                          May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,
                          May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,
                          May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Tmaletmnfh <-data.frame(
  c(1:366),
  mean_dailytmn_Tmalefh
)

write.csv(Seasonal_cycle_Tmaletmnfh, 
          file = "data_for_seasonal_cycle_of_Min tmp of Tamale(1960-1988).csv")

gg <- ksmooth(
  c(1:366),
  mean_dailytmn_Tmalefh,
  bandwidth = 15
)

gg.1 <- data.frame(gg) # converting to a dataframe
ggplot(Seasonal_cycle_Tmaletmnfh, aes(x=c(1:366), y=mean_dailytmn_Tmalefh)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1960-1988)\n Tamale", 
                                           x="Day", y=yyy) + 
  geom_line(data = gg.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Minimum Tmp of Tamale (1960-1988).png",
         width=1000, height=750)
dev.off()



#Second Half
head(Tmin.sh)
data <- Tmin.sh
names(data) <- c("dates","Rain")
head(data)

mean_dailytmn_Tmalesh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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
                          Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,
                          May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Tmaletmnsh <-data.frame(
  c(1:366),
  mean_dailytmn_Tmalesh
  )

write.csv(Seasonal_cycle_Tmaletmnsh, 
          file = "data_for_seasonal_cycle_of_Min tmp of Tamale(1989-2016).csv")

ee <- ksmooth(
  c(1:366), 
  mean_dailytmn_Tmalesh,
  bandwidth = 15
  )

ee.1 <- data.frame(ee) # converting to a dataframe

ggplot(Seasonal_cycle_Tmaletmnsh, aes(x=c(1:366), y=mean_dailytmn_Tmalesh)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1989-2016)\n Tamale", 
                                           x="Day", y=yyy) + 
  geom_line(data = ee.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Minimum Tmp of Tamale (1989-2016).png",
         width=1000, height=750)
dev.off()



#plot(Seasonal_cycle_Tmaletmnfh, lwd=1, type="l", col="darkblue",
#    main="Seasonal Min Tmp Cycle of Tamale- 1960-2017", xlab="Days", ylab=yyy)

#lines(gg.1, col="blue", lwd=3)
#lines(Seasonal_cycle_Tmaletmnsh, type="l", col="darkblue")
#lines(ee.1, lwd=3, col="red")

#legend("topright", col=c("blue","red"), 
#       legend = c("1960-1988","1989-2017"),
#       bty = "n", lty = 1, lwd=3)



ggplot(Seasonal_cycle_Tmaletmnfh, aes(x=Seasonal_cycle_Tmaletmnfh[,1])) +
  geom_line(aes(y=Seasonal_cycle_Tmaletmnfh[,2], lwd=.25)) +
  geom_line(aes(y= gg.1[,2],col="1960-1988",lwd=1.25)) +
  geom_line(aes(y=Seasonal_cycle_Tmaletmnsh[,2],lwd=.25)) + 
  geom_line(aes(y=ee.1[,2],col="1989-2016", lwd=1.25)) +
  scale_colour_manual("",
                      values = c("1960-1988"="darkblue", "1989-2016"="red")) +
  ggtitle("Seasonal Cycle of Minimum Temperature (1960-2016)\n Tamale") + xlab("Day") +  
  ylab(yyy) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Min Tmp of Tamale(1960-88-2016).png",
         width = 1000, height=750)
dev.off()




#Seasonal Max Tmp on Monthly Basis
head(Tmin)
Ap3 <- mean(subset(Tmin,format.Date(date, "%m")=="04")$Tmin, na.rm=T)
Ma3 <- mean(subset(Tmin,format.Date(date, "%m")=="05")$Tmin, na.rm=T)
Ju3 <- mean(subset(Tmin,format.Date(date, "%m")=="06")$Tmin, na.rm=T)
Jl3 <- mean(subset(Tmin,format.Date(date, "%m")=="07")$Tmin, na.rm=T)
Au3 <- mean(subset(Tmin,format.Date(date, "%m")=="08")$Tmin, na.rm=T)
Se3 <- mean(subset(Tmin,format.Date(date, "%m")=="09")$Tmin, na.rm=T)
Oc3 <- mean(subset(Tmin,format.Date(date, "%m")=="10")$Tmin, na.rm=T)

Seas3 <- data.frame(
  dates=Mn.mn.tmx[4:10, 1],
  Tmin=c(Ap3,Ma3,Ju3,Jl3,Au3,Se3,Oc3)
)

ggplot(data=Seas3,
       aes(x=dates,y=Tmin)) +
  geom_line(col="firebrick", lwd=2) +
  ggtitle(" Seasonal Minimum Temperature (1960-2016)\n Tamale") + xlab("Month") +
  ylab(yyy) + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Tmin.png",
         width = 1000, height = 650
)
dev.off()




#Indexing
#first half
#head(Tmin_fh)
#lng.mn.Tminfh <- mean(Tmin_fh$Tmin)
#sd.Tminfh <- sd(Tmin_fh$Tmin)
#Tmin_fh_anom.v <- (Tmin_fh$Tmin - lng.mn.Tminfh)/sd.Tminfh
#Tmin_fh_anom <- data.frame(Tmin_fh$date, Tmin_fh_anom.v)
#ggplot(Tmin_fh_anom, aes(Tmin_fh$date, Tmin_fh_anom.v, ymin = 0,ymax = Tmin_fh_anom.v)) + 
#  geom_linerange(data = Tmin_fh_anom, aes(colour = ifelse(Tmin_fh_anom.v >0, "Positive", "Negative")),stat = "identity",
#                                                                                                         position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + (scale_x_date(date_breaks="4 year",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + labs(title="Index of Annual Minimum Tmp of Tamale \n (1960-1988)", x="Years", y="Index")
#dev.copy(png, filename="Index of Minimum Tmp of Tamale (1960-1988).png", width=1000, height=750)
#dev.off()


#Second half
#head(Tmin_sh)
#lng.mn.Tminsh <- mean(Tmin_sh$Tmin)
#sd.Tminsh <- sd(Tmin_sh$Tmin)
#Tmin_sh_anom.v <- (Tmin_sh$Tmin - lng.mn.Tminsh)/sd.Tminsh
#Tmin_sh_anom <- data.frame(Tmin_sh$date, Tmin_sh_anom.v)
#ggplot(Tmin_sh_anom, aes(Tmin_sh$date, Tmin_sh_anom.v, ymin = 0,ymax = Tmin_sh_anom.v)) + 
#  geom_linerange(data = Tmin_sh_anom, aes(colour = ifelse(Tmin_sh_anom.v >0, "Positive", "Negative")),stat = "identity",
#                                                                                                         position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + labs(title="Index of Annual Minimum Tmp of Tamale \n (1989-2016)", x="Years", y="Index")
#dev.copy(png, filename="Index of Minimum Tmp of Tamale (1989-2016).png", width=1000, height=750)
#dev.off()

