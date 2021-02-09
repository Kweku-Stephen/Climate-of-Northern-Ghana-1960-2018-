
#Minimum Temperature

head(data1)
Tmin <- data.frame(data1$dates, data1$Tmin)
names(Tmin) <- c("date","Tmin")
head(Tmin)

#......................................................................................................#

#Mean Annual Temperature
Tmin.xts <- xts(Tmin$Tmin,Tmin$date)
Tmin.ann.xts <- apply.yearly(Tmin.xts, mean, na.rm=T)
head(Tmin.ann.xts)
colSums(is.na(Tmin.ann.xts))

#conversion to d.f
Tmin.ann.df <- data.frame(
  date=index(Tmin.ann.xts), 
  coredata(Tmin.ann.xts)
)

Tmin.ann.df$date <- as.Date(
  Tmin.ann.df$date, 
  format="%Y-%m-%d"
)

names(Tmin.ann.df)[2] <- "Tmin"
#estimating values for missing cases in this dataset (2014 - 2015)
Tmin.ann.df$Tmin <- na_kalman(
  Tmin.ann.df$Tmin, 
  model = "StructTS",
  smooth=T, nit = -1
)

head(Tmin.ann.df)
colSums(is.na(Tmin.ann.df))

MK <- MannKendall(Tmin.ann.df[,2])
summary(MK)

write.csv(Tmin.ann.df,file = "AAnnuAl Min Tmp of Zuarungu.csv")


ggplot(Tmin.ann.df, aes(x=date, y=Tmin)) + geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm) +  
  labs(title="Annual Minimum Temperature (1960-2016)\n Zuarungu", 
       x="Year", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "4 years", date_labels = "%Y") + 
  annotate("text",x=Tmin.ann.df[Tmin.ann.df[,1] == "1978-12-31", ]$date,
           cex=5, label="P-value =< 2.22e-16", y= 20.5)

dev.copy(png, filename="Mean Annual Min Tmmp of Zuarungu (1960-2016).png",
         width = 1000, height = 750)
dev.off()

sumn <- summary(Tmin.ann.df)
write.csv(sumn,
          file="Summary statistics for mean annual Mn Tmp of Zuarungu(1960-2016).csv")


sumnn <- data.frame(
  Tmin.ann.df[
    Tmin.ann.df[,2]==max(Tmin.ann.df[,2]),],
  Tmin.ann.df[
    Tmin.ann.df[,2]==min(Tmin.ann.df[,2]),]
)

sumnn[2,c(1,2)] <- sumnn[1, c(3:4)] 
sumnn[-c(3,4)] -> sumnn
#sumxx3[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumnn) <- list(
  c("Max:","Min:"), 
  c("Year","Tmin")
) 
write.csv(sumnn1,
          file = "summary stats_1 for Zuarungu Ann Min Tmp(1960-2017).csv")



#Mean Monthly Temperature
head(Tmin.xts)
Tmin.mn.xts <- apply.monthly(Tmin.xts, mean,na.rm=T)

#conversion (d.f)
Tmin.mn.df <- data.frame(
  date=index(Tmin.mn.xts),
  coredata(Tmin.mn.xts)
)

names(Tmin.mn.df)[2] <- "Tmin"
head(Tmin.mn.df)

Tmin.mn.df$date <- as.Date(
  Tmin.mn.df$date,
  format="%Y-%m-%d"
)

head(Tmin.mn.df)
Tmin.mn.df <- drop_na(Tmin.mn.df)

#Estimating missing cases
#subsetting all individual months
Janun <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="01")
#Janu$Tmax <- na.kalman(Janu$Tmax, model="StructTS",smooth = T, nit = -1)
#
Febrn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="02")
#Febr$Tmax <- na.kalman(Febr$Tmax, model="StructTS",smooth = T, nit = -1)
#
Marcn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="03")
#Marc$Tmax <- na.kalman(Marc$Tmax, model="StructTS",smooth = T, nit = -1)
#
Aprin <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="04")
#Apri$Tmax <- na.kalman(Apri$Tmax, model="StructTS",smooth = T, nit = -1)
#4 missing
Mayyn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="05")
#Mayy$Tmax <- na.kalman(Mayy$Tmax, model="StructTS",smooth = T, nit = -1)
#
Juneen <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="06")
#Junee$Tmax <- na.kalman(Junee$Tmax, model="StructTS",smooth = T, nit = -1)
#
Julyyn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="07")
#Julyy$Tmax <- na.kalman(Julyy$Tmax, model="StructTS",smooth = T, nit = -1)
#
Augusn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="08")
#Augus$Tmax <- na.kalman(Augus$Tmax, model="StructTS",smooth = T, nit = -1)

Septen <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="09")
#Septe$Tmax <- na.kalman(Septe$Tmax, model="StructTS",smooth = T, nit = -1)

Octon <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="10")
#Octo$Tmax <- na.kalman(Octo$Tmax, model="StructTS",smooth = T, nit = -1)

Novemn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="11")
#Novem$Tmax <- na.kalman(Novem$Tmax, model="StructTS",smooth = T, nit = -1)

Decemn <- subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="12")
#Decem$Tmax <- na.kalman(Decem$Tmax, model="StructTS",smooth = T, nit = -1)

#creating a dataframe with the above

#Tmax.mn.df.new <- data.frame(Janu$date, Janu$Tmax, Febr$Tmax, Marc$Tmax,
#                             Apri$Tmax,Mayy$Tmax,Junee$Tmax,Julyy$Tmax,
#                              Augus$Tmax,Septe$Tmax,Octo$Tmax,
#                             Novem$Tmax,Decem$Tmax)


#Averaging
J.n <- mean(Janun$Tmin, na.rm=T)
F.n <- mean(Febrn$Tmin, na.rm=T)
M.n <- mean(Marcn$Tmin, na.rm=T)
A.n <- mean(Aprin$Tmin, na.rm=T)
Ma.n <- mean(Mayyn$Tmin, na.rm=T)
Ju.n <- mean(Juneen$Tmin, na.rm=T)
Jl.n <- mean(Julyyn$Tmin, na.rm=T)
Au.n <- mean(Augusn$Tmin, na.rm=T)
S.n <- mean(Septen$Tmin, na.rm=T)
O.n <- mean(Octon$Tmin, na.rm=T)
N.n <- mean(Novemn$Tmin, na.rm=T)
D.n <- mean(Decemn$Tmin, na.rm=T)

Mn.m.tn.v <- c(J.n, F.n,M.n,A.n,Ma.n,Ju.n,Jl.n,
               Au.n,S.n,O.n,N.n,D.n)

date.mn
Mn.mn.tmn <- data.frame(date.mn, Mn.m.tn.v)
write.csv(Mn.mn.tmn,file = "Mean Monthly Tmin of Zuarungu.csv")

ggplot(Mn.mn.tmn, aes(x=date.mn, y=Mn.m.tn.v)) +
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Minimum Temperature (1960-2016)\n Zuarungu",
       x="Months", y=expression("Temperature("*~degree*c*")")) +
  scale_x_date(date_breaks= "1 month", date_labels = "%b") 

dev.copy(png, filename="Mean Monthly Min Tmmp of Zuarungu (1960-2016).png",
         width = 1000, height = 750)
dev.off()


sumn1 <- summary(Mn.mn.tmn)
write.csv(sumn1,
          file="Summary statistics for mean monthly Mn Tmp of Zuarungu(1960-2016).csv")

sumnn1 <- data.frame(
  Mn.mn.tmn[
    Mn.mn.tmn[,2]==max(Mn.mn.tmn[,2]),],
  Mn.mn.tmn[
    Mn.mn.tmn[,2]==min(Mn.mn.tmn[,2]),]
)

sumnn1[2,c(1,2)] <- sumnn1[1, c(3:4)] 
sumnn1[-c(3,4)] -> sumnn1
sumnn1[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn1) <- list(
  c("Max:","Min:"), 
  c("Month","Tmin")
) 
write.csv(sumnn1,
          file = "summary stats_1 for Zuarungu Mntly Min Tmp(1960-2017).csv")



# seasonal Cycle of Max Tmp of Tamale
head(Tmin)
data <- Tmin
names(data) <- c("dates","Rain")
head(data)

mean_dailytmn_Zuarungu<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,
                           Jan_10,
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

Seasonal_cycle_Zuarungutmn <-data.frame(
  c(1:366),
  mean_dailytmn_Zuarungu
)

write.csv(Seasonal_cycle_Zuarungutmn, 
          file = "data_for_seasonal_cycle_of_Min tmp of Zuarungu (1960-2016).csv")

t <- ksmooth(
  c(1:366),
  mean_dailytmn_Zuarungu, bandwidth = 15
)

t.1 <- data.frame(t) # converting to a dataframe
ggplot(Seasonal_cycle_Zuarungutmn, aes(x=c(1:366), y=mean_dailytmn_Zuarungu)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1960-2016)\n Zuarungu",
       x="Day", y=yyy) + 
  geom_line(data = t.1, aes(x=x, y=y), col="blue", lwd=1)

dev.copy(png, filename="Seasonal Cycle of Mean Min Tmmp of Zuarunug (1960-2016).png",
         width = 1000, height = 750)
dev.off()


sumnn2 <- data.frame(
  Seasonal_cycle_Zuarungutmn[
    Seasonal_cycle_Zuarungutmn[,2]==max(Seasonal_cycle_Zuarungutmn[,2]),],
  Seasonal_cycle_Zuarungutmn[
    Seasonal_cycle_Zuarungutmn[,2]==min(Seasonal_cycle_Zuarungutmn[,2]),]
)

sumnn2[2,c(1,2)] <- sumnn2[1, c(3:4)] 
sumnn2[-c(3,4)] -> sumnn2
#sumnn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn2) <- list(
  c("Max:","Min:"), 
  c("Day","Tmin")
) 
write.csv(sumnn2,
          file = "summary stats_1 for Zuarungu Seas.cycle Min Tmp(1960-2017).csv")



#Indexing of Max tmp of Tamale (1960-2016)
#Tmin.ann.df.1 <- drop_na(Tmin.ann.df)
lng.mn.Tmin <- mean(Tmin.ann.df$Tmin)
sd.Tmin <- sd(Tmin.ann.df$Tmin)

Tmin.ann.df.anom.v <- (Tmin.ann.df$Tmin - lng.mn.Tmin)/
  sd.Tmin

Tmin.ann.df.anom <- data.frame(
  Tmin.ann.df$date, 
  Tmin.ann.df.anom.v
)

write.csv(Tmin.ann.df.anom, 
          file="Index of Annual Min Tmp of Zuarungu (1960-2016).csv")

ggplot(Tmin.ann.df.anom, aes(Tmin.ann.df.anom[,1], Tmin.ann.df.anom[,2], ymin = 0,
                             ymax = Tmin.ann.df.anom[,2])) + 
  geom_linerange(data = Tmin.ann.df.anom, aes(colour = ifelse(Tmin.ann.df.anom[,2] >0,
        "Positive", "Negative")),stat = "identity", position = "identity",size=2) +
  theme(legend.title = element_text(size = 2, colour = F)) + 
  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title="Minimum Temperature Anomaly (1960-2016) \n Zuarungu",
       x="Years", y="Index")

dev.copy(png, filename='Index of Minimum Tmp of Zuarungu (1960-2016).png',
         width=1000, height = 750)
dev.off()


sumn2 <- summary(Tmin.ann.df.anom)
write.csv(sumn2,
          file="Summary statistics for Index of annual Mn Tmp of Zuarungu(1989-2016).csv")


sumnn2 <- data.frame(
  Tmin.ann.df.anom[
    Tmin.ann.df.anom[,2]==max(Tmin.ann.df.anom[,2]),],
  Tmin.ann.df.anom[
    Tmin.ann.df.anom[,2]==min(Tmin.ann.df.anom[,2]),]
)

sumnn2[2,c(1,2)] <- sumnn2[1, c(3:4)] 
sumnn2[-c(3,4)] -> sumnn2
#sumnn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn2) <- list(
  c("Max:","Min:"), 
  c("Year","Anomaly")
) 
write.csv(sumnn2,
          file = "summary stats_1 for Zuarungu Anomaly_Min Tmp(1960-2017).csv")




#Dividing Into two halves
#Dividing Tmin into two halves
#Dividing daily Tmin
head(Tmin)
Tmin.fh <- Tmin[(Tmin$date >= "1960-01-01" &
                   Tmin$date <= "1988-12-31"), ]

Tmin.sh <- Tmin[(Tmin$date >= "1989-01-01" & 
                   Tmin$date <= "2016-12-31"), ]

#Dividing annual Prcp
Tmin_fh <- Tmin.ann.df[(Tmin.ann.df$date >= "1960-12-31" & 
                          Tmin.ann.df$date <= "1988-12-31"), ]

Tmin_sh <- Tmin.ann.df[(Tmin.ann.df$date >= "1989-12-31" & 
                          Tmin.ann.df$date <= "2016-12-31"), ]


#Annual Tmax first half (1975-1996)
Tmin_fh
MK <- MannKendall(Tmin_fh[,2])
summary(MK)

write.csv(Tmin_fh,
          file="Annual Min.Tmp of Vea (1972-1994).csv")

ggplot(Tmin_fh, aes(x=Tmin_fh$date, y=Tmin_fh$Tmin)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + scale_x_date(date_breaks = "3 years",
                                                      date_labels = "%Y") +
  labs(title='Annual Minimum Temperature(1960-1988)\n Zuarungu',
       x="Year", y=yyy) + 
  annotate("text", x=Tmin.fh[Tmin.fh[,1] == "1968-12-31", ]$date, y=23,
           cex=5, label="P-value = 0.13735")

dev.copy(png, filename="Mean Annual Min Tmp of Zuarungu (1960-1988).png",
         width = 1000, height = 750)
dev.off()


sumn3 <- summary(Tmin_fh)
write.csv(sumn3,
          file="Summary statistics for mean annual Mn Tmp of Zuarungu(1960-1988).csv")


sumnn3 <- data.frame(
  Tmin_fh[
    Tmin_fh[,2]==max(Tmin_fh[,2]),],
  Tmin_fh[
    Tmin_fh[,2]==min(Tmin_fh[,2]),]
)

sumnn3[2,c(1,2)] <- sumnn3[1, c(3:4)] 
sumnn3[-c(3,4)] -> sumnn3
#sumnn3[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn3) <- list(
  c("Max:","Min:"), 
  c("Year","Tmin")
) 
write.csv(sumnn3,
          file = "summary stats_1 for Zuarungu Annual Min Tmp(1960-1988).csv")




#Mean Monthly Tmax
head(Tmin.mn.df)
Tmin.mn.dffh <- Tmin.mn.df[(Tmin.mn.df$date >= "1960-01-01" &
                              Tmin.mn.df$date <= "1988-12-31"), ]

Tmin.mn.dfsh <- Tmin.mn.df[(Tmin.mn.df$date >= "1989-01-01" & 
                              Tmin.mn.df$date <= "2016-12-31"), ]

#First Half
head(Tmin.mn.dffh)

#Averaging all months
Jann.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="01")$Tmin, na.rm = T
)
Febn.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="02")$Tmin, na.rm = T
)
Marchn.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="03")$Tmin, na.rm = T
)
Apriln.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="04")$Tmin, na.rm = T
)
Mayn.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="05")$Tmin, na.rm = T
)
Junen.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="06")$Tmin, na.rm = T
)
Julyn.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="07")$Tmin, na.rm = T
)
Augustn.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="08")$Tmin, na.rm = T
)
Septembern.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="09")$Tmin, na.rm = T
)
Octobern.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="10")$Tmin, na.rm = T
)
Novembern.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="11")$Tmin, na.rm = T
)
Decembern.fh <- mean(subset(Tmin.mn.dffh,format.Date(
  Tmin.mn.dffh[,1], "%m")=="12")$Tmin, na.rm = T
)

#creating a vector
mean.mn.Tminfh.v <- c(Jann.fh,Febn.fh,Marchn.fh,Apriln.fh,Mayn.fh,Junen.fh,
                      Julyn.fh,Augustn.fh,Septembern.fh,Octobern.fh,
                      Novembern.fh,Decembern.fh)

date.mn 
mean.mn.Tminfh <- data.frame(
  date.mn, 
  mean.mn.Tminfh.v
)

write.csv(mean.mn.Tminfh,
          file = "Mean Monthly Min Tmp of Zuarungu(1960-1988).csv")


ggplot(mean.mn.Tminfh, aes(x=date.mn, y=mean.mn.Tminfh.v)) + 
  geom_line(col="firebrick", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Minimum Temperature (1960-1988)\n Zuarungu", 
       x="Month",y=yyy)

dev.copy(png, filename="Mean Monthly Min Tmp of Zuarungu (1960-1988).png",
         width = 1000, height=750)
dev.off()

sumn4 <- summary(mean.mn.Tminfh)
write.csv(sumn4,
          file="Summary statistics for mean monthly Mn Tmp of Zuarungu(1960-1988).csv")

sumnn4 <- data.frame(
  mean.mn.Tminfh[
    mean.mn.Tminfh[,2]==max(mean.mn.Tminfh[,2]),],
  mean.mn.Tminfh[
    mean.mn.Tminfh[,2]==min(mean.mn.Tminfh[,2]),]
)

sumnn4[2,c(1,2)] <- sumnn4[1, c(3:4)] 
sumnn4[-c(3,4)] -> sumnn4
sumnn4[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn4) <- list(
  c("Max:","Min:"), 
  c("Month","Tmin")
) 
write.csv(sumnn4,
          file = "summary stats_1 for Zuarungu Mntly Min Tmp(1960-1988).csv")



#Annual Tmax second half (1995 - 2017)
head(Tmin_sh)
MK <- MannKendall(Tmin_sh[,2])
summary(MK)

write.csv(Tmin_sh, 
          file="Annual Tmin of Vea (1995-2017).csv")

ggplot(Tmin_sh, aes(x=Tmin_sh$date, y=Tmin_sh$Tmin)) + 
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(title='Annual Minimum Temperature (1989-2016)\n Zuarungu',
       x="Year", y=yyy) + 
  annotate("text", x=Tmin_sh[Tmin_sh[,1] == "2000-12-31", ]$date,cex=5,
           y=23.5,label="P-value=0.042578")

dev.copy(png, filename="Mean Monthly Min Tmp of Zuarungu(1989-2016).png",
         width = 1000, height=750)
dev.off()


sumn5 <- summary(Tmin_sh)
write.csv(sumn5,
          file="Summary statistics for mean annual Mn Tmp of Zuarungu(1960-1988).csv")

sumnn5 <- data.frame(
  Tmin_sh[
    Tmin_sh[,2]==max(Tmin_sh[,2]),],
  Tmin_sh[
    Tmin_sh[,2]==min(Tmin_sh[,2]),]
)

sumnn5[2,c(1,2)] <- sumnn5[1, c(3:4)] 
sumnn5[-c(3,4)] -> sumnn5
sumnn5[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn5) <- list(
  c("Max:","Min:"), 
  c("Year","Tmin")
) 
write.csv(sumnn5,
          file = "summary stats_1 for Zuarungu Ann. Min Tmp(1989-2017).csv")



#second Half
head(Tmax.mn.dfsh)

#Averaging all months
Jann.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="01")$Tmin, na.rm = T
)
Febn.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="02")$Tmin, na.rm = T
)
Marchn.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="03")$Tmin, na.rm = T
)
Apriln.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="04")$Tmin, na.rm = T
)
Mayn.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="05")$Tmin, na.rm = T
)
Junen.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="06")$Tmin, na.rm = T
)
Julyn.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="07")$Tmin, na.rm = T
)
Augustn.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="08")$Tmin, na.rm = T
)
Septembern.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="09")$Tmin, na.rm = T
)
Octobern.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="10")$Tmin, na.rm = T
)
Novembern.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="11")$Tmin, na.rm = T
)
Decembern.sh <- mean(subset(Tmin.mn.dfsh,format.Date(
  Tmin.mn.dfsh[,1], "%m")=="12")$Tmin, na.rm = T
)


#creating a vector
mean.mn.Tminsh.v <- c(Jann.sh,Febn.sh,Marchn.sh,Apriln.sh,Mayn.sh,Junen.sh,Julyn.sh,
                      Augustn.sh,Septembern.sh,Octobern.sh,Novembern.sh,Decembern.sh)

date.mn 

mean.mn.Tminsh <- data.frame(
  date.mn, 
  mean.mn.Tminsh.v
)

write.csv(mean.mn.Tminsh,
          file = "Mean Monthly Min Tmp of Zuarungu(1989-2017).csv")



ggplot(mean.mn.Tminsh, aes(x=date.mn, y=mean.mn.Tminsh.v)) +
  geom_line(col="firebrick", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Minimum Temperature (1989-2016)\n Zuarungu",
       x="Month",y=yyy)

dev.copy(png, filename="Mean Monthly Tmin of Zuarungu (1989-2016).png",
         width=1000, height = 750)
dev.off()

sumnn6 <- data.frame(
  mean.mn.Tminsh[
    mean.mn.Tminsh[,2]==max(mean.mn.Tminsh[,2]),],
  mean.mn.Tminsh[
    mean.mn.Tminsh[,2]==min(mean.mn.Tminsh[,2]),]
)

sumnn6[2,c(1,2)] <- sumnn6[1, c(3:4)] 
sumnn6[-c(3,4)] -> sumnn6
sumnn6[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn6) <- list(
  c("Max:","Min:"), 
  c("Month","Tmin")
) 
write.csv(sumnn6,
          file = "summary stats_1 for Zuarungu Mntly Min Tmp(1989-2017).csv")



#plot(mean.mn.Tmaxfh, type="l", lwd=3, col="darkblue", 
#     main="Mean Monthly Max Tmp of Tamale- 1960-2016", 
#     ylim=c(30,40), xlab="Months", ylab=yyy)

#lines(mean.mn.Tmaxsh, col="red", lwd=3, type="l")
#legend("topright", legend = c("1960-1988", "1989-2016"), 
#       col=c("darkblue","red"), bty = "n", lty = 1,lwd=3)


#ggplot(mean.mn.Tminfh, aes(x=mean.mn.Tminfh[,1],y=mean.mn.Tminfh[,2])) +
#  geom_line(col="darkblue", lwd=2) +
#  geom_line(data=mean.mn.Tminsh, mapping=aes(x=mean.mn.Tminfh[,1], 
#                                             y=mean.mn.Tminsh[,2]),col="red", lwd=2) +
#  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
#  labs(x="Month",y=yyy, title="Mean Monthly Min Tmp of Zurungu \n (1960-2016)") #

#dev.copy(png,filename="Mean Monthly Min Tmp of Zuarungu (1960-88-2016).png",
#         width = 1000, height=750)
#dev.off()


#Superimposing both plots
ggplot(data = mean.mn.Tminfh, aes(x = mean.mn.Tminfh[,1])) +
  geom_line(aes(y = mean.mn.Tminfh[,2], colour = "1960-1988"), lwd=1.5) +
  geom_line(aes(y = mean.mn.Tminsh[,2], colour = "1989-2016"), lwd=1.5) +
  scale_colour_manual("", 
                      values = c("1960-1988"="darkblue", "1989-2016"="red")) +
  xlab("Months") + ylab(yyy) + 
  ggtitle("Mean Monthly Minimum Temperature (1960-2016) \n Zuarungu") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename = "Mean Monthly Min Tmp of Zuarungu (1960-88-2016).png",
         width = 1000, height = 750)
dev.off()


sumn6 <- summary(mean.mn.Tminfh)
write.csv(sumn6,
          file="Summary statistics for mean monthly Mn Tmp of Zuarungu(1989-2016).csv")



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

mean_dailytmn_Zuarungufh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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

Seasonal_cycle_Zuarungutmnfh <-data.frame(
  c(1:366),
  mean_dailytmn_Zuarungufh)

write.csv(Seasonal_cycle_Zuarungutmnfh, 
          file = "data_for_seasonal_cycle_of_Min tmp of Zuarungu(1960-1988).csv")

ww <- ksmooth(
  c(1:366), 
  mean_dailytmn_Zuarungufh, 
  bandwidth = 15)

ww.1 <- data.frame(ww) # converting to a dataframe
ggplot(Seasonal_cycle_Zuarungutmnfh, aes(x=c(1:366), 
                                    y=mean_dailytmn_Zuarungufh)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1960-1988)\n Zuaurngu", 
       x="Day", y=yyy) +
  geom_line(data = ww.1, aes(x=x, y=y), col="blue", lwd=1)

dev.copy(png, filename="Seasonal Cycle of Min Tmp of Zuarungu (1960-1988).png", 
         width = 1000, height=750)
dev.off()

sumnn7 <- data.frame(
  Seasonal_cycle_Zuarungutmnfh[
    Seasonal_cycle_Zuarungutmnfh[,2]==max(Seasonal_cycle_Zuarungutmnfh[,2]),],
  Seasonal_cycle_Zuarungutmnfh[
    Seasonal_cycle_Zuarungutmnfh[,2]==min(Seasonal_cycle_Zuarungutmnfh[,2]),]
)

sumnn7[2,c(1,2)] <- sumnn7[1, c(3:4)] 
sumnn7[-c(3,4)] -> sumnn7
#sumnn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn7) <- list(
  c("Max:","Min:"), 
  c("Day","Tmin")
) 
write.csv(sumnn7,
          file = "summary stats_1 for Zuarungu Seas.cycle Min Tmp(1960-1988).csv")



#Second Half
head(Tmin.sh)
data <- Tmin.sh
names(data) <- c("dates","Rain")
head(data)

mean_dailytmn_Zuarungush<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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

Seasonal_cycle_Zuarungutmnsh <-data.frame(
  c(1:366),
  mean_dailytmn_Zuarungush)

write.csv(Seasonal_cycle_Zuarungutmnsh,
          file = "data_for_seasonal_cycle_of_Min tmp of Zuarungu(1989-2016).csv")

yy <- ksmooth(
  c(1:366), 
  mean_dailytmn_Zuarungush, 
  bandwidth = 15)

yy.1 <- data.frame(yy) # converting to a dataframe
ggplot(Seasonal_cycle_Zuarungutmnsh, aes(x=c(1:366), 
                                    y=mean_dailytmn_Zuarungush)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1988-2016)\n Zuarungu", 
       x="Day", y=yyy) + 
  geom_line(data = yy.1, aes(x=x, y=y), col="blue", lwd=1)

dev.copy(png, filename='Seaonal Cycle of Min Tmp of Zuarungu (1989-2016).png',
         width =1000, height=750)
dev.off()


sumnn8 <- data.frame(
  Seasonal_cycle_Zuarungutmnsh[
    Seasonal_cycle_Zuarungutmnsh[,2]==max(Seasonal_cycle_Zuarungutmnsh[,2]),],
  Seasonal_cycle_Zuarungutmnsh[
    Seasonal_cycle_Zuarungutmnsh[,2]==min(Seasonal_cycle_Zuarungutmnsh[,2]),]
)

sumnn8[2,c(1,2)] <- sumnn8[1, c(3:4)] 
sumnn8[-c(3,4)] -> sumnn8
#sumnn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumnn8) <- list(
  c("Max:","Min:"), 
  c("Day","Tmin")
) 
write.csv(sumnn8,
          file = "summary stats_1 for Zuarungu Seas.cycle Min Tmp(1989-2017).csv")



#plot(Seasonal_cycle_Tmaletmxfh, lwd=1, type="l", 
#     col="darkblue", main="Seasonal Max Tmp Cycle of Tamale- 1960-2017", 
#     xlab="Days", ylab=yyy)
#lines(aa.1, col="blue", lwd=3)
#lines(Seasonal_cycle_Tmaletmxsh, type="l", col="darkblue")
#lines(nn.1, lwd=3, col="red")

#legend("topright", col=c("blue","red"), 
#       legend = c("1960-1988","1989-2017"),
#       bty = "n", lty = 1, lwd=3)


#ggplot(Seasonal_cycle_Zuarungutmnfh, aes(x=Seasonal_cycle_Zuarungutmnfh[,1], 
#                                    Seasonal_cycle_Zuarungutmnfh[,2])) + 
#  geom_line(col="darkblue", lwd = .25) + geom_line(data=aa.1, mapping = aes(x=x, y=y),
#                                                   col="darkgreen", lwd=1.25) + 
#  geom_line(data=Seasonal_cycle_Zuarungutmnsh, aes(x= Seasonal_cycle_Zuarungutmnsh[,1], 
#                                             y=Seasonal_cycle_Zuarungutmnsh[,2]),col="darkblue",
#            lwd=.25) + geom_line(data=nn.1, mapping = aes(x=x, y=y), col="red",
#                                 lwd=1.25) + xlab("Days") + ylab(yyy) + 
#  ggtitle("Seaonal Cycle of Min Tmp of  Zuarungu \n (1960-2016)")



#Superimposing both plots

ggplot(data=Seasonal_cycle_Zuarungutmnfh,aes(x=Seasonal_cycle_Zuarungutmnfh[,1]),lwd=.25) +
  geom_line(aes(y=Seasonal_cycle_Zuarungutmnfh[,2]), lwd=.25) +
  geom_line(aes(y = ww.1[,2], colour = "1960-1988"), lwd=1.5) +
  geom_line(aes(y = Seasonal_cycle_Zuarungutmnsh[,2]), lwd=.25) +
  geom_line(aes(y=yy.1[,2], colour = "1989-2016"), lwd=1.5) +
  scale_colour_manual("", 
                      values = c("1960-1988"="darkblue", "1989-2016"="red")) +
  xlab("Day") + ylab(yyy) + 
  ggtitle("Seaonal Cycle of Minimum Temperature of (1960-2016) \n Zuarungu") 

dev.copy(png, filename="Seasonal Cycle of Min Tmp of Zuarungu(1960-88-2016).png", 
         width = 1000, height = 750)
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

require(ggplot2)
ggplot(data=Seas3,
       aes(x=dates,y=Tmin)) +
  geom_line(col="firebrick", lwd=2) +
  ggtitle(" Seasonal Minimum Temperature (1960-2017)\n Vea") + xlab("Month") +
  ylab(yyy) + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Tmin.png",
         width = 1000, height = 650)
dev.off()





#Indexing
#first half
#head(Tmin_fh)
#min_fh.1 <- drop_na(Tmin_fh)
#lng.mn.Tminfh <- mean(Tmin_fh.1$Tmin)
#sd.Tminfh <- sd(Tmin_fh.1$Tmin)
#
#Tmin_fh_anom.v <- (Tmin_fh.1$Tmin - lng.mn.Tminfh)/
#  sd.Tminfh

#Tmin_fh_anom <- data.frame(
 # Tmin_fh.1$date,
#  Tmin_fh_anom.v
#)
#
#ggplot(Tmin_fh_anom, aes(Tmin_fh.1$date, Tmin_fh_anom.v, 
#                         ymin = 0,ymax = Tmin_fh_anom.v)) + 
#  geom_linerange(data = Tmin_fh_anom, aes(colour = ifelse(Tmin_fh_anom.v >0,
 #                                                         "Positive","Negative")),stat = "identity",position="identity",size=2) +
#  theme(legend.title = element_text(size = 2, colour = F)) + 
#  (scale_x_date(date_breaks="4 year",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Minimum Tmp of Zuarungu \n (1960-1988)", 
#       x="Years", y="Index")

#dev.copy(png, filename="Seasonal Cycle of Annual Min tMP OF Zuarungu(1960-1988).png",
#         width=1000, height=750)
#dev.off()


#Second half
#head(Tmin_sh)
#Tmin_sh.1 <- drop_na(Tmin_sh)
#lng.mn.Tminsh <- mean(Tmin_sh.1$Tmin)
#sd.Tminsh <- sd(Tmin_sh.1$Tmin)

#Tmin_sh_anom.v <- (Tmin_sh.1$Tmin - lng.mn.Tminsh)/
#  sd.Tminsh

#Tmin_sh_anom <- data.frame(
#  Tmin_sh.1$date, 
#  Tmin_sh_anom.v
#)

#ggplot(Tmin_sh_anom, aes(Tmin_sh.1$date, Tmin_sh_anom.v, ymin = 0,
#                         ymax = Tmin_sh_anom.v)) + 
#  geom_linerange(data = Tmin_sh_anom, aes(colour = ifelse(Tmin_sh_anom.v >0,
#                                                          "Positive","Negative")),stat="identity",position="identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) +
#  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Minimum Tmp of Zuarungu \n (1989-2016)",
#       x="Years", y="Index")

#dev.copy(png, filename="Seasonal Cycle of Annual Min Tmp OF Zuarungu (1989-2016).png",
#         width=1000, height=750)
#dev.off()
