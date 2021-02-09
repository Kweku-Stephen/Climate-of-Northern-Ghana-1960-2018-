###########################################################################################

head(data1)
Tmn <- data.frame(data1$date,data1$Tmin)
names(Tmn) <- c("date", "Tmin")
head(Tmn)

#Mean Annual Minimum Temperature
Tmn.xts <- xts(Tmn$Tmin, Tmn$date)
Tmn.xts.ann <- apply.yearly(
  Tmn.xts, 
  mean, 
  na.rm=T)

head(Tmn.xts.ann)

#conversion to d.f
Tmn.df.ann <- data.frame(
  date=index(Tmn.xts.ann), 
  coredata(Tmn.xts.ann))

names(Tmn.df.ann)[2] <- "Tmin"
head(Tmn.df.ann)

#date conversion to date class
Tmn.df.ann$date <- as.Date(
  Tmn.df.ann$date, 
  format="%Y-%m-%d")

#delete all NaN'S
#Tmn.df.ann <- Tmn.df.ann %>% drop_na()
#deleting 2004 cos only two months data present
#Tmn.df.ann <- Tmn.df.ann[!(Tmn.df.ann$date == "2004-12-31"), ]

write.csv(Tmn.df.ann, 
          file="Mean Annual Min Tmp of Babile (1960-2014).csv") 

MK <- MannKendall(Tmn.df.ann[,2])
summary(MK)

ggplot(Tmn.df.ann, aes(x=date,y=Tmin)) + geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method=lm,col="blue") + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  labs(title="Minimum Temperature (1960-2014)\n Babile",
       x="Year",y=expression("Temperature("*~degree*c*")")) + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  annotate("text", x=Tmn.df.ann[Tmn.df.ann[,1] == "1969-12-31", ]$date,
           y=23, cex=5, label="P-value = 0.0011677")

dev.copy(png, filename="Annual Minimum Tmp of Babile (1960-2014).png", 
         width = 1000, height = 750)
dev.off()

sumn <- summary(Tmn.df.ann)
write.csv(sumn,
          file="Summary stats for mean annual min Tmp of Babile(1960-2014).csv")
#or
Tmin.ann.df.1 <- drop_na(Tmn.df.ann)
sumn <- data.frame(Tmin.ann.df.1[Tmin.ann.df.1[,2] == max(Tmin.ann.df.1[,2]), ],
                   Tmin.ann.df.1[Tmin.ann.df.1[,2] == min(Tmin.ann.df.1[,2]), ])

sumn[2,c(1,2)] <- sumn[1, c(3,4)] 
sumn[-c(3,4)] -> sumn
#sumxn[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumn) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn,
          file = "summary stats for ann Min Tmp (1960-2014).csv")




#Mean Monthly Tmin
#Mean Monthly Temperature
head(Tmn.xts)
Tmn.mn.xts <- apply.monthly(
  Tmn.xts,
  mean,
  na.rm=T)

#coversion to d.f
Tmn.mn.df <- data.frame(
  date=index(Tmn.mn.xts), 
  coredata(Tmn.mn.xts)
)

names(Tmn.mn.df)[1:2] <- c("date","Tmin")
head(Tmn.mn.df)

#subsetting all individual months
Januu <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="01")
Febrr <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="02")
Marcc <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="03")
Aprii <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="04")
Mayyy <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="05")
Juneee <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="06")
Julyyy <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="07")
Auguss <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="08")
Septee <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="09")
Octoo <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="10")
Novemm <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="11")
Decemm <- subset(Tmn.mn.df, format.Date(Tmn.mn.df$date, "%m")=="12")


#Averaging
J.n <- mean(Januu$Tmin, na.rm=T)
F.n <- mean(Febrr$Tmin, na.rm=T)
M.n <- mean(Marcc$Tmin, na.rm=T)
A.n <- mean(Aprii$Tmin, na.rm=T)
Ma.n <- mean(Mayyy$Tmin, na.rm=T)
Ju.n <- mean(Juneee$Tmin, na.rm=T)
Jl.n <- mean(Julyyy$Tmin, na.rm=T)
Au.n <- mean(Auguss$Tmin, na.rm=T)
S.n <- mean(Septee$Tmin, na.rm=T)
O.n <- mean(Octoo$Tmin, na.rm=T)
N.n <- mean(Novemm$Tmin, na.rm=T)
D.n <- mean(Decemm$Tmin, na.rm=T)

Mn.m.tn.v <- c(J.n, F.n,M.n,A.n,Ma.n,Ju.n,Jl.n,
               Au.n,S.n,O.n,N.n,D.n)

date.mn.m
Mn.mn.tmn <- data.frame(
  date.mn.m, 
  Mn.m.tn.v
)

write.csv(Mn.mn.tmn, 
          file = "Mean Monthly Min Tmp of Babile(1960-2014).csv")

ggplot(Mn.mn.tmn, aes(x=date.mn.m, y=Mn.m.tn.v)) + 
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Minimum Temperature (1960-2014)\n Babile",
       x="Month", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Minimum Monthly Temp of Babile (1960-2014).png")
dev.off()

sumn1 <- summary(Mn.mn.tmn)
write.csv(sumn1,
          file="summary of Mean Monthly Min Tmp of Bbaile (1960-2014).CSV")
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
          file = "summary stats for Monthly Min Tmp (1960-2014).csv")



#Indexing Annual Minimum Temperature
head(Tmn.df.ann)
Tmn.df.ann.1 <- drop_na(Tmn.df.ann)
lng.mn.tn <- mean(Tmn.df.ann.1$Tmin)
sd.tn <- sd(Tmn.df.ann.1$Tmin)
Tmn.df.ann.anom.v <- (Tmn.df.ann.1$Tmin - lng.mn.tn)/
  sd.tn

Tmn.df.ann.anom <- data.frame(
  Tmn.df.ann.1$date, 
  Tmn.df.ann.anom.v)

write.csv(Tmn.df.ann.anom, 
          file="Index of Annual Minimum Temperature of Babile(1960-2014).csv")

ggplot(Tmn.df.ann.anom, aes(Tmn.df.ann.1$date, Tmn.df.ann.anom.v, ymin = 0,
                            ymax = Tmn.df.ann.anom.v )) + 
  geom_linerange(data = Tmn.df.ann.anom, aes(colour = ifelse(Tmn.df.ann.anom.v > 0,
  "Positive", "Negative")),stat = "identity", position = "identity",size=2) +
  geom_hline(yintercept=0) + 
  theme(legend.title = element_text(size = 2, colour = F)) +
  scale_x_date(date_breaks="4 years",date_labels="%Y") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  labs(title="Minimum Temperature Anomaly (1960-2014)\n Babile", 
       x="Year", y="Anomaly")

dev.copy(png, filename="Index of AnnuAl Min Temp of Babile (1960-2014).png")
dev.off()

sumn2 <- summary(Tmn.df.ann.anom)
write.csv(sumn2,
          file="summary stats for Anomaly of miimum Tmp of Babiie (1960-2014).CSV")
#or
sumn2 <- data.frame(Tmn.df.ann.anom[Tmn.df.ann.anom[,2] == max(Tmn.df.ann.anom[,2]), ],
                    Tmn.df.ann.anom[Tmn.df.ann.anom[,2] == min(Tmn.df.ann.anom[,2]), ])

sumn2[2,c(1,2)] <- sumn2[1, c(3,4)] 
sumn2[-c(3,4)] -> sumn2
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn2) <- list(
  c("Max:","Min:"), 
  c("Year","Anomaly")
)  

write.csv(sumn2,
          file = "summary stats for Ann Min Tmp  Anomaly(1960-2014).csv")




#seasonal Cycle of Minimum Temperature
head(Tmn)
data <- Tmn
names(data) <- c("dates","Rain")
head(data)

mean_dailymn_Babile<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                        Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                        Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,
                        Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                        Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,
                        Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,
                        Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,
                        Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,
                        Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,
                        Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,
                        Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,
                        Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,
                        Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

seasonal.cycle.Tmn.Babile <- data.frame(
  c(1:366), 
  mean_dailymn_Babile) 

write.csv(seasonal.cycle.Tmn.Babile, 
          file="Sesonal cycle of min tmp of Babile(1960-2014).csv")

b <- ksmooth(
  c(1:366),
  mean_dailymn_Babile, 
  bandwidth = 15)

b.1 <- data.frame(b)
ggplot(seasonal.cycle.Tmn.Babile, aes(x=c(1:366), y=mean_dailymn_Babile)) + 
  geom_line(col="firebrick", lwd=1) +
  labs(title="Seasonal Cycle of Minimum Temperature (1960-2014)\n Babile", 
       x="Day", y=expression("Temperature("*~degree*c*")")) + 
  geom_line(data = b.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) 

dev.copy(png, filename="Seasonal Cycle of AnnuAl Min Temp of Babile (1960-2014).png",
         width = 1000, height =1000)
dev.off()

sumn3<- summary(seasonal.cycle.Tmn.Babile)
write.csv(sumn3,
          file="summary for Seasonal Cycle of Min Tmp of Bbaile (1960-2014).CSV")




# dividing the period 1960-2014 into two halves
Tmn.df.annfh <- Tmn.df.ann[(Tmn.df.ann$date >= "1960-01-01" &
                              Tmn.df.ann$date <= "1987-12-31"), ]

Tmn.df.annsh <- Tmn.df.ann[(Tmn.df.ann$date >= "1988-01-01" & 
                              Tmn.df.ann$date <= "2014-12-31"), ]


#first Half
head(Tmn.df.annfh)
MK <- MannKendall(Tmn.df.annfh[,2])
summary(MK)

write.csv(Tmn.df.annfh,file = "Mean Annual Min Tmp of Babile(1960-1987).csv")


ggplot(Tmn.df.annfh, aes(x=date,y=Tmin)) + geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm) + 
  labs(title="Minimum temperature (1960-1987)\n Babile",
       x="Year", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  annotate("text", x=Tmn.df.annfh[Tmn.df.annfh[,1] == "1963-12-31", ]$date,
           y=22, cex = 5, label = "P-value = 0.064411") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="AnnuAl Min Temp of Babile (1960-1987).png",
         width = 1000, height = 750)
dev.off()

sumn3 <- summary(Tmn.df.annfh)
write.csv(sumn4,
          file="summary for Min Tmp of Bbaile (1960-1987).CSV")
#or
Tmin_fh.1 <- drop_na(Tmn.df.annfh)
sumn3 <- data.frame(Tmin_fh.1[Tmin_fh.1[,2] == max(Tmin_fh.1[,2]), ],
                    Tmin_fh.1[Tmin_fh.1[,2] == min(Tmin_fh.1[,2]), ])

sumn3[2,c(1,2)] <- sumn3[1, c(3,4)] 
sumn3[-c(3,4)] -> sumn3
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn3) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn3,
          file = "summary stats for Ann Min Tmp  (1960-1987).csv")




#Indexing
#first half
#head(Tmn.df.annfh)
#Tmn.df.annfh.1 <- drop_na(Tmn.df.annfh)

#lng.mn.tnfh <- mean(Tmn.df.annfh.1$Tmin)
#sd.tnfh <- sd(Tmn.df.annfh.1$Tmin)
#Tmn.df.annfh.anom.v <- (Tmn.df.annfh.1$Tmin - lng.mn.tnfh)/
#  sd.tnfh

#Tmn.df.annfh.anom <- data.frame(
#  Tmn.df.annfh.1$date, 
#  Tmn.df.annfh.anom.v)

#write.csv(Tmn.df.annfh.anom, 
#          file="Index of Minimumm tmp of Babile(1960-1987).csv")

#ggplot(Tmn.df.annfh.anom, aes(Tmn.df.annfh.1$date, Tmn.df.annfh.anom.v, ymin = 0,
#                              ymax = Tmn.df.annfh.anom.v)) + 
#  geom_linerange(data = Tmn.df.annfh.anom, aes(colour = ifelse(Tmn.df.annfh.anom.v>0,
#  "Positive", "Negative")),stat = "identity", position = "identity",size=2) + 
#  geom_hline(yintercept=0) + 
#  theme(legend.title = element_text(size = 2, colour = F)) +
#  scale_x_date(date_breaks="4 years",date_labels="%Y") +  
#  labs(title="Index of Annual Minimum Temperature of Babile \n (1960-1987)",
#      x="Years", y="Index")      

#dev.copy(png, filename="Index of AnnuAl Min Temp of Babile (1960-1987).png",
#         width = 1000, height = 750)
#dev.off()

                                                                                                                                                                                                                                                          

#second half
head(Tmn.df.annsh)
MK <- MannKendall(Tmn.df.annsh[,2])
summary(MK)

write.csv(Tmn.df.annsh,
  "Mean Annual Tmin of Babile(1988-2014).csv"
)

ggplot(Tmn.df.annsh, aes(x=date,y=Tmin)) +
  geom_line(col="firebrick", lwd=2) + geom_smooth(method = lm) +
  labs(title="Minimum temperature (1988-2014)\n Babile", 
       x="Year", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks = "3 years", date_labels="%Y") + 
  annotate("text", x=Tmn.df.annsh[Tmn.df.annsh[,1] == "1993-12-31", ]$date,
           y=20.5, cex = 5, label = "P-value = 0.70984") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="AnnuAl Min Temp of Babile (1988-2014).png",
         width = 1000, height = 750)
dev.off()

sum5 <- summary(Tmn.df.annsh)
write.csv(sumn5,
          file = "summary stats for Ann Min Tmp.1(1988-2014).csv")
#or
Tmin_sh.1 <- drop_na(Tmn.df.annsh)
sumn55 <- data.frame(Tmin_sh.1[Tmin_sh.1[,2] == max(Tmin_sh.1[,2]), ],
                     Tmin_sh.1[Tmin_sh.1[,2] == min(Tmin_sh.1[,2]), ])

sumn55[2,c(1,2)] <- sumn55[1, c(3,4)] 
sumn55[-c(3,4)] -> sumn55
#sumn2[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn55) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn55,
          file = "summary stats for Ann Min Tmp (1989-2017).csv")



#Mean Monthly(
#first half)

head(Tmn.mn.df)
Tmn.mn.dffh <- Tmn.mn.df[(Tmn.mn.df[,1] >= "1960-01-31" & 
                            Tmn.mn.df[,1] <= "1987-12-31"), ]

Tmn.mn.dfsh <- Tmn.mn.df[(Tmn.mn.df[,1] >= "1988-01-31" & 
                            Tmn.mn.df[,1] <= "2014-12-31"), ]

names(Tmn.mn.df)[1:2] <- c("date", "Tmin")


Januu1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="01")
Febrr1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="02")
Marcc1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="03")
Aprii1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="04")
Mayyy1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="05")
Juneee1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="06")
Julyyy1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="07")
Auguss1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="08")
Septee1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="09")
Octoo1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="10")
Novemm1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="11")
Decemm1 <- subset(Tmn.mn.dffh, format.Date(Tmn.mn.dffh$date, "%m")=="12")


#Averaging
J.n1 <- mean(Januu1$Tmin, na.rm=T)
F.n1 <- mean(Febrr1$Tmin, na.rm=T)
M.n1 <- mean(Marcc1$Tmin, na.rm=T)
A.n1 <- mean(Aprii1$Tmin, na.rm=T)
Ma.n1 <- mean(Mayyy1$Tmin, na.rm=T)
Ju.n1 <- mean(Juneee1$Tmin, na.rm=T)
Jl.n1 <- mean(Julyyy1$Tmin, na.rm=T)
Au.n1 <- mean(Auguss1$Tmin, na.rm=T)
S.n1 <- mean(Septee1$Tmin, na.rm=T)
O.n1 <- mean(Octoo1$Tmin, na.rm=T)
N.n1 <- mean(Novemm1$Tmin, na.rm=T)
D.n1 <- mean(Decemm1$Tmin, na.rm=T)

Mn.m.tn.vfh <- c(J.n1, F.n1,M.n1,A.n1,Ma.n1,Ju.n1,Jl.n1,
               Au.n1,S.n1,O.n1,N.n1,D.n1)

date.mn.m
Mn.mn.tmnfh <- data.frame(
  date.mn.m, 
  Mn.m.tn.vfh
)

write.csv(Mn.mn.tmnfh, file = "Mean Monthly Min Tmp of Babile(1960-1987).csv")

ggplot(Mn.mn.tmnfh, aes(x=date.mn.m, y=Mn.m.tn.vfh)) + 
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Minimum Temperature (1960-1987)\n Babile",
       x="Month", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Minimum Monthly Temp of Babile (1960-1987).png",
         width =1000, height = 750)
dev.off()

sumn4 <- summary(Mn.mn.tmnfh)
write.csv(sumn5,
          file="summary of Mean Monthly Min Tmp of Bbaile (1960-1987).CSV")
#or
sumn4 <- data.frame(Mn.mn.tmnfh[Mn.mn.tmnfh[,2] == max(Mn.mn.tmnfh[,2]), ],
                    Mn.mn.tmnfh[Mn.mn.tmnfh[,2] == min(Mn.mn.tmnfh[,2]), ])

sumn4[2,c(1,2)] <- sumn4[1, c(3,4)] 
sumn4[-c(3,4)] -> sumn4
sumn4[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn4) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn4,
          file = "summary stats for mean month Min Tmp (1960-1987).csv")


  

#Second Half

head(Tmn.mn.dfsh)

Januu2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="01")
Febrr2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="02")
Marcc2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="03")
Aprii2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="04")
Mayyy2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="05")
Juneee2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="06")
Julyyy2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="07")
Auguss2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="08")
Septee2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="09")
Octoo2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="10")
Novemm2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="11")
Decemm2 <- subset(Tmn.mn.dfsh, format.Date(Tmn.mn.dfsh$date, "%m")=="12")


#Averaging
J.n2 <- mean(Januu2$Tmin, na.rm=T)
F.n2 <- mean(Febrr2$Tmin, na.rm=T)
M.n2 <- mean(Marcc2$Tmin, na.rm=T)
A.n2 <- mean(Aprii2$Tmin, na.rm=T)
Ma.n2 <- mean(Mayyy2$Tmin, na.rm=T)
Ju.n2 <- mean(Juneee2$Tmin, na.rm=T)
Jl.n2 <- mean(Julyyy2$Tmin, na.rm=T)
Au.n2 <- mean(Auguss2$Tmin, na.rm=T)
S.n2 <- mean(Septee2$Tmin, na.rm=T)
O.n2 <- mean(Octoo2$Tmin, na.rm=T)
N.n2 <- mean(Novemm2$Tmin, na.rm=T)
D.n2 <- mean(Decemm2$Tmin, na.rm=T)

Mn.m.tn.vsh <- c(J.n2, F.n2,M.n2,A.n2,Ma.n2,Ju.n2,Jl.n2,
               Au.n2,S.n2,O.n2,N.n2,D.n2)

date.mn.m
Mn.mn.tmnsh <- data.frame(
  date.mn.m, 
  Mn.m.tn.vsh
)

write.csv(Mn.mn.tmnsh, 
          file = "Mean Monthly Min Tmp of Babile(1988-2014).csv")

ggplot(Mn.mn.tmnsh, aes(x=date.mn.m, y=Mn.m.tn.vsh)) + 
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Minimum Temperature (1988-2014)\n Babile",
       x="Month", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Minimum Monthly Temp of Babile (1988-2014).png",
         width =1000, height = 750)
dev.off()

sumn6 <- summary(Mn.mn.tmnsh)
write.csv(sumn6,
          file="summary of Mean Monthly Min Tmp of Bbaile (1987-2014).CSV")
#or
sumn66 <- data.frame(Mn.mn.tmnsh[Mn.mn.tmnsh[,2] == max(Mn.mn.tmnsh[,2]), ],
                     Mn.mn.tmnsh[Mn.mn.tmnsh[,2] == min(Mn.mn.tmnsh[,2]), ])

sumn66[2,c(1,2)] <- sumn66[1, c(3,4)] 
sumn66[-c(3,4)] -> sumn66
sumn66[,1] <- data.frame(month.name[c(04,12)])
dimnames(sumn66) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn66,
          file = "summary stats for Ann Min Tmp (1988-2014).csv")




#Plotting both
ggplot(Mn.mn.tmnfh, aes(x=date.mn.m)) + 
  geom_line(aes(y=Mn.mn.tmnfh[,2],col="1960-1987"), lwd=2) + 
  geom_line(aes(y=Mn.mn.tmnsh[,2],col="1988-2014"), lwd=2) +
  labs(title="Mean Monthly Min Temperature (1960-2014)\n Babile",
       x="Month", y=yyy) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_colour_manual("",
                      values = c("1960-1987"="darkblue", "1988-2014"="red")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Max Tmp of Babile (1960-87-2016).png",
         width = 1000, height = 750)
dev.off()




#Max Tmp Change between two halves
#Just build a fuction with it ( its simpler and tidy)
Tmpn_change <- function(x,y){
  ((mean(Tmn.sh[,2], na.rm = T) - mean(Tmn.fh[,2], na.rm = T))*100)/
    mean(Tmn.fh[,2], na.rm = T)
}

Tmpn_change(mean(Tmn.sh[,2], na.rm = T), mean(Tmn.fh[,2], na.rm = T))

#or
Tmpn_change <- mean(Tmn_sh[,2], na.rm = T) - mean(Tmn_fh[,2], na.rm = T)
#change <- expression("0.3363875(" ~ degree * c * ")")


#Monthly difference in Max Tmp b/n two halves
head(Mn.mn.tmnfh)
head(Mn.mn.tmnsh)

#First Half
storage5 <- numeric(12)
m <- c(Mn.mn.tmnfh[,2])

for (i in 1:12) {
  storage5[i] <- m[i]
}

#Second Half
storage6 <- numeric(12)
n <- c(Mn.mn.tmnsh[,2])

for (i in 1:12) {
  storage6[i] <- n[i]
}


change_in_Min_Temp <- data.frame(
  Month=month.name,
  Change=storage6-storage5)


#Percentage Change in Monthly Tmax 
storage7 <- numeric(12)
r <- c(change_in_Min_Temp[,2])

for (i in 1:12) {
  storage7[i] <- ((r*100)/Mn.mn.tmnfh[,2])[i]
}

Percen_change_tmax <- storage7




#Indexing
#head(Tmn.df.annsh)
#Tmn.df.annsh.1 <- drop_na(Tmn.df.annsh)
#lng.mn.tnsh <- mean(Tmn.df.annsh.1$Tmin)
#sd.tnsh <- sd(Tmn.df.annsh.1$Tmin)
#Tmn.df.annsh.anom.v <- (Tmn.df.annsh.1$Tmin - lng.mn.tnsh)/
#  sd.tnsh

#Tmn.df.annsh.anom <- data.frame(
#  Tmn.df.annsh.1$date, 
#  Tmn.df.annsh.anom.v)

#write.csv(Tmn.df.annsh.anom, 
#         file="Index of Minimumm tmp of Babile(1988-2014).csv")

#ggplot(Tmn.df.annsh.anom, aes(Tmn.df.annsh.1$date, Tmn.df.annsh.anom.v, 
#                              ymin = 0,ymax = Tmn.df.annsh.anom.v)) + 
#  geom_linerange(data = Tmn.df.annsh.anom, aes(colour = ifelse(Tmn.df.annsh.anom.v > 0,
#  "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#  geom_hline(yintercept=0) + 
# theme(legend.title = element_text(size = 2, colour = F)) + 
##  scale_x_date(date_labels = "%Y",date_breaks = "4 years") +  
#  labs(title="Index of Annual Minimum Temperature of Babile \n (1988-2014)", 
#      x="Years", y="Index")                       

#dev.copy(png, filename="Index of AnnuAl Min Temp of Babile (1988-2014).png",
#         width = 1000, height = 750)
#dev.off()



#Seasonal Cycle of 2 halves Tmn
head(Tmn)
Tmn.fh <- Tmn[(Tmn$date >= "1960-01-01" & Tmn$date <= "1987-12-31"), ]
Tmn.sh <- Tmn[(Tmn$date >= "1988-01-01" & Tmn$date <= "2014-12-31"), ]

#first half
data <- Tmn.fh
names(data) <- c("dates","Rain")
head(data)

mean_dailymn_Babilefh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,
                          Jan_10,Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,
                          Jan_18,Jan_19,Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,
                          Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,
                          Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,
                          Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,
                          Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,
                          Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,
                          Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,
                          Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,
                          Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,
                          Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,
                          Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,
                          Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

seasonal.cycle.Tmn.Babilefh <- data.frame(
  c(1:366), 
  mean_dailymn_Babilefh) 

write.csv(seasonal.cycle.Tmn.Babilefh, 
          file="Sesonal cycle of min tmp of Babile(1960-1987).csv")

o <- ksmooth(c(1:366), 
             mean_dailymn_Babilefh,
             bandwidth = 15)

o.1 <- data.frame(o)

ggplot(seasonal.cycle.Tmn.Babilefh, aes(x=c(1:366), y=mean_dailymn_Babilefh)) +
  geom_line(col="firebrick", lwd=1) + 
  labs(title="seasonal Cycle of Minimum Temperature (1960-1987)\n Babile", 
       x="Day", y=expression("Temperature("*~degree*c*")")) + 
  geom_line(data = o.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename = "Seaonal Min Tmp Cycle of Babile (1960-1987).png",
         width = 1000, height = 750)
dev.off()


sumn7 <- summary(seasonal.cycle.Tmn.Babilefh)
write.csv(sumn7,
          file="summary for seasonal cycle of Min Tmp of Bbaile (1960-1987).CSV")



#second half
data <- Tmn.sh
names(data) <- c("dates","Rain")
head(data)

mean_dailymn_Babilesh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,
                          Jan_10,Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,
                          Jan_18,Jan_19,Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,
                          Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,
                          Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,
                          Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,
                          Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,
                          Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,
                          Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,
                          Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,
                          Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,
                          Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,
                          Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,
                          Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

seasonal.cycle.Tmn.Babilesh <- data.frame(
  c(1:366), 
  mean_dailymn_Babilesh) 

write.csv(seasonal.cycle.Tmn.Babilesh,
          file="Sesonal cycle of min tmp of Babile(1988-2014).csv")

a <- ksmooth(
  c(1:366), 
  mean_dailymn_Babilesh, 
  bandwidth = 15)

a.1 <- data.frame(a)
ggplot(seasonal.cycle.Tmn.Babilesh, aes(x=c(1:366), y=mean_dailymn_Babilesh)) +
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1988-2014)\n Babile", 
       x="Day", y=expression("Temperature("*~degree*c*")")) + 
  geom_line(data = a.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename = "Seaonal Min Tmp Cycle of Babile (1988-2014).png",
         width = 1000, height = 750)
dev.off()

sumn8 <- summary(seasonal.cycle.Tmn.Babilesh)
write.csv(sumn8,
          file="summary for seasonal cycle of Min Tmp of Bbaile (1988-2014).CSV")



ggplot(data=seasonal.cycle.Tmn.Babilefh, aes(x=c(1:366))) + 
  geom_line(aes(y=seasonal.cycle.Tmn.Babilefh[,2]),lwd=0.25) +
  geom_line(aes(y=o.1[,2],colour="1960-1987"),lwd=1.25) +
  geom_line(aes(y=seasonal.cycle.Tmn.Babilesh[,2]),lwd=0.25) +
  geom_line(aes(y=a.1[,2],colour="1988-2014"),lwd=1.25) +
  labs(title="Seasonal Cycle of Minimum Temperature (1960-2014)\n Babile", 
       x="Day", y="Rainfall(mm)") +
  scale_colour_manual("",
                      values = c("1960-1987"="darkblue","1988-2014"="red")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Min Tmp Cycle of Babile (1960-88-2014).png",
         width = 1000,  height = 750)
dev.off()




#Seasonal Max Tmp on Monthly Basis
head(Tmn)
Ap3 <- mean(subset(Tmn,format.Date(date, "%m")=="04")$Tmin, na.rm=T)
Ma3 <- mean(subset(Tmn,format.Date(date, "%m")=="05")$Tmin, na.rm=T)
Ju3 <- mean(subset(Tmn,format.Date(date, "%m")=="06")$Tmin, na.rm=T)
Jl3 <- mean(subset(Tmn,format.Date(date, "%m")=="07")$Tmin, na.rm=T)
Au3 <- mean(subset(Tmn,format.Date(date, "%m")=="08")$Tmin, na.rm=T)
Se3 <- mean(subset(Tmn,format.Date(date, "%m")=="09")$Tmin, na.rm=T)
Oc3 <- mean(subset(Tmn,format.Date(date, "%m")=="10")$Tmin, na.rm=T)

Seas3 <- data.frame(
  dates=Mn.mn.tmx[4:10, 1],
  Tmin=c(Ap3,Ma3,Ju3,Jl3,Au3,Se3,Oc3)
)

require(ggplot2)
ggplot(data=Seas3,
       aes(x=dates,y=Tmin)) +
  geom_line(col="firebrick", lwd=2) +
  ggtitle(" Seasonal Minimum Temperature (1960-2014)\n Babile") + xlab("Month") +
  ylab(yyy) + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Tmin.png",
         width = 1000, height = 650
)
dev.off()





#plot(seasonal.cycle.Tmn.Babilefh, type="l", lwd=1,
#     col="firebrick", 
#     main="Seasonal Min.Temp Cycle of Babile (1960-1987, 1988 - 2014)",
#     xlab="Days",ylab="Rainfall(mm)", ylim=c(14,28))

#lines(o.1, lwd=3, col="darkblue")
#lines(seasonal.cycle.Tmn.Babilesh, type="l", 
#      lwd=1, col="firebrick")

#lines(a.1, col="red", lwd=3)#

#legend("topright", col=c("darkblue","red"), 
#       bty = n, lty = 1, 
#       legend = c("1960-1987", '1988-2014'), lwd=3)
