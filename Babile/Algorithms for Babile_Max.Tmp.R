##########################################################################################

head(data1)
Tmx <- data.frame(
        data1$date, 
        data1$Tmax
        )

names(Tmx) <- c("date","Tmax")
head(Tmx)

#deleting a year period in a dataframe #df[!(df$col1 == 0 & df$col3 == 5), ]#
#Tmx <- Tmx[!(Tmx$date >= "2007-01-01" & Tmx$date <= "2007-12-31"), ]
#head(Tmx)

Tmx.xts <- xts(Tmx$Tmax, Tmx$date)
Tmx.xts.ann <- apply.yearly(
        Tmx.xts, 
        mean,
        na.rm=T
        )

Tmx.ann.df <- data.frame(
        date=index(Tmx.xts.ann), 
        coredata(Tmx.xts.ann)
)

names(Tmx.ann.df) [2]<- "Tmax"

head(Tmx.ann.df)
#Tmx.ann.df <- Tmx.ann.df %>% na.omit(Tmx.ann.df)


Tmx.ann.df$date <- as.Date(
        Tmx.ann.df$date, 
        format="%Y-%m-%d"
)

MK <- MannKendall(Tmx.ann.df[,2])
summary(MK)


ggplot(Tmx.ann.df, aes(x=date, y=Tmax)) +
        geom_line(col="firebrick", lwd=2) + 
        geom_smooth(method=lm, col="blue") + 
        labs(title="Maximum Temperature  (1960-2014)\n Babile", 
             x="Year", y=expression("Temperature("*~degree*c*")")) + 
        scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) +
        annotate("text", x=Tmx.ann.df[Tmx.ann.df[,1] == "1978-12-31", ]$date,
                 y=32.5, cex = 5, label ="P-value = 0.014788")

dev.copy(png, filename="Annual Max Tmp of Babile (1960-2014).png",
         width = 1000,  height = 750)
dev.off()

sumx <- summary(Tmx.ann.df)
write.csv(sumx,
          file="Summary Stats of Annual Mean Mx Tmp of BAbile (1960-2014).csv")
#or
Tmax.ann.df.1 <- drop_na(Tmax.ann.df)
sumxx <- data.frame(Tmx.ann.df.1[Tmx.ann.df.1[,2] == max(Tmx.ann.df.1[,2]), ],
                    Tmx.ann.df.1[Tmx.ann.df.1[,2] == min(Tmx.ann.df.1[,2]), ])

sumxx[2,c(1,2)] <- sumxx[1, c(3,4)] 
sumxx[-c(3,4)] -> sumxx
#sumxx[,1] <- data.frame(month.name[c(09,01)])
dimnames(sumxx) <- list(
        c("Max:","Min:"), 
        c("Year","Tmax")
)  

write.csv(sumxx,
          file = "summary stats for Max Tmp (1960-2014).csv")



#ggplot(Tmx.ann.df, aes(x=date, y=Tmax)) + geom_line(col="firebrick", lwd=2) +
#        geom_smooth(method=lm, col="blue") +
#        labs(title="Annual Maximum temperature of Babile(1960-2014)",
#             x="Years", y=expression("Temperature("*~degree*c*")")) + 
#        scale_x_date(breaks = Tmx.ann.df$date, labels = date_format("%Y"))

write.csv(Tmx.ann.df,
          file="Data for Mean Annual Max Tmp of Babile(1960-2014).csv")


# code for labelling X-AXIS based on the on data
#     scale_x_date(breaks=df$date, labels = date_format("%m/%d")) 




#Mean Monthly Temperature
head(Tmx.xts)
Tmx.mn.xts <- apply.monthly(
        Tmx.xts, 
        mean, 
        na.rm=T
        )

#coversion to d.f
Tmx.mn.df <- data.frame(
        date=index(Tmx.mn.xts),
        coredata(Tmx.mn.xts)
)

names(Tmx.mn.df)[1:2] <- c("date","Tmax")

#subsetting all individual months
Janu <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="01")
Febr <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="02")
Marc <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="03")
Apri <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="04")
Mayy <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="05")
Junee <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="06")
Julyy <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="07")
Augus <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="08")
Septe <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="09")
Octo <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="10")
Novem <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="11")
Decem <- subset(Tmax.mn.df, format.Date(Tmax.mn.df$date, "%m")=="12")


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

date.mn.m
Mn.mn.tmx <- data.frame(
        date.mn.m,
        Mn.m.tx.v)

write.csv(Mn.mn.tmx, 
          file = "Mean Monthly Max Tmp of Babile(1960-2014).csv")

ggplot(Mn.mn.tmx, aes(x=date.mn.m, y=Mn.m.tx.v)) + 
        geom_line(col="firebrick", lwd=2) +
        labs(title="Mean Monthly Maximum Temperature (1960-2014)\n Babile", 
             x="Month", y=expression("Temperature("*~degree*c*")")) + 
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) +
        scale_x_date(date_breaks= "1 month", date_labels = "%b") 

dev.copy(png, 
         filename="Mean Monthly Maximum Temp of Babile (1960-2014).png")

dev.off()

sumx1 <- summary(Mn.mn.tmx)
write.csv(sumx1,
          file="summary stats for mean monthly T,max of Bbabile(1960-14).csv")
#or
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
          file = "summary stats for Max Tmp (1960-2014).csv")




#seasonal Cycle of Maximum Tmp
head(Tmx)
#let, 
data <- Tmx
names(data)[2] <- "Rain"
names(data)[1] <- "dates"
head(data)

mean_dailymx_Babile<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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
                        Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

seasonal.cycle.Tmx.Babile <- data.frame(
        c(1:366), 
        mean_dailymx_Babile) 

write.csv(seasonal.cycle.Tmx.Babile, 
          file="Sesonal cycle of maxi tmp of Babile(1960-2014).csv")

p <- ksmooth(
        c(1:366), 
        mean_dailymx_Babile, 
        bandwidth = 15)

p.1 <- data.frame(p)

ggplot(seasonal.cycle.Tmx.Babile, aes(x=c(1:366), y=mean_dailymx_Babile)) + 
        geom_line(col="firebrick", lwd=1) + 
        labs(title="Seasonal Cycle of Maximum Temperature (1960-2014)\n Babile", 
             x="Days", y=expression("Temperature("*~degree*c*")")) + 
        geom_line(data = p.1, aes(x=x, y=y), col="blue", lwd=1) +
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Maximum Temp of Babile (1960-2014).png",
         width =1000, height = 750)
dev.off()

sumx2 <- summary(seasonal.cycle.Tmx.Babile)
write.csv(sumx2, 
          file="summary stats for seasonal cycle of mean max atmp ofBabile (1960-14).csv")



#trying to run something like a loop
#qq <- 34.08384 - sd.tx(Tmx.ann.df[(Tmx.ann.df$date >= "1060-12-31" & 
#                                           Tmx.ann.df$date >= "2014-12-31")], )


# Index Maximum Temperature
head(Tmx.ann.df)
Tmx.ann.df.1 <- drop_na(Tmx.ann.df)
lng.mn.tx <- mean(Tmx.ann.df.1$Tmax)
sd.tx <- sd(Tmx.ann.df.1$Tmax)

anom.v.mx <- (Tmx.ann.df.1$Tmax - lng.mn.tx)/
        sd.tx

Tmx.ann.df.anom <- data.frame(
        Tmx.ann.df.1$date, 
        anom.v.mx)

write.csv(Tmx.ann.df.anom, 
          file="Index of Annual Maximum Temperature(1960-2014).csv")

ggplot(Tmx.ann.df.anom, aes(Tmx.ann.df.1$date, anom.v.mx, ymin = 0,ymax = anom.v.mx)) +
        geom_linerange(data = Tmx.ann.df.anom, aes(colour = ifelse(anom.v.mx > 0, 
        "Positive", "Negative")),stat = "identity", position = "identity",size=2) + 
        geom_hline(yintercept=0) + theme(legend.title = element_text(size = 2, 
                                                                     colour = F)) + 
        (scale_x_date(date_breaks="4 years",date_labels="%Y")) + 
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) +
        labs(title="Maximum Temperature Anomaly (1960-2014)\n Babile",
             x="Year", y="Anomaly")

dev.copy(png, filename="Index of Maximum Temp of Babile (1960-2014).png", 
         width = 1000, height = 750)
dev.off()

sumx2 <- summary(Tmx.ann.df.anom)
write.csv(sumx2,
            file="Summary stats for Anomaly of Max Tmp of Babile (1960-14).csv")
#or
sumx22 <- data.frame(Tmx.ann.df.anom[Tmx.ann.df.anom[,2] == max(Tmx.ann.df.anom[,2]), ],
                     Tmx.ann.df.anom[Tmx.ann.df.anom[,2] == min(Tmx.ann.df.anom[,2]), ])

sumx22[2,c(1,2)] <- sumx22[1, c(3,4)] 
sumx22[-c(3,4)] -> sumx22
#sumx22[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumx22) <- list(
        c("Max:","Min:"), 
        c("Year","Anomaly")
)  

write.csv(sumx22,
          file = "summary stats for Ann. Max Tmp Anomaly(1960-2014).csv")




#.......................................................................................#




#Max Temp for the two halves of the  year period (1960-2014)
#Dividing the data into 2 parts
Tmx.fh <- Tmx[(Tmx$date >= "1960-01-01" & Tmx$date <= "1987-12-31"), ]
Tmx.sh <- Tmx[(Tmx$date >= "1988-01-01" & Tmx$date <= "2014-12-31"), ]

# Annual Maximum Temperature for the first half (1960-2014)
head(Tmx.fh)
Tmx.xts.fh <- xts(Tmx.fh$Tmax, Tmx.fh$date)
Tmx.ann.fh <- apply.yearly(
        Tmx.xts.fh,
        mean, na.rm=T
) 

head(Tmx.ann.fh)

#converting to datafranme
Tmxfh.ann.df <- data.frame(
        date=index(Tmx.ann.fh), 
        coredata(Tmx.ann.fh)
)

names(Tmxfh.ann.df) <- c("date","Tmax")

Tmxfh.ann.df$date <- as.Date(
        Tmxfh.ann.df$date, 
        format="%Y-%m-%d")

write.csv(Tmxfh.ann.df,file="Mean Annual Tmax of Babile (1960-187).csv ")

MK <- MannKendall(Tmxfh.ann.df[,2])
summary(MK)

#Removing all dates with NA's
#Tmxfh.ann.df <- Tmxfh.ann.df %>% drop_na()
ggplot(Tmxfh.ann.df, aes(x=date, y=Tmax)) + 
        geom_line(col="firebrick", lwd=2) + 
        labs(title="Maximum Temperature (1960-1987)\n Babile",
             x='Year', y=expression("Temperature("*~degree*c*")")) + 
        scale_x_date(date_breaks= "4 years", date_labels = "%Y") + 
        geom_smooth(method = lm) + 
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) +
        annotate("text", x=Tmxfh.ann.df[Tmxfh.ann.df[,1] == "1964-12-31", ]$date,
                 y=36, cex = 5, label = "P-value = 0.22997")

dev.copy(png, filename="Annual Maximum Temp of Babile (1960-1987).png",
         width = 1000, height =  750)
dev.off()

sumx3 <- summary(Tmxfh.ann.df)
write.csv(sumx3,
          file="Summary stats for Maximum Tmpof Babile(1960-1987).csv")

#or
Tmax_fh.1 <- drop_na(Tmxfh.ann.df)
sumx33 <- data.frame(Tmax_fh.1[Tmax_fh.1[,2] == max(Tmax_fh.1[,2]), ],
                     Tmax_fh.1[Tmax_fh.1[,2] == min(Tmax_fh.1[,2]), ])

sumx33[2,c(1,2)] <- sumx33[1, c(3,4)] 
sumx33[-c(3,4)] -> sumx33
#sumx33[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumx33) <- list(
        c("Max:","Min:"), 
        c("Year","Tmax")
)  

write.csv(sumx33,
          file = "summary stats for ann Max Tmp (1960-1987).csv")



#Annual Maximum Temperature for the second half (1960-2014)
head(Tmx.sh)
Tmx.xts.sh <- xts(Tmx.sh$Tmax, Tmx.sh$date)

Tmx.ann.sh <- apply.yearly(
        Tmx.xts.sh, 
        mean, na.rm=T)

head(Tmx.ann.sh)

#converting to dataframe
Tmxsh.ann.df <- data.frame(
        date=index(Tmx.ann.sh), 
        coredata(Tmx.ann.sh))

names(Tmxsh.ann.df)[2] <- "Tmax"
head(Tmxsh.ann.df)

#date conversion
Tmxsh.ann.df$date <- as.Date(
        Tmxsh.ann.df$date, 
        format="%Y-%m-%d")

write.csv(Tmxsh.ann.df,file = "Mean Annual Tmax of BABILE(1988-2017).csv")

#Removed NAs from the second half since there were no estimations made for
#mising value at the 55 yr period level

#New Tmxsh.ann.df
#Tmxsh.ann.df <- Tmxsh.ann.df %>% drop_na()
MK <- MannKendall(Tmxsh.ann.df$Tmax)
summary(MK)

ggplot(Tmxsh.ann.df, aes(x=date, y=Tmax)) + 
        geom_line(col="firebrick", lwd=2) + 
        geom_smooth(method=lm, col="blue") + 
        scale_x_date(date_breaks="4 years", date_labels= "%Y") + 
        labs(title="Maximum Temperature (1988-2014)\n Babile", 
             x="Year", y=expression("Temperature("*~degree*c*")")) + 
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) +
        annotate("text", x=Tmxsh.ann.df[Tmxsh.ann.df[,1] == "1995-12-31", ]$date,
                 y=31.5, cex=5, label="P-value =0.20586")

dev.copy(png, filename="Annual Maximum Temp of Babile (1988-2014).png",
         width = 1000, height = 750)
dev.off()

sumx5 <- summary(Tmxsh.ann.df)
write.csv(sumx5,
            file="Summary stats for Maximum Tmpof Babile(1988-2014).csv")
#or
Tmax_sh.1 <- drop_na(Tmxsh.ann.df)
sumx55 <- data.frame(Tmax_sh.1[Tmax_sh.1[,2] == max(Tmax_sh.1[,2]), ],
                     Tmax_sh.1[Tmax_sh.1[,2] == min(Tmax_sh.1[,2]), ])

sumx55[2,c(1,2)] <- sumx55[1, c(3,4)] 
sumx55[-c(3,4)] -> sumx55
#sumx55[,1] <- data.frame(month.name[c(02,08)])
dimnames(sumx55) <- list(
        c("Max:","Min:"), 
        c("Year","Tmax")
)  

write.csv(sumx55,
          file = "summary stats for Ann Max Tmp (1988-2014).csv")




#Mean Minthly(
#Second half)

head(Tmx.mn.df)
Tmx.mn.dffh <- Tmx.mn.df[(Tmx.mn.df[,1] >= "1960-01-31" & 
                                  Tmx.mn.df[,1] <= "1987-12-31"), ]

Tmx.mn.dfsh <- Tmx.mn.df[(Tmx.mn.df[,1] >= "1988-01-31" & 
                                  Tmx.mn.df[,1] <= "2014-12-31"), ]

Januu11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="01")
Febrr11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="02")
Marcc11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="03")
Aprii11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="04")
Mayyy11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="05")
Juneee11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="06")
Julyyy11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="07")
Auguss11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="08")
Septee11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="09")
Octoo11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="10")
Novemm11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="11")
Decemm11 <- subset(Tmx.mn.dfsh, format.Date(Tmx.mn.dfsh$date, "%m")=="12")


#Averaging
J.n11 <- mean(Januu11$Tmax, na.rm=T)
F.n11 <- mean(Febrr11$Tmax, na.rm=T)
M.n11 <- mean(Marcc11$Tmax, na.rm=T)
A.n11 <- mean(Aprii11$Tmax, na.rm=T)
Ma.n11 <- mean(Mayyy11$Tmax, na.rm=T)
Ju.n11 <- mean(Juneee11$Tmax, na.rm=T)
Jl.n11 <- mean(Julyyy11$Tmax, na.rm=T)
Au.n11 <- mean(Auguss11$Tmax, na.rm=T)
S.n11 <- mean(Septee11$Tmax, na.rm=T)
O.n11 <- mean(Octoo11$Tmax, na.rm=T)
N.n11 <- mean(Novemm11$Tmax, na.rm=T)
D.n11 <- mean(Decemm11$Tmax, na.rm=T)

Mn.m.tx.vsh <- c(J.n11, F.n11,M.n11,A.n11,Ma.n11,Ju.n11,Jl.n11,
                 Au.n11,S.n11,O.n11,N.n11,D.n11)

date.mn.m
Mn.mn.tmxsh <- data.frame(
        date.mn.m, 
        Mn.m.tx.vsh
)

write.csv(Mn.mn.tmxsh, 
          file = "Mean Monthly Max Tmp of Babile(1988-2014).csv")

ggplot(Mn.mn.tmxsh, aes(x=date.mn.m, y=Mn.m.tx.vsh)) + 
        geom_line(col="firebrick", lwd=2) +
        labs(title="Mean Monthly Maximum Temperature (1988-2014)\n Babile",
             x="Month", y=expression("Temperature("*~degree*c*")")) + 
        scale_x_date(date_breaks= "1 month", date_labels = "%b") +
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Maximum Monthly Temp of Babile (1988-2014).png",
         width =1000, height = 750)
dev.off()

sumx6 <- summary(Mn.mn.tmxsh)
write.csv(sumx6,
          file="Summary Stats for Mean monthly Tmax of Babible(1988-2014).csv")
#or
sumx66 <- data.frame(Mn.mn.tmxsh[Mn.mn.tmxsh[,2] == max(Mn.mn.tmxsh[,2]), ],
                     Mn.mn.tmxsh[Mn.mn.tmxsh[,2] == min(Mn.mn.tmxsh[,2]), ])

sumx66[2,c(1,2)] <- sumx66[1, c(3,4)] 
sumx66[-c(3,4)] -> sumx66
sumx66[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumx66) <- list(
        c("Max:","Min:"), 
        c("Month","Tmax")
)  

write.csv(sumx66,
          file = "summary stats for monthly Max Tmp (1988-2014).csv")



#First Half (Monthly Tmax)
head(Tmx.mn.dffh)

Januu22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="01")
Febrr22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="02")
Marcc22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="03")
Aprii22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="04")
Mayyy22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="05")
Juneee22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="06")
Julyyy22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="07")
Auguss22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="08")
Septee22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="09")
Octoo22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="10")
Novemm22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="11")
Decemm22 <- subset(Tmx.mn.dffh, format.Date(Tmx.mn.dffh$date, "%m")=="12")


#Averaging
J.n22 <- mean(Januu22$Tmax, na.rm=T)
F.n22 <- mean(Febrr22$Tmax, na.rm=T)
M.n22 <- mean(Marcc22$Tmax, na.rm=T)
A.n22 <- mean(Aprii22$Tmax, na.rm=T)
Ma.n22 <- mean(Mayyy22$Tmax, na.rm=T)
Ju.n22 <- mean(Juneee22$Tmax, na.rm=T)
Jl.n22 <- mean(Julyyy22$Tmax, na.rm=T)
Au.n22 <- mean(Auguss22$Tmax, na.rm=T)
S.n22 <- mean(Septee22$Tmax, na.rm=T)
O.n22 <- mean(Octoo22$Tmax, na.rm=T)
N.n22 <- mean(Novemm22$Tmax, na.rm=T)
D.n22 <- mean(Decemm22$Tmax, na.rm=T)

Mn.m.tx.vfh <- c(J.n22, F.n22,M.n22,A.n22,Ma.n22,Ju.n22,Jl.n22,
                 Au.n22,S.n22,O.n22,N.n22,D.n22)

date.mn.m
Mn.mn.tmxfh <- data.frame(
        date.mn.m, 
        Mn.m.tx.vfh
)

write.csv(Mn.mn.tmxfh, 
          file = "Mean Monthly Max Tmp of Babile(1960-1987).csv")

ggplot(Mn.mn.tmxfh, aes(x=date.mn.m, y=Mn.m.tx.vfh)) + 
        geom_line(col="firebrick", lwd=2) +
        labs(title="Mean Monthly Maximum Temperature (1960-1987)\n Babile",
             x="Month", y=expression("Temperature("*~degree*c*")")) + 
        scale_x_date(date_breaks= "1 month", date_labels = "%b") +
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) +

dev.copy(png, filename="Mean Maximum Monthly Temp of Babile (1960-1987).png",
         width =1000, height = 750)
dev.off()

sumx7 <- summary(Mn.mn.tmxfh)
write.csv(sumx7,
          file="Summary Stats for mean monthly Tmax of Babile (1988-2014).csv")
#or
sumx77 <- data.frame(Mn.mn.tmxfh[Mn.mn.tmxfh[,2] == max(Mn.mn.tmxfh[,2]), ],
                     Mn.mn.tmxfh[Mn.mn.tmxfh[,2] == min(Mn.mn.tmxfh[,2]), ])

sumx77[2,c(1,2)] <- sumx77[1, c(3,4)] 
sumx77[-c(3,4)] -> sumx77
sumx77[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumx77) <- list(
        c("Max:","Min:"), 
        c("Month","Tmax")
)  

write.csv(sumx77,
          file = "summary stats for monthly Max Tmp (1988-2014).csv")



#Plotting both
ggplot(data=Mn.mn.tmxfh, aes(x=date.mn.m), lwd=1.5) + 
        geom_line(aes(y=Mn.m.tx.vfh, colour="1960-1987"), lwd=1.5) +
        geom_line(aes(y=Mn.m.tx.vsh, colour="1988-2014"), lwd=1.5) + 
        labs(title="Mean Monthly Maximum Temperature (1960-2014)\n Babile",
             x="Month", y=yyy) + 
        scale_colour_manual("",
                            values = c("1960-1987"="darkblue","1988-2014"="red")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) 

dev.copy(png, filename="Mean Monthly Max Tmp of Babile (1960-87-2016).png",
         width = 1000, height = 750)
dev.off()




#Max Tmp Change between two halves
#Just build a fuction with it ( its simpler and tidy)
Tmpx_change <- function(x,y){
        ((mean(Tmx.sh[,2], na.rm = T) - mean(Tmx.fh[,2], na.rm = T))*100)/
                mean(Tmx.fh[,2], na.rm = T)
}

Tmpx_change(mean(Tmx.sh[,2], na.rm = T), mean(Tmx.fh[,2], na.rm = T))

#or
Tmpx_change <- mean(Tmax_sh[,2], na.rm = T) - mean(Tmax_fh[,2], na.rm = T)
#change <- expression("0.3363875(" ~ degree * c * ")")


#Monthly difference in Max Tmp b/n two halves
head(Mn.mn.tmxfh)
head(Mn.mn.tmxsh)

#First Half
storage <- numeric(12)
m <- c(Mn.mn.tmxfh[,2])

for (i in 1:12) {
        storage[i] <- m[i]
}

#Second Half
storage1 <- numeric(12)
n <- c(Mn.mn.tmxsh[,2])

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
        storage4[i] <- ((r*100)/Mn.mn.tmxfh[,2])[i]
}

Percen_change_tmax <- storage4





#seasonal Cycle of the first half
head(Tmx.xts.fh)
#convert to d.f
Tmx.df.fh <- data.frame(
        date=index(Tmx.xts.fh), 
        coredata(Tmx.xts.fh))

names(Tmx.df.fh)[2] <- "Tmax"

#DELETING ALL NAs
#Tmx.df.fh <- na.omit(Tmx.df.fh)

#let data, 
data <- Tmx.df.fh
names(data) <- c("dates", "Rain")

mean_dailymx_Babilefh <- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,
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
                           Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,
                           Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,
                           Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,
                           May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

seasonal.cycle.Tmx.Babile.fh <- data.frame(
        c(1:366),
        mean_dailymx_Babilefh) 

write.csv(seasonal.cycle.Tmx.Babile.fh, 
          file="Sesonal cycle of maxi tmp of first half of Babile(1960-1987).csv")

h <- ksmooth(c(1:366), 
             mean_dailymx_Babilefh,
             bandwidth = 15)

h.1 <- data.frame(h)

ggplot(seasonal.cycle.Tmx.Babile.fh, aes(x=c(1:366), y=mean_dailymx_Babilefh)) + 
        geom_line(col="firebrick", lwd=1) + 
        labs(title=
         "Seasonal Cycle of Maximum Temperature (1960-1987)\n babile", 
         x="Day", y=expression("Temperature("*~degree*c*")")) +
        geom_line(data = h.1, aes(x=x, y=y), col="blue", lwd=1) +
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13)) 

dev.copy(png, filename="Seasonal Rainfall Cycle of Babile (1960-1987).png",
         width = 1000, height = 750)
dev.off()



#seasonal Cycle of the second half
head(Tmx.xts.sh)
#convert to d.f
Tmx.df.sh <- data.frame(
        date=index(Tmx.xts.sh), 
        coredata(Tmx.xts.sh))

names(Tmx.df.sh)[2] <- "Tmax"

#DELETING ALL NAs
#Tmx.df.sh <- na.omit(Tmx.df.sh)


#let data, 
data <- Tmx.df.sh
names(data) <- c("dates", "Rain")

mean_dailymx_Babilesh <- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,
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
                           Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,
                           Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

seasonal.cycle.Tmx.Babile.sh <- data.frame(
        c(1:366), 
        mean_dailymx_Babilesh) 

write.csv(seasonal.cycle.Tmx.Babile.sh,
          file="Sesonal cycle of maxi tmp of second half of Babile(1988-2014).csv")

n <- ksmooth(
        c(1:366), 
        mean_dailymx_Babilesh, 
        bandwidth = 15)

n.1 <- data.frame(n)

ggplot(seasonal.cycle.Tmx.Babile.sh, aes(x=c(1:366), y=mean_dailymx_Babilesh)) +
        geom_line(col="firebrick", lwd=1) + 
        labs(title= "Seasonal Cycle of Maximum Temperature (1988-2014)\n Babile",
         x="Day", y=expression("Temperature("*~degree*c*")")) +
        geom_line(data = n.1, aes(x=x, y=y), col="blue", lwd=1) +
        theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
              axis.text = element_text(size = 13),
              axis.title = element_text(size = 13))
        
dev.copy(png, filename="Seasonal Cycle of Max Tmp of Babile (1988-2014).png",
         width = 1000, height = 750)
dev.off()


#plotiing both
ggplot(data=seasonal.cycle.Tmx.Babile.fh, aes(x=c(1:366))) + 
        geom_line(aes(y=seasonal.cycle.Tmx.Babile.fh[,2], lwd=0.25)) +
        geom_line(aes(y=h.1[,2],col="1960-1987", lwd=1.5)) +
        geom_line(aes(y=seasonal.cycle.Tmx.Babile.sh[,2], lwd=.25)) +
        geom_line(aes(y=n.1[,2], colour="1988-2014", lwd=1.5))
        labs(title="Seasonal Cycle of Maximum Temperature (1960-2014)\n Babile", 
             x="Days", y="Rainfall(mm)") 
        scale_colour_manual("",
                         values = c("1960-1987"="darkblue","1988-2014"="red")) +
                theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
                      axis.text = element_text(size = 13),
                      axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Max Tmp Cycle of Babile (1960-88-2014).png",
         width = 1000,  height = 750)
dev.off()



#Seasonal Max Tmp on Monthly Basis
head(Tmx)
Ap2 <- mean(subset(Tmx,format.Date(date, "%m")=="04")$Tmax, na.rm=T)
Ma2 <- mean(subset(Tmx,format.Date(date, "%m")=="05")$Tmax, na.rm=T)
Ju2 <- mean(subset(Tmx,format.Date(date, "%m")=="06")$Tmax, na.rm=T)
Jl2 <- mean(subset(Tmx,format.Date(date, "%m")=="07")$Tmax, na.rm=T)
Au2 <- mean(subset(Tmx,format.Date(date, "%m")=="08")$Tmax, na.rm=T)
Se2 <- mean(subset(Tmx,format.Date(date, "%m")=="09")$Tmax, na.rm=T)
Oc2 <- mean(subset(Tmx,format.Date(date, "%m")=="10")$Tmax, na.rm=T)

Seas2 <- data.frame(
        dates=Mn.mn.tmx[4:10, 1],
        Tmax=c(Ap2,Ma2,Ju2,Jl2,Au2,Se2,Oc2)
)

ggplot(data=Seas2,
       aes(x=dates,y=Tmax)) +
        geom_line(col="firebrick", lwd=2) +
        ggtitle(" Seasonal Maximum Temperature (1960-2014)\n Babile") + xlab("Month") +
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
head(Tmx)
sjx <- subset(Tmx, format.Date(date,  "%m")=="01")
sfx <- subset(Tmx, format.Date(date, "%m")=="02")
smx <- subset(Tmx, format.Date(date, "%m")=="03")
sax <- subset(Tmx, format.Date(date, "%m")=="04")
smax <- subset(Tmx, format.Date(date, "%m")=="05")
sjux <- subset(Tmx, format.Date(date, "%m")=="06")
sjlx <- subset(Tmx, format.Date(date, "%m")=="07")
saux <- subset(Tmx, format.Date(date, "%m")=="08")
ssx <- subset(Tmx, format.Date(date, "%m")=="09")
sox <- subset(Tmx, format.Date(date, "%m")=="10")
snx <- subset(Tmx, format.Date(date, "%m")=="11")
sdx <- subset(Tmx, format.Date(date, "%m")=="12")






#plot(seasonal.cycle.Tmx.Babile.fh, type="l", lwd=1, col="firebrick", 
#     main="Seasonal Max.Temp Cycle of Babile (1960-1987, 1988 - 2014)", 
#     xlab="Days",ylab="Rainfall(mm)")

#lines(h.1, lwd=3, col="darkblue")
#lines(seasonal.cycle.Tmx.Babile.sh, type="l", 
#      lwd=1, col="firebrick")
#
#lines(n.1, col="red", lwd=3)
#legend("bottomright", col=c("darkblue","red"),
#       bty = n, lty = 1, 
#       legend = c("1960-1987", '1988-2014'), lwd=3)






#Anomalie/Indexing of the first half of Babile(1960-1987)
#head(Tmxfh.ann.df)
#Tmxfh.ann.df.1 <- drop_na(Tmxfh.ann.df)
#lng.mn.txfh <- mean(Tmxfh.ann.df.1$Tmax)
#sd.txfh <- sd(Tmxfh.ann.df.1$Tmax)

#Tmxfh.ann.anom.v <- (Tmxfh.ann.df.1$Tmax - lng.mn.txfh)/
#        sd.txfh

#Tmxfh.Ann.anom <- data.frame(
#        Tmxfh.ann.df.1$date,
#        Tmxfh.ann.anom.v)

#write.csv(Tmxfh.Ann.anom,  
#          file="Index of Maximum Temp of Babile(1960-1987).csv")

#ggplot(Tmxfh.Ann.anom, aes(Tmxfh.ann.df.1$date, Tmxfh.ann.anom.v, ymin = 0,
#                           ymax = Tmxfh.ann.anom.v)) + 
#        geom_linerange(data = Tmxfh.Ann.anom, aes(colour = ifelse(Tmxfh.ann.anom.v > 0,
#        "Positive", "Negative")),stat = "identity", position = "identity",size=2) + 
#       geom_hline(yintercept=0) + 
#        theme(legend.title = element_text(size = 2, colour = F)) + 
#       scale_x_date(date_breaks= "4 years",date_labels="%Y") + 
#        labs(title="Index of Annual Maximum Temperature of Babile \n (1960-1987)", 
#             x="Years", y="Index")

#dev.copy(png, filename="Index of Annual Maximum Tmp of Babile (1960-1988).png",
#         width = 1000,  height = 750)
#dev.off()




#Anomalie/Indexing of the second half of Babile(1988-2014)
#head(Tmxsh.ann.df)
#mxsh.ann.df.1 <- drop_na(Tmxsh.ann.df)#

#lng.mn.txsh <- mean(Tmxsh.ann.df.1$Tmax)
#sd.txsh <- sd(Tmxsh.ann.df.1$Tmax)

#Tmxsh.ann.anom.v <- (Tmxsh.ann.df.1$Tmax - lng.mn.txsh)/
#        sd.txsh

#Tmxsh.Ann.anom <- data.frame(
#        Tmxsh.ann.df.1$date, 
#        Tmxsh.ann.anom.v)#

#write.csv(Tmxsh.Ann.anom,  
#          file="Index of Maximum Temp of Babile(1988-2014).csv")

#ggplot(Tmxsh.Ann.anom, aes(Tmxsh.ann.df.1$date, Tmxsh.ann.anom.v, ymin = 0,
#                           ymax = Tmxsh.ann.anom.v)) + 
#        geom_linerange(data = Tmxsh.Ann.anom, aes(colour = ifelse(Tmxsh.ann.anom.v > 0,
#        "Positive", "Negative")),stat = "identity",position = "identity",size=2) +
#        geom_hline(yintercept=0) + 
#        theme(legend.title = element_text(size = 2, colour = F)) + 
#        scale_x_date(date_breaks="4 years",date_labels="%Y") + 
#        labs(title="Index of Annual Maximum Temperature of Babile \n (1988-2014)",
#             x="Years", y="Index")

#dev.copy(png, filename="Seasonal Max Tmp Cycle of Babile (1988-2014).png",
#         width = 1000,  height = 750)
#dev.off()