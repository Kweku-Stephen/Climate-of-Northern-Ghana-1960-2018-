#Seasonal Cycle of Rainfall of Baible (1960-22014)
head(Prcp.xts_df)
#let,
data <- Prcp.xts_df
names(data)[1:2] <- c("dates","Rain")
head(data)


#Creating a vector
mean_daily_Babile<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                      Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                      Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,
                      Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                      Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,
                      Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,
                      Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,
                      Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,
                      Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,
                      Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Days_1 <- c(1:366)
Seasonal_cycle_Babile <-data.frame(
  c(1:366),
  mean_daily_Babile)

write.csv(Seasonal_cycle_Babile,
          file = "data_for_seasonal_cycle_of_Rainfall of Babile(1960-2014).csv")

s <- ksmooth(
  c(1:366), 
  mean_daily_Babile,
  bandwidth = 15)

s.1 <- data.frame(s) # converting to a dataframe
ggplot(Seasonal_cycle_Babile, aes(x=c(1:366), y=mean_daily_Babile)) + 
  geom_line(col="darkblue", lwd=1) + 
  labs(title="Seasonal Cycle of Rinfall (1960-2014)\n Babile",
       x="Day", y="Rainfall(mm)") + 
  geom_line(data = s.1, aes(x=x, y=y), col="red", lwd=1)

dev.copy(png, filename="Seasonal Cycle of ANNUAL Rainfall of Babile (1960-2014).png",
         width = 1000, height = 750)
dev.copy()


#alternatively
#s <- ksmooth(c(1:366), mean_daily_Babile, bandwidth = 15)
#plot(c(1:366),mean_daily_Babile, 
#     main = "Rainfall Seasonal Cycle of Baibile (1960-2014)", 
#     type = "l", lwd=2, col="darkblue", xlab = "Days", ylab="Daily Mean")
#lines(s, lwd = 2, col = "red")
#grid(col='grey')



#Dividing the data into 2 halves
fh.data <- data[1:8096, ]
sh.data <- data[8097: 17529, ]

#First Half
#let, 
data <- fh.data
mean_daily_Babile_fh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
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

Seasonal_cycle_Babile_fh <-data.frame(
  c(1:366),
  mean_daily_Babile_fh)

write.csv(Seasonal_cycle_Babile_fh, 
          file = "data_for_seasonal_cycle_of_Rainfall of Babile(1960-1987).csv")

t <- ksmooth(
  c(1:366),
  mean_daily_Babile_fh, 
  bandwidth = 15)

t.1 <- data.frame(t) # converting to a dataframe
ggplot(Seasonal_cycle_Babile_fh, aes(x=c(1:366), y=mean_daily_Babile_fh)) + 
  geom_line(col="darkblue", lwd=1) + 
  labs(title="Seasonal Cycle of Rainfall (1960-1987)\n Babile",
       x="Day", y="Rainfall(mm)") + 
  geom_line(data = t.1, aes(x=x, y=y), col="red", lwd=1)

dev.copy(png, filename="Seasonal Rainfall of Babile (1960-1987).png",
         width = 1000,  height = 750)
dev.off()


#Second Half
data <- sh.data
mean_daily_Babile_sh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,
                         Jan_10,Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,
                         Jan_18,Jan_19,Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,
                         Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,
                         Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,
                         Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,
                         Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,
                         Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,
                         Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,
                         Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,Mar_82,Mar_83,
                         Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Babile_sh <-data.frame(
  c(1:366),
  mean_daily_Babile_sh)

write.csv(Seasonal_cycle_Babile_sh,
          file = "data_for_seasonal_cycle_of_Rainfall of Babile(1988-2014).csv")

e <- ksmooth(
  c(1:366), 
  mean_daily_Babile_sh, 
  bandwidth = 15)

e.1 <- data.frame(e) # converting to a dataframe
ggplot(Seasonal_cycle_Babile_sh, aes(x=c(1:366), y=mean_daily_Babile_sh)) + 
  geom_line(col="darkblue", lwd=1) + 
  labs(title="Seasonal Cycle of Rainfall (1988-2014)\n Babile", 
       x="Day", y="Rainfall(mm)") + 
  geom_line(data = e.1, aes(x=x, y=y), col="red", lwd=1)

dev.copy(png, filename="Seasonal Rainfall of Babile (1988-2014).png",
         width = 1000,  height = 750)
dev.off()




#plotting both datasets on the same plot
ggplot(data=Seasonal_cycle_Babile_fh, aes(x=c(1:366))) + 
  geom_line(aes(y=Seasonal_cycle_Babile_fh[,2], lwd=0.25)) + 
  labs(title="Seasonal Rainfall Cycle of Babile \n (1960-1987)", 
       x="Days", y="Rainfall(mm)") +
  geom_line(aes(y= t.1[,2], colour="1960-1987", lwd=1.25)) + 
  geom_line(aes(y=Seasonal_cycle_Babile_sh[,2], lwd=0.25)) + 
  geom_line(aes(y= e.1[,2], colour="1988-2014", lwd=1.25)) +
  scale_colour_manual("",
                      values = c("1960-1987"="darkblue", "1988-2014"="red"))
    

dev.copy(png, filename="Seasonal Rainfall of Babile (1960-88-2014).png",
         width = 1000,  height = 750)
dev.off()







#plot(Seasonal_cycle_Babile_fh, type="l",lwd=1,
#     main="Seasonal Rainfall Cycle of Babile 1960-2014", 
#     xlab="Days",ylab="Rainfall(mm)", col="darkblue")
#lines(t.1, lwd=3, col="blue")
#lines(Seasonal_cycle_Babile_sh, type="l", lwd=1, col="darkblue")
#lines(e.1, lwd=3, col="red")
#legend("topright", col = c("blue","red"), lty=1, 
#       bty = n, legend = c("1960-1987","1988-2014"), lwd=3)
