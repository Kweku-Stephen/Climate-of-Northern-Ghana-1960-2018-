###############################################################################################

setwd("F:/R_PROJECTS/Vea")
getwd()



require(ggplot2)
require(xts)
require(timeSeries)
require(scales)
require(tidyr)
require(ggthemes)
require(imputeTS)
require(Kendall)

#........................................................................................................................................#

data1 <- read.table("Vea_rr_temp.txt",header = T, 
                    sep = "\t", na.strings = -99.9)

head(data1)
dates <- with(
  data1, as.Date(paste(year,month,day), 
                 paste("%Y %m %d"))
)

data1 <- data.frame(
  dates, data1$prcp, 
  data1$tmax,data1$tmin
)

names(data1)[2:4] <- c("Prcp","Tmax","Tmin")
head(data1)



#........................................................................................................................................#

#Vea Prcp
Prcp <- data.frame(dates, data1$Prcp)
names(Prcp)[2] <- "Prcp"
head(Prcp)

colSums(is.na(Prcp))

Prcp.xts <- xts(Prcp$Prcp, Prcp$dates)
Prcp.xts.ann <- apply.yearly(Prcp.xts, sum, na.rm=T)
head(Prcp.xts.ann)
#conversion to dataframe
Prcp.df.ann <- data.frame(
  date=index(Prcp.xts.ann), 
  coredata(Prcp.xts.ann)
)

#date conversion
Prcp.df.ann$date <- as.Date(
  Prcp.df.ann$date, 
  format="%Y-%m-%d"
)

names(Prcp.df.ann)[2] <- "Prcp"
head(Prcp.df.ann)
#Deleting 2012 (loss of DATA (Jan-Aug))
Prcp.df.ann <- Prcp.df.ann[!Prcp.df.ann[,1] == "2012-12-31", ]
colSums(is.na(Prcp.df.ann))

write.csv(Prcp.df.ann, 
          file="Annual Rainfall of Vea (1960-2017).csv")
MK <-MannKendall(Prcp.df.ann[,2])
summary(MK)

ggplot(Prcp.df.ann, aes(x=date, y=Prcp)) + geom_line(col="darkblue", lwd=2) + 
  geom_smooth(method = lm, col="red") + 
  labs(title="Annual Rainfall Total (1972-2017)\n Vea",
       x="Year", y="Rainfall(mm)") + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  annotate("text", x=Prcp.df.ann[Prcp.df.ann[,1] == "1977-12-31", ]$date,
           y=1200, label="P-value=0.42815", cex = 5)

dev.copy(png, filename="Annual Rainfall of Vea (1972-2017).png", 
         height=750, width=1000)
dev.off()

sum1 <- summary(Prcp.df.ann)
write.csv(sum1,
          file = "Summary stats for Annual rAINFALL OF Vea (1972-2017).csv")
#0r
sum11 <- data.frame(Prcp.df.ann[Prcp.df.ann$Prcp == max(Prcp.df.ann$Prcp), ],
                    Prcp.df.ann[Prcp.df.ann$Prcp == min(Prcp.df.ann$Prcp), ])

sum11[2,c(1,2)] <- sum11[1, c(3,4)] 
sum11[-c(3,4)] -> sum11
dimnames(sum11) <- list(
  c("Max:","Min:"), 
  c("Year","Rainfall")
)  

write.csv(sum11,
          file = "summary stats for Annual Prcp.1 (Vea).csv")




#Mean Monthly Rainfall
head(Prcp)
Prcp.xts.1 <-drop_na(Prcp)

Prcp.xts.1 <- xts(
  Prcp.xts.1[,2], 
  Prcp.xts.1[,1]
)

Prcp.mn.xts <- apply.monthly(
  Prcp.xts.1, 
  sum, na.rm=T)

head(Prcp.mn.xts)

#conversion to data.frame
Prcp.mn.df <- data.frame(
  date=index(Prcp.mn.xts), 
  coredata(Prcp.mn.xts)
)

names(Prcp.mn.df)[2] <- "Prcp"
head(Prcp.mn.df)

#date conversion
Prcp.mn.df$date <- as.Date(
  Prcp.mn.df$date, 
  format="%Y-%m-%d"
)

head(Prcp.mn.df)
any(is.na(Prcp.mn.df))
#Averaging all months
Jan <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="01")$Prcp)
Feb <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="02")$Prcp)
March <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="03")$Prcp)
April <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="04")$Prcp)
May <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="05")$Prcp)
June <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="06")$Prcp)
July <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="07")$Prcp)
August <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="08")$Prcp)
September <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="09")$Prcp)
October <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="10")$Prcp)
November <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="11")$Prcp)
December <- mean(subset(Prcp.mn.df, format.Date(Prcp.mn.df$date, "%m")=="12")$Prcp)

#creating a vector
mean.mn.Prcp.v <- c(Jan,Feb,March,April,May,June,July,August,
                    September,October,November,December)

date.mn <- seq(
  as.Date("1990-01-01"),
  by="months",
  length.out = 12
)

mean.mn.Prcp <- data.frame(
  date.mn,
  mean.mn.Prcp.v
)

write.csv(mean.mn.Prcp, 
          file="Mean Monthly Rainfall of Vea (1972-2017).csv")

ggplot(mean.mn.Prcp, aes(x=date.mn, y=mean.mn.Prcp.v)) + 
  geom_line(col="darkblue", lwd=2) + scale_x_date(date_breaks = "1 month",
                                                  date_labels = "%b") + 
  labs(title="Mean Monthly Rainfall (1972-2017)\n Vea", 
       x="Month",y="Rainfall(mm)")

dev.copy(png, filename="Mean Monthly Rainfall of Vea (1972-2017).png", 
         height=750, width=1000)
dev.off()

sum2 <- summary(mean.mn.Prcp)
write.csv(sum2,
          file = "Summary Stats for Mean monthly Rainfall of Vea(1972-2017).csv")
#or
sum22 <- data.frame(mean.mn.Prcp[mean.mn.Prcp[,2] == max(mean.mn.Prcp[,2]), ],
                    mean.mn.Prcp[mean.mn.Prcp[,2] == min(mean.mn.Prcp[,2]), ])

sum22[2,c(1,2)] <- sum22[1, c(3,4)] 
sum22[-c(3,4)] -> sum22
sum22[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum22) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall")
)  


write.csv(sum22,
          file = "summary stats for mean mOnth Prcp.1 (Vea).csv")




#Annual Rainy Days of Vea
#Affected by missing cases
head(Prcp.xts)

#conversion to data frame
Prcp.xts.df <- data.frame(
  date=index(Prcp.xts), 
  coredata(Prcp.xts)
)

names(Prcp.xts.df)[2] <- "Prcp"
head(Prcp.xts.df)
#Prcp.xts.df <- Prcp.xts.df %>% drop_na()
#subsetting individual years
b.72.1 <- nrow(subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="72")
               [subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="72")$Prcp >= 0.85, ]
)

b.17.1 <- nrow(subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="17")
               [subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="17")$Prcp >= 0.85, ]
)

Ann.rndys.v <- c( b.72.1, b.73.1, b.74.1, b.75.1, 
                 b.76.1, b.77.1, b.78.1, b.79.1, b.80.1, b.81.1, b.82.1, b.83.1, 
                 b.84.1, b.85.1, b.86.1, b.87.1, b.88.1, b.89.1, b.90.1, b.91.1, 
                 b.92.1, b.93.1, b.94.1, b.95.1, b.96.1, b.97.1, b.98.1, b.99.1, 
                 b.00.1, b.01.1, b.02.1, b.03.1, b.04.1, b.05.1, b.06.1, b.07.1, 
                 b.08.1, b.09.1, b.10.1, b.11.1, b.12.1, b.13.1, b.14.1, b.15.1,
                 b.16.1, b.17.1)

date.ann <- seq(
  as.Date("1972-01-01"),
  by="years", 
  length.out = length(Ann.rndys.v)
)

Ann.rndys <- data.frame(date.ann, Ann.rndys.v)

#Delete 2012
Ann.rndys <- Ann.rndys[
  !Ann.rndys$date.ann == "2012-01-01", 
  ]

write.csv(Ann.rndys, 
          file="Annual Rainy Days of Damongo (1960-2017).csv")

MK <- MannKendall(Ann.rndys[,2])
summary(MK)

ggplot(Ann.rndys, aes(x=date.ann, y=Ann.rndys.v)) + 
  geom_line(col="darkblue", lwd=2) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  labs(title="Number of Days when Rainfall >= 0.85mm (1972-2017)\n Vea",
       x="Year",y="Day") + 
  geom_smooth(method = lm, col="red") + 
  annotate("text", x=Ann.rndys[Ann.rndys$date.ann == "1993-01-01", ]$date.ann,
           y=75, label="P-value=0.80638", cex = 5)

dev.copy(png, filename="Annual Rainy Days of Damongo (1972-2017).png", 
         width=1000, height=750)
dev.off()

sum3 <- summary(Ann.rndys)
write.csv(sum3,
          file="Summary Stats for Annual Rainy Days grt 0.87 mm of Vea (1972-2017).csv")
#or
sum33 <- data.frame(Ann.rndys[Ann.rndys[,2] == max(Ann.rndys[,2]), ],
                    Ann.rndys[Ann.rndys[,2] == min(Ann.rndys[,2]), ])

sum33[2,c(1,2)] <- sum33[1, c(3,4)] 
sum33[-c(3,4)] -> sum33
#sum33[,1] <- data.frame(month.name[c(09,01)])
dimnames(sum33) <- list(
  c("Max:","Min:"), 
  c("Year","Rainfall")
)  


write.csv(sum33,
          file = "summary stats for ann rainy days (Vea).csv")




# Annual Rainy Days of Vea >= 20mm
head(Prcp.xts.df)
Prcp.ann.rdys20 <- Prcp.xts.df[Prcp.xts.df$Prcp >= 20, ]
b.72.2 <- nrow(subset(Prcp.ann.rdys20, format.Date(Prcp.ann.rdys20$date, "%y")=="72"))
#to
b.17.2 <- nrow(subset(Prcp.ann.rdys20, format.Date(Prcp.ann.rdys20$date, "%y")=="17"))

#vectorization
Ann.rndys20.v <- c( b.72.2, b.73.2, 
                   b.74.2, b.75.2, b.76.2, b.77.2, b.78.2, b.79.2, b.80.2,
                   b.81.2, b.82.2, b.83.2, b.84.2, b.85.2, b.86.2, b.87.2,
                   b.88.2, b.89.2, b.90.2, b.91.2, b.92.2, b.93.2, b.94.2,
                   b.95.2, b.96.2, b.97.2, b.98.2, b.99.2, b.00.2, b.01.2, 
                   b.02.2, b.03.2, b.04.2, b.05.2, b.06.2, b.07.2, b.08.2, 
                   b.09.2, b.10.2, b.11.2, b.12.2, b.13.2, b.14.2, b.15.2, 
                   b.16.2, b.17.2)

date.rdys20 <- seq(
  as.Date("1972-01-01"), 
  by = "years", 
  length.out = length(Ann.rndys20.v)
)

Ann.rdys20.Vea <- data.frame(date.rdys20, Ann.rndys20.v)

#Delete 2012
Ann.rdys20.Vea <- Ann.rdys20.Vea[!Ann.rdys20.Vea$date.rdys20 == "2012-01-01", ]
write.csv(Ann.rdys20.Vea,
          file="Annual Rainy Days of Vea grt than 20mm (1972-2017).csv")

MK <- MannKendall(Ann.rdys20.Vea[,2])
summary(MK)

ggplot(Ann.rdys20.Vea, aes(x=date.rdys20, y=Ann.rndys20.v)) + 
  geom_line(col="darkblue", lwd=2) + geom_smooth(method = lm, col="red") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  ggtitle("Number of Days when Rainfall >= 20mm (1972-2017)\n Vea") +
  xlab("Year") +ylab("Rainfall(mm)") + 
  annotate("text", x=Ann.rdys20.Vea[Ann.rdys20.Vea[,1] == "1965-01-01", ]$date.rdys20,
           y=23, label="P-value=0.056842",cex=5)

dev.copy(png, filename="Annual Rainy Days of Vea grt than 20mm (1972-2017).png",
         height=750, width=1000)
dev.off()

sum4 <- summary(Ann.rdys20.Vea)
write.csv(sum4,
          file="Summary Stats for number of Days when RAINFLL GRT 20mm of Vea (1972-2017).csv")
#or
sum44 <- Ann.rdys20.Vea[Ann.rdys20.Vea[,2] == max(Ann.rdys20.Vea[,2]), ]
sum44[3,c(1,2)] <- Ann.rdys20.Vea[Ann.rdys20.Vea[,2] == min(Ann.rdys20.Vea[,2]), ]

dimnames(sum44) <- list(
  c("Max:","//","Min:"), 
  c("Year","Rainfall(mm)")
)  

write.csv(sum44,
          file = "summary stats for ann rainy days grt 20 (Vea).csv")



#Monthly Rainy Days
head(Prcp)
#Monthly Rainy Days of Bole >= 1mm (1960-2017)
head(Prcp)
#subsetting all days with rainfall >= 1mm
Prcp.mn.rdys <- Prcp[Prcp$Prcp >= 0.85, ]
width <- length(1972:2017) #....#length of 1960-2016)
#subsetting individual months out of the Prcp.mn.rdys data and identifying number of rainy days
J <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="01"))/width
F <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="02"))/width
M <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="03"))/width
A <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="04"))/width
Ma <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="05"))/width
Ju <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="06"))/width
Jl <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="07"))/width
Au <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="08"))/width
S <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="09"))/width
O <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="10"))/width
N <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="11"))/width
D <- nrow(subset(Prcp.mn.rdys, format.Date(Prcp.mn.rdys$date, "%m")=="12"))/width

#creating a vector with the aboves
Vea.ann.rdys <- c(J,F,M,A,Ma,Ju,Jl,Au,S,O,N,D)
date.mn
monthly.rdys <- data.frame(date.mn,Vea.ann.rdys) 
write.csv(monthly.rdys, 
          file = "Monthly Rainy Days of Vea (1972-2016).csv")

ggplot(monthly.rdys, aes(x=date.mn, y=Vea.ann.rdys)) + 
  geom_line(col="darkblue", lwd=2) +
  labs(title="Number of Days when Rainfall >= 0.87 (1972-2017)\n Vea",
       x="Month", y="Rainfall(mm)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Monthly Rainy Days of Vea (1972-2017)", 
         height = 500, width = 1000)
dev.off()

sum5 <- summary(monthly.rdys)
write.csv(sum5,
          file="Summary Stats for monthly rainy days of Vea.csv")
#or
sum55 <- data.frame(monthly.rdys[monthly.rdys[,2] == max(monthly.rdys[,2]), ],
                    monthly.rdys[monthly.rdys[,2] == min(monthly.rdys[,2]), ])

sum55[2,c(1,2)] <- sum55[1, c(3,4)] 
sum55[-c(3,4)] -> sum55
sum55[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum55) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum55,
          file = "summary stats for monthly rainy days (Vea).csv")



#Monthly Rainy Days >= 20nn
head(Prcp)
#Monthly Rainy Days of Bole >= 20mm (1960-2017)
head(Prcp)
#subsetting all days with rainfall >= 20mm
Prcp.mn.rdys20 <- Prcp[Prcp$Prcp >= 20, ]

#width20 <- length(1960:2017) #....#length of 1960-2017)
#subsetting individual months out of the Prcp.mn.rdys data and identifying number of rainy days
J.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="01"))/width
F.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="02"))/width
M.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="03"))/width
A.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="04"))/width
Ma.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="05"))/width
Ju.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="06"))/width
Jl.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="07"))/width
Au.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="08"))/width
S.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="09"))/width
O.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="10"))/width
N.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="11"))/width
D.1 <- nrow(subset(Prcp.mn.rdys20, format.Date(Prcp.mn.rdys20$date, "%m")=="12"))/width

#creating a vector with the aboves
Vea.ann.rdys20 <- c(J.1,F.1,M.1,A.1,Ma.1,Ju.1,
                    Jl.1,Au.1,S.1,O.1,N.1,D.1)
date.mn
monthly.rdys20 <- data.frame(
  date.mn,
  Vea.ann.rdys20
)

write.csv(monthly.rdys20, 
          file = "Monthly Rainy Days of Vea grt 20 (1972-2017).csv")

ggplot(monthly.rdys20, aes(x=date.mn, y=Vea.ann.rdys20)) + 
  geom_line(col="darkblue", lwd=2) +
  labs(title="Nmber of Days when Rainfall >= 20mm (1972-2017)\n Vea", 
       x="Month", y="Rainfall(mm)") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Monthly Rainy Days of Vea (1972-2017)", 
         height = 500, width = 1000)
dev.off()

sum6 <- summary(monthly.rdys20)
write.csv(sum6,
          file = "Monthly Rainy Days grt 20mm of Vea.csv")
#OR
sum66 <- data.frame(monthly.rdys20[monthly.rdys20[,2] == max(monthly.rdys20[,2]), ],
                    monthly.rdys20[monthly.rdys20[,2] == min(monthly.rdys20[,2]), ])

sum66[2,c(1,2)] <- sum66[1, c(3,4)] 
sum66[-c(3,4)] -> sum66
sum66[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum66) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum66,
          file = "summary stats for monthly rainy days grt 20mm (Vea).csv")



#Indexing Annual Rainfall (Dam)
head(Prcp.df.ann)
lng.mn.Prcp <- mean(Prcp.df.ann$Prcp)
sd.mn.Prcp <- sd(Prcp.df.ann$Prcp)

Prcp.df.ann.anom.v <- (Prcp.df.ann$Prcp - lng.mn.Prcp)/
  sd.mn.Prcp

date.ann.1 <- Prcp.df.ann$date 

Prcp.df.ann.anom <- data.frame(
  date.ann.1, 
  Prcp.df.ann.anom.v
)

write.csv(Prcp.df.ann.anom, 
          file="Data for Index of Annual RAINFALL of Vea(1972-2017).csv")

ggplot(Prcp.df.ann.anom, aes(date.ann.1, Prcp.df.ann.anom.v, ymin = 0,
                             ymax = Prcp.df.ann.anom.v)) + 
  geom_linerange(data = Prcp.df.ann.anom, aes(colour = ifelse(Prcp.df.ann.anom.v >0,
  "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = F)) + 
  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title="Annual Rainfall Total Anomaly (1972-2017)\n Vea",
       x="Year", y="Anomaly")

dev.copy(png, filename="Index of Annual Rainfall of Vea (1972-2017).png", 
         height=750, width=1000)
dev.off()

sum7 <- summary(Prcp.df.ann.anom)
write.csv(sum7,
            file = "Annual Rainfall ANOMALY of Vea.csv")
#or
sum77 <- data.frame(Prcp.df.ann.anom[Prcp.df.ann.anom[,2] == max(Prcp.df.ann.anom[,2]), ],
                    Prcp.df.ann.anom[Prcp.df.ann.anom[,2] == min(Prcp.df.ann.anom[,2]), ])

sum77[2,c(1,2)] <- sum77[1, c(3,4)] 
sum77[-c(3,4)] -> sum77
#sum77[,1] <- data.frame(month.name[c(09,01)])
dimnames(sum77) <- list(
  c("Max:","Min:"), 
  c("Year","Rainfall(mm)")
)  

write.csv(sum77,
          file = "summary stats for annual Prcp Anomaly (Vea).csv")



#Seasonal Cycle of Rainfall of vEA (1960-2017)
head(Prcp)
#let Prcp,
data <- Prcp
names(data) <- c("dates","Rain")
head(data)

mean_daily_Vea <- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,Jan_11, 
                   Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,Jan_20,Jan_21,Jan_22,
                   Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,
                   Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,
                   Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,
                   Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,
                   Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,
                   Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,
                   Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,
                   May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,
                   Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,
                   Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,
                   Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,
                   Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Vea <-data.frame(
  c(1:366),
  mean_daily_Vea
)

write.csv(Seasonal_cycle_Vea, 
          file = "data_for_seasonal_cycle_of_Rainfall of Vea(1972-2017).csv")

s <- ksmooth(
  c(1:366), 
  mean_daily_Vea, 
  bandwidth = 15
)

s.1 <- data.frame(s) # converting to a dataframe

ggplot(Seasonal_cycle_Vea, aes(x=c(1:366), y=mean_daily_Vea)) + 
  geom_line(col="darkblue", lwd=1) + 
  labs(title="Seasonal Cycle of Rainfall (1960-2017)\n Vea", 
       x="Day", y="Rainfall(mm)") + 
  geom_line(data = s.1, aes(x=x, y=y), col="red", lwd=1)

dev.copy(png, filename="Seasonal Rainfall Cycle of Vea (1972-2017).png", 
         height=750, width=1000)
dev.off()

sum7 <- summary(Seasonal_cycle_Vea)
write.csv(sum7,
          file = "Rainfall Seasonal Cycle of Vea.csv")





#Dividing Prcp into two halves
#Dividing daily Prcp
head(Prcp)
Prcp.fh <- Prcp[(Prcp$dates >= "1972-01-01" &
                   Prcp$dates <= "1994-12-31"), ]

Prcp.sh <- Prcp[(Prcp$dates >= "1995-01-01" &
                   Prcp$dates <= "2017-12-31"), ]

#Dividing annual Prcp
Prcp_fh <- Prcp.df.ann[(Prcp.df.ann$date >= "1972-12-31" &
                          Prcp.df.ann$date <= "1994-12-31"), ]
write.csv(Prcp_fh,file = "Anual Rainfall Total of Vea (1972-1994).csv")


Prcp_sh <- Prcp.df.ann[(Prcp.df.ann$date >= "1995-12-31" &
                          Prcp.df.ann$date <= "2017-12-31"), ]
write.csv(Prcp_sh,file = "Annual Rianfall Total of Vea (1995-2017).csv")


#Annual Rainfall first half (1960-1988)
#Prcp_fh
#write.csv(Prcp_fh, file="Annual Prcp of Bole (1960-88).csv")
#ggplot(Prcp_fh, aes(x=Prcp_fh$date, y=Prcp_fh$Prcp)) + 
#geom_line(col="darkblue", lwd=2) + geom_smooth(method = lm, col="red") +
#  labs(title='ANNUAL Rainfall of Bole (1960-1988)',
#       x="Years", y="Rainfall(mm)") 


#Mean Monthly Rainfall
head(Prcp.mn.df)
Prcp.mn.dffh <- Prcp.mn.df[(Prcp.mn.df$date >= "1972-01-31" &
                              Prcp.mn.df$date <= "1994-12-31"), ]

Prcp.mn.dfsh <- Prcp.mn.df[(Prcp.mn.df$date >= "1995-01-31" & 
                              Prcp.mn.df$date <= "2017-12-31"), ]

#First Half
head(Prcp.mn.dffh)
#Averaging all months
Jan.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="01")$Prcp)
Feb.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="02")$Prcp)
March.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="03")$Prcp)
April.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="04")$Prcp)
May.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="05")$Prcp)
June.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="06")$Prcp)
July.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="07")$Prcp)
August.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="08")$Prcp)
September.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="09")$Prcp)
October.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="10")$Prcp)
November.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="11")$Prcp)
December.fh <- mean(subset(Prcp.mn.dffh, format.Date(Prcp.mn.dffh$date, "%m")=="12")$Prcp)

#creating a vector
mean.mn.Prcpfh.v <- c(Jan.fh,Feb.fh,March.fh,April.fh,May.fh,June.fh,July.fh,
                      August.fh,September.fh,October.fh,November.fh,December.fh)

date.mn 
mean.mn.Prcpfh <- data.frame(
  date.mn, 
  mean.mn.Prcpfh.v
)

write.csv(mean.mn.Prcpfh, 
          file="Mean Monthly Rainfall of Damongo (1972-1995).csv")

ggplot(mean.mn.Prcpfh, aes(x=date.mn, y=mean.mn.Prcpfh.v)) + 
  geom_line(col="darkblue", lwd=2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Rainfall (1972-1994)\n Vea", 
       x="Month",y="Rainfall(mm)")

dev.copy(png, filename="Mean Monthly Rainfall of Vea (1972-1994).png",
         height=750, width=1000)
dev.off()
  
sum8 <- summary(mean.mn.Prcpfh)
write.csv(sum8,
          file = "mean monthly rainfall of VEA (1972-1994).csv")
#or
sum88 <- data.frame(mean.mn.Prcpfh[mean.mn.Prcpfh[,2] == max(mean.mn.Prcpfh[,2]), ],
                    mean.mn.Prcpfh[mean.mn.Prcpfh[,2] == min(mean.mn.Prcpfh[,2]), ])

sum88[2,c(1,2)] <- sum88[1, c(3,4)] 
sum88[-c(3,4)] -> sum88
sum88[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum88) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum88,
          file = "summary stats for monthly rainfall (1960-1988).csv")




#Annual Rainfall second half (1989 - 2017)
#head(Prcp_sh)
write.csv(Prcp_sh, file="Annual Prcp of Bole (1989-2017).csv")
#ggplot(Prcp_sh, aes(x=Prcp_sh$date, y=Prcp_sh$Prcp)) + 
#geom_line(col="darkblue", lwd=2) + geom_smooth(method = lm, col="red") + 
#  labs(title='ANNUAL Rainfall of Bole (1989-2017)',
#       x="Years", y="Rainfall(mm)") 


#second Half
head(Prcp.mn.dfsh)
#Averaging all months
Jan.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="01")$Prcp)
Feb.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="02")$Prcp)
March.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="03")$Prcp)
April.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="04")$Prcp)
May.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="05")$Prcp)
June.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="06")$Prcp)
July.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="07")$Prcp)
August.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="08")$Prcp)
September.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="09")$Prcp)
October.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="10")$Prcp)
November.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="11")$Prcp)
December.sh <- mean(subset(Prcp.mn.dfsh, format.Date(Prcp.mn.dfsh$date, "%m")=="12")$Prcp)
#creating a vector
mean.mn.Prcpsh.v <- c(Jan.sh,Feb.sh,March.sh,April.sh,May.sh,June.sh,July.sh,
                      August.sh,September.sh,October.sh,November.sh,December.sh)
date.mn 
mean.mn.Prcpsh <- data.frame(
  date.mn, 
  mean.mn.Prcpsh.v
)

write.csv(mean.mn.Prcpsh,
          "Mean Monthly Rainfall of Vea(1995-2017).csv")

ggplot(mean.mn.Prcpsh, aes(x=date.mn, y=mean.mn.Prcpsh.v)) + 
  geom_line(col="darkblue", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Rainfall (1995-2017)\n Vea", 
       x="Month",y="Rainfall(mm)")

dev.copy(png, filename="Mean Monthly Rainfall of Vea (1995-2017.png",
         height=750, width=1000)
dev.off()

sum9 <- summary(mean.mn.Prcpsh)
write.csv(sum9,
          file = "mean monthly rainfall of VEA (1995-2017).csv")
#or
sum99 <- data.frame(mean.mn.Prcpsh[mean.mn.Prcpsh[,2] == max(mean.mn.Prcpsh[,2]), ],
                    mean.mn.Prcpsh[mean.mn.Prcpsh[,2] == min(mean.mn.Prcpsh[,2]), ])

sum99[2,c(1,2)] <- sum99[1, c(3,4)] 
sum99[-c(3,4)] -> sum99
sum99[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum99) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum99,
          file = "summary stats for monthly rainfall (1989-2016).csv")



#plot(mean.mn.Prcpfh, type="l",lwd=3,
#     main='Mean Monthly Rainfall(mm)- 1960-2017', col="darkblue",
#     xlab="Months",ylab='Rainfall(mm)')

#lines(mean.mn.Prcpsh, col="red",lwd=3)
#legend("topright", col=c("darkblue", "red"), 
#       legend = c("1960-1988","1989-2017"),
#       bty = "n", lwd=3, lty = 1)


ggplot(mean.mn.Prcpfh,aes(x=mean.mn.Prcpfh[,1])) + 
  geom_line(aes(y=mean.mn.Prcpfh[,2],col="1972-1994", lwd= 2)) +
  geom_line(aes(y=mean.mn.Prcpsh[,2],col="1995-2017", lwd=2)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + xlab("Month") +
  ylab("Rainfall(mm)") + ggtitle("Mean Monthly Rainfall of Vea\n (1972-2017)") #

dev.copy(png, filename = "Mean Monthly Min Tmp of Vea (1972-94-2017).png",
         width = 1000, height = 750)
dev.off()




#Percentage Change between both climatologies(1960-88, 1989-2016)

Pec <- (mean(Prcp_sh[,2], na.rm = T)-mean(Prcp_fh[,2], na.rm = T))*100/
  mean(Prcp_fh[,2], na.rm = T)



#Comparing Rainy Days of 1mm
head(Ann.rndys)
Ann.rndys_fh <- Ann.rndys[Ann.rndys[,1] >= "1972-01-01" & 
                            Ann.rndys[,1] <= "1994-01-01", ]

Ann.rndys_sh <- Ann.rndys[Ann.rndys[,1] >= "1995-01-01" & 
                            Ann.rndys[,1] <= "2017-01-01", ]

#introducing another row to make Ann.rndys[,2] == Ann.rndys[,2], so we can plot both
Ann.rndys_sh[23,2] <- NA

ggplot(data = data.frame(c(1:29), Ann.rndys_fh[,2]),aes(x=c(1:29))) +
  geom_line(aes(y=Ann.rndys_fh[,2], colour = "1972-1994"), lwd=1.5) +
  geom_line(aes(y=Ann.rndys_sh[,2], colour = "1995-2017"), lwd=1.5) +
  scale_color_manual("",values = c("1972-1994"="darkblue",
                                   "1995-2017"="red")) +
  labs(title = "Comparing Number of Days when Rainfall >= 0.87 (1972-2017)\n Vea",
       x="X", y="Day")

dev.copy(png,filename="Comparing Days with Rainfall grt 0.85.png",
         width = 1000, height = 750)
dev.off


#Percentage Change
(mean(Ann.rndys_sh[,2], na.rm = T) - mean(Ann.rndys_fh[,2], na.rm = T))*100/
  mean(Ann.rndys_fh[,2], na.rm = T)



#COMPARING ANNUAL RAINY DAYS OF 20mm
Ann.rndys20_fh <- Ann.rdys20.Vea[Ann.rdys20.Vea[,1] >= "1972-01-01" &
                                   Ann.rdys20.Vea[,1] <= "1994-01-01", ]

Ann.rndys20_sh <- Ann.rdys20.Vea[Ann.rdys20.Vea[,1] >= "1995-01-01" &
                                   Ann.rdys20.Vea[,1] <= "2017-01-01", ]

#introducing another row to make Ann.rndys20.Zua[,2] == Ann.rndys20.Zua[,2], 
#so we can plot both
Ann.rndys20_sh[23,2] <- NA   

ggplot(data = data.frame(c(1:29), Ann.rndys20_fh[,2]),aes(x=c(1:29))) +
  geom_line(aes(y=Ann.rndys20_fh[,2], colour = "1972-1994"), lwd=1.5) +
  geom_line(aes(y=Ann.rndys20_sh[,2], colour = "1995-2017"), lwd=1.5) +
  scale_color_manual("",values = c("1972-1994"="darkblue",
                                   "1995-2017"="red")) +
  labs(title = "Comparing Number of Days when Rainfall >= 20mm (1960-2016)\nVea",
       x="X", y="Day")

dev.copy(png,filename="Comparing Days with Rainfall grt 20mm Vea.png",
         width = 1000, height = 750)
dev.off


#Percentage Change
(mean(Ann.rndys20_sh[,2], na.rm = T) - mean(Ann.rndys20_fh[,2], na.rm = T))*100/
  mean(Ann.rndys20_fh[,2], na.rm = T)  





#COMPARING MONTHLY RAINY DAYS
head(Prcp.mn.rdys) 
Prcp.mn.rdysfh <- Prcp.mn.rdys[Prcp.mn.rdys[,1] >= "1972-01-01" &
                                 Prcp.mn.rdys[,1] <= "1994-12-31", ] %>% drop_na()

Prcp.mn.rdysh <- Prcp.mn.rdys[Prcp.mn.rdys[,1] >= "1995-01-01" &
                                Prcp.mn.rdys[,1] <= "2017-12-31", ] %>% drop_na()



#First Half (Monthly Prcp days)
head(Prcp.mn.rdysfh)

#subseting all individual months
width.rd20 <- length(1972:2017)

Jan1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="01"))/width.rd20

Feb1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="02"))/width.rd20

Mar1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="03"))/width.rd20

Apr1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="04"))/width.rd20

May1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="05"))/width.rd20

June1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="06"))/width.rd20

July1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="07"))/width.rd20

Aug1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="08"))/width.rd20

Sept1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="09"))/width.rd20

Oct1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="10"))/width.rd20

Nov1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="11"))/width.rd20

Dec1fh <- nrow(subset(Prcp.mn.rdysfh, format.Date(
  Prcp.mn.rdysfh$date, "%m")=="12"))/width.rd20


mn_rdys.vfh <- c(Jan1fh, Feb1fh, Mar1fh, Apr1fh, May1fh, June1fh, July1fh, Aug1fh,
                 Sept1fh, Oct1fh, Nov1fh, Dec1fh) 

date.mn.rdys_fh <- seq(
  as.Date("1990-01-01"),
  by="month",
  length.out = 12)

mn.rdys.fh <- data.frame(
  date.mn.rdys_fh,
  mn_rdys.vfh
)


#Second half Rainy Days of the Month#
head(Prcp.mn.rdysh)

Jan1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="01"))/width.rd20

Feb1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="02"))/width.rd20

Mar1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="03"))/width.rd20

Apr1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="04"))/width.rd20

May1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="05"))/width.rd20

June1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="06"))/width.rd20

July1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="07"))/width.rd20

Aug1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="08"))/width.rd20

Sept1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="09"))/width.rd20

Oct1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="10"))/width.rd20

Nov1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="11"))/width.rd20

Dec1sh <- nrow(subset(Prcp.mn.rdysh, format.Date(
  Prcp.mn.rdysh$date, "%m")=="12"))/width.rd20


mn_rdys.vsh <- c(Jan1sh, Feb1sh, Mar1sh, Apr1sh, May1sh, June1sh, July1sh, Aug1sh,
                 Sept1sh, Oct1sh, Nov1sh, Dec1sh) 

date.mn.rdys_sh <- seq(
  as.Date("1990-01-01"),
  by="month",
  length.out = 12)

mn.rdys.sh <- data.frame(
  date.mn.rdys_sh,
  mn_rdys.vsh
)

#  Percentage change
(mean(mn.rdys.sh[,2], na.rm = T) - mean(mn.rdys.fh[,2],na.rm = T))*100/
  mean(mn.rdys.fh[,2], na.rm = T)





##COMPARING MONTHLY RAINY DAYS >= 20mm

head(Prcp.mn.rdys20) 
Prcp.mn.rdysfh20 <- Prcp.mn.rdys20[Prcp.mn.rdys20[,1] >= "1972-01-31" &
                                      Prcp.mn.rdys20[,1] <= "1994-12-31", ] %>% drop_na()

Prcp.mn.rdysh20 <- Prcp.mn.rdys20[Prcp.mn.rdys20[,1] >= "1995-01-31" &
                                     Prcp.mn.rdys20[,1] <= "2017-12-31", ] %>% drop_na()


#First Half (Monthly Prcp days grt 20mm)
head(Prcp.mn.rdysfh20)

#subseting all individual months
width.rd20 <- length(1972:2017)

Jan1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="01"))/width.rd20

Feb1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="02"))/width.rd20

Mar1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="03"))/width.rd20

Apr1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="04"))/width.rd20

May1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="05"))/width.rd20

June1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="06"))/width.rd20

July1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="07"))/width.rd20

Aug1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="08"))/width.rd20

Sept1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="09"))/width.rd20

Oct1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="10"))/width.rd20

Nov1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="11"))/width.rd20

Dec1fh2 <- nrow(subset(Prcp.mn.rdysfh20, format.Date(
  Prcp.mn.rdysfh20$date, "%m")=="12"))/width.rd20


mn_rdys.vfh2 <- c(Jan1fh2, Feb1fh2, Mar1fh2, Apr1fh2, May1fh2, June1fh2, July1fh2, Aug1fh2,
                  Sept1fh2, Oct1fh2, Nov1fh2, Dec1fh2) 

date.mn.rdys_fh <- seq(
  as.Date("1990-01-01"),
  by="month",
  length.out = 12)

mn.rdys.fh20 <- data.frame(
  date.mn.rdys_fh,
  mn_rdys.vfh2
)


#Second half Rainy Days of the Month#
head(Prcp.mn.rdysh20)

Jan1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="01"))/width.rd20

Feb1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="02"))/width.rd20

Mar1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="03"))/width.rd20

Apr1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="04"))/width.rd20

May1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="05"))/width.rd20

June1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="06"))/width.rd20

July1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="07"))/width.rd20

Aug1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="08"))/width.rd20

Sept1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="09"))/width.rd20

Oct1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="10"))/width.rd20

Nov1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="11"))/width.rd20

Dec1sh2 <- nrow(subset(Prcp.mn.rdysh20, format.Date(
  Prcp.mn.rdysh20$date, "%m")=="12"))/width.rd20


mn_rdys.vsh2 <- c(Jan1sh2, Feb1sh2, Mar1sh2, Apr1sh2, May1sh2, June1sh2, July1sh2, Aug1sh2,
                  Sept1sh2, Oct1sh2, Nov1sh2, Dec1sh2) 

date.mn.rdys_sh <- seq(
  as.Date("1990-01-01"),
  by="month",
  length.out = 12)

mn.rdys.sh20 <- data.frame(
  date.mn.rdys_sh,
  mn_rdys.vsh2
)


#  Percentage change
(mean(mn.rdys.sh20[,2], na.rm = T) - mean(mn.rdys.fh20[,2],na.rm = T))*100/
  mean(mn.rdys.fh20[,2], na.rm = T)





#seasonal Cycle for first half
head(Prcp.fh)
#let Prcp,
data <- Prcp.fh
names(data) <- c("dates","Rain")
head(data)

mean_daily_Veafh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,Jan_11, Jan_12,
                     Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,
                     Jan_25,Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                     Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,Feb_50,
                     Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,
                     Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,
                     Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,
                     Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Veafh <-data.frame(
  c(1:366),
  mean_daily_Veafh
)

write.csv(Seasonal_cycle_Veafh, 
          file = "data_for_seasonal_cycle_of_Rainfall of Vea (1972-1994).csv")

sss <- ksmooth(
  c(1:366), 
  mean_daily_Veafh, bandwidth = 15
)

sss.1 <- data.frame(sss) # converting to a dataframe

ggplot(Seasonal_cycle_Veafh, aes(x=c(1:366), y=mean_daily_Veafh)) + 
  geom_line(col="darkblue", lwd=1) + 
  labs(title="Seasonl Cycle of Rainfall (1972-1994)\n Vea", 
       x="Day", y="Rainfall(mm)") + 
  geom_line(data = sss.1, aes(x=x, y=y), col="red", lwd=1)

dev.copy(png, filename="Seasonal Cycle of Vea (1960-1994).png", 
         height=750, width=1000)
dev.off()

sum10 <- summary(Seasonal_cycle_Veafh)
write.csv(sum9,
            file = "rainfall Seasonal cycle ofof VEA (1960-1994).csv")



#second half
head(Prcp.sh)
#let Prcp,
data <- Prcp.sh
names(data) <- c("dates","Rain")
head(data)

mean_daily_Veash<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,Jan_11, 
                     Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,
                     Jan_17,Jan_18,Jan_19,Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,
                     Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,
                     Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,
                     Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,
                     Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,
                     Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,
                     Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,
                     Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,
                     Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Veash <-data.frame(
  c(1:366),
  mean_daily_Veash
)

write.csv(Seasonal_cycle_Veash, 
          file = "data_for_seasonal_cycle_of_Rainfall of Vea (1995-2017).csv")

bb <- ksmooth(
  c(1:366), 
  mean_daily_Veash, bandwidth = 15
)

bb.1 <- data.frame(bb) # converting to a dataframe

ggplot(Seasonal_cycle_Veash, aes(x=c(1:366), y=mean_daily_Veash)) + 
  geom_line(col="darkblue", lwd=1) + 
  labs(title="Seasonal Cycle of Rainfall (1995-2017)\n Vea", 
       x="Day", y="Rainfall(mm)") + 
  geom_line(data = bb.1, aes(x=x, y=y), col="red", lwd=1)

dev.copy(png, filename="Seasonal Rainfall Cycle of Vea (1995-2017).png",
         height=750, width=1000)
dev.off()

sum11 <- summary(Seasonal_cycle_Veash)
write.csv(sum11,
          file = "rainfall Seasonal cycle ofof VEA (1995-2017).csv")



#plot(Seasonal_cycle_Damfh, type="l", lwd=1, 
 #    col="darkblue", main="seasonal Rainfall Cycle of Damongo - 1960-2017",
#     xlab="Days",ylab="Rainfall(mm)")

#lines(sss.1, col="blue", lwd=3)
#lines(Seasonal_cycle_Damsh, col="darkblue")
#lines(bb.1, col="red", lwd=3)


ggplot(Seasonal_cycle_Veafh, aes(x=Seasonal_cycle_Veafh[,1])) + 
  geom_line(aes(y=Seasonal_cycle_Veafh[,2]),lwd = .25) + 
  geom_line(aes(y=sss.1[,2], col="1972-1994"), lwd=1.25) + 
  geom_line(aes(y=Seasonal_cycle_Veash[,2]),lwd=.25) +
  geom_line(aes(y=bb.1[,2],col="1995-2017"),lwd=1.25) +
  scale_colour_manual("",
                      values = c("1972-1994"="darkblue","1995-2017"="red")) +
              xlab("Day") + ylab("Rainfall(mm)") + 
  ggtitle("Seaonal Cycle of Rainfall (1972-2017)\n Vea")

dev.copy(png, filename="Seasonal Cycle of Rainfall of Vea(1972-94-2017).png", 
         width = 1000, height = 750)
dev.off()



#Seasonal Max Tmp on Monthly Basis
head(Prcp)
Ap <- mean(subset(Prcp,format.Date(dates, "%m")=="04")$Prcp, na.rm=T)
Ma <- mean(subset(Prcp,format.Date(dates, "%m")=="05")$Prcp, na.rm=T)
Ju <- mean(subset(Prcp,format.Date(dates, "%m")=="06")$Prcp, na.rm=T)
Jl <- mean(subset(Prcp,format.Date(dates, "%m")=="07")$Prcp, na.rm=T)
Au <- mean(subset(Prcp,format.Date(dates, "%m")=="08")$Prcp, na.rm=T)
Se <- mean(subset(Prcp,format.Date(dates, "%m")=="09")$Prcp, na.rm=T)
Oc <- mean(subset(Prcp,format.Date(dates, "%m")=="10")$Prcp, na.rm=T)

Seas <- data.frame(
  dates=Mn.mn.tmx[4:10, 1],
  Prcp=c(Ap,Ma,Ju,Jl,Au,Se,Oc)
)

require(ggplot2)
ggplot(data=Seas,
       aes(x=dates,y=Prcp)) +
  geom_line(col="darkblue", lwd=2) +
  ggtitle(" Seasonal Rainfall Total (1972-2017)\n Vea") + xlab("Month") +
  ylab("Rainfall(mm)") + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Prcp.png",
         width = 1000, height = 650)
dev.off()




#Indexing first and second halves
#lng.mn.Prcp_fh <- mean(Prcp_fh$Prcp)
#sd.Prcp_fh <- sd(Prcp_fh$Prcp)
#Prcp_fh.anom.v <- (Prcp_fh$Prcp - lng.mn.Prcp_fh)/
#  sd.Prcp_fh

#Prcp_fhtmale.anom <- data.frame(
#  Prcp_fh$date, 
#  Prcp_fh.anom.v
#)

#ggplot(Prcp_fhtmale.anom, aes(Prcp_fh$date, Prcp_fh.anom.v, ymin = 0,
#                              ymax = Prcp_fh.anom.v)) + 
#  geom_linerange(data = Prcp_fhtmale.anom, aes(colour = ifelse(Prcp_fh.anom.v >0,
#      "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + 
#  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Rainfall of Vea \n (1972-1994)", 
#       x="Years", y="Index")

#dev.copy(png, filename="Index of Annual Rainfall of Vea (1972-1994).png",
##         height=750, width=1000)
#dev.off()


#second half
#lng.mn.Prcp_sh <- mean(Prcp_sh$Prcp)
#sd.Prcp_sh <- sd(Prcp_sh$Prcp)
##Prcp_sh.anom.v <- (Prcp_sh$Prcp - lng.mn.Prcp_sh)/
#  sd.Prcp_sh

##Prcp_shtmale.anom <- data.frame(
#  Prcp_sh$date, 
#  Prcp_sh.anom.v#
#)

#ggplot(Prcp_shtmale.anom, aes(Prcp_sh$date, Prcp_sh.anom.v, ymin = 0,
#                              ymax = Prcp_sh.anom.v)) + 
#  geom_linerange(data = Prcp_shtmale.anom, aes(colour = ifelse(Prcp_sh.anom.v >0,
##      "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + 
##  (scale_x_date(date_breaks="4 year",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Rainfall of Vea \n (1995-2017)",
#       x="Years", y="Index")

#dev.copy(png, filename="Index of Annual Rainfall of Vea (1995-2017).png",
#         height=750, width=1000)
#dev.off()


