#############################################################################################

setwd <- ("F:/R_PROJECTS/Babile")
getwd()

dat <- read.table("Babile_rr_temp_1.txt", sep = "\t", header = T,
                  na.strings = -99.9)

head(dat)
any(is.na(dat))
colSums(is.na(dat))

date <- with(
  dat, 
  as.Date(paste(year, month, day),
          paste("%Y %m %d")))


#creating the main dataframe
data1 <- data.frame(
  date, 
  dat$Prcp,
  dat$Tmax,
  dat$Tmin)

names(data1) <- c("date", "Prcp","Tmax", "Tmin")
head(data1)

#alternatively


#...........................................................................................#

library("xts")
library("timeSeries")
require("ggplot2")
library("ggthemes")
library("scales")
library("Kendall")
library("KernSmooth")
library("imputeTS")
library("tidyverse")


#Aggregating daily into and plotting annual rainfall
Prcp <- data.frame(data1[,1], data1[,2])
Prcp <- drop_na(Prcp)
names(Prcp) <- c("date","Prcp")
Prcp.xts <-xts(data1$Prcp,data1$date)
Prcp.ann.xts <- apply.yearly(Prcp.xts, sum, na.rm=T)
head(Prcp.ann.xts)

#conversion to data.frame
Prcp.ann <- data.frame(
  date=index(Prcp.ann.xts), 
  coredata(Prcp.ann.xts))

head(Prcp.ann)
names(Prcp.ann)[2] <- "Prcp"

#Replacing all 0.0s with NA
Prcp.ann$Prcp <- replace(
  Prcp.ann$Prcp, 
  Prcp.ann$Prcp==0.0,NA)

#Time series missing value imputation
#Prcp.ann$Prcp <- na_kalman(
#  Prcp.ann$Prcp, 
#  model = "StructTS", 
#  smooth =T, nit = -1)

#converting date vector to a date class
Prcp.ann$date <- as.Date(
  Prcp.ann$date, 
  format="%Y-%m-%d")

class(Prcp.ann$date)
head(Prcp.ann)
write.csv(Prcp.ann, 
          file="Annual Rainfall Total for Babire(1960-2014).csv")

#plotting Prcp.ann
ggplot(Prcp.ann, aes(x=date, y=Prcp)) + geom_line(col="darkblue", lwd=2) + 
  geom_smooth(method = lm, col="red") + 
  labs(title="Annual Rainfall (1960-2014)\n Babile", 
       x="Year", y="Rainfall(mm)") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") + 
  annotate("text", x=Prcp.ann[Prcp.ann[,1] == "1985-12-31", ]$date, y=1300,
           cex = 7, label = "P-value = 0.16496")
dev.copy(png, filename="Annual Rainfall of BABILE (1960-2014).png",
         width = 1000, height=750)
dev.off()

sum1 <- summary(Prcp.ann)
write.csv(sum1,
          file="Aummary stats for annual Rainfall of Babile(1960-2014).csv")
#or
Prcp.df.ann <- drop_na(Prcp.ann)
sum11 <- data.frame(Prcp.df.ann[Prcp.df.ann$Prcp == max(Prcp.df.ann$Prcp), ],
                    Prcp.df.ann[Prcp.df.ann$Prcp == min(Prcp.df.ann$Prcp), ])

sum11[2,c(1,2)] <- sum11[1, c(3,4)] 
sum11[-c(3,4)] -> sum11
dimnames(sum11) <- list(
  c("Max:","Min:"), 
  c("Year","Rainfall")
)  

write.csv(sum11,
          file = "summary stats for Annual Prcp.1 (Babile).csv")


.................................................................................................#


#Mean Monthly Rainfall for Babire
#Mean Monthly Rainfall
head(Prcp.xts)
Prcp.1 <- drop_na(Prcp)

Prcp.xts <- xts(Prcp.1[,2], Prcp.1[,1])

#aggregating daily rainfall into monthly
head(Prcp.xts)
Prcp.mn.xts <- apply.monthly(Prcp.xts, sum, na.rm=T)
head(Prcp.mn.xts)

#converting to dataframe
Prcp.mn.1 <- data.frame(date=index(Prcp.mn.xts), coredata(Prcp.mn.xts))
names(Prcp.mn.1)[2] <- "Prcp"
Prcp.mn.1$date <- as.Date(Prcp.mn.1$date, format="%Y-%m-%d")
class(Prcp.mn.1$date)
head(Prcp.mn.1)

#EXPORTING TO CSV TO IDENTIFY DATES WITH ABSOLUTE NAs
#write.csv(Prcp.mn, file="Prcp_mn_1.csv")

#reading the reviewed data back into R
#Prcp.mn.1 <- read.table("Prcp_mn_1.txt", header=T, sep = "\t", na.strings = -99.9)
#head(Prcp.mn.1)
#any(is.na(Prcp.mn.1))

#converting date vector of Prcp.mn.1 to a date class
#Prcp.mn.1$date <- as.Date(Prcp.mn.1$date, format="%d-%m-%Y")
#class(Prcp.mn.1$date)
#head(Prcp.mn.1)
#write.csv(Prcp.mn.1, 
#  file = "Estimated missing values monthly Rainfall data for Babila(1960-2014).csv")

#Subsetting and averaging all individual months and Time series missing value imputation for individual months
Jan <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="01")
#Jan$Prcp <- na.kalman(Jan$Prcp, model = "StructTS", smooth = T, nit = -1)
#head(Jan)
Feb <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="02")
#Feb$Prcp <- na.kalman(Feb$Prcp, model = "StructTS", smooth =T, nit = -1)
#..#
March <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="03")
#March$Prcp <- na.kalman(March$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
April <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="04")
#April$Prcp <- na.kalman(April$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
May <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="05")
#May$Prcp <- na.kalman(May$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
June <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="06")
#June$Prcp <- na.kalman(June$Prcp,model = "StructTS", smooth = T, nit = -1)
#..#
July <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="07")
#July$Prcp <- na.kalman(July$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
August <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="08")
#August$Prcp <- na.kalman(August$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
September <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="09")
#September$Prcp <- na.kalman(September$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
October <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="10")
#October$Prcp <- na.kalman(October$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
November <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="11")
#November$Prcp <- na.kalman(November$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
December <- subset(Prcp.mn.1, format.Date(Prcp.mn.1$date, "%m")=="12")
#December$Prcp <- na.kalman(December$Prcp, model = "StructTS", smooth = T, nit = -1)
# na.kalman(subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="64")$Prcp, model = "StructTS", smooth = T, nit = -1)

#mean monthly Rainfall (Babile)
#Mean_monthly_v <- c(Jan,Feb,March,April,
#                           May,June,July,August,
#                           September,October,November,December)

#names(Mean_monthly)[1] <- "date"

#write.csv(Mean_monthly, 
#          file="kalman imputed ts missing values data.csv")
#Averaging
Jan.av <- mean(Jan$Prcp)
Feb.av <- mean(Feb$Prcp)
March.av <- mean(March$Prcp)
April.av <- mean(April$Prcp)
May.av <- mean(May$Prcp)
June.av <- mean(June$Prcp)
July.av <- mean(July$Prcp)
August.av <- mean(August$Prcp)
September.av <- mean(September$Prcp)
October.av <- mean(October$Prcp)
November.av <- mean(November$Prcp)
December.av <- mean(December$Prcp)
#creating a vector with the monthly mean atop
Babile.m.v <-c(Jan.av,Feb.av,March.av,April.av,May.av,June.av,
               July.av,August.av,September.av,October.av,November.av,December.av) 

#creating a date vector for the new dataframe
date.mn.m <- seq(as.Date("1990-01-01"), by="month", length.out = 12)
Babile_mn_m <- data.frame(date.mn.m, Babile.m.v)
write.csv(Babile_mn_m, 
          file = "Mean Monthly Rainfall of Babile (1960-2014).csv")

#plotting(ggplot)
ggplot(Babile_mn_m, aes(x=date.mn.m, y=Babile.m.v)) +
  geom_line(col="darkblue", lwd=2) + 
  labs(title="Mean Monthly Rainfall (1960-2014)\n Babile",
       x="Months", y="Rainfall(mm)") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks="1 month", date_labels = "%b")

dev.copy(png, filename="Mean Monthly Rainfall for Babile (1960-2014)_1.png",
         height=500, width=1000)
dev.off()

sum2 <- summary(Babile_mn_m)
write.csv(sum2,
             file="Summary Stats for mean monthly Rainfall of Babile (1960-2014).csv")
#or
sum22 <- data.frame(Babile_mn_m[Babile_mn_m[,2] == max(Babile_mn_m[,2]), ],
                    Babile_mn_m[Babile_mn_m[,2] == min(Babile_mn_m[,2]), ])

sum22[2,c(1,2)] <- sum22[1, c(3,4)] 
sum22[-c(3,4)] -> sum22
sum22[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum22) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall")
)  


write.csv(sum22,
          file = "summary stats for mean mOnth Prcp.1 (Babile).csv")


#alternatively
#ggsave("Mean Monthly Rainfall for Babile (1960-2014).png", height = 500, width = 1000)


#........................................................................................................................#


#Annual Rainy Days of Babile (days with rain >= 1mm )
#converting Prcp.xts(Daily Rainfall data of Babile (1960-2014)) to a dataframe
head(Prcp.xts)

#converting to dataframe
Prcp.xts.df <- data.frame(
  date=index(Prcp.xts),
  coredata(Prcp.xts)
  )

#or
#Prcp.xts_df <- data.frame(date=index(Prcp.xts), coredata(Prcp.xts))

names(Prcp.xts.df)[2] <- "Prcp"
head(Prcp.xts.df)

#Prcp.xts_df, #......with NAs
#convert date vector of Prcp.xts.df to date class
Prcp.xts.df$date <- as.Date(
  Prcp.xts.df$date, 
  format="%Y-%m-%d"
  )

class(Prcp.xts.df$date)
any(is.na(Prcp.xts.df))
colSums(is.na(Prcp.xts.df))

#head(Prcp.xts.df)
#subseeting individual years from the above data
#b.60 <- subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="60")
#to
#b.14 <- subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="14")

#Annual Rainy Days of 1960 i.e >= 1mm
#Prcp.xts.df <- Prcp.xts.df%>% na.omit(Prcp.xts.df), ......# without NA's
# or
Prcp.xts.df<- Prcp.xts.df %>% drop_na()

Prcp.xts.df <- Prcp.xts.df[Prcp.xts.df$Prcp >= 0.85, ]

#alternatively and a shortcut
b.60.1 <- nrow(subset(
  Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="60")
  [subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="60")$Prcp >= 0.85, ]
  )
#to
b.14.1 <- nrow(subset(
  Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="14")
  [subset(Prcp.xts.df, format.Date(Prcp.xts.df$date, "%y")=="14")$Prcp >= 0.85, ])

# creating a vector for annual rainydays >= 1mm excluding 1964-1968
#ommited the years 64-68
Ann.rndys.v <- c(b.60.1, b.61.1, b.62.1, b.63.1,
                 b.69.1, b.70.1, b.71.1, b.72.1, b.73.1, 
                 b.74.1, b.75.1, b.76.1, b.77.1, b.78.1, b.79.1, b.80.1, b.81.1, b.82.1,
                 b.83.1, b.84.1, b.85.1, b.86.1, b.87.1, b.88.1, b.89.1, b.90.1, b.91.1,
                 b.92.1, b.93.1, b.94.1, b.95.1, b.96.1, b.97.1, b.98.1, b.99.1, b.00.1,
                 b.01.1, b.02.1, b.03.1, b.04.1, b.05.1, b.06.1, b.07.1, b.08.1, b.09.1,
                 b.10.1, b.11.1, b.12.1, b.13.1, b.14.1)

#Excluding the years 1964-1968 using Prcp.ann data to create a dataframe
head(Prcp.ann)
Ann.rdys.BDate <- Prcp.ann[-5:-9, ]
Ann.rdys.BDate <- Ann.rdys.BDate$date

Ann.rdys.Babile <- data.frame(
  Ann.rdys.BDate, 
  Ann.rndys.v)

write.csv(Ann.rdys.Babile, 
          file="Annual Rainy Days of Babire (1960-2014).csv")

require(Kendall)
MK <- MannKendall(Ann.rdys.Babile[,2])
summary(MK)

ggplot(Ann.rdys.Babile, aes(x=Ann.rdys.BDate, y=Ann.rndys.v)) + 
  geom_line(col="darkblue", lwd=2) + geom_smooth(method = lm, col="red") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  ggtitle("Number of Days when Rainfall >= 0.85mm (1960-2014)\n Babile") + 
  xlab("Year") +ylab("Day") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  annotate("text", 
           x=Ann.rdys.Babile[Ann.rdys.Babile[,1] == "1969-12-31", ]$Ann.rdys.BDate,
           y=48, cex=7, label = "P-value = 0.80154")

dev.copy(png, filename="Annual Rainy Days of Babile (1960-2014)_1.png", 
         height=500, width=1000)
dev.off()

sum3 <- summary(Ann.rdys.Babile)
write.csv(sum3,
          file = "AAnnuAl Rainy Days of Babile (1960-2014).csv")
#or
sum33 <- data.frame(Ann.rdys.Babile[Ann.rdys.Babile[,2] == max(Ann.rdys.Babile[,2]), ],
                    Ann.rdys.Babile[Ann.rdys.Babile[,2] == min(Ann.rdys.Babile[,2]), ])

sum33[2,c(1,2)] <- sum33[1, c(3,4)] 
sum33[-c(3,4)] -> sum33
#sum33[,1] <- data.frame(month.name[c(09,01)])
dimnames(sum33) <- list(
  c("Max:","Min:"), 
  c("Year","Rainfall")
)  


write.csv(sum33,
          file = "summary stats for ann rainy days (Babile).csv")

#.......................................................................................#


                                     

#.....................................................................................#


#Annual Rainy Days  of Babile > 20mm
head(Prcp.xts.df)
Prcp.ann.rdys20 <- Prcp.xts.df[Prcp.xts.df$Prcp >= 20, ]
#subsetting individual years from the Prcp.ann.rdys20 data
b.60.2 <- nrow(subset(Prcp.ann.rdys20, format.Date(Prcp.ann.rdys20$date, "%y")=="60"))
#to
b.14.2 <- nrow(subset(Prcp.ann.rdys20, format.Date(Prcp.ann.rdys20$date, "%y")=="14"))  

#creating a vector with the above values form b.60.2 to b.14.2
Ann.rndys20.v <- c(b.60.2,b.61.2, b.62.2, b.63.2, b.69.2, b.70.2, b.71.2, b.72.2, 
                   b.73.2, b.74.2, b.75.2, b.76.2, b.77.2, b.78.2, b.79.2, b.80.2,
                   b.81.2, b.82.2, b.83.2, b.84.2, b.85.2, b.86.2, b.87.2, b.88.2, 
                   b.89.2, b.90.2, b.91.2, b.92.2, b.93.2, b.94.2, b.95.2, b.96.2, 
                   b.97.2, b.98.2, b.99.2, b.00.2, b.01.2, b.02.2, b.03.2, b.04.2, 
                   b.05.2, b.06.2, b.07.2, b.08.2, b.09.2, b.10.2, b.11.2, b.12.2, 
                   b.13.2, b.14.2)

Ann.rdys.BDate
Ann.rdys20.Babile <- data.frame(
  Ann.rdys.BDate, 
  Ann.rndys20.v)

write.csv(Ann.rdys20.Babile,
          file="Annual Rainy Days of Babire grt than 20mm (1960-2014).csv")

ggplot(Ann.rdys20.Babile, aes(x=Ann.rdys.BDate, y=Ann.rndys20.v)) + 
  geom_line(col="darkblue", lwd=2) + geom_smooth(method = lm, col="red") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  ggtitle("Number of Days when Rainfall >= 20mm (1960-2014)\n Babile") +
  xlab("Year") +ylab("Days") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  annotate("text", 
           x=Ann.rdys20.Babile[Ann.rdys20.Babile[,1] == "1982-12-31", ]$Ann.rdys.BDate,
           y=25, cex=7, label = "P-value = 0.26694")

dev.copy(png, filename="Annual Rainy Days of Babile grt 20 mm (1960-2014)_1.png",
         height=500, width=1000)
dev.off()

MK <- MannKendall(Ann.rdys20.Babile$Ann.rndys20.v)
summary(MK)

sum4 <- summary(Ann.rdys20.Babile)
write.csv(sum4,
          file="Annual Rainy DAYS when Rainfall grt 20mm (1960-2014).csv")
#or
sum44 <- Ann.rdys20.Babile[Ann.rdys20.Babile[,2] == max(Ann.rdys20.Babile[,2]), ]
sum44[c(2,3),c(1,2)] <- Ann.rdys20.Babile[Ann.rdys20.Babile[,2] == min(Ann.rdys20.Babile[,2]), ]

dimnames(sum44) <- list(
  c("Max:","Min:","//"), 
  c("Year","Rainfall(mm)")
)  

write.csv(sum44,
          file = "summary stats for ann rainy days grt 20 (Babile).csv")



#Monthly Rainy Days
head(Prcp.xts.df)
Prcp.mn.df <- Prcp.xts.df[Prcp.xts.df[,2] >= 0.85, ]
Prcp.mn.df <- drop_na(Prcp.mn.df)

#subseting all individual months
width.rd20 <- length(1960:2014)

Jan1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="01"))/width.rd20

Feb1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="02"))/width.rd20

Mar1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="03"))/width.rd20

Apr1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="04"))/width.rd20

May1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="05"))/width.rd20

June1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="06"))/width.rd20

July1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="07"))/width.rd20

Aug1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="08"))/width.rd20

Sept1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="09"))/width.rd20

Oct1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="10"))/width.rd20

Nov1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="11"))/width.rd20

Dec1 <- nrow(subset(Prcp.mn.df, format.Date(
  Prcp.mn.df$date, "%m")=="12"))/width.rd20


mn_rdys.v <- c(Jan1, Feb1, Mar1, Apr1, May1, June1, July1, Aug1,
               Sept1, Oct1, Nov1, Dec1) 

date.mn.rdys20 <- seq(
  as.Date("1990-01-01"),
  by="month",
  length.out = 12)

mn.rdys <- data.frame(
  date.mn.rdys20,
  mn_rdys.v
)

ggplot(mn.rdys, aes(x=mn.rdys[,1], y=mn.rdys[,2])) + geom_line(col="darkblue",
                                                               lwd=2) + 
  labs(title = "Mean Monthly Rainfall (1960-2014)\n Babile", x="Month",
       y="Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Monthly Rainy Days of Babile (1960-2014).png",
         width =1000, height=750)
dev.off()


sum5 <- summary(mn.rdys)
write.csv(sum5 ,
          file="summary stats for mean monthly Rainfall of Babile (1960-2014).csv")
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
          file = "summary stats for monthly rainy days (Babile).csv")



#Monthly Rainy Days grt 20mm
head(Prcp.xts.df)
Prcp.mn.df_rdys20 <- Prcp.xts.df[Prcp.xts.df[,2] >= 20, ]
#subsetting all individual months
width.rd20 <- length(1960:2014)

Jan.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="01"))/width.rd20

Feb.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="02"))/width.rd20

Mar.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="03"))/width.rd20

Apr.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="04"))/width.rd20

May.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="05"))/width.rd20

June.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="06"))/width.rd20

July.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="07"))/width.rd20

Aug.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="08"))/width.rd20

Sept.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="09"))/width.rd20

Oct.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="10"))/width.rd20

Nov.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="11"))/width.rd20

Dec.20 <- nrow(subset(Prcp.mn.df_rdys20, format.Date(
  Prcp.mn.df_rdys20$date, "%m")=="12"))/width.rd20

#creating a vector with the above
Prcp.mn.rdys20 <- c(Jan.20,Feb.20,Mar.20,Apr.20,May.20,June.20,July.20,
                    Aug.20,Sept.20,Oct.20,Nov.20,Dec.20)


monthly.rdys20 <- data.frame(
  date.mn.rdys20,
  Prcp.mn.rdys20)

write.csv(monthly.rdys20, 
          file="Monthly Rainy Days of Babile grt 20mm.csv")


ggplot(monthly.rdys20,
       aes(x=date.mn.rdys20,y=monthly.rdys20[,2])) +
  geom_line(col="darkblue", lwd = 2) + 
  ggtitle("Number of Days when Rainfall >= 20mm(1960-2014)\n Babile") +
  xlab("Month") + ylab("Day") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Monthly RAINY dAYS GRT 20mm of Babile.png",
         width = 1000, height=750)
dev.off()
  
sum6 <- summary(monthly.rdys20)
write.csv(sum6,
          file="summsry stats for monthly Rainy Days grt 20mm of Babiel.csv")
#or
sum66 <- monthly.rdys20[monthly.rdys20[,2] == max(monthly.rdys20[,2]), ]
sum66[c(2,3),c(1,2)] <- monthly.rdys20[monthly.rdys20[,2] == min(monthly.rdys20[,2]), ]

dimnames(sum66) <- list(
  c("Max:","Min:","//"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum66,
          file = "summary stats for monthly rainy days grt 20mm (Babile).csv")


#............................................................................................#


#Indexing Annual Rainfall for Babile
head(Prcp.ann)
Prcp.ann.1 <- drop_na(Prcp.ann)

Prcp.lnmn.B <- mean(Prcp.ann.1$Prcp)
Prcp.sd.B <- sd(Prcp.ann.1$Prcp)

anom.prcp <- (Prcp.ann.1[,2] - Prcp.lnmn.B)/
  Prcp.sd.B

#creating a dataframe with the above values n dates
#Anom.PrcpB <- data.frame(seq(
#  as.Date("1960-01-01"),
#  by="year",
#  length.out =55)
#)


Prcp.ann.anomB <- data.frame(
  Ann.rdys.BDate,
  anom.prcp
)

write.csv(Prcp.ann.anomB, 
          file = "Index of Annual Rainfall of Babile(1960-2014).csv")

ggplot(Prcp.ann.anomB, aes(Ann.rdys.BDate, anom.prcp, ymin = 0,
                           ymax = anom.prcp)) + 
  geom_linerange(data = Prcp.ann.anomB, aes(colour = ifelse(Prcp.ann.anomB[,2] >0,
        "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = F)) + 
  (scale_x_date(date_breaks="4 years",date_labels="%Y")) + 
  geom_hline(yintercept=0) + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  labs(title="Annual Rainfall Total Anomaly (1960-2014)\n Babile", 
       x="Year", y="Anomaly")

dev.copy(png, filename="Index of Annual Rainfall of Babile(1960-2014).png",
         width=1000, height=750)
dev.off()

sum7 <- summary(Prcp.ann.anomB)
write.csv(sum7,
          file="summsry stats for Anomalies of rainfall total Babiel.csv")
#or
sum77 <- data.frame(Prcp.ann.anomB[Prcp.ann.anomB[,2] == max(Prcp.ann.anomB[,2]), ],
                    Prcp.ann.anomB[Prcp.ann.anomB[,2] == min(Prcp.ann.anomB[,2]), ])

sum77[2,c(1,2)] <- sum77[1, c(3,4)] 
sum77[-c(3,4)] -> sum77
#sum77[,1] <- data.frame(month.name[c(09,01)])
dimnames(sum77) <- list(
  c("Max:","Min:"), 
  c("Year","Rainfall(mm)")
)  

write.csv(sum77,
          file = "summary stats for annual Prcp Anomaly (Babile).csv")

#.......................................................................................................#



#Annual Rainfall for the two halves of the Babile data(Prcp.ann)
head(Prcp.ann)
#length(1960:1987)
#[1] 28
#length(1988:2014)
#[1] 27

first.half <- Prcp.ann[1:28, ]
#or
first.half <- Prcp.ann[(Prcp.ann[,1] >= "1960-12-31" & 
                          Prcp.ann[,1] <= "1987-12-31"), ]

write.csv(first.half, 
          file="Annual Rainfall(Babile 1960-1987 original).csv")

second.half <- Prcp.ann[29:55, ]
#or
second.half <- Prcp.ann[(Prcp.ann[,1] >= "1988-12-31" &
                          Prcp.ann[,1] <= "2016-12-31"), ]
#.................................................................................#

#firsthalf
head(first.half)
MK <- MannKendall(first.half$Prcp)
summary(MK)
write.csv(first.half,
          "Annual Rainfall of Babile (1960-1987).csv")

ggplot(first.half, aes(x=date, y=Prcp)) +
  geom_line(col="darkblue", lwd=2) + 
  labs(title="Rainfall Total (1960-1987)\n Babile", 
       x="Year", y="Rainfall(mm)") + 
  scale_x_date(date_breaks = "4 year", date_labels = "%Y") + 
  geom_smooth(method = lm, col="red") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  annotate("text", x=first.half[first.half[,1] == "1969-12-31", ]$date,
           y=1350, cex=5, label="P-value =0.31557")

dev.copy(png, filename="Annual Rainfall of Babile (1960-2014).png",
         width = 1000, height = 750)
dev.off()

sum7 <- summary(first.half)
write.csv(sum7,
          file="Rainfall Total of Babile (1960-1987).csv")



#secondhalf (including a dummy row to make equal to first.half)
second.half <- rbind(
  second.half, 
  list(as.Date("2015-12-31", format="%Y-%m-%d"), NA))

tail(second.half)

write.csv(second.half, 
          file = "Annual Rainfall of Babile(1988-2014) original.csv")

#let second half.1 be = 
#plotting both halves on the same plot

#first.half.1 <- data.frame(
#  c(1:28), 
#  first.half$Prcp)

#names(first.half.1) <- c("date", "Prcp")

#write.csv(first.half.1, 
#          file="Annual Rainfall of Babile (1960-2014).csv")

#second.half.1 <- data.frame(
#  c(1:28), 
#  second.half$Prcp)

#names(second.half.1) <- c("date", "Prcp")

#seond.half.1 = data.frame(
#  first.half$date, 
#  second.half$Prcp)

#write.csv(second.half.1,
#          file = "Annual Rainfall of Babile (1988-2014).csv")

#ggplot(second.half.1, aes(x=date, y=Prcp)) + geom_line(col="darkblue", lwd=2) + 
#  labs(title="Annual Rainfall of Babile (1988-2014)", 
#       x="Years", y="Rainfall9mm)") + 
# scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
#  geom_smooth(method = lm, col="red")


#Plotting
#ggplot(first.half.1, aes(x=date, y=Prcp)) +
#  geom_line(col="darkblue", lwd=2) + 
#  geom_line(data=second.half.1, aes(x=date, y=Prcp), 
#            col="red", lwd=2) + 
#  labs(title="Annual Rainfall of Babile (1960-1987 and 1988-2014)", 
#       x="length", y="Rainfall(mm)") + 
#  geom_smooth(method = lm, col="black")


#.................................................................................................#



#Mean Monthly Rainfall for the two halves
head(Prcp.mn.1)
fh.Prcp.mn.1 <- Prcp.mn.1[(Prcp.mn.1[,1] >= "1960-01-31" & 
                            Prcp.mn.1[,1] <= "1987-12-31"), ]

sh.Prcp.mn.1 <- Prcp.mn.1[(Prcp.mn.1[,1] >= "1988-01-31" & 
                            Prcp.mn.1[,1] <= "2014-12-31"), ]


#subsetting individual months out of the fh.Prcp.mn.1 data above
Jan.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="01")
#Jan.mn.1$Prcp <- na.kalman(Jan.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
head(Jan.mn.1)
Feb.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="02")
#Feb.mn.1$Prcp <- na.kalman(Feb.mn.1$Prcp, model = "StructTS", smooth =T, nit = -1)
#..#
March.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="03")
#March.mn.1$Prcp <- na.kalman(March.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
April.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="04")
#April.mn.1$Prcp <- na.kalman(April.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
May.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="05")
#May.mn.1$Prcp <- na.kalman(May.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
June.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="06")
#June.mn.1$Prcp <- na.kalman(June.mn.1$Prcp,model = "StructTS", smooth = T, nit = -1)
#..#
July.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="07")
#July.mn.1$Prcp <- na.kalman(July.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
August.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="08")
#August.mn.1$Prcp <- na.kalman(August.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
September.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="09")
#September.mn.1$Prcp <- na.kalman(September.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
October.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="10")
#October.mn.1$Prcp <- na.kalman(October.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
November.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="11")
#November.mn.1$Prcp <- na.kalman(November.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
December.mn.1 <- subset(fh.Prcp.mn.1, format.Date(fh.Prcp.mn.1$date, "%m")=="12")
#December.mn.1$Prcp <- na.kalman(December.mn.1$Prcp, model = "StructTS", smooth = T, nit = -1)

#mean of the above's Prcp or second column
Jan.avfh <- mean(Jan.mn.1$Prcp)
Feb.avfh <- mean(Feb.mn.1$Prcp)
March.avfh <- mean(March.mn.1$Prcp)
April.avfh <- mean(April.mn.1$Prcp)
May.avfh <- mean(May.mn.1$Prcp)
June.avfh <- mean(June.mn.1$Prcp)
July.avfh <- mean(July.mn.1$Prcp)
August.avfh <- mean(August.mn.1$Prcp)
September.avfh <- mean(September.mn.1$Prcp)
October.avfh <- mean(October.mn.1$Prcp)
November.avfh <- mean(November.mn.1$Prcp)
December.avfh <- mean(December.mn.1$Prcp)

#creating a vector with above objects
fh_mn.1.v <- c(Jan.avfh, Feb.avfh,March.avfh,April.avfh,May.avfh,June.avfh,
               July.avfh,August.avfh,September.avfh,October.avfh,
               November.avfh,December.avfh)

date.mn.m
fh.mn.1 <- data.frame(date.mn.m, fh_mn.1.v)
write.csv(fh.mn.1, 
          file="mean monthly rainfall for Babile(1960-1987).csv")

ggplot(fh.mn.1, aes(x=date.mn.m, y=fh_mn.1.v)) + 
  geom_line(col="darkblue", lwd=2) + 
  labs(title="Mean Monthly Rainfall (1960-1987)\n Babile", 
       x="Month", y="Rainfall(mm)") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
dev.copy(png, filename="Mean Monthly Rainfall of Babile(1960-1987).png",
         width = 1000, height = 750)
dev.off()

sum8 <- summary(fh.mn.1)
write.csv(sum8, 
          file = "Mean Monthly Rainfall of BABILE (1960-1987).CSV")
#or
sum88 <- data.frame(fh.mn.1[fh.mn.1[,2] == max(fh.mn.1[,2]), ],
                    fh.mn.1[fh.mn.1[,2] == min(fh.mn.1[,2]), ])

sum88[2,c(1,2)] <- sum88[1, c(3,4)] 
sum88[-c(3,4)] -> sum88
sum88[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum88) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum88,
          file = "summary stats for monthly rainfall (1960-1987).csv")



#Mean monthly Rainfall for second half
head(sh.Prcp.mn.1)
Jan.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="01")
#Jan.mn.1.1$Prcp <- na.kalman(Jan.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#.#
Feb.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="02")
#Feb.mn.1.1$Prcp <- na.kalman(Feb.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#
March.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="03")
#March.mn.1.1$Prcp <- na.kalman(March.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
April.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="04")
#April.mn.1.1$Prcp <- na.kalman(April.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
May.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="05")
#May.mn.1.1$Prcp <- na.kalman(May.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
June.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="06")
#June.mn.1.1$Prcp <- na.kalman(June.mn.1.1$Prcp,model = "StructTS", smooth = T, nit = -1)
#..#
July.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="07")
#July.mn.1.1$Prcp <- na.kalman(July.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
August.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="08")
#August.mn.1.1$Prcp <- na.kalman(August.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
September.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="09")
#September.mn.1.1$Prcp <- na.kalman(September.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
October.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="10")
#October.mn.1.1$Prcp <- na.kalman(October.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
November.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="11")
#November.mn.1.1$Prcp <- na.kalman(November.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)
#..#
December.mn.1.1 <- subset(sh.Prcp.mn.1, format.Date(sh.Prcp.mn.1$date, "%m")=="12")
#December.mn.1.1$Prcp <- na.kalman(December.mn.1.1$Prcp, model = "StructTS", smooth = T, nit = -1)

#mean
Jan.avsh <- mean(Jan.mn.1.1$Prcp)
Feb.avsh <- mean(Feb.mn.1.1$Prcp)
March.avsh <- mean(March.mn.1.1$Prcp)
April.avsh <- mean(April.mn.1.1$Prcp)
May.avsh <- mean(May.mn.1.1$Prcp)
June.avsh <- mean(June.mn.1.1$Prcp)
July.avsh <- mean(July.mn.1.1$Prcp)
August.avsh <- mean(August.mn.1.1$Prcp)
September.avsh <- mean(September.mn.1.1$Prcp)
October.avsh <- mean(October.mn.1.1$Prcp)
November.avsh <- mean(November.mn.1.1$Prcp)
December.avsh <- mean(December.mn.1.1$Prcp)

#
#creating a vector with above objects
sh_mn.1.v <- c(Jan.avsh, Feb.avsh,March.avsh,April.avsh,May.avsh,June.avsh,
               July.avsh,August.avsh,September.avsh,October.avsh,
               November.avsh,December.avsh)

date.mn.m
sh.mn.1 <- data.frame(
  date.mn.m,
  sh_mn.1.v)

write.csv(sh.mn.1, 
          file="mean monthly rainfall for Babile(1988-2014).csv")

sum9 <- summary(sh.mn.1)
write.csv(sum9,
          file = "mean monthly Rainfall of Babile (1988-2014).csv")
#or
sum99 <- data.frame(sh.mn.1[sh.mn.1[,2] == max(sh.mn.1[,2]), ],
                    sh.mn.1[sh.mn.1[,2] == min(sh.mn.1[,2]), ])

sum99[2,c(1,2)] <- sum99[1, c(3,4)] 
sum99[-c(3,4)] -> sum99
sum99[,1] <- data.frame(month.name[c(08,01)])
dimnames(sum99) <- list(
  c("Max:","Min:"), 
  c("Month","Rainfall(mm)")
)  

write.csv(sum99,
          file = "summary stats for monthly rainfall (1988-2014).csv")



#plotiing both sets on the same plot

ggplot(data=fh.mn.1, mapping=aes(x=fh.mn.1[,1])) + 
  geom_line(aes(y=fh.mn.1[,2], colour = "1960-1987"), lwd=1.5) + 
  geom_line(aes(y=sh.mn.1[,2], colour = "1988-2014"), lwd=1.5) +
  labs(title="Mean Monthly Rainfall (1960-2014)\n Babile",
       x="Month", y="Rainfall(mm)") + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_colour_manual("",
                      values = c("1960-1987"="darkblue", "1988-2014"="red")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") 

dev.copy(png, filename="Mean Monthly Rainfall of Babile (1960-87-2016).png",
         width = 1000, height = 750)
dev.off()





##Percentage Change between both climatologies(1960-88, 1989-2016)
#Function
Pec  <- function(x,y){
  (mean(second.half[,2], na.rm = T)-mean(first.half[,2], na.rm = T))*100/
    mean(first.half[,2], na.rm = T)
}

# or
Pec <- (mean(second.half[,2], na.rm = T)-mean(first.half[,2], na.rm = T))*100/
  mean(first.half[,2], na.rm = T)



#Comparing Rainy Days of 1mm
head(Ann.rndys)
Ann.rndys_fh <- Ann.rdys.Babile[Ann.rdys.Babile[,1] >= "1960-12-31" & 
                                  Ann.rdys.Babile[,1] <= "1987-12-31", ]

Ann.rndys_sh <- Ann.rdys.Babile[Ann.rdys.Babile[,1] >= "1988-12-31" & 
                                  Ann.rdys.Babile[,1] <= "2014-12-31", ]

#introducing another 4 rows to make Ann.rndys[,1] == Ann.rndys[,2], so we can plot both
Ann.rndys_fh[24:27,2] <- NA

ggplot(data = data.frame(c(1:27), Ann.rndys_fh[,2]),aes(x=c(1:27))) +
  geom_line(aes(y=Ann.rndys_fh[,2], colour = "1960-1987"), lwd=1.5) +
  geom_line(aes(y=Ann.rndys_sh[,2], colour = "1988-2014"), lwd=1.5) +
  scale_color_manual("",values = c("1960-1987"="darkblue",
                                   "1988-2014"="red")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  labs(title = "Comparing Number of Days when Rainfall >= 0.87 (1960-2014)\nBabile",
       x="X", y="Day")

dev.copy(png,filename="Comparing Days with Rainfall grt 0.87.png",
         width = 1000, height = 750)
dev.off


#Percentage Change(Annual Riany dAYS)
(mean(Ann.rndys_sh[,2], na.rm = T) - mean(Ann.rndys_fh[,2], na.rm = T))*100/
  mean(Ann.rndys_fh[,2], na.rm = T)



#COMPARING ANNUAL RAINY DAYS OF 20mm
Ann.rndys20_fh <- Ann.rdys20.Babile[Ann.rdys20.Babile[,1] >= "1960-12-31" &
                                      Ann.rdys20.Babile[,1] <= "1987-12-31", ]

Ann.rndys20_sh <- Ann.rdys20.Babile[Ann.rdys20.Babile[,1] >= "1988-12-31" &
                                        Ann.rdys20.Babile [,1] <= "2014-12-31", ]

#introducing another row to make Ann.rndys20.Zua[,2] == Ann.rndys20.Zua[,2], 
#so we can plot both
Ann.rndys20_fh[24:27,2] <- NA   

ggplot(data = data.frame(c(1:27), Ann.rndys20_fh[,2]),aes(x=c(1:27))) +
  geom_line(aes(y=Ann.rndys20_fh[,2], colour = "1960-1987"), lwd=1.5) +
  geom_line(aes(y=Ann.rndys20_sh[,2], colour = "1988-2014"), lwd=1.5) +
  scale_color_manual("",values = c("1960-1987"="darkblue",
                                   "1988-2014"="red")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) + 
  labs(title = "Comparing Number of Days when Rainfall >= 20mm (1960-2016)\nBabile",
       x="X", y="Day")

dev.copy(png,filename="Comparing Days with Rainfall grt 20mm Zuarungu.png",
         width = 1000, height = 750)
dev.off()


#Percentage Change
(mean(Ann.rndys20_sh[,2], na.rm = T) - mean(Ann.rndys20_fh[,2], na.rm = T))*100/
  mean(Ann.rndys20_fh[,2], na.rm = T)  


#COMPARING MONTHLY RAINY DAYS
head(Prcp.mn.rdys) 
Prcp.mn.rdysfh <- Prcp.mn.df[Prcp.mn.df[,1] >= "1960-01-01" &
                                 Prcp.mn.df[,1] <= "1987-12-31", ] 

Prcp.mn.rdysh <- Prcp.mn.df[Prcp.mn.df[,1] >= "1988-01-01" &
                                Prcp.mn.df[,1] <= "2014-12-31", ] 


#First Half (Monthly Prcp days)
head(Prcp.mn.rdysfh)

#subseting all individual months
width.rd20 <- length(1960:2014)

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

head(Prcp.ann.rdys20) 
Prcp.mn.rdysfh20 <- Prcp.ann.rdys20[Prcp.ann.rdys20[,1] >= "1960-12-31" &
                                      Prcp.ann.rdys20[,1] <= "1987-12-31", ] 

Prcp.mn.rdysh20 <- Prcp.ann.rdys20[Prcp.ann.rdys20[,1] >= "1988-12-31" &
                                       Prcp.ann.rdys20[,1] <= "2014-12-31", ] 


#First Half (Monthly Prcp days grt 20mm)
head(Prcp.mn.rdysfh20)

#subseting all individual months
width.rd20 <- length(1960:2014)

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




#Seasonal Max Tmp on Monthly Basis
head(Prcp)
Ap <- mean(subset(Prcp,format.Date(date, "%m")=="04")$Prcp)
Ma <- mean(subset(Prcp,format.Date(date, "%m")=="05")$Prcp, na.rm=T)
Ju <- mean(subset(Prcp,format.Date(date, "%m")=="06")$Prcp, na.rm=T)
Jl <- mean(subset(Prcp,format.Date(date, "%m")=="07")$Prcp, na.rm=T)
Au <- mean(subset(Prcp,format.Date(date, "%m")=="08")$Prcp, na.rm=T)
Se <- mean(subset(Prcp,format.Date(date, "%m")=="09")$Prcp, na.rm=T)
Oc <- mean(subset(Prcp,format.Date(date, "%m")=="10")$Prcp, na.rm=T)

Seas <- data.frame(
  dates=Mn.mn.tmx[4:10, 1],
  Prcp=c(Ap,Ma,Ju,Jl,Au,Se,Oc)
)

ggplot(data=Seas,
       aes(x=dates,y=Prcp)) +
  geom_line(col="darkblue", lwd=2) +
  ggtitle(" Seasonal Rainfall Total (1960-2014)\n Babile") + xlab("Month") +
  ylab("Rainfall(mm)") + 
  theme(plot.title = element_text(size=18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename="Seasonal Cycle of Mothly Prcp.png",
         width = 1000, height = 650
)
dev.off()




#plot(fh.mn.1, type="l", col="darkblue", 
#     main="Mean Monthly Rainfall of Babile (1960-1987, 1988 - 2014",
#     xlab="Months",ylab="Rainfall(mm)", lwd=4)
#lines(sh.mn.1, type="l", col="red", lwd=4)
#legend("topright",legend = c("1960-1987","1988-2014"), 
#      col = c("darkblue","red"), bty = n,lty = 1,lwd=4)





#Indexing first and second halves
#head(first.half)
#first.half.1 <- drop_na(first.half)
#lng.mn.Prcp_fh <- mean(first.half.1$Prcp)
#sd.Prcp_fh <- sd(first.half.1$Prcp)
#Prcp_fh.anom.v <- (first.half.1$Prcp - lng.mn.Prcp_fh)/
#  sd.Prcp_fh

#Prcp_fhtmale.anom <- data.frame(
#  first.half.1$date, 
#  Prcp_fh.anom.v
#)

#ggplot(Prcp_fhtmale.anom, aes(first.half.1$date, Prcp_fh.anom.v, ymin = 0,
#                              ymax = Prcp_fh.anom.v)) + 
# geom_linerange(data = Prcp_fhtmale.anom, aes(colour = ifelse(Prcp_fh.anom.v >0,
#        "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + 
#  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Rainfall of Babile \n (1960-1987)", 
#       x="Years", y="Index")

#dev.copy(png, filename="Index of Annual Rainfall of Babile (1960-1987).png",
#         height=750, width=1000)
#dev.off()


#second half
#head(second.half)
#second.half.1 <- drop_na(second.half)
#lng.mn.Prcp_fh <- mean(second.half.1$Prcp)
#sd.Prcp_fh <- sd(second.half.1$Prcp)
#Prcp_fh.anom.v <- (second.half.1$Prcp - lng.mn.Prcp_fh)/
#  sd.Prcp_fh

#Prcp_fhtmale.anom <- data.frame(
#  second.half.1$date, 
#  Prcp_fh.anom.v
#)

#ggplot(Prcp_fhtmale.anom, aes(second.half.1$date, Prcp_fh.anom.v, ymin = 0,
#                              ymax = Prcp_fh.anom.v)) + 
#  geom_linerange(data = Prcp_fhtmale.anom, aes(colour = ifelse(Prcp_fh.anom.v >0,
#   "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + 
#  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Rainfall of Babile \n (1988-2016)", 
#       x="Years", y="Index")#

#dev.copy(png, filename="Index of Annual Rainfall of Babile (1988-2016).png",
#         height=750, width=1000)
#dev.off()


