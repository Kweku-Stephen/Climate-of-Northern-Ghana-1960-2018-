
setwd("C:/Users/Kofi/Desktop/Amazon Grace/GAEC/Abstract/ICCS5/Data_analysis/Data")
df<-read.table("Abetifi_tmax.txt",header=T, na.string=-99.9)
library(dplyr)

k<-df %>%
  group_by(year, month) %>%
  summarise(Rain = mean(prcp))

write.table(k, "abt_annual_mean", col.names=F, row.names=T,quote=T,sep="\t")





