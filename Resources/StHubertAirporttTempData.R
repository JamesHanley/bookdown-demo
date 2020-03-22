setwd("/Users/jameshanley/git_repos/statbook/Resources") 

ds=read.csv("en_climate_monthly_QC_7027320_1928-2015_P1M.csv",
             as.is=TRUE)
str(ds) 
summary(ds)

ds=ds[ds$Month==12 & !is.na(ds$Mean.Temp...C.),c("Year",
                      "Mean.Max.Temp...C.",
                     "Mean.Min.Temp...C.",
                     "Mean.Temp...C.") ]
dim(ds)
summary(ds)
plot(ds$Year,ds$Mean.Temp...C., ylim=c(-25,5),type="l")
lines(ds$Year,ds$Mean.Min.Temp...C.,pch=19)
lines(ds$Year,ds$Mean.Max.Temp...C.,pch=19)
abline(h = seq(-25,5,5),lwd=0.5,col="blue")

ds$Year.c = ds$Year-mean(ds$Year)

sd(ds$Mean.Temp...C.)
sd(ds$Mean.Temp...C.)

cor(ds[,2:3])
( V = cov(ds[,2:4]) )
V[1,1]/4 + V[2,2]/4
sd(ds$Mean.Temp...C.)
mean(ds$Mean.Temp...C.)
summary( lm(ds$Mean.Temp...C. ~ ds$Year.c))


                     
              
   

