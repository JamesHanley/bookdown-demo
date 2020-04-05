path="https://docs.google.com/spreadsheets/d/1nfQuBoAPoPPuXCthfze5XU3dZWw0ugdTrm-0yXL3JXU/edit#gid=0"

path='https://docs.google.com/spreadsheets/d/1nfQuBoAPoPPuXCthfze5XU3dZWw0ugdTrm-0yXL3JXU/edit#gid=0'
library(readxl)

library(xlsx)


library(googlesheets)

gs_ls()

?read_xlsx
ds=read_xlsx(path)


ds=read.xlsx("https://docs.google.com/spreadsheets/d/1lAhFCtGGWJUoFXkXwLe7zyiXM8LRhM-TfgukYVtykuw/export?format=xlsx")


ds=read.csv("pFile.csv",as.is=TRUE)
str(ds)

ds$day = (ds$Month-4)*30 + (ds$Day-1) + (1/2)*(ds$AMorPM=="PM") +ds$Hour/24

hist(ds$day)   

ds$CDF =  ( (ds$page.no   - 1) * 200  + 
            (ds$column.no - 1) * 100  +
               ds$row.no ) / 241929
head(ds) ; tail(ds)

plot(ds$day,ds$CDF,type="l")
abline(h=1/2)

L = length(ds$CDF)

ds$p = ds$CDF - c(0, ds$CDF[1:(L-1)])

plot(ds$day, ds$p, type="h",lwd=3)
abline(v=30.1,col="blue",lwd=0.2)

mean = sum(ds$p*ds$day)
sd  = sqrt( sum( ds$p * (ds$day-mean)^2 ) )

plot(ds$ds$d)

