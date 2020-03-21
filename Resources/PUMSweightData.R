if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

setwd("/Users/jameshanley/git_repos/statbook/Resources")

library(ipumsr)
ddi <- read_ipums_ddi("nhis_00009.xml")
ds <- read_ipums_micro(ddi)
str(ds)
table(ds$WEIGHT)
table(ds$HEIGHT)
table(ds$SEX)
ds=ds[ds$AGE    >= 18 & 
      ds$WEIGHT >  100 &
      ds$WEIGHT <  300  &
      ds$HEIGHT >=  59 &
      ds$HEIGHT <=  75, 17:20] 
       
ds$WEIGHT = 10*ceiling(ds$WEIGHT/10)
fr=table(ds$WEIGHT,ds$HEIGHT,ds$SEX)
str(fr)
dim(fr)

ds = as.data.frame(fr,stringsAsFactors =FALSE)
ds[,1] = as.numeric(ds[,1])
ds[,2] = as.numeric(ds[,2])
ds[,3] = as.numeric(ds[,3])
str(ds)


names(ds)[1:3]=c("Weight.lbs","Height.ins","Sex")
head(ds)
str(ds)

write.table(ds,"weightsEtc.txt")