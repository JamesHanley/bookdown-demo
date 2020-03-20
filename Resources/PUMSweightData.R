if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

setwd("/Users/jameshanley/git_repos/statbook/Resources")

library(ipumsr)
ddi <- read_ipums_ddi("nhis_00008.xml")
ds <- read_ipums_micro(ddi)
str(ds)
table(ds$)
ds=ds[ds$AGE>=18 & 
      ds$WEIGHT > 100 &
      ds$WEIGHT < 300 ,17:19] ; 
fr=table(ds$WEIGHT)
plot(fr)
ds = data.frame(fr)
names(ds)[1]="Weight.lbs"
head(ds)
write.table(ds,"weights.txt")