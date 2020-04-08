setwd("/Users/jameshanley/git_repos/statbook/Resources")
n=737
TXT = readLines("MLBatBats.txt",n=n)
txt=strsplit(TXT,"\n");
str(txt)


AB=NULL;H=NULL; ab=NULL; h=NULL; f1=NULL ; Name=NULL
players = data.frame(Name,ab,h,AB,H)
for(i in 1:n){
  t = txt[[i]]
  t = strsplit(t,"\t")[[1]]
  print(t)
  f1 = as.numeric(t[1])
  if( !is.na(f1) | i==n ) {
    if(length(ab)>0 ) {
      ab = sum(ab)
      h = sum(h)
      AB = AB-ab
      H = H - h
      x = data.frame(Name,ab,h,AB,H)
      players  = rbind(players,x)
    }
    ab = NULL; h=NULL
    Name = t[2]
    H = as.numeric(t[8])
    AB = as.numeric(t[6])

  }

  f4 = as.numeric(t[4])
  if( !is.na(f4) & t[1] != "March" ) {
    ab = c(ab, f4)
    h  = c(h, as.numeric(t[6]) )
  }
}
players$ave=round(players$h/players$ab,3)
players$AVE=round(players$H/players$AB,3)
players$y  = 1
players$Y  = 0

players

write.table(players,"MLBatBats.txt")

par(mfrow=c(1,1),mar = c(5,5,1,1))

plot(c(0.15,0.5),c(0,1.1),col="white",
     ylab="", yaxt="n",
     xlab="Batting Average, April 16 to end of September")
segments(players$ave, players$y, players$AVE, players$Y,
         col=2:20)
segments(seq(.15,.50,0.01),0,seq(.15,.50,0.01),1,col="grey85")
segments(seq(.15,.50,0.05),0,seq(.15,.50,0.05),1,col="grey65")
text(0.325,1.085,"Batting Average, mid March to April 15, 2019")



