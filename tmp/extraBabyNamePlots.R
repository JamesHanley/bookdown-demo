

```{r,eval=T, echo=F, fig.align="center", fig.height=10, fig.width=9, warning=FALSE, message=F}

ds = read.table(  # str(ds)
  "http://www.biostat.mcgill.ca/hanley/statbook/QuebecBabyNames2013to2018.txt", as.is=TRUE)


par(mfrow=c(1,1),mar = c(0,0,1,0))

for( year in max(ds$Year) ) {

  m = matrix(c( ds$Name[ds$Year == year & ds$Gender == "M"],
                ds$Name[ds$Year == year & ds$Gender == "F"] ),
             500,2)

  n = 50 ; dy=3
  X = 20
  CEX = 0.5

  plot(-11, -11,
       ylim=c(0,n*1.05*dy),
       xlim=c(-12,50))

  dx = strwidth("1", units = "user",
                cex = CEX, font = 1, family= "mono")

  segments(0,1,0,(n+1)*dy)
  segments(X,1,X,(n+1)*dy)


  ADJ = rep(1,n)
  str = rep("",n)
  Diff = rep(NA,n)

  for(i in 1:n){

    M.minus.F = nchar(m[i,1]) - nchar(m[i,2])
    Diff[i] = M.minus.F

    if(M.minus.F>=0){

      text(0+M.minus.F*dx, i*dy+2,m[i,1],
           adj=c(1,0),family="mono",cex=CEX)
      text(0,              i*dy+1,m[i,2],
           adj=c(1,0),family="mono",cex=CEX)


      if(M.minus.F > 0){
        ss = substr(m[i,1], nchar(m[i,2])+1,
                    nchar(m[i,1]) )
        text(0,i*dy+2,ss,
             adj=c(0,0),family="mono",
             cex=CEX,col="blue")

        str[i] = ss
        ADJ[i] = 0
      }


    }

    if(M.minus.F < 0){
      text(0,               i*dy+2,m[i,1],
           adj=c(0,0),family="mono",cex=CEX)
      text(0+M.minus.F*dx,  i*dy+1,m[i,2],
           adj=c(0,0),family="mono",cex=CEX)
      ss = substr(m[i,2],1,-M.minus.F)
      text(0,i*dy+1,ss,
           adj=c(1,0),family="mono",
           cex=CEX,col="red" )
      str[i] = ss
      ADJ[i] = 1
    }

    text(X, i*dy+1.5, str[i],
         adj=c(ADJ[i],0), family="mono",
         cex=CEX,
         col= c("red","blue")[ 2-ADJ[i] ]
    )

  } # i

  sorted.d = data.frame(Diff,str)
  sorted.d = sorted.d[order(sorted.d$Diff),]
  w = max(abs( sorted.d$Diff ))

  XX = 40
  segments(XX,1.5,XX,(n+1)*dy)
  Dx = w * dx
  segments(XX-Dx,(n/2+1)*dy, XX+Dx,(n/2+1)*dy)
  rect( XX-Dx, 0.5*dy, XX+Dx,(n+1)*dy )

  for(i in 1:n){
    e = sorted.d$Diff[i]
    text(XX+(e<0)*e*dx, i*dy+1.5,
         toString( sorted.d$str[i] ),
         cex=CEX,  family="mono",adj=c(0,0))
  }

} # year

```
