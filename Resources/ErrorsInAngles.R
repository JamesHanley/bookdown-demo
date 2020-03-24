DA = 10

xmax=0.75
DX=3+xmax
ymax=6


par(mfrow=c(1,2),mar = rep(0.01,4))

for (example in 1:2) {
	

 
plot(c(-3,xmax),c(-0.5,ymax),col="white",
 xaxt="n", yaxt="n",xlab="",ylab="",
 xlim=c(-3,xmax),ylim=c(-0.5,ymax))
lines(c(-4,0,0),c(0,0,6),lty="dotted")

segments(xmax, 0, xmax,  6)
for(h in seq(0,6,1)){
	text(xmax,h,toString(h),adj=c(-0.2,0.5))
	segments(xmax-0.025, h, xmax,  h)
}
for(d in seq(1,3,1)){
	text(-d,-0.25,toString(d),adj=c(0.5,1))
	segments(-d, 0, -d,  -0.1)
}


H = runif(1,1.8,3)
H = 2.5

x=0.1
U=5
L=0
COL=1
dd = 1
for (d in seq(dd,3,dd) ) {	
   ANGLE   = atan( H/d ) * 360/(2*pi) 
   angle.hat   = DA*round(ANGLE/DA)
   h.hat   = d*tan(  angle.hat       / ( 360/(2*pi) ) )
   segments(-d,0,0,h.hat,col=COL)
   text(-d+0.1,0,toString(angle.hat),
    adj=c(0,-0.25), col=COL)
   h.upper = d*tan( (angle.hat+DA/2) / ( 360/(2*pi) ) )
   h.lower = d*tan( (angle.hat-DA/2) / ( 360/(2*pi) ) )
   segments(x,h.lower,x,h.upper,col=COL)
   points(x,h.hat,pch=19,cex=0.4,col=COL)
   U = min(U,h.upper)
   L = max(L,h.lower)
   x=x+0.1
   COL=COL+1
}
segments(xmax-0.1,L,xmax-0.1,U,lwd=3)

}