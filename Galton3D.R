
n.steps = 5

XYLIMS = c(-1,1)*ceiling(n.steps/2)

par(mfrow=c(1,1),mar = rep(0,4))

plot(n.steps,xlim= XYLIMS, ylim=XYLIMS)


for(step in n.steps:1){
	rect(-step/2, -step/2, step/2, step/2)
}

a = seq(0,2*pi,length.out=61)
x = cos(a); y=sin(a)
xc = 0; yc=0
for(step in 1:n.steps){
	#polygon(xc+ (1/2)*x, yc+ (1/2)*y)
	#points( xc, yc,pch=19,cex=step/10)
	text(xc,yc,toString(step),cex=0.7)
	xc = xc + sample(c(-1/2,1/2),1)
	yc = yc + sample(c(-1/2,1/2),1)
	
}
