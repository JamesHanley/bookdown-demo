p=c(1:7)/8

q = c(22.5, 24.75, 27.6, 29.6, 30+c(1.1,3.7,6.7))

sd = (-0.5/qnorm(.25))*(q[6]-q[2])
mean = q[4]

plot(q,qnorm(p,mean,sd))
abline(a=0,b=1)

pre="$makebox[$linewidth]{$hspace*{-20cm}$includegraphics[page="
post=",width=0.10$paperwidth,trim=3cm 33.3cm 15.3cm 1.8cm,clip]{2018Book.pdf}}"

for(page in seq(11,1210,10)) {
	x = paste("Page ",toString(page)," ",
	pre,toString(page),post,"\n",sep="")
	cat(noquote(x))
}