stickperson =function(xc, yc, dx, dy, LWD=1,COL="black"){
  fig.x=c(-1,0, 1,0, 0,-1,0, 1,0,
           0,  -1, -1,   0, 1,   1,0)
  fig.y=1+c(-1,0,-1,0, 2, 1,2, 1,2,
          3, 3.7,  4.4, 5.1, 4.4, 3.7,3)
lines(xc+dx*fig.x,yc+dy*fig.y,lwd=LWD,col=COL)
}
