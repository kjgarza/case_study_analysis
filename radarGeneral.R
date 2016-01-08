library(fmsb)

maxmin <- data.frame(
total=c(7,2),
phys=c(15,5),
psycho=c(3,0),
social=c(5,1))
# data for radarchart function version 1 series, minimum value must be omitted from above.

RNGkind("Mersenne-Twister")
set.seed(123)
dat <- data.frame(
total=runif(3,1,5),
phys=rnorm(3,10,2),
psycho=c(0.5,NA,3),
social=runif(3,1,5))


dat <- rbind(maxmin,dat)

op <- par(mar=c(1,2,2,1),mfrow=c(2,2))
plot(1)
radarchart(dat,axistype=1,seg=5,plty=1,title="(axis=1, 5 segments)")
plot(1)
radarchart(dat,axistype=2,pcol=topo.colors(3),plty=1,title="(topo.colors, axis=2)")
plot(1)
radarchart(dat,axistype=3,pty=32,plty=1,axislabcol="grey",na.itp=FALSE,title="(no points, axis=3, na.itp=FALSE)")
plot(1)
radarchart(dat,axistype=0,plwd=1:4,pcol=1,title="(use lty and lwd but b/w, axis=0)")
par(op)	
