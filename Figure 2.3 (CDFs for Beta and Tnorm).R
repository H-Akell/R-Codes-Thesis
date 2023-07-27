

x<-seq(0,1,len=100000)
a1<-0.44
b1<-15
a2<-1
b2<-3
a3<-3
b3<-3
a4<-3
b4<-1
a5<-8
b5<-0.5

y11<-pbeta(x,a1,b1)
y22<-pbeta(x,a2,b2)
y33<-pbeta(x,a3,b3)
y44<-pbeta(x,a4,b4)
y55<-pbeta(x,a5,b5)
###################################
###################################

library(truncnorm)
m1<-0
s1<-0.07
m2<-0.26
s2<-0.2
m3<-0.5
s3<-0.2
m4<-0.85
s4<-0.2
m5<-1
s5<-0.07

ay11<-ptruncnorm(x, 0, 1,m1,s1)
ay22<-ptruncnorm(x, 0, 1,m2,s2)
ay33<-ptruncnorm(x, 0, 1,m3,s3)
ay44<-ptruncnorm(x, 0, 1,m4,s4)
ay55<-ptruncnorm(x, 0, 1,m5,s5)



###############################
###############################

par(mfrow=c(1,2))
plot(x,y11,type="l",xlab="v",col="black",
ylab="CDF", main="Beta CDF")
text(0.12,0.8,expression(paste(alpha, " = 20.8")),col="black")
lines(x,y22, col="red",lty=2)
text(0.25,0.7,expression(paste(alpha, " = 3")),col="red")
lines(x,y33, col="grey3",lty=3)
text(0.47,0.6,expression(paste(alpha, " = 1.5")),col="grey3")
lines(x,y44, col="blue",lty=4)
text(0.72,0.5,expression(paste(alpha, " = 0.5")),col="blue")
lines(x,y55, col="green3",lty=5)
text(0.88,0.4,expression(paste(alpha, " = 0.1")),col="green3")



plot(x,ay11,type="l",xlab="v",col="black",
ylab="CDF", main="Truncated normal CDF")
text(0.17,0.8,expression(paste(alpha, " = 20.8")))
lines(x,ay22, col="red",lty=2)
text(0.31,0.7,expression(paste(alpha, " = 3")),col="red")
lines(x,ay33, col="grey3",lty=3)
text(0.47,0.6,expression(paste(alpha, " = 1.5")),col="grey3")
lines(x,ay44, col="blue",lty=4)
text(0.72,0.5,expression(paste(alpha, " = 0.5")),col="blue")
lines(x,ay55, col="green3",lty=5)
text(0.87,0.4,expression(paste(alpha, " = 0.1")),col="green3")





