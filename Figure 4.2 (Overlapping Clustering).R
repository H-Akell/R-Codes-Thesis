
a1<-1
b1<-30
mu_0_1<-0
sigma_0_1<-0.3
sigma_1<-2

library(truncnorm)

n <- 1000

mu_H2<-0
sigma_H2<-1
mu_0_2<-0
sigma_0_2<-0.3
sigma_2<-2

###########################################################
###########################################################
G_0_1 <- function(n) rnorm(n, mu_0_1, sigma_0_1)
b <- rbeta(n, a1,b1)
p <- numeric(n)
p[1] <- b[1]
p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))
x1 <- cbind(G_0_1(n),G_0_1(n))
index1 <- sample(1:n, prob = p, replace = TRUE)
G1<-x1[index1,]
y1<-c()
for (i in 1:n){
yy1<-rnorm(cbind(1,1),G1[i,],cbind(sigma_1,sigma_1))
y1<-rbind(y1,yy1)
               }

BT1<-c(1:n)*0
m1<-c(1:n)*0
k1=1
BT1[1]<-k1
m1[1]<-k1

for (i in 2:n){
for (j in 1:(i-1)){
if(index1[i]==index1[j]){BT1[i]<-BT1[j]
                 m1[i]<-m1[i-1]}
                     }
if(BT1[i]==0){k1<-k1+1
           BT1[i]<-k1
           m1[i]<-m1[i-1]+1}
 }
################
###### choose colors without "black"
color1<-c(1:n)*0
color1<-BT1
for (i in 1:n){
for (j in 1:trunc(max(BT1)/8+1)){
if(color1[i]==((j-1)*8+1)){color1[i]<-(2*color1[i]+2*j)}
                }}

###########################################################
#############################################################
#############################################################
G_0_2 <- function(n) rnorm(n, mu_0_2, sigma_0_2)
b <- rtruncnorm(n,0,1,mu_H2,sigma_H2)
p <- numeric(n)
p[1] <- b[1]
p[2:n] <- sapply(2:n, function(i) b[i] * prod(1 - b[1:(i-1)]))
x2 <- cbind(G_0_2(n),G_0_2(n))
index2 <- sample(1:n, prob = p, replace = TRUE)
G2<-x2[index2,]
y2<-c()
for (i in 1:n){
yy2<-rnorm(cbind(1,1),G2[i,],cbind(sigma_2,sigma_2))
y2<-rbind(y2,yy2)
               }

BT2<-c(1:n)*0
m2<-c(1:n)*0
k2=1
BT2[1]<-k2
m2[1]<-k2

for (i in 2:n){
for (j in 1:(i-1)){
if(index2[i]==index2[j]){BT2[i]<-BT2[j]
                 m2[i]<-m2[i-1]}
                     }
if(BT2[i]==0){k2<-k2+1
           BT2[i]<-k2
           m2[i]<-m2[i-1]+1}
 }
################
###### choose colors without "black"
color2<-c(1:n)*0
color2<-BT2
for (i in 1:n){
for (j in 1:trunc(max(BT2)/8+1)){
if(color2[i]==((j-1)*8+1)){color2[i]<-(2*color2[i]+2*j)}
                }}

###########################################################
###########################################################
###########################################################
###########################################################

par(mfrow=c(1,2)) 
plot(y1,pch=19,xlim=c(-7,7),ylim=c(-8,8),col=color1,xlab="Y",ylab="",
main=bquote("H = Beta("~.(a1)~","~.(b1)~"),"~sigma~"="~.(sigma_1)~","~sigma~"_0="~.(sigma_0_1)))
		points(G1,
			pch=15,
			col="black"
			)
plot(y2,pch=19,xlim=c(-7,7),ylim=c(-8,8),col=color2,xlab="Y",ylab="",
main=bquote("H = TN("~.(mu_H2)~","~.(sigma_H2)~"),"~sigma~"="~.(sigma_2)~","~sigma~"_0="~.(sigma_0_2)))
		points(G2,
			pch=15,
			col="black"
			)

max(BT1)
max(BT2)



