
n <- 10000
nn<-50
mean1<-numeric(n)
mean2<-numeric(n)
mean3<-numeric(n)
mean4<-numeric(n)
mean5<-numeric(n)

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

####################################
                
for (i in 1:nn){
G_0 <- function(n) rnorm(n, 0, 1)
y <- G_0(n)
#############

rb1 <- rbeta(n,a1,b1)
p1 <- numeric(n)
p1[1] <- rb1[1]
p1[2:n] <- sapply(2:n, function(i) rb1[i] * prod(1 - rb1[1:(i-1)]))
G1 <- sample(y, prob = p1, replace = TRUE)
BTPP1<-c(1:n)*0
m1<-c(1:n)*0
k1=1
BTPP1[1]<-k1
m1[1]<-k1
##############

rb2 <- rbeta(n,a2,b2)
p2 <- numeric(n)
p2[1] <- rb2[1]
p2[2:n] <- sapply(2:n, function(i) rb2[i] * prod(1 - rb2[1:(i-1)]))
G2 <- sample(y, prob = p2, replace = TRUE)
BTPP2<-c(1:n)*0
m2<-c(1:n)*0
k2=1
BTPP2[1]<-k2
m2[1]<-k2
##############

rb3 <- rbeta(n,a3,b3)
p3 <- numeric(n)
p3[1] <- rb3[1]
p3[2:n] <- sapply(2:n, function(i) rb3[i] * prod(1 - rb3[1:(i-1)]))
G3 <- sample(y, prob = p3, replace = TRUE)
BTPP3<-c(1:n)*0
m3<-c(1:n)*0
k3=1
BTPP3[1]<-k3
m3[1]<-k3
##############

rb4 <- rbeta(n,a4,b4)
p4 <- numeric(n)
p4[1] <- rb4[1]
p4[2:n] <- sapply(2:n, function(i) rb4[i] * prod(1 - rb4[1:(i-1)]))
G4 <- sample(y, prob = p4, replace = TRUE)
BTPP4<-c(1:n)*0
m4<-c(1:n)*0
k4=1
BTPP4[1]<-k4
m4[1]<-k4
##############

rb5 <- rbeta(n,a5,b5)
p5 <- numeric(n)
p5[1] <- rb5[1]
p5[2:n] <- sapply(2:n, function(i) rb5[i] * prod(1 - rb5[1:(i-1)]))
G5 <- sample(y, prob = p5, replace = TRUE)
BTPP5<-c(1:n)*0
m5<-c(1:n)*0
k5=1
BTPP5[1]<-k5
m5[1]<-k5
##############

for (i in 2:n){

for (j in 1:(i-1)){
if(G1[i]==G1[j]){BTPP1[i]<-BTPP1[j]
                 m1[i]<-m1[i-1]}
if(G2[i]==G2[j]){BTPP2[i]<-BTPP2[j]
                 m2[i]<-m2[i-1]}
if(G3[i]==G3[j]){BTPP3[i]<-BTPP3[j]
                 m3[i]<-m3[i-1]}
if(G4[i]==G4[j]){BTPP4[i]<-BTPP4[j]
                 m4[i]<-m4[i-1]}
if(G5[i]==G5[j]){BTPP5[i]<-BTPP5[j]
                 m5[i]<-m5[i-1]}

                   }
#######
if(BTPP1[i]==0){k1<-k1+1
           BTPP1[i]<-k1
           m1[i]<-m1[i-1]+1}
if(BTPP2[i]==0){k2<-k2+1
           BTPP2[i]<-k2
           m2[i]<-m2[i-1]+1}
if(BTPP3[i]==0){k3<-k3+1
           BTPP3[i]<-k3
           m3[i]<-m3[i-1]+1}
if(BTPP4[i]==0){k4<-k4+1
           BTPP4[i]<-k4
           m4[i]<-m4[i-1]+1}
if(BTPP5[i]==0){k5<-k5+1
           BTPP5[i]<-k5
           m5[i]<-m5[i-1]+1}
               }
#######
mean1<-mean1+m1
mean2<-mean2+m2
mean3<-mean3+m3
mean4<-mean4+m4
mean5<-mean5+m5
                      }
mean1<-mean1/nn
mean2<-mean2/nn
mean3<-mean3/nn
mean4<-mean4/nn
mean5<-mean5/nn
#################################################################
#################################################################
#################################################################
                 ## Truncated normal ##

amean1<-numeric(n)
amean2<-numeric(n)
amean3<-numeric(n)
amean4<-numeric(n)
amean5<-numeric(n)


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


####################################
                
for (i in 1:nn){
G_0 <- function(n) rnorm(n, 0, 1)
y <- G_0(n)
#############
library(truncnorm)
m1<-0
s1<-0.07
rnt1 <- rtruncnorm(n,0,1,m1,s1)
p1 <- numeric(n)
p1[1] <- rnt1[1]
p1[2:n] <- sapply(2:n, function(i) rnt1[i] * prod(1 - rnt1[1:(i-1)]))
G1 <- sample(y, prob = p1, replace = TRUE)
GDPTN1<-c(1:n)*0
m1<-c(1:n)*0
k1=1
GDPTN1[1]<-k1
m1[1]<-k1
##############
m2<-0.26
s2<-0.2
rnt2 <- rtruncnorm(n,0,1,m2,s2)
p2 <- numeric(n)
p2[1] <- rnt2[1]
p2[2:n] <- sapply(2:n, function(i) rnt2[i] * prod(1 - rnt2[1:(i-1)]))
G2 <- sample(y, prob = p2, replace = TRUE)
GDPTN2<-c(1:n)*0
m2<-c(1:n)*0
k2=1
GDPTN2[1]<-k2
m2[1]<-k2
##############
m3<-0.5
s3<-0.2
rnt3 <- rtruncnorm(n,0,1,m3,s3)
p3 <- numeric(n)
p3[1] <- rnt3[1]
p3[2:n] <- sapply(2:n, function(i) rnt3[i] * prod(1 - rnt3[1:(i-1)]))
G3 <- sample(y, prob = p3, replace = TRUE)
GDPTN3<-c(1:n)*0
m3<-c(1:n)*0
k3=1
GDPTN3[1]<-k3
m3[1]<-k3
##############
m4<-0.85
s4<-0.2
rnt4 <- rtruncnorm(n,0,1,m4,s4)
p4 <- numeric(n)
p4[1] <- rnt4[1]
p4[2:n] <- sapply(2:n, function(i) rnt4[i] * prod(1 - rnt4[1:(i-1)]))
G4 <- sample(y, prob = p4, replace = TRUE)
GDPTN4<-c(1:n)*0
m4<-c(1:n)*0
k4=1
GDPTN4[1]<-k4
m4[1]<-k4
##############
m5<-1
s5<-0.07
rnt5 <- rtruncnorm(n,0,1,m5,s5)
p5 <- numeric(n)
p5[1] <- rnt5[1]
p5[2:n] <- sapply(2:n, function(i) rnt5[i] * prod(1 - rnt5[1:(i-1)]))
G5 <- sample(y, prob = p5, replace = TRUE)
GDPTN5<-c(1:n)*0
m5<-c(1:n)*0
k5=1
GDPTN5[1]<-k5
m5[1]<-k5
##############

for (i in 2:n){

for (j in 1:(i-1)){
if(G1[i]==G1[j]){GDPTN1[i]<-GDPTN1[j]
                 m1[i]<-m1[i-1]}
if(G2[i]==G2[j]){GDPTN2[i]<-GDPTN2[j]
                 m2[i]<-m2[i-1]}
if(G3[i]==G3[j]){GDPTN3[i]<-GDPTN3[j]
                 m3[i]<-m3[i-1]}
if(G4[i]==G4[j]){GDPTN4[i]<-GDPTN4[j]
                 m4[i]<-m4[i-1]}
if(G5[i]==G5[j]){GDPTN5[i]<-GDPTN5[j]
                 m5[i]<-m5[i-1]}

                   }
#######
if(GDPTN1[i]==0){k1<-k1+1
           GDPTN1[i]<-k1
           m1[i]<-m1[i-1]+1}
if(GDPTN2[i]==0){k2<-k2+1
           GDPTN2[i]<-k2
           m2[i]<-m2[i-1]+1}
if(GDPTN3[i]==0){k3<-k3+1
           GDPTN3[i]<-k3
           m3[i]<-m3[i-1]+1}
if(GDPTN4[i]==0){k4<-k4+1
           GDPTN4[i]<-k4
           m4[i]<-m4[i-1]+1}
if(GDPTN5[i]==0){k5<-k5+1
           GDPTN5[i]<-k5
           m5[i]<-m5[i-1]+1}
               }
#######
amean1<-amean1+m1
amean2<-amean2+m2
amean3<-amean3+m3
amean4<-amean4+m4
amean5<-amean5+m5
                      }
amean1<-amean1/nn
amean2<-amean2/nn
amean3<-amean3/nn
amean4<-amean4/nn
amean5<-amean5/nn
#################################################################
#################################################################
#################################################################
#################################################################

xx<-1:n

par(mfrow=c(1,2))

plot(xx,mean1,type="l",xlab="Samples", 
ylab="Average number of clusters", main="H=beta (a,b)",col="black")
lines(xx,mean2, col="red")
lines(xx,mean3, col="pink2")
lines(xx,mean4, col="yellow3")
lines(xx,mean5, col="blue")
points(xx[c(1000,2000,4000,8000)],mean1[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],mean2[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],mean3[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],mean4[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],mean5[c(1000,2000,4000,8000)],col="purple")
legend(5500,120,c("a=0.44,  b=15", "a=1,       b=3","a=3,       b=3"
,"a=3,       b=1","a=8,       b=0.5"),lty=1,col=c("black","red","pink2","yellow3","blue"))



plot(xx,amean1,type="l",xlab="Samples",
ylab="Average number of clusters", main="H=TN_|0,1|  (m,s)",col="black")
lines(xx,amean2, col="red")
lines(xx,amean3, col="pink2")
lines(xx,amean4, col="yellow3")
lines(xx,amean5, col="blue")
points(xx[c(1000,2000,4000,8000)],amean1[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],amean2[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],amean3[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],amean4[c(1000,2000,4000,8000)],col="purple")
points(xx[c(1000,2000,4000,8000)],amean5[c(1000,2000,4000,8000)],col="purple")
legend(4900,80,c("m=0,         s=0.07","m=0.26,   s=0.2",
"m=0.5,     s=0.2","m=0.85,   s=0.2","m-1,         s=0.07"),lty=1,
col=c("black","red","pink2","yellow3","blue"))


#################################################################
#################################################################
