
########### E[p_1+p_2+...+p_(N-1)]
###combination###
comb1 = function(n, j) {
if (j==0){comb1 <-1} else 
if (j==n){comb1 <-1} else {
comb1 <-1
for (i in 1:j){comb1<-comb1*(n-i+1)/i}
                      }
comb1 
                      }
#######################
moment1 = function(a, j) {
moment1 <-1
for (i in 1:a){moment1<-moment1*(i/(j+i))}
moment1 
                      }
###########################################
###########################################
################################### a=1, DP
N<-150
m<-1000
a<-1
b<-3
x<-c(1:m)
F<-c(1:m)*0
R<-c(1:m)*0
L<-c(1:m)*0
A<-c(1:m)*0

for (n in 1:m){
F[n]<-n*((b/(b+1))^(N-1))
R[n]<-0
for (j in 2:n){R[n]<-R[n]+((-1)^(j+1))*comb1(n,j)*((b/(b+j))^(N-1))}
L[n]<-4*(F[n]+R[n])
F[n]<-4*F[n]
A[n]<-4*n*exp(-(N-1)/b)
                }
######################################
n<-1000
t1<-150
t2<-155
XX<-c(1:t2)
FF<-c(1:t2)*0
RR<-c(1:t2)*0
LL<-c(1:t2)*0
AA<-c(1:t2)*0

for (N in t1:t2){
FF[N]<-n*((b/(b+1))^(N-1))
for (j in 2:n){RR[N]<-RR[N]+((-1)^(j+1))*comb1(n,j)*((b/(b+j))^(N-1))}
LL[N]<-4*(FF[N]+RR[N])
FF[N]<-4*FF[N]
AA[N]<-4*n*exp(-(N-1)/b)
                }


###########################################
###########################################
################################### b=1, a<-3
N<-150
m<-1000
a<-3
b<-1
x<-c(1:m)
F1<-c(1:m)*0
R1<-c(1:m)*0
L1<-c(1:m)*0
A1<-c(1:m)*0

for (n in 1:m){

F1[n]<-n*((moment1(a,1))^(N-1))
for (j in 2:n){R1[n]<-R1[n]+((-1)^(j+1))*comb1(n,j)*((moment1(a,j))^(N-1))}
L1[n]<-4*(F1[n]+R1[n])
F1[n]<-4*F1[n]
                 }
######################################
n<-1000
XX<-c(1:t2)
FF1<-c(1:t2)*0
RR1<-c(1:t2)*0
LL1<-c(1:t2)*0

for (N in t1:t2){
FF1[N]<-n*((moment1(a,1))^(N-1))
for (j in 2:n){RR1[N]<-RR1[N]+((-1)^(j+1))*comb1(n,j)*((moment1(a,j))^(N-1))}
LL1[N]<-4*(FF1[N]+RR1[N])
FF1[N]<-4*FF1[N]

                }

###################################################
par(mfrow=c(1,2))


plot(x,L,type="l",xlab="n",col="black",ylab="Error bounds",main="N=150, H=beta(1,3)")
lines(x[100:200],F[100:200],col="red")
lines(x[400:600],F[400:600],col="red")
lines(x[800:900],F[800:900],col="red")
lines(x,A,col="blue")
legend(0,0.98e-15,c("Bounds of distance","Approximate 1","Approximate 2"),lty=1,
col=c("black","red","blue"))

plot(XX[t1:t2],LL[t1:t2],ylim=c(0,1e-15),type="l",xlab="N",ylab="Error bounds",col="black",main="n=1000, H=beta(1,3)")
lines(XX[151:152],FF[151:152],col="red")
lines(XX[154:155],FF[154:155],col="red")
lines(XX[t1:t2],AA[t1:t2],col="blue")
legend(152.5,1e-15,c("Bounds of distance","Approximate 1","Approximate 2"),lty=1,
col=c("black","red","blue"))


###################################################

par(mfrow=c(1,2))

plot(x,L1,type="l",xlab="n",col="black",ylab="Error bounds",main="N=150, H=beta(3,1)")
lines(x[100:200],F1[100:200],col="red")
lines(x[400:600],F1[400:600],col="red")
lines(x[800:900],F1[800:900],col="red")
legend(0,8e-87,c("Bounds of distance","Approximate 1"),lty=1,
col=c("black","red"))

plot(XX[t1:t2],LL1[t1:t2],type="l",xlab="N",col="black",ylab="Error bounds",main="n=1000, H=beta(3,1)")
lines(XX[151:152],FF1[151:152],col="red")
lines(XX[154:155],FF1[154:155],col="red")
legend(151.5,8e-87,c("Bounds of distance","Approximate 1"),lty=1,
col=c("black","red"))






