
       ### Bell Number Function ###
bell = function(n){
bel<-array(c(1:(n)^2),dim=c((n),(n)))
bel<-t(bel)
bel[1,1] <- 1
for (i in 2:n){
               bel[i,1] <- bel[i-1,i-1]
               for (j in 2:i){
                              bel[i,j] <- bel[i-1,j-1] + bel[i,j-1]
                              }
                }
bel[n,n]
                      }
##############################################################################

      ### Ways to write n as sum of positive integers Function ###
partition_n = function(n){
par_n<-c(1:(n+1))
for (i in 2:(n+1)){par_n[i]<-0}
par_n[1]<-1
for (i in 2:n){ 
              for (j in i:(n+1)){
                                 par_n[j]<- par_n[j]+par_n[j-(i-1)]
                                 }
               }
if (n<2){1} else {1+par_n[n+1]}
                             }

     ### The values of partition_n function for 1,2,...,n ###

par_n=function(n){
vector_par_n<-c(1:n)
for (i in 1:n){
               vector_par_n[i]<- partition_n(i)
               }
vector_par_n
                  }
##############################################################################

     ### Function of Combination
comb = function(n, x) {
  factorial(n) /(factorial(n-x)*factorial(x))
                       }

##############################################################################

     ### Function of Moment for beta(a,b) distribution ###

moment = function(a,b,i,j){
 m<-(gamma(a+i)*gamma(b+j)*gamma(a+b))/(gamma(a)*gamma(b)*gamma(a+b+i+j))
 m 
                           }

     ### Matrix of Moments ###

n<-5
a<-2
b<-5
M<- array(c(1:(n+1)^2)*0,dim=c((n+1),(n+1)))
for (i in 0:n){
               for (j in 0:i)
                               M[(i+1)-j,j+1]<-moment(a,b,i-j,j)
               }

     ### Initial Matrices ###

pa_n<-par_n(n)    ##The values of partitions for 1,2,...,n
r<-1+sum(pa_n)-pa_n[n]    
G<-II<-III<-array(c(1:n*r)*0,dim=c((n),(r)))
rr<-pa_n[n]
V<-I<-array(c(1:n*r)*0,dim=c((n),(rr))) ## weights and identity distributions
C<-c(1:n)     ### default values for identify the distributions

     ### Initial values for n=1

V[1,1]<-1
I[1,1]<-C[1]

     ### Calculate G,II,III,I,V from row 3 to row n

for (i in 2:n){
G[i,1]<-V[i,1]<-(M[i+1,1]/(1-M[1,i+1]))
I[i,1]<-C[i]
s<-1
for (j in 1:(i-1)){
G[i,(s+c(1:pa_n[j]))]<-comb(i,j)*(M[i-j+1,j+1]/(1-M[1,i+1]))*V[j,c(1:pa_n[j])]
     ### writing II[i,]
for (m in 1:pa_n[j]){
CC<-c(C[i-j])
T<-I[j,m]
for (mm in (j-1):0){
TT<-trunc(T/(10^mm))
if(TT >= 1){
CC<-c(CC,TT)
T<-T-TT*(10^mm)            
            } # if
                    } # mm
MM<-sort(CC)
kk<-length(CC)
for (ii in 1:kk){
II[i,s+m]<-II[i,s+m]+MM[ii]*10^(ii-1)
                    } #ii
                     } # m
s<-s+pa_n[j]
                   } # j
III[i,]<-II[i,]
k=1
for (j in 2:s){
k<-k+1
l<-0
for (m in j:s){
if(II[i,j]==III[i,m]){
V[i,k]<-V[i,k]+G[i,m]
III[i,m]<-0
I[i,k]<-II[i,j]
l<-l+1
                      } # if
               } # m
if(l == 0){k<-(k-1)} #if
               } # j
               } # i




     ### writing W and G
G<-c(I[n,1])
W<-c(V[n,1])
for (i in 2:pa_n[n]){
CC<-c()
T<-I[n,i]
for (mm in (n-1):0){
TT<-trunc(T/(10^mm))
if(TT >= 1){
CC<-c(CC,TT)
T<-T-TT*(10^mm)            
            } # if
                    } # mm
MM<-sort(CC)
kk<-length(CC)
LL<-c()
l<-1
for (k in 1:(kk-1)){
if(MM[k]==MM[k+1]){l<-(l+1)} # if
else {
LL<-c(LL,l)
l<-1
      } # else
                    } # k
LL<-c(LL,l)
rrr<-1
rr<-length(LL)
for(j in 1:rr){
rrr<-rrr*factorial(LL[j])
               } # j
kkk<-1
for(j in 1:kk){
kkk<-kkk*factorial(MM[j])
               } # j
R<-factorial(n)/(kkk*rrr)
for(j in 1:R){
G<-c(G,I[n,i])
W<-c(W,(V[n,i]/R))
               } # j
                    } # i
G<-t(t(G))
sum(W)
G
W

