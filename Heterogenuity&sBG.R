#### Ruse of Heterogenuity ###
N<-10000
prop<-1/3
n<- data.frame(seg1=rep(NA,10), seg2=rep(NA,10))
ret<-c(.9,.5)
n$seg1[1]<-round(N*prop)
n$seg2[1]<-round((1-prop)*N)

for(k in 2:10){
    n[k,]<-round(n[k-1,]*ret)
}

avgr<-(n$seg1*ret[1]+n$seg2*ret[2])/rowSums(n)
t=seq(1,10)

par(mfrow=c(1,2))
plot(t,avgr,ylab="retention rate",xlab="period",main="",ylim=c(.4,1),type="b", xaxt="none", lwd=3)
axis(1, seq(1,10,1))
abline(h= ret[1], lty=2, lwd=3, col="blue")
abline(h= ret[2], lty=2, lwd=3, col="red")
text(2.5, .78, "avg. ret.")
text(3, .52, "seg. 2", cex=1, pos=3, col="black")
text(3, .92, "seg. 1", cex=1, pos=3, col="black")

plot(t,n$seg2, xlab="period",main="",type="b", lwd=3, xaxt="none", col="red", ylab="Number of customers")
axis(1, seq(1,10,1))
lines(t,n$seg1, lwd=3, type="b", col="blue")
text(4, 3000, "seg. 1", cex=1, pos=3, col="black")
text(2.3, 5000, "seg. 2", cex=1, pos=3, col="black")



#### shifted Beta Geometric ###
t=seq(1,10)
r_sBG=function(a,b,t){
    # a<-1
    #  b<-4
    (b+t-1)/(a+b+t-1)
}
par(mfrow=c(1,1))
plot(t,r_sBG(.1,.3,t),ylab="retention rate",xlab="period",main="retention rate: shifted Beta geometric model for different parameter values",ylim=c(.65,1),type="b", xaxt="none")
points(t,r_sBG(1,3,t),type="b",col="red")
points(t,r_sBG(100,300,t),type="b",col="green")
axis(1, seq(0,10,1))
text(8, .7, "a=100,b=300", cex=1, pos=3, col="black")
text(8, .85, "a=1,b=3", cex=1, pos=3, col="black")
text(8, .94, "a=0.1,b=0.3", cex=1, pos=3, col="black")

\

set.seed(19103)
N=1000  
a<-2  
b<-8
par(mfrow=c(2,2))
for (t in 1:4){
    cust<-rbeta(N, a,b)  # draw N times from a beta distribution with parameters a and b
    par(mai=c(.7,.8,.2,.2))
    g<-hist(cust,breaks = 99,xlim = c(0,1),density = 10, main=paste("churn prob. in period", t), xlab =     expression(paste("churn probability (", theta, ")")), ylab = "number of customers",)
    text(.8,.8*par("yaxp")[2], paste("N=",round(N)),cex=1,pos=3,col="black")
    abline(v=mean(cust),col = "red", lwd = 2) # draw average churn
    b<-b+1  # Bayes update churn distribution
    N<-N*(b+t-1)/(a+b+t-1) # churners leave
}


##### Estimating the Model ####
lost<- -diff(active_cust)
active<- active_cust[-1]

loop.lik<-function(params) {
    a<-params[1]
    b<-params[2]
    ll<-0
    for (i in 1:length(lost)) {
        ll<-ll+lost[i]*log(beta(a+1,b+i-1)/beta(a,b))
    }
    ll<-ll+active[i]*log(beta(a,b+i)/beta(a,b))
    return(-ll)    #return the negative of the function to maximize likelihood
} 

#find parameters for a and b with optim
sBG<-optim(par=c(1,1),loop.lik)

a<-sBG$par[1]
b<-sBG$par[2]

#calculate retention using model parameters
t<-1:length(active)
r_pred<-r_sBG(a,b,t)

# plot actual and predicted retention rate
par(mfrow=c(1,1))
plot(t,r_pred,ylab="retention rate",xlab="period",main="actual vs. predicted retention rate",type="b", xaxt="none")
#points(t,r_pred,type="b",col="red")
axis(1, seq(0,10,1))
legend('right',legend=c("actual", "predicted"),col=c("black","red"), pch=c(1,1))
