#### Fit ####
lost<- -diff(active_cust)
active<- active_cust[-1]

loop.lik<-function(params) {
    p<-params[1]
    ll<-0
    for (i in 1:length(lost)) {
        ll<-ll+lost[i]*(log(1-p)+(i-1)*log(p))
    }
    ll<-ll+active[i]*i*log(p)
    return(-ll)    #return the negative of the function to maximize likelihood
} 

#find parameters for p with optim
geom<-optimize(loop.lik, c(0, 1), tol = 0.0001)

p_hat<-geom$minimum

\

par(mfrow=c(1,1))
par(mai=c(.8,.8,.2,.2))

plot(t+1,rep(p_hat,11),ylab="Retention Rate",xlab="Period",main="",ylim=c(.55,1),type="b")
text(8, .73, "predicted: geom. model", cex=1, pos=3, col="black")
text(6, .95, "actual", cex=1, pos=3, col="black")

\

t=seq(0,11)
S_geo=p_hat^(t)
plot(t,S_geo,ylab="Survivor function",xlab="Period",main="",ylim=c(.1,1),type="b")
text(3, .8, "predicted: geom. model", cex=1, pos=3, col="black")
text(3, .32, "actual", cex=1, pos=3, col="black")