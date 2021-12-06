##### Geomteric CLV ####
p<-0.8    # retention probability (rate) 

t<-seq(0,10)  # time period starting at 0

par(mfrow=c(1,2))
par(mai=c(.8,.8,.2,.2))
plot(1:10, rep(p,length(t)-1), type="b",ylab="Probability the customer is retained", xlab="periods", main="Retention rate",ylim=par("yaxp")[1:2]) 

plot(t, p^t, type="b",ylab="Probability the customer has survived", xlab="periods", main="Survivor function")
text(1.5, .78, " 0.8", cex=1, pos=3, col="black")
text(2.5, .62, parse(text= '.8^2'), cex=1, pos=3, col="black")

\

geoCLV<-function(p,m,d){
    m*(1+d)/(1+d-p)
}
p<-0.8    # retention probability (rate) 
m<-100    # margin (profit)
d<-0.1    # discount rate

geoCLV(p,m,d)

\

t<-seq(0,10)  # time period starting at 0

m*(p/(1+d))^t # the first 10 terms of CLV

sum(m*(p/(1+d))^t)  # CLV using only the first 10 terms

