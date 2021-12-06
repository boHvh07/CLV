##### Calculating CLV with the sBG #####
t<-seq(1,200) # time periods
r_pred<-r_sBG(a,b,t) # predicted retention rate
S_pred<-c(1,cumprod(r_pred)[1:199]) # predicted survivor function

dis<-1/(1+d)^(t-1) # discount factor, first term is present so no discounting

CLV_sBG<-sum(m*S_pred*dis) # the sum of margin x survivor x discount factor

CLV_sBG

\

geo<-geoCLV(p_hat,m,d) # use that estimate in the geometric CLV model
geo


##### RLV with the sBG #####
tau<-4

t<-seq(1,tau+200)
r_pred<-r_sBG(a,b,t)
S_pred<-cumprod(r_pred)

S_shift<- S_pred[(tau+1):length(S_pred)]  # survival function from tau + 1 until T

dis<-1/(1+d)^(t(1:200)-1) # discount rate

RLV_sBG<-sum(m*S_shift/S_pred[tau]*dis)  # sum of margin x S(tau + t)/ S(tau) x discount

RLV_sBG
