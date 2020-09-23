FOMC <- read.csv("~/Downloads/FOMC.csv")
FOMC <- FOMC[3000:9333,]
colnames(FOMC)<-c('Date','FOMC','Ret')
Event<-data.frame(FOMC[FOMC$FOMC == '1',])
print(Events)
Estimate_Window<-data.frame(FOMC[FOMC$FOMC == '0',])

L1 <- length(Estimate_Window)
L2 <- length(Events)

#constant_return
mu<-mean(Estimate_Window$Ret)
print(mu)

#AR model
AR<-Event$Ret-mu
CAR<-sum(AR)
var_abnormal_r<-sum(AR^2)/(L1-1)
print(var_abnormal_r)

#V
x_star<-matrix(1,L1,1)
x<-matrix(1,L2,1)
Vi<-var_abnormal_r+x_star%*%(t(x)%*%x)^(-1)%*%t(x_star)*var_abnormal_r
gama<-matrix(1,L1,1)

#Hypothesis Testing
Var_CAR_bar <- t(gama) %*% Vi %*% gama
denom = sqrt(Var_CAR_bar)
above = matrix(1,L1,1)%*%CAR
z<-(above)/(denom)
print (z)

#Density Graphs
Evnt_window<-density(Event$Ret)
Est_window<-density(Estimate_Window$Ret)
plot(Evnt_window,col="red",main='Kernel Density of Daily S&P 500 Returns',ylim = c(0, 60))
lines(Est_window,col="blue",lty=2)
legend(x='topright',y=1, c("FOMC", "non-FOMC"),col=c("red", "blue"),pch=c("_","_"))

