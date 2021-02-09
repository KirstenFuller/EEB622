mos1<-read.csv("mosquitoes_in_gold_mines.csv") #load in data
mine_cover<-mos1$mine_cover
mosquito<-mos1$mosquito_presence

m1<-glm(mosquito~mine_cover,family="binomial") #fit model
confint(m1) #confidence intervals: does slope overlap zero

library("coefplot")
coefplot(m1) #coefficient plot

#following code is to add confidence intervals to a plot:
newx<-seq(from=min(mine_cover),to=max(mine_cover),length=100) 

plot(mosquito~mine_cover,xlab="Mine cover (%)",ylab="Mosquito presence")

conf_interval <- predict(m1, newdata=data.frame(mine_cover=newx), 
                         type="link",se.fit=TRUE) 

critval <- 1.96 ## approx 95% CI
upr <- plogis(conf_interval$fit + (critval *conf_interval$se.fit))
lwr <- plogis(conf_interval$fit - (critval *conf_interval$se.fit))
fit <- plogis(conf_interval$fit)

lines(fit~newx,lwd=2)
lines(upr~newx,lty=2,col="red",lwd=1.5)
lines(lwr~newx,lty=2,col="red",lwd=1.5)

#source: https://www.r-bloggers.com/2015/12/making-sense-of-logarithmic-loss/
LogLossBinary = function(actual, predicted, eps = 1e-15) {
   predicted = pmin(pmax(predicted, eps), 1-eps)
   - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}


m0<-glm(mosquito~1,family="binomial") #intercept only model
m1<-glm(mosquito~mine_cover,family="binomial")

p_m0<-plogis(predict(m0)) #note: predictions must be on probability scale!
p_m1<-plogis(predict(m1))

LogLossBinary(mosquito,p_m0) #Log loss: lower = better
LogLossBinary(mosquito,p_m1)

