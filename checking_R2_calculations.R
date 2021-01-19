N=500
Predictor=runif(N,min=1,max=50)
Intercept=5
Slope=10
sigma=44.5
mu=Intercept+Slope*Predictor
plot(mu~Predictor,pch=19)
Response=rnorm(N,mean=mu,sd=sigma)
plot(Response~Predictor,pch=19)

#extracting R-squared from least squares
summary(lm(Response~Predictor))$r.squared


preds=predict(lm(Response~Predictor)) #predictions from R-squared

SSy=sum((Response-mean(Response))^2) #total variance in data
RSS=sum((Response-preds)^2) #residual variance

SSreg=SSy-RSS #explained variance

SSreg/SSy #does this match with r.squared above?
