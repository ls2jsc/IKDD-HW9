data<-read.table(url("http://www.datagarage.io/api/5488687d9cbc60e12d300ba5"))
o<-seq(2, 4000, by=2)
d_c<-as.character(data[o, ])
p_m<-sapply(strsplit(d_c[], ""), function(d_c) which(d_c == ":"))
X=as.double(substr(d_c,p_m[2,]+1, 38))
Y=as.double(substr(d_c,p_m[1,]+1, 17))
data<-data.frame(X, Y)
#p1 = 1
#p2 = 0.2
#fit = nls(Y ~ p1*cos(p2*X) + p2*sin(p1*X), start=list(p1=p1,p2=p2))
ssp <- spectrum(Y)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
fit <- lm(Y ~ sin(2*pi/per*X)+cos(2*pi/per*X)+sin(4*pi/per*X)+cos(4*pi/per*X))
summary(fit)
plot(data)
lines(fitted(fit)~X,col="red")
RMSE <- sqrt(mean((Y-predict(fit, data))^2))
