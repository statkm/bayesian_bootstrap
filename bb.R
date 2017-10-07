dye<-c(1.15,1.7,1.42,1.38,2.8,4.7,4.8,1.41,3.9)
dfp<-c(1.38,1.72,1.59,1.47,1.66,3.45,3.87,1.31,3.75)
dat<-cbind(dye,dfp)
#plot(dye,dfp)
#cor(dye,dfp)
n<-dim(dat)[1]


#bootstrap
make.Boot<-function(xx,nn,B=100){
  boots<-numeric(B)
  for(ii in 1:B){
    index<-sample(1:nn,replace = T)
    boots[ii]<-cor( xx[index,1],xx[index,2] )
  } 
  return(boots)
}



#bayesian bootstrap
make.BBoot<-function(xx,nn,B=100){
  boots<-numeric(B)
  for(ii in 1:B){
    ru<-runif(nn-1); ru<-c(0,ru[order(ru)],1)
    pr<-ru[-1]-ru[-length(ru)]
    boots[ii]<- ( sum(pr*xx[,1]*xx[,2]) - sum(pr*xx[,1])*sum(pr*xx[,2]) )/
                ( sqrt( sum(pr*(xx[,1]^2))-sum(pr*xx[,1])^2 ) *sqrt( sum(pr*(xx[,2]^2))-sum(pr*xx[,2])^2) )
  } 
  return(boots)
}


data.boot<-make.Boot(dat,nn = n,B=1000)-cor(dye,dfp)
data.bb<-make.BBoot(dat,nn=n,B=1000)-cor(dye,dfp)
hist(data.boot,breaks = seq(-10,10,0.01),col=rgb(0,0,1,0.3),xlim=c(-0.24,0.06),ylim=c(0,20),main="",ylab="",xlab="",probability = T)
par(new=T)
hist(data.bb,breaks = seq(-10,10,0.01),col=rgb(1,0,0,0.3),xlim=c(-0.24,0.06),ylim=c(0,20),ylab="",xlab="bias",probability = T,main="bootstrap and BB")
legend("topleft", legend = c("bootstrap","BB"), col = c(rgb(0,0,1,0.3),rgb(1,0,0,0.3)),lty=c(1,1))

summary(data.boot)
summary(data.bb)
