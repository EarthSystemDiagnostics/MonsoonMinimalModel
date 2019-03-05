
clusterPlotCMeans<-function(field,timesteps,NCLUSTER=3)
#Plot cluster and timeseries
{
    q<-unclass(scale(unclass(field)))
    index<-!is.na(q[1,])
    result<-cmeans(t(q[,index]),NCLUSTER,iter.max=100)

    n<-pField(NA,1:NCLUSTER,getlat(field),getlon(field))
    for (i in 1:NCLUSTER) n[i,index]<-result$membership[,i]
    par(mfrow=c(2,3))
    for (i in 1:NCLUSTER) plotmap.square(n[i,],xlab="",ssub="",col=rbow(20),legend=FALSE,bGrid=TRUE,zlim=c(0,1))
    cent<-result$centers

plot(timesteps,cent[1,],type="l",ylim=c(-2,2),col="blue",lwd=3,xlab="kyr BP",ylab="sd precip in cluster")
    for (i in 2:NCLUSTER) plot(timesteps,cent[i,],type="l",ylim=c(-2,2),col=i,lwd=3,xlab="kyr BP",ylab="sd precip in cluster")
    return(n)
}
