

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param climate 
##' @param latitude 
##' @param bPlot 
##' @param b3plot 
##' @param main 
##' @param negative 
##' @param clab 
##' @param polynom.order  order of the polynom, 2 or 3
##' @return 
##' @author Thomas Laepple 
AnalyseTransferfunction<-function(climate,latitude,bPlot=T,b3plot=T,main="",negative=FALSE,clab="",polynom.order=3, bPlotLin=FALSE,bLinPlot=FALSE,col1="cyan")
{
	a.matrix<-matrix(NA,365,365)  
	#Insolation 
	insol<-daily_insolation(0,latitude,1:365)$Fsw
        if (negative) insol=(-1*insol)
	ins.1<-insol
	ins.2<-insol^2
	ins.3<-insol^3
        ins.4<-insol^4

        t<-NULL
        
	#daily temperature data
        a<-climate
	basis<-rep(a,2)

	for (iopt in 1:365) a.matrix[,iopt]<-basis[iopt:(iopt+364)]  

        if (polynom.order==3)
            {
                t<-lm(a.matrix~ins.1+ins.2+ins.3)
               
                

                ul<-max(insol)*t$coefficients[2,]+max(insol)^2*t$coefficients[3,]+max(insol)^3*t$coefficients[4,]
                ll<-min(insol)*t$coefficients[2,]+min(insol)^2*t$coefficients[3,]+min(insol)^3*t$coefficients[4,]

            } else
                {
                    t<-lm(a.matrix~ins.1+ins.2)
                      ul<-max(insol)*t$coefficients[2,]+max(insol)^2*t$coefficients[3,]
                          ll<-min(insol)*t$coefficients[2,]+min(insol)^2*t$coefficients[3,]
                }

                
      
        
	rmse<-colMeans(t$residuals^2)						 

	index.neg<-((ul-ll)<=0)
	rmse[index.neg]<-1e6
	bestfit.index<-which.min(rmse)

	coeff<-t$coefficients[,bestfit.index]

	
	t.lin<-lm(a.matrix[,bestfit.index]~ins.1)
     
        t.p2<-lm(a.matrix[,bestfit.index]~ins.1+ins.2)
           t.p3<-lm(a.matrix[,bestfit.index]~ins.1+ins.2+ins.3)
         t.p4<-lm(a.matrix[,bestfit.index]~ins.1+ins.2+ins.3+ins.4)

        coeff.lin<-t.lin$coeff

	
############# Plotting routine
if (bPlot)
{
	at<-basis
	at<-stats::filter(at,rep(1/20,20),circular=T)
	at.scale<-scale(at)
	insol.scale<-scale(rep(insol,2))
	
	

	at.sc<-attr(at.scale,"scaled:scale")
	at.offset<-attr(at.scale,"scaled:center")

	labels.at<-pretty(c(-3*at.sc+at.offset,3*at.sc+at.offset),5)
	at.at<-(labels.at-at.offset)/at.sc

	insol.sc<-attr(insol.scale,"scaled:scale")
	insol.offset<-attr(insol.scale,"scaled:center")

	labels.insol<-pretty(c(-3*insol.sc+insol.offset,3*insol.sc+insol.offset),5)
	labels.insol<-labels.insol[labels.insol>0]
	at.insol<-(labels.insol-insol.offset)/insol.sc

if (b3plot)
{
    if (bLinPlot)
        {
	par(mfrow=c(2,2))

	plot(at.scale,ylim=c(-2,2),axes=F,main=main,ylab=clab,xlab="day of year",type="l",lwd=2)
if (bPlotLin)	lines(insol.scale,col="blue",lwd=2,lty=2)

	
	axis(1,at=c(0,120,240,365,365+120,365+240,365*2),labels=c(0,120,240,0,120,240,365))
	box()
	axis(2,at=at.at,labels=labels.at)
	axis(4,at=at.insol,labels=labels.insol)
}

}
	############## Plot after cutting
	ylab="insolation (W/m²)"


        if (b3plot) {main<-"response function"}
	
	add<-diff(range(a.matrix))/10
	main<-""

	plot(ins.1,a.matrix[,bestfit.index],xlab="insolation (W/m²)",ylab=clab,main=main,ylim=range(a.matrix[,bestfit.index])+c(-add,add))
	lines(ins.1,t$fitted.values[,bestfit.index],col=col1,lwd=2)
        if (bPlotLin)	lines(ins.1,t.lin$fitted.values,col="blue",lwd=2)



        
	plot(at,ylim=c(-2*at.sc+at.offset,2*at.sc+at.offset),ylab=clab,xlab="day of year",type="l",lwd=2,axes=F)
	
	axis(1,at=c(0,120,240,365,365+120,365+240,365*2),labels=c(0,120,240,0,120,240,365))
	box()
	axis(2)
	
	
	insol.scale<-rep(t.lin$fitted.values,2)
	iopt<-365-1*bestfit.index
	insol.scale<-rep(insol.scale[iopt:(iopt+364)],2)
if (bPlotLin)	lines(insol.scale,col="blue",lwd=2,lty=1)


	insol.scale<-rep(t$fitted.values[,bestfit.index],2)
	iopt<-365-1*bestfit.index
	insol.scale<-rep(insol.scale[iopt:(iopt+364)],2)
       lines(insol.scale,col=col1,lwd=2)




	

}



	rsq1<-cor(a.matrix[,bestfit.index],ins.1)^2
	rsq2<-cor(a.matrix[,bestfit.index],t$fitted.values[,bestfit.index])^2
	coeff.lin<-lm(a.matrix[,bestfit.index]~ins.1)$coeff

	
	fitval<-rep(t$fitted.values[,bestfit.index],3)[(367-bestfit.index):(367-bestfit.index+364)]

	#RSQ1 ist RSQ eines linearen Fits, RSQ2 der Fit des Polynoms
	return(list(coeff=coeff,coeff.lin=coeff.lin,rsq=c(rsq1,rsq2),lag=bestfit.index,fitted=fitval,negative=negative,models=list(m1=t.lin,m2=t.p2,m3=t.p3,m4=t.p4)))

	

}



