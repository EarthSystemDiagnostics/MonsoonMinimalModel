
##' @title Add wind vectors to a 2D plot
##' @param dataU 
##' @param dataV 
##' @param shift 
##' @param scale 
##' @param col 
##' @param l 
##' @return 
##' @author Thomas Laepple 
addwind <- function(dataU,dataV,shift=F,scale=10,col="black",l=0.2)
{
	tmpV<-plot.preparation(dataV,shift)
	tmpU<-plot.preparation(dataU,shift)

	quiver(tmpV$lon,tmpU$lat,tmpU$data,tmpV$data,scale=scale,col=col,length=l)
	quiver(tmpV$lon+360,tmpU$lat,tmpU$data,tmpV$data,scale=scale,col=col,length=l)
	quiver(tmpV$lon-360,tmpU$lat,tmpU$data,tmpV$data,scale=scale,col=col,length=l)




}

quiver <- function(lon,lat,u,v,scale=1,length=0.2,maxv=max(abs(na.omit(u)),abs(na.omit(v))), ...)
 # first stab at matlab's quiver in R
 # from http://tolstoy.newcastle.edu.au/R/help/01c/2711.html
 # Robin Hankin Tue 20 Nov 2001 - 13:10:28 EST
  {
    ypos <- lat[col(u)]
    xpos <- lon[row(u)]

    speed <- sqrt(u*u+v*v)

    u <- u*scale/maxv
    v <- v*scale/maxv


   # matplot(xpos,ypos,type="p",cex=0,xlab="lon",ylab="lat", ...)
    arrows(xpos,ypos,xpos+c(u),ypos+c(v),length=length*min(par.uin()), ...)

  }
