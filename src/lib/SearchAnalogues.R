##' .. content for \description{} (no empty lines) ..
##'
##' If more than NAnalogue are found give the first NAnalogue best matching
##' ones are returned
##' @title 
##' @param value 
##' @param data 
##' @param tolerance 
##' @param NAnalogue Maximum number of analogues
##' @return index (1D or 2D) of analogues, NA if no analogue could be found
##' @author Thomas Laepple 
SearchAnalogues<-function(value,data,tolerance=5,NAnalogue=200)
    {
        
        difference<-abs(value-data)
        d<-sort(difference,index.return=TRUE)
        if (is.null(dim(data)))
            { result<-d$ix[d$x<=tolerance]
              if (length(result)>NAnalogue) result<-result[1:NAnalogue]
          } else
              {
                  result<-arrayInd(d$ix[d$x<=tolerance],dim(data))
                  if (dim(result)[1] > NAnalogue)  result<-result[1:NAnalogue,]
              }
        return(result)
    }


