predict.elnet=function(object,newx,s=NULL,type=c("link","response","coefficients","nonzero"),exact=FALSE,offset,...){
  # Provision for one-row prediction on numeric
  if (!missing(newx) && class(newx) != "matrix") {
    newx <- t(as.matrix(newx))
  }
  NextMethod("predict")
} 
