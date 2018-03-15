#Evaluatioon writing
evaDef<-function(target,optimized = NULL,comfortable = NULL,endurable = NULL,limit = NULL){
  if(!is.character(target)) target <- as.character(deparse(substitute(target)))
  assign(paste(target,'eva',sep = ''),{
         function(x,optimized = optimized,
                  comfortable = comfortable,
                  endurable = endurable,
                  limit = limit){
           if(x>optimized[1] & x<optimized[2]){
             return("OPTIMIZED")
           }
           if(x>comfortable[1] & x <comfortable[2]){
             return("COMFORTABLE")
           }
           if(x>endurable[1] & x<endurable[2]){
             return("ENDURABLE")
           }
           if(x>limit[1] & x<limit[2]){
             return("VALID")
           }else{
             return("INVALID")
           }
         }
  },
         envir = .GlobalEnv)
}


#evaDef("eyeTilt",optimized = c(0,-15),comfortable = c(5,-30),endurable = c(25,-35),limit= c(25,-45))
