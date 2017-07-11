#' Anslab matlab scripts
#' @keywords internal
maxdist <- function(mt, mv, mingap){
        diffmt <- diff(mt)
        for(i in 1:(length(mt)-1)){
                if(diffmt[i] < mingap){
                        if(mv[i] < mv[i + 1]){
                                mt[i] <- 0
                                i <- i + 1
                        }else{
                                mt[i+1] <- 0
                                i <- i + 1
                        }
                }


        }
        n <- which(mt==0)
        return(n)
}

