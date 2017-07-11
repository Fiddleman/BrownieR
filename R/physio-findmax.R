#' Anslab matlab scripts
#' @keywords internal
findmax <- function(y, area, back, rise, mingap = NULL, forward = NULL, rise2 = NULL){

        nargin <- 0
        if(is.null(rise2)){
                rise2 <- 0
                nargin <- nargin + 1
        }
        if(is.null(forward)){
                forward <- 0
                nargin <- nargin + 1
        }
        if(is.null(mingap)){
                mingap <- area
                nargin <- nargin + 1
        }

        nargin <- 7 - nargin


        #*** Partition y into windows
        y <- c(y)
        len  <-  length(y) # fill y with 0 to a length of multiples of area
        zer <- area - (len %% area)
        if(len %% area == 0){zer <- 0}
        y[(len + 1):(len + zer)] <- rep(0, zer)

        Y <- matrix(y, nrow = area, ncol = (len + zer)/area)
        mt <- apply(Y, 2, which.max)
        mv <- apply(Y, 2, max)

        #*** 'Reshape' maxtime-vector
        mt <- mt + (0:(length(mt) - 1)) * area
        lenm <- length(mt)
        if(mt[lenm] > len){
                mv <- mv[-lenm]
                mt <- mt[-lenm]
        }

        #*** Choose bigger one of two close max
        # find indices of max that are too close, n: indices of smaller values
        n <- maxdist(mt,mv,mingap)
        mt <- mt[-n]
        mv <- mv[-n]

        if(nargin > 2 & length(mt)>0){
                if(mt[1] <= back){
                        mt <- mt[-1]
                        mv <- mv[-1]
                }
                #*** Check for general slope backward
                slope <- mv - y[mt-1]
                n <- (slope >= 0)
                mt <- mt[n]
                mv <- mv[n]

                if(length(mt) > 0){
                        #*** Check for general slope forward
                        i <- length(mt)
                        if(mt[i]>=(len-1)){
                                mt <- mt[-i]
                                mv <- mv[-i]
                        }
                        slope <- mv - y[mt + 1]
                        n <- (slope >= 0)
                        mt <- mt[n]
                        mv <- mv[n]

                        #*** Check for valid slope backward
                        slope <- mv - y[mt - back]
                        n <- (slope > rise)
                        mt <- mt[n]
                        mv <- mv[n]

                        if(nargin>5){
                                if(forward > 0){
                                        i <- length(mt)
                                        if(mt[i] >= len - forward) {
                                                mt <- mt[-i]
                                                mv <- mv[-i]
                                        }

                                        #*** Check for valid slope forward
                                        slope <- mv - y[mt + forward]
                                        n <- (slope > rise2)
                                        mt <- mt[n]
                                        mv <- mv[n]
                                }
                        }

                }
        }

        return(list(mt = mt, mv = mv))

}
