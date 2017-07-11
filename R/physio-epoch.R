#' Anslab matlab scripts
#' @keywords internal
epoch <- function(x, y, points){
        valnan <- 0.50     # percent valid

        if (!all(is.nan(y))){
                # resample at end
                if(floor(points) != points){
                        if(floor(points*4)  != points*4){
                                print('Error: epoch length has to contain number of sample points that are integer if multiplied by 4.')
                                #c <- v    # produce error!
                        }

                        rsyes <- 1
                        pointsold <- points
                        points <- floor(points)
                }else{
                        rsyes <- 0
                }

                n <- which(!is.nan(x))
                if(length(n)){

                        i <- 1                      # remove leading NaNs from x
                        while(is.nan(x[i])){
                                x <- x[-i]
                                y <- y[-i]
                        }

                        i <- length(x)             # remove last NaNs from x
                        while(is.nan(x[i])){
                                x <- x[-i]
                                y <- y[-i]
                                i <- i-1
                        }



                        if(points == 1){
                                n <- rep(NaN, length(x))
                                for(i in 1:(length(x)-1)){
                                        n[x[i]:(x[i+1]-1)] <- y[i]*rep(1, length(x[i]:(x[i+1] - 1)))
                                        x1 <- n
                                }

                        }else{
                                dt <- diff(x)

                                # length of filled must be multiple of points
                                len <- trunc(x[length(x)] / points) * points + points
                                if((x[length(x)] %% points) == 0){
                                        len <- len - points
                                }

                                filled <- rep(NaN, len)
                                #filled(1,1:x(1)) <- ones(1,x(1))*NaN  end         # begin is not valid

                                lent <- length(x)
                                i <- 1
                                while(i < lent){
                                        if(!is.nan(dt[i])){
                                                # fill with values!
                                                # valid
                                                n <- (x[i] + 1):x[i+1]
                                                filled[round(n)] <- rep(1, length(n))  * y[i]
                                                i <- i + 1
                                        }else{# diff is NaN
                                                beg <- x[i] + 1
                                                i <- i + 1                        # remove following NaN
                                                while(is.nan(x(i))){
                                                        i <- i + 1
                                                }


                                        }

                                }

                                #if rem(x(length(x)),points)~ <- 0                         # end not valid
                                #  filled(1,x(length(x))+1:len) <- ones(1,len-x(length(x)))*NaN
                                #en

                                filled <- matrix(filled, points, len/points)

                                x1 <- colMeans(filled)                # mean, epochs with NaN -> mean = NaN
                                n <- which(is.nan(x1))                  # mean, epochs with NaN approximated
                                for(i in 1:length(n)){
                                        meancol <- filled[,n[i]]
                                        meancol <- meancol[which(!is.nan(meancol))]

                                        if(length(meancol)>= points * valnan){
                                                # how many  # valid points have to be
                                                x1[n[i]] <- mean(meancol)      # in one epoch so that this epoch
                                                # is valid (typical: 50 #  <- > j <- 0.5)
                                        }
                                }
                                #
                                if(rsyes){
                                        #         x1i <- ???ipfast(x1, points * 4)
                                        #         x1 <- ???decfast(x1i ,pointsold * 4)
                                        stop("Please check the source code!")
                                }


                        }




                }else{
                        print('only NaN in time vector')
                        x1 <- NaN
                }


        }else{
                print('only NaN values')
                l <- max(x, na.rm = TRUE)
                len <- ceiling(l/points)
                x1 <- rep(NaN, len)
                #if length(n)
        }

        return(x1)

}
