#'@title Implement the Euclidean algorithm
#'
#'@param x A number.
#'@param y A number.
#'@description The greatest common divisor of \code{x} and \code{y}.
#'
#'@references More information of euclidean algorithm \href{http://en.wikipedia.org/wiki/Euclidean}{here}
#'    

#1.1.1 Euclidean

euclidean <- function (x, y){
  #Stop condition
  stopifnot(length(x) == 1, length(y) == 1, is.numeric(x) == TRUE, is.numeric(y) == TRUE)
  
  #Code starts here
  minmax <- c(x,y)
  mini <- c()
  maxi <- c()
  i = 1
  maxi[i] <- minmax[which.max(minmax)]
  mini[i] <- minmax[which.min(minmax)]
  while(i < i+1){
    a <- trunc(maxi[i]/mini[i])
    maxi[i + 1] <- mini[i]
    mini[i + 1] <- maxi[i] - (a * mini[i])
    if(mini[i + 1] <= 0){
      gcd <- mini[i]
      break()
    }
    i <- i + 1
  }
  return(gcd)
  }

#1.1.2 dijkstra

