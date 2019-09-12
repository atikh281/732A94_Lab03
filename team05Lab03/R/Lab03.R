name = c("Sara", "Atieh")


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

