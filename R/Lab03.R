#'@title Implement the Euclidean algorithm
#'
#'@param x A number.
#'@param y A number.
#'@description The greatest common divisor of \code{x} and \code{y}.
#'
#'@references More information of euclidean algorithm \href{http://en.wikipedia.org/wiki/Euclidean}{here}
#'@export

#1.1.1 Euclidean

euclidean <- function (x, y){
  #Stop condition
  stopifnot(length(x) == 1, length(y) == 1, is.numeric(x) == TRUE, is.numeric(y) == TRUE)
  
  #Code starts here
  minmax <- c(abs(x),abs(y))
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


#'@title Implement the dijkstra algorithm
#'
#'@param graph, A data frame with three variables, v1, v2 and w that contains the edges of the graph (from v1 to v2) with the weight of the edge (w)
#'@param init_node, A number.
#'@description The function returns the shortest path to every other node in \code{graph} from \code{init_node}.
#'
#'@references More information of dijkstra algorithm \href{https://en.wikipedia.org/wiki/Dijkstra's algorithm}{here}
#'@export

#1.1.2 dijkstra

dijkstra <- function(graph, init_node){
  stopifnot(length(init_node) == 1, is.numeric(init_node) == TRUE, is.data.frame(graph) == TRUE, length(graph[1,]) == 3, names(graph) == c("v1", "v2", "w"), is.numeric(graph[,1]), is.numeric(graph[,2]), is.numeric(graph[,3]), init_node %in% unique(graph$v1))
  Q <- c()
  dist <- c()
  prev <- c()
  
  for (i in 1:length(unique(graph[[1]]))){
    dist[i] <- Inf
    prev[i] <- NA
    Q[i] <- unique(graph[[1]])[i]
  }
  
  dist[init_node] <- 0
  
  #Init node closest neighbor
  while (length(Q) > 1){
    u <- Q[which.min(dist[Q])]
    Q <- Q[Q!=u]
    for(i in 1:sum(graph[graph$v1 == u,]$v2 %in% Q)){
      alt <- dist[u] + graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$w[i]
      if (alt < dist[graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$v2[i]]){
        dist[graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$v2[i]] <- alt
        prev[graph[graph$v1 == u,][graph[graph$v1 == u,]$v2 %in% Q,]$v2[i]] <- u
      }
    }
  }
  return (dist)
}

#' Distances w between 6 nodes v.
#'
#' A dataset containing the pathway showed on top of the wikipedia page from Dijkstra's algorithm
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{point 1}
#'   \item{v2}{point 2}
#'   \item{w}{weight or distance from point 1 to point 2}
#' }
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra's algorithm}
"wiki_graph"