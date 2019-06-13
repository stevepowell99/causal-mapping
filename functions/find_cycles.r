

# https://stackoverflow.com/questions/31034730/graph-analysis-identify-loop-paths

library(igraph)

# breadth first search of paths and unique loops
get_loops <- function(adj, paths, maxlen){
  # tracking the actual path length:
  maxlen <- maxlen - 1
  nxt_paths <- list()
  # iterating over all paths:
  for(path in paths$paths){
    # iterating neighbors of the last vertex in the path:
    for(nxt in adj[[path[length(path)]]]){
      # attaching the next vertex to the path:
      nxt_path <- c(path, nxt)
      if(path[1] == nxt & min(path) == nxt){
        # the next vertex is the starting vertex, we found a loop
        # we keep the loop only if the starting vertex has the 
        # lowest vertex id, to avoid having the same loops 
        # more than once
        paths$loops <- c(paths$loops, list(nxt_path))
        # if you don't need the starting vertex included 
        # at the end:
        # paths$loops <- c(paths$loops, list(path))
      }else if(!(nxt %in% path)){
        # keep the path only if we don't create 
        # an internal loop in the path
        nxt_paths <- c(nxt_paths, list(nxt_path))
      }
    }
  }
  # paths grown by one step:
  paths$paths <- nxt_paths
  if(maxlen == 0){
    # the final return when maximum search length reached
    return(paths)
  }else{
    # recursive return, to grow paths further
    return(get_loops(adj, paths, maxlen))
  }
}

adj <- list()
loops <- list()
# the maximum length to limit computation time on large graphs
# maximum could be vcount(graph), but that might take for ages
maxlen <- 4
# 
# g <- erdos.renyi.game(n = 100, p.or.m = 0.04)
# # creating an adjacency list
# for(v in V(g)){
#   # for directed graphs use the 'mode' argument of neighbors() 
#   # according to your needs ('in', 'out' or 'all')
#   adj[[as.numeric(v)]] <- neighbors(g, v)
# }


find_cycles <- function(adj){
  for(start in seq(length(adj))){
    loops <- c(loops, get_loops(adj, list(paths = list(c(start)), 
      loops = list()), maxlen)$loops)
    
  }
  loops
  
}

# get_loops_all(g)
# recursive search of loops 
# for each vertex as candidate starting point

# create_notable()


