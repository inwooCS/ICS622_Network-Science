######################################################################
# Utility - Find top n vertices on some values 
# Dan Suthers 
# Last edit Mon Mar  2 2020 added stopnv 
######################################################################

topnv <- function(graph, values, n=10) {
  return(V(graph)[sort.list(values, decreasing=TRUE)[1:n]])
}

# Stable sort version of the same 
stopnv <- function(graph, values, n=10) {
  return(V(graph)[order(values, decreasing=TRUE, method="radix")[1:n]])
}

######################################################################
# Pau 