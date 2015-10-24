#  The function saves in a list the inverse of the matrix entered as parameter.
#  Made by Gonzalo Cifuentes!!!

#  This function creates a special "matrix-list" object that can cache its inverse.

makeCacheMatrix <- function (M = matrix()){
  inv <- NULL
  set <- function(y){
    M <<- y
    inv <<- NULL
  }
  get <- function() M
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get=get, setinv= setinv, getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(M,...){
  inv <- M$getinv()
  #Check if inv has been calculated before
  if (!is.null(inv)){
    message("getting inverse")
    return(inv)
  }
  mat <- M$get()
  inv <- solve(mat)
  M$setinv(inv)
  inv
}
