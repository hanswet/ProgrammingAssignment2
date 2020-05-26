#this set of functions saves time by caching the information, 
# to be called upon at a later time. 

makeCacheMatrix <- function(x = matrix()) {
  J <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#this function explains te formation of the cache matrix. This includes
# inverse functions, where j is the null vector. 


cacheSolve <- function(x, ...) {
  y <<- x
  j <- x$getInverse()
  if(!is.null(j)){
    message("receiving cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
}
#this code forms the cachesolve with a filler funtion, which 
#includes the character vector to form the illogical function, 
#"if" function, and the "return" function. 