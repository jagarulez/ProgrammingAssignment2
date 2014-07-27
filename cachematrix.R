## Matrix inversion is usually a costly computation and there 
##may be some benefit to caching the inverse of a matrix rather 
##than compute it repeatedly. There are also alternatives to matrix inversion that we will not discuss here. 
##This assignment is to write a pair of functions that cache the inverse of a matrix.



##This function creates a special "matrix" object that can cache its inverse
  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) m <<- solve
  getinvers <- function() m
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvers()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvers(m)
  m
}
