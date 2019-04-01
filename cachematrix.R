## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.

##  So, we are creating a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y){
  x <<- y
  inv_m <<- NULL
  }
  get <- function() x
  setinv <- function(solveMatrix) inv_m <<- solveMatrix
  getinv <- function() inv_m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinv()
  if(!is.null(inv_m)){
      message("getting cached data")
      return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data)
  x$setinv(inv_m)
  inv_m
}
