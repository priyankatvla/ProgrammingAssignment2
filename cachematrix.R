## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  
      inv <- NULL
      ##set function to set matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ##get function to get matrix
    get <- function() x
    ##set inverse using solve function
    setinv <- function(solve) inv <<- solve
    ##get inverse
    getinv <- function() inv
    ##make a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  

}


## caching inverse of matrix

cacheSolve <- function(x, ...) {
  ##check current inverse to see if already calculated
      inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
    }
      ##get matrix and calculate inverse if not already present
    data <- x$get()
    inv <- solve(data, ...)
    ##cache the inverse
    x$setinv(inv)
    inv
  
}
