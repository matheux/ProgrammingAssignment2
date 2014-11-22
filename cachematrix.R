## Below functions are meant to be used to calculate the inverse of a matrix
## Because it might be a time consuming operation (if the matrix is big), one might
## not want to do this twice for the same matrix. Those function allow this.

## Creates a special "matrix"-like object that caches its inverse, when its calculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of a matrix. If it has already been calculated, then
## the function would retrieve the inverse from cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
