## Allows program to calculate the inverse of a matrix only once.
## It will check to see if values are stored in cache.
## If so it returns the previously calculated inverse.
## If not, it will calculate the inverse, store it, and return it.


## This function creates a special "matrix" object that contains
## the functions used to cache the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)  {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

