## Put comments here that give an overall description of what your
## functions do


#This function includes functions that create a "matrix" capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }



 
#This function comuptes the inverse of the matrix-type object returned by the makeCacheMatrix function.
#If this had already been solved and the matrix is unchanged, a cached value will be returned.
cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
  }
        ## Return a matrix that is the inverse of 'x'

