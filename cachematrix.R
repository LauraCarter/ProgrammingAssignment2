## These functions cache the inverse of a matrix, so that it can be retrieved if it has previously been calculated

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## cacheSolve: computes the inverse of the special "matrix" returned by MakeCacheMatrix
## if f the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...) ## inverts the matrix
      x$setsolve(m)
      m ## Return a matrix that is the inverse of 'x'
}