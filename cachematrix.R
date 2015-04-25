## The following funcitons will cache the inverse of a given matrix

## makeCacheMatrix will create a list with the function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) m <<- inv
      getinverse <- function() m
      list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse=getinverse
        )
}


## This function will return the inverse of the matrix.  
# If the inverse has already been computed, it will get the result and skip the rest of the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

