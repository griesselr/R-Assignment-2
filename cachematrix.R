## This function aims to create a matrix that is cacheable

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  matrix(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function aims to check if there is a cached inverse of the given matrix
## if there is not a cached matrix, this function will return the inverse by making use of the solve() function


cacheSolve <- function(x,...) { 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(x)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
