## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Takes a matrix and creates an inverse 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve(x)
  
  getsolve <- function() m

  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## Write a short comment describing this function
##Uses the result to determine if it should use a cached version or
## run a new inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }else{
    message("No cached value, calculating...")
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}


