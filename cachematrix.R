## The two functions that I have created, makeCacheMatrix and cachesolve, work together to alleviate the computational price of matrix inversion.

## makeCacheMatrix permits the storage of the inverse of the matrix it creates. It is composed of a list of functions and the objects s and x, which are able to be used beyond the functions.    

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInv <- function(solve) s <<- solve
  getInv <- function() s
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cachesolve will first determine if the inverse of the matrix has been cached, and that being the case, will then retrieve it. If it's not the case, it will then compute the inverse and retreive it.  

cacheSolve <- function(x, ...) {
  s <- x$getInv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setInv(s)
  s
}
