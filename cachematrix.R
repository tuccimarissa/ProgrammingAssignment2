## Pair of functions ('makeCacheMatrix' and 'cacheSolve')
## to cache and compute the inversion of a matrix
## rather than computing the matrix inversion repeatedly

## Caches the inverse of a given matrix and makes available a list to:
  ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of the inverse
  ## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setiv <- function(inverse) iv <<- inverse
  getiv <- function() iv
  list(set = set, get = get,
       setiv = setiv,
       getiv = getiv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## Then cacheSolve should retrieve the inverse from the cache.
## Else the function returns a matrix that is the inverse of 'x' using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  iv <- x$getiv()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setiv(iv)
  iv
  
}