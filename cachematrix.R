## The following functions can return the inverse of a matrix. If the matrix has
## be calcualted, it will direclty read it from the cache. If not, it will calculate
## the inverse matrix and store the inverse matrix in cache. 

## This function, 'makeCacheMatrix creates a special "matrix", which is really a list containg a function to
### 1. set the value of the vector
### 2. get the value of the vector
### 3. set the value of the mean
### 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function() m
  list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}

## This function 'cacheSolve' calculates the inverse matrix of the special "matrix"
## created with the above function. However, it first checks to see if the inverse matrix has
## already been calcualted. If so, it 'get's the inverse matrix from the cache and skips the
## computation. Otherwise, it calcualtes the inverse matrix of the data and sets the value of
## inverse matrix in the cache via th 'setinversematrix' fuction.
cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.nll(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
