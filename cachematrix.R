## makeCacheMatrix will create a list, which contains a function that will:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inverse
## 4. Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invr <<- inverse
  getInverse <- function() invr
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will compute the inverse of the matrix created
##    with makeCacheMatrix.
## If the inverse has already been calculated, and remains unchanged,
##    then cacheSolve will retrieved the cached inverse.

cacheSolve <- function(x, ...) {
  invr <- x$getInverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setInverse(invr)
  invr
}