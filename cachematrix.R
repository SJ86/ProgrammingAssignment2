## Code comprising a pair of functions that cache the inverse of a matrix

## The first function, makeCacheMatrix creates a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() ## calling function 'getinv' to assign cached inverse. Will be null if inverse has not been calculated
  if(!is.null(i)) { ##checking if the assigned matrix is inverse of the matrix has been calculated
    message("getting cached data")
    return(i) ## returning cached inverse of the matrix
  }
  data <- x$get() ## calling function 'get' of get the matrix
  i <- solve(data) ## computing inverse of the matrix
  x$setinv(i) ## calling function 'setinv' to cache inverse of the matrix
  i
}

## End
