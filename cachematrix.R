## an overall description of what the 2 functions do
## makeCacheMatrix - this function creates a special "matrix" object that can cache its inverse
## cacheSolve - this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              if the inverse has already been calculated (and the matrix has not changed), 
##              then the cacheSolve should retrieve the inverse from the cache

## -------------------------------------------------------------------------------
## a short comment describing the function makeCacheMatrix
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse
## -------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv , 
       getinv = getinv)
}



## --------------------------------------------------------------------------------
## a short comment describing this function - cacheSolve
## the following function calculates the inverse of the special "vector" created with the above function. 
## if the inverse has already been calculated, it gets the inverse from the cache and skips the computation.
## assume that the matrix supplied is always invertible.
## --------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' input to above function makeCacheMatrix
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
