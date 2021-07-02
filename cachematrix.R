## These functions were written as part of the Coursera course on R programming
## As part of the Week 3 assignment. 

## This function creates an object of the class matrix, which is able to create a cache of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  find <- function() x ## I have replaced 'get' in the example code with 'find' as 'get' is a base R function. 'find' finds the value of the vector
  setInverse <- function(inverse) inv <<- inverse ## this sets the value of the inverse
  findInverse <- function() inv ## this finds the value of the inverse
  list(set = set, find = find, 
       setInverse = setInverse,
       findInverse = findInverse)
}


## This function retrieves the inverse of the matrix produced by the above function. If the inverse has already been calculated,
## the inverse will be retrieved from the cache instead, and a message will be displayed to allow the user to recognise that the
## cached matrix is being retrieved instead of being calculated again. 

cacheSolve <- function(x, ...) {
  inv <- x$findInverse()
  if(!is.null(inv)) {
    message('retrieving cached data...')
    return(inv)
  }
  data <- x$find()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
