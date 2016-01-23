## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#This functionis to create a special "matrix" and return list containing 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the inverse matrix of the matrix
#4.get the inverse matrix of the matrix

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse

  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Return a matrix that is the inverse of 'x'
#This function is to calculates the inverse matrix of the special "matrix"
#created with the above makeCacheMatrix function.It will first check to see if the inverse 
#has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
#if not,  it calculates the inverse of the data and sets the value of the inverse.  
 
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
