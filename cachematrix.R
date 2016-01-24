## makeCacheMatrix() function assigns a matrix to the function
## that is accessible to other functions. cacheSolve() function
## calls the matrix in makeCacheMatrix(), checks to see if the
## inverse has already been calculated and  if not calculates
## the inverse of the matrix, saving the information back into
## makeCacheMatrix function.

## Generates a list with 4 functions, 1) allows a matrix to be set,
## 2) calls the matrix, 3) sets the inverse, 4) retrieve an
## already calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve() has to be used in conjunction with makeCacheMatrix(). 
## Checks if the inverse has already been calculated, if it has,
## returns that solution. Otherwise, it calculates the inverse
## matrix and assigns it using $setinverse() to makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
