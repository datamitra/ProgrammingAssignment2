## A pair of functions that cache the inverse of a matrix
## so that the inverse of a matrix does not need to be computed
## if the inverse is already cached

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##initialize inverse
  inv_matrix <- NULL
  
  ## function to set variables to environments
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  ## function to get matrix
  get <- function() {
    x
  }
  
  ## function to set inverse matrix
  setinverse <- function(inverse) {
    inv_matrix <<- inverse
  }
  
  ## function to get inverse matrix
  getinverse <- function() {
    inv_matrix
  }
  
  ## return list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## return a matrix that is the inverse of 'm'
  
  inv_matrix <- x$getinverse()
  
  ## if inverse is available already, then return the inverse without computing again
  if( !is.null(inv_matrix) ) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  ## otherwise get the matrix from matrix object
  data <- x$get()
  
  ## then calculate the inverse solve()
  message("inverse of matrix computed")
  inv_matrix <- solve(data) 
  
  ## set the inverse to the matrix object
  x$setinverse(inv_matrix)
  
  ## Return the matrix
  inv_matrix
}

## code to test the functions
## x = rbind(c(-10,0.1), c(-0.1, 10))
## > m = makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
## [1,] -10.0  0.1
## [2,]  -0.1 10.0

## when called first time, matrix computed
## > cacheSolve(m)
## inverse of matrix computed
##            [,1]      [,2]
## [1,] -0.1000100 0.0010001
## [2,] -0.0010001 0.1000100


## getting from the cache without doing the computation in the second run
## > cacheSolve(m)
## getting cached data.
##            [,1]      [,2]
## [1,] -0.1000100 0.0010001
## [2,] -0.0010001 0.1000100

## > 
