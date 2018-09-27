## R Programming assignment 2. 


## These functions store a matrix and its inverse, and then return the inversed matrix. 
## The inversed matrix is cached so that it does not have to be calculated if it has 
## already been calculated before. 


## The makeCacheMatrix funciton creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solveMatrix) inverse <<- solveMatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function returns the inverse of a square matrix (stored in makeCacheMatrix). 
## If the inverse of the matrix has already been computed, it gets the inverse matrix from the cache.
## If the inverse of the matrix has not already been computed, it computes the inverese matrix and returns it.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}


## Example of how the makeCacheMatrix and cacheSolve functions work:

## make a square invertible matrix [remove # to let it run]
#matrix <- matrix(c(1,1,0,0,1,1,1,0,1), nrow=3, ncol=3)

## store the matrix and it's inverse using makeCacheMatrix [remove # to let it run]
#storeMatrix <- makeCacheMatrix(matrix)

## return the matrix using cacheSolve [remove # to let it run]
#cacheSolve(storeMatrix)

## if we run it again, there is a not that says we are using a cached version (i.e. not calculated again) [remove # to let it run]
#cacheSolve(storeMatrix)
