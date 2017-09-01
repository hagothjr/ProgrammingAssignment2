## create the test matrix to be used in the assignment
test = matrix( 
  c(2, 4, 3, 1, 5, 7,9,10,4), # the data elements 
  nrow=3,              # number of rows 
  ncol=3,              # number of columns 
  byrow = TRUE) 

## Coursera R Programming Week 3 Assignment 2

## The makeCacheMatrix function recieves a matrix as input
## it then creates a new special version of that matrix
## object that is capable of caching its own inverse 


makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    Minv <<- NULL
  }
  get <- function() x
  setInverse <- function(findinverse) minv <<- findinverse
  getInverse <- function() minv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function accpets as input the special version of
## a matrix which is created by the makeChacheMatrix function.
## It calculates the inverse of this matrix. If the inverse has 
## already been stored in cache and if the original matrix has not
## been altered then the inverse is imply retreivced from cache.
## If this is not the case then the inverse is calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getInverse()
  if (!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  mat <- x$get()
  minv <- solve(mat, ...)
  x$setInverse(minv)
  minv
}


cacheSolve(makeCacheMatrix(test))  ## test the functions using test matrix
