## Module Description:
## Create a utility to cache the calculated value for a matrix inverse 
## the first time it is calculated and return the stored value for 
## every subsequent call to calculate the inverse. This avoids unnecessarily
## repeating the computationally intensive call to solve() when the matrix
## has not changed.
## The cacheable matrix is really a list containing utility functions and
## a stored matrix data.



## makeCacheMatrix
## Description:
## Utility function to create a new cacheable matrix from the matrix object
## Usage:
## makeCacheMatrix( x = matrix()) 
## Arguments:
## x      a matrix object to create a cacheable matrix from
## Details:
## creates a special cacheable "matrix", which is really a list containing 
## functions to
## 1. set the value of the matrix.
##    Usage: set(y) to update the cacheable matrix values
## 2. get the value of the matrix
##    Usage: x <- get() to retrieve the cacheable matrix values
## 3. set the value of the matrix inverse
##    Usage: setsolve(matrixInverse) to store the cached matrix inverse
## 4. get the value of the matrix inverse
##    Usage: getsolve() to retrieve the cached matrix inverse or NULL if not set
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the cached inverse to NULL when the cacheable matrix is created
  ## This will be updated as needed
  mInv <- NULL
  ## Create the set() function to update the data of the cacheable matrix
  ## y is the new matrix object use. Note: The cached matrix inverse must
  ## be set to NULL until the inverse of the new matrix is calculated.
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  ## Create the get() function to return the stored data object of the
  ## cacheable matrix
  get <- function() x
  ## Create the setsolve() function to set the cached matrix inverse value
  setsolve <- function(solveCalc) mInv <<- solveCalc
  ## Create the getsolve() function to return the cached matrix inverse value
  getsolve <- function() mInv
  ## Create a list of the helper functions for the cacheable matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



##
## cacheSolve
## Description:
## Calculates the inverse of a cacheable matrix. Value is cached and returned
## without recalculating on subsequent calls unless the cacheable matrix
## data has been updated. 
## Usage:
## cacheSolve( x, ...) 
## Arguments:
## x      The cacheable matrix object to calculate the inverse of
## ...    argument list to pass to the solve() function
## Details:
## Wrapper function for solve() to use with a cacheable matrix. The cacheable 
## matrix must be invertable (i.e. the determinant != 0) for the function to
## succeed. 
cacheSolve <- function(x, ...) {
  ## Get the cached inverse matrix for 'x'
  mInv <- x$getsolve()
  ## Check to see if the cached inverse has already been calculated
  if(!is.null(mInv)) {
    ## If the cached inverse has already been calculated, return the 
    ## existing value instead of recalculating
    message("getting cached data")
    return(mInv)
  }
  ## Get the matrix data object for the cacheable matrix 'x'
  data <- x$get()
  ## Calculate a matrix that is the inverse of 'x'
  mInv <- solve(data, ...)
  ## Set the the cached inverse for 'x'
  x$setsolve(mInv)
  ## Return the Inverse
  mInv
}

## Test code. 
## To run these functions testing the operation of caching the matrix inverse
## enter TestCacheMatrix <- TRUE prior to source("cachematrix.R")
if(exists("TestCacheMatrix")) {
  if (TestCacheMatrix) {
    ## Create an invertable matrix and pass to makeCacheMatrix()
    X <-  matrix(c(4, 2, 7, 6), 2, 2)
    matrixX <- makeCacheMatrix(X)
    ## Call the cacheSolve() function to get the inverse
    invX <- cacheSolve(matrixX)
    ## Print the inverse and matrix multiplying the original by the
    ## inverse.
    ## Expected invX:
    # [,1] [,2]
    # [1,]  0.6 -0.7
    # [2,] -0.2  0.4
    ## Expected X%*%invX: Identity matrix
    # [,1] [,2]
    # [1,]    1    0
    # [2,]    0    1
    print(X)
    print(invX)
    print(X %*% invX)
    
    ## Do the operation again. This time the call to cacheSolve()
    ## should print the message that it used the cached solution.
    invX <- cacheSolve(matrixX)
    print(X)
    print(invX)
    print(X %*% invX)
    
    ## Do the operation again but first update the matrix to a new value. 
    ## This time the call to cacheSolve() should recalculate the inverse
    ## using the new matrix instead of returning the cached inverse
    Y <-  matrix(c(2, 4, 6, 7), 2, 2)
    matrixX$set(Y)
    invY <- cacheSolve(matrixX)
    ## Print the inverse and matrix multiplying the original by the
    ## inverse.
    ## Expected invX:
    # [,1] [,2]
    # [1,] -0.7  0.6
    # [2,]  0.4 -0.2
    ## Expected X%*%invX: Identity matrix
    # [,1] [,2]
    # [1,]    1    0
    # [2,]    0    1    
    print(Y)
    print(invY)
    print(Y %*% invY)
  }
}