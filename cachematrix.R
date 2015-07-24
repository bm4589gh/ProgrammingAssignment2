#################################
## Assignment 2: Caching the Inverse of a Matrix
## Barry Moore  7/20/2015
## R Programming - Following sample provided in class for numbers and changing
## it for matrix's using commands from the class and command examples from 
## online tutorials.
#################################
## These two functions are part of completing the second programming assignment 
## for the R Programming course offered on Coursera through the Johns Hopkins
## School for Public Health.
##
## The two functions shown here help in caching the inverse of a matrix. 
## Matrix inversion is usually very computationally intensive - especially for 
## large size matrices. Sometimes in code (and especially in loops), the 
## inverse of a matrix need only be computed once. To avoid recomputing the 
## inverse and generating the same result repeatedly, we can simply compute the
## result once. If you try to recompute the inverse again, we have already 
## computed this already and so we should just return this pre-computed result.
################################
## Example Run using this code.
## > mat <- matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3)
## > matc <- makeCacheMatrix(mat)
## > matc$getmat()
##      [,1] [,2] [,3]
## [1,]    1    0    5
## [2,]    2    1    6
## [3,]    3    4    0
## > matc$getinvmat()
## NULL
## > cacheSolve(matc)
## Informational: Creating Inverse Matrix.
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## > matc$getinvmat()
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
## > cacheSolve(matc)
## Success: Reading Inverse Matrix from cache.
##      [,1] [,2] [,3]
## [1,]  -24   20   -5
## [2,]   18  -15    4
## [3,]    5   -4    1
####################################

## 1) makeCacheMatrix: To facilitate this caching, you first create a special 
## matrix that will help us with this by using the makeCacheMatrix function. 
## The input into this function is simply a variable of type matrix.

makeCacheMatrix <- function(mat = matrix()) {
  ## This functions sets/gets a matrix and sets/gets the inverse of a matrix
  ## mat = Matrix, invmat = Inverse Matrix, matvar = passed in matrix
  
  ## Initialize variable
  invmat <- NULL
  
  ## set the matrix and reset the inverse matrix
  setmat <- function(matvar) {
    ## Note use of << indicates saving variable in another environment to reuse.
    mat <<- matvar
    invmat <<- NULL
  }
  
  ## get the matrix
  getmat <- function() mat
  
  # Manually set the inverse
  setinvmat <- function(invmat) invmat <<- invmat
  
  # Get the inverse
  getinvmat <- function() invmat
  
  # Encapsulate into a list
  list(setmat = setmat, getmat = getmat, setinvmat = setinvmat, getinvmat = getinvmat) 
}


## 2) cacheSolve: This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already been 
## calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(mat, ...) {
  ## Return the inverse matrix from cache, if available, or calculate it new.
  
  ## Read the cache, so we can use it, if available.
  invmat <- mat$getinvmat()
  
  ## Check the cache value. 
  ## If available, use it, show success message and return the inverse matrix.
  ## If not available, continue to calculate and return inverse matrix.
  if(!is.null(invmat)) {
    message("Success: Reading Inverse Matrix from cache.")
    return(invmat)
  }
  
  ## Load the matrix from the calling parameter.
  matvar <- mat$getmat()
  
  ## Use the Solve function to set the inverse of the matrix.
  invmat <- solve(matvar, ...)
  
  ## Set the Inverse Matrix into cache.
  mat$setinvmat(invmat)
  
  ## Return this newly calculated Inverse Matrix.
  message("Informational: Creating Inverse Matrix.")
  invmat
}
