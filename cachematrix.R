## 
## This is PA2 for R programming course https://class.coursera.org/rprog-016/ in Dec2014
##
## Author: hannu.pahkala@outlook.com
## Date: 2014-12-13
## Version: v0.1
## 
###########################################################################################
##
## This file implements an object which stores matrix and its inverse
## The following functions are defined:-
##    makeCacheMatrix()
##        -- creates a special "matrix object" that can store both the matrix and its inverse
##    cacheSolve()
##        -- computes the inverse of the special "matrix" created by makeCacheMatrix 
##    set() 
##        -- sets a value for a matrix
##    get() 
##        -- returns the stored matrix
##    getinv() 
##        -- returns the inverse of a stored matrix
##    setinv() 
##        --this is private function and should not be called

makeCacheMatrix <- function(x = matrix()) {
  
  # Initial value for matrix inverse
  mInv <- NULL
  
  # set() 
  #
  # Sets a new value for matrix and clears (possible) previous inverse
  # Called:  makeCacheMatrix(m)
  # Preconditions:
  #  -- class(m) == "matrix"
  #  -- ncol(m) == nrow(m)
  set <- function(y) {
    
    # Tests if input is a matrix. If not,stop execution
    if (!is.matrix(y)) {
      stop("Not a matrix input")
    }
    
    x <<- y
    mInv <<- NULL
  }
  
  ## get() 
  ##
  ## Returns stored matrix
  ## Called:  myMatrix$get()
  get <- function() x

  ## getinv() 
  ##
  ## Gets the stored inverse
  ## Called:  myMatrix$get()
  getinv <- function() mInv
  
  ## setinv() 
  ## NOTE: this is a private function which should not be called
  ## Assumes that the given value is truly an inverse of stored matrix and stores the given value
  setinv <- function(givenInv) mInv <<- givenInv
  
  ## Creates a list of function references
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve() 
##
## Calculates the inverse of a stored matrix. Stores and returns the inverse
## Called:  cacheSolve()
cacheSolve <- function(x, ...) {
  
  # Read inverse and test if it is already calculated.
  mInv <- x$getinv()
  if (!is.null(mInv)) {
    # Inverse exists and it is returned
    print("from cache");
    return(mInv)
  } 
  else {
    # Inverse does not exist. It is calculated, stored and returned
    storedMatrix <- x$get();
    mInv <- solve(storedMatrix, ...);
    x$setinv(mInv);  # Inverse is stored for further use
    return(mInv)
  }
}
