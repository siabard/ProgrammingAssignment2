## Programming Assignment 2
## R-Programming
## Original Author :  Roger D. Peng <rdpeng@gmail.com>
## Forked By : Yeonho Jang <siabard@gmail.com>

## creates a special vector containing functions

makeCacheMatrix <- function(x = matrix()) {
  # set the value of the vector
  inv <- NULL
  src <- x
  set <- function( mA ) {
    src <<- mA
    inv <- NULL
  }
  # get the value of the matrix
  
  get <- function() src
  
  # set the value of the invert of given matrix
  
  setInv <- function( mInv ) inv <<- mInv
  
  # get the value of the invert of given matrix
  getInv <- function() inv
  
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## TEST case
# A = matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0) , nrow=3, ncol=3, byrow=TRUE)
# invA = solve(A)
# cA = makeCacheMatrix(A)
# cInvA = cacheSolve(A)

## should be all true matrix
# invA == cA$getInv()
