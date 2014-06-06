## Title:  Inverse Matrix Cache Function
##         for Programming Assignment 2: Lexical Scoping
## Course: Coursera RPROG-004
## Author: Adam Mills (adam.mills@netcall.com)
## Date:   06/06/2014

## Derivative work from cacheMean and makeVector

#########################################################
## testCacheMatrix function
## --Allows for easy testing and debugging of makeCacheMatrix and cacheSolve
## Arguments testCacheMatrix(dimension, debug=F) default:5
#########################################################

testCacheMatrix <- function(x=5, debug=F) {
print("Generating Matrix")
mx <- matrix(rnorm(x*x), nrow=x)              # Creates a matrix using a random distribution of deviates.

print(mx)                                     # Displays generated Matrix

rmx <- makeCacheMatrix(mx)                    # Generates Matrix Functions

if (debug) {print("Generating Matrix Functions")
            print (rmx)}                      # On debug prints rmx functions

print("Solving for Inverted Matrix - Unique")

print(cacheSolve(rmx))                        # Inverts our Matrix (Unique)

print("Cached Response")

print(imx <- cacheSolve(rmx))                 # Inverts from Cache

if (debug) {print("Proof of Inversion - Multiply for Identity Matrix")
            mx %*% imx}                       # On debug attempts to multimply inversion by source to reveal identity
}

#########################################################
## makeCacheMatrix function
## -- embeded functions with matrix
## Arguments makeCacheMatrix(Square Matrix) - Test using matrix(20, nrow=5)
#########################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                 # Initialise inv which will take our inverse

  set <- function(y) {                        # Set function for the matrix
    x <<- y                                   # <<- assigns to x out of scope
    inv <<- NULL                              # Reinitialise inverse in alternative scope
  }
  
  get <- function() x                         # Get function for the matrix
  
  setinverse <- function(inverse) inv <<- inverse # Setter for the inverse
  
  getinverse <- function() inv                    # Getter for the inverse
  
                                                  # Return the matrix with defined functions
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

#########################################################
## cacheSolve function 
## --Calculates the inverse of the matrix using solve. Caches result for future retrieval
## Arguments cacheSolve(makeCacheMatrix)
#########################################################
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                           # Get cached inversions
  
  if (!is.null(inv)) {
    print("getting cached data")                  # Test if get inverse is full (not null),
    return(inv)                                   # if so return inverse of x from cache and exit
  }
  
  data <- x$get()                                 # If we make it here the inverse is not yet calculated
  inv <- solve(data, ...)
  
  x$setinverse(inv)                               # Cache it for next time
  
  return(inv)                                     # Return Inverse
}

## EOF