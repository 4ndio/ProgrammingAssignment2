## This script defines two functions: makeCacheMatrix() and cacheSolve().
##
## makeCacheMatrix(x) creates a "cached matrix" that stores the 
## matrix x and caches the matrix inverse of x for use with cacheSolve().
## Input:
##          x - a square inverible matrix
## Output:
##          a list of four functions: (set,get,setinverse,getinverse)
##                set(x) - saves the matrix x
##                get() - returns the matrix x
##                setinverse(inv) - saves inv as the inverse of x
##                getinverse() - returns inv
##                               (returns NULL if no inverse is saved)
##
## cacheSolve(x) is a cached verion of the function solve(x).
## The input x must be a "cached matrix" of the form output by makeCacheMatrix().
## Input:
##          x - a "cached matrix" created by makeCacheMatrix()
## Output:
##          a matrix y, where y = solve(x) is the matrix inverse of x


## makeCacheMatrix(x) creates a "cached matrix" out of matrix x.
makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve(x) is a cached verion of the function solve(x).
## The input x is assumed to be a square invertible matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
