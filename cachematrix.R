## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix returns a list of functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
      # inv to store cached inverse matrix; initialize it to NULL
      inv <- NULL
      
      # set value of matrix
      set <- function(y) 
      {
            x <<- y
            inv <<- NULL
      }
      
      # get value of matrix
      get <- function() x
      
      # set value of inverse
      set_inverse <- function(inverse) inv <<- inverse
      
      # get value of inverse
      get_inverse <- function() inv
      
      # return matrix with list of the 4 functions
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

## Write a short comment describing this function
# cacheSolve function returns inverse of the matrix created by above funtcion.
# First it checks if inverse has already been computed. If yes, it gets the 
# result and skips computation. If not, it computes inverse and sets the value
# in cache via setinverse function.

cacheSolve <- function(x, ...) {

      inv <- x$get_inverse()
      
      # if inverse is already calculated, get from cache
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # if inverse is not yet calculated, get the matrix and calculate inverse
      data <- x$get()
      inv <- solve(data, ...)
      
      # cache the inverse
      x$set_inverse(inv)
      
      # return the inverse
      inv
}