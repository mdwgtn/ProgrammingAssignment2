## These functions cache the inverse of a matrix,
## avoiding unnecessary re-calculation of that inverse.
## USAGE: 
##  Calculate the inverse of an invertible matrix with the following steps:
##  o Create a cacheable-inverse matrix m by first invoking makeCacheMatrix, 
##  o Invoke cacheSolve against m.
##  Second and subsequent invocations of cacheSolve return the pre-calculated inverse.

#######################
## makeCacheMatrix(mtx)
##
## This function creates a special "matrix" object that can cache its inverse. 
## Use in conjunction with cacheSolve().
## PARAMETER: mtx, an invertible matrix
## RETURNS: An object with the following members:
##  o get_matrix(), returning the value of the invertible matrix;
##  o set_mstrix(m), setting the value of the invertible matrix and clearing any 
##     cached inverse;
##  o set_inverse(inv), setting the value of the cached inverse;
##  o get_inverse(), getting the value of any cached inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  cached_inverse <- NULL
  set_matrix <- function(m){
    mtx <<- m
    cached_inverse <<- NULL
  }
  get_matrix <- function() mtx
  set_inverse <- function(inv) cached_inverse <<- inv
  get_inverse <- function() cached_inverse
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

################################
## cacheSolve(cache_matrix, ...)
##
## This function returns the inverse of the special "matrix" returned 
##  by makeCacheMatrix().
## PARAMETERS:
##  o cached_matrix, the result of a call to makeCacheMatrix()
##  o ..., additional parameters for the solve() function.
## RETURNS: The inverse of the matrix used to initialise cached_matrix. 
##  Note that second and subsequent invocations for given cached_matrix are 
##   faster than the first invocation, because the calculated inverse is cached.

cacheSolve <- function(cached_matrix, ...) {
  inv <- cached_matrix$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- cached_matrix$get_matrix()
  inv <- solve(data, ...)
  cached_matrix$set_inverse(inv)
  ## Return a matrix that is the inverse of 'cached_matrix'
  inv
}
