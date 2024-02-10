## Put comments here that give an overall description of what your
## functions do
#     - makeCacheMatrix : Creates a Matrix object that can cache its inverse.
#     - cacheSolve : Calculates or retrieves the inverse of a Matrix initialized 
#                    with the makeCacheMatrix function depending on if the inverse
#                    was already calculated and cached or not. 


## Write a short comment describing this function
## 1st function:
# Creates a Matrix object
# Input : A matrix X. By default, it is set to a 2x2 matrix of value c(2,2,3,2).
# Output : A list of 4 functions to manipulate the Matrix Object.
#     - set(data, n, m) : Sets the Matrix X to a new set of values data with
#                         n rows and m columns, and its inverse parameter to NULL.
#     - get() : Returns the values of the matrix X.
#     - set_inverse(inv) : Sets the value of the parameter inverse to inv.
#     - get_inverse() : Returns the value of the parameter inverse.

makeCacheMatrix <- function(X = matrix(c(2,2,3,2), 2, 2)) {
      
      inverse <- NULL
      
      set <- function(data, n, m, byrow = FALSE, dimnames = NULL) {
            X <<- matrix(data, n, m, byrow, dimnames)
            inverse <<- NULL
      }
      
      get <- function() X
      set_inverse <- function(inv) inverse <<- inv
      get_inverse <- function() inverse
      
      return(list(set = set, get = get, 
                  set_inverse = set_inverse, 
                  get_inverse = get_inverse))
      
      
}

## Write a short comment describing this function
## 2nd function:
# The function calculates (if needed) and returns the inverse of an invertible matrix X. 
# It first checks if the inverse is already cached in the Matrix Object.
# If this is the case, it simply retrieves the inverse from the cache.
# Otherwise, it calculates the inverse and saves it in the object.
# Input : A matrix X.
# Output : The retrieved or calculated inverse.
# The function always assumes that the input matrix X is always invertible for 
# this assignment

cacheSolve <- function(X, ...) {
      
      inverse <- X$get_inverse()
      if(!is.null(inverse)) {
            message("Retrieving cached data.")
            return(inverse)
      }
      matrix <- X$get()
      inverse <- solve(matrix, ...)
      X$set_inverse(inverse)
      return(inverse)
}
