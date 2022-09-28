##########  Write a pair of functions that cache the inverse of a matrix
## setwd('C:/.../Assignment_2_Lexical_Scoping')

library(MASS)       # for 'ginv' function (it will give the generalized inverse of a matri)
library(matrixcalc) # for 'is.square.matrix' function

## Function that creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
    m <- NULL
    ## Set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Get the matrix
    get <- function() x
    ## Set the inverse of a square and non-square matrix
    setsolve <- if(is.square.matrix(x) == TRUE) {
        function(solve) m <<- solve
    }
    else{
        function(ginv) m <<- ginv
    }
    ## Get the inverse of the matrix
    getsolve <- function() m
    ## Return a list of the methods (set & get)
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function that computes the inverse of a matrix
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Get the matrix from our object
    data <- x$get()
    ## Computing the inverse of a square and non-square matrix
    m <- if(is.square.matrix(data) == TRUE) {
        solve(data, ...)
    }
    else{
        ginv(data, ...)
    }
    ## Set the inverse to the object
    x$setsolve(m)
    ## Return the matrix
    m
}

###################################
############  Testing  ############
###################################
## source("C:/.../cache_inverse2.R")

my_matrix <- makeCacheMatrix(matrix(20:25, 3, 2))
my_matrix$get()
my_matrix$getsolve()

cacheSolve(my_matrix)
my_matrix$getsolve()
