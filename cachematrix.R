## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly. This file provides two functions which help to cache the 
## inverse of a matrix:
## - makeCacheMatrix(x): create a special object which have caching functionalities for inverse matrix
## - cacheSolve(x): calculate the inverse matrix of an object created by 'makeCacheMatrix'
## Usage:
## A <- matrix(1:4, 2, 2)
## B  <- makeCacheMatrix(A)
## cacheSolve(B)
## cacheSolve(B)

## 'makeCacheMatrix' function create an object with caching functionalities from the provided matrix x.
## This object provides 4 API functions:
## - set: for initializing the raw matrix data
## - get: for extracting the raw matrix data
## - setsolve: for storing the inverse matrix for future use
## - getsolve: for extracting the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## initialized the cached inverse matrix
    s <- NULL
    
    ## define function for matrix initialization
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## define function for getting the matrix data
    get <- function() x
    
    ## define function for storing the inverse matrix
    setsolve <- function(solve) s <<- solve
    
    #define function for getting the inverse matrix
    getsolve <- function() s
    
    ## create and return an object providing caching functionalities
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## 'cacheSolve' function return the inverse matrix of object x created by the
## 'makeCacheMatrix' function. This function is a wrapper of the 'solve' function
## and utilize the cache functionality provided by 'makeCacheMatrix'

cacheSolve <- function(x) {
    
    ## get the inverse matrix from the cache
    s <- x$getsolve()
    
    ## if initialized then it has been already calculated and could be returned
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## otherwise, calculate the inverse and store it in the cache
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    
    ## Return a matrix that is the inverse of 'x'
    s
}
