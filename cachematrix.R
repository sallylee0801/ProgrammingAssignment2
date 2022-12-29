## Put comments here that give an overall description of what your
## functions do

## These functions are written for Coursera Data Science R Programming
## Week 3 Assignment; Github user: sallylee0801

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                           ## initializing inverse as NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function(){
        inver <- ginv(x)
        inver%*%x
    } 
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
