## Functions below provide a mechanism to cache the inverse of a matrix
## 'makeCacheMatrix' creates an object capable of keeping a matrix and it's
## inverse inside scope, and 'cahceSolve' is a function that, using 'makeCacheMatrix'
## gets the cached inverse if it exists, calculating, storing and setting it otherwise.


## This function creates the 'cacheMatrix' element, which provides
## functions to get/set the value of the matrix, and get/set the
## value of the inverse. The matrix and the inverse are 'stored' inside
## function scope.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(calculatedinverse) inverse <<- calculatedinverse
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix. It receives as argument the
## function list returned by 'makeCacheMatrix', and tries first to get the
## cached inverse, if it was calculated before. 
## If not, it will calculate the inverse, store it using 'setinverse' method, and
## then return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    inverse
}

## How to test :

## > mymatrix <- matrix(1:4,2,2)
## > cacheMatrix <- makeCacheMatrix(mymatrix)
## > cacheSolve(cacheMatrix)
## > cacheSolve(cacheMatrix)

## The second (and all subsequent) call to 'cacheSolve' 
## should, besides showing the inverse, display the message 'getting cached data'

