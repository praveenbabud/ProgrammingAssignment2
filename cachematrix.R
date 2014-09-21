## The following functions create a special matrix
## that can cache its inverse

## Function : makeCacheMatrix
## creates a special matrix that is a list of 
## functions that set/get matrix and its inverse

makeCacheMatrix <- function(dMatrix = matrix()) {

    dInverse <- NULL

    set <- function(inMatrix) {
        dMatrix <<- inMatrix
        dInverse <<- NULL
    }

    get <- function() dMatrix

    setinverse <- function(inverseMatrix) dInverse <<- inverseMatrix

    getinverse <- function() dInverse

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function : cacheSolve 
## returns inverse of the special matrix created by 
## function makeCacheMatrix()
## function returns inverse if its already calculated
## otherwise calculates inverse and stores and returns
## the function assumes the matrix is always square and
## non-singular.

cacheSolve <- function(inMatrix, ...) {

    tInverse <- inMatrix$getinverse()

    if(!is.null(tInverse)) {
        message("getting cached data")
        return(tInverse)
    }

    tMatrix <- inMatrix$get()
    tInverse <- solve(tMatrix, ...)
    inMatrix$setinverse(tInverse)
    tInverse
}
