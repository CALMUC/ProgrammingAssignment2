## The purpose of the following two functions is to avoid unnecessary repeats
## of the costly computation inverting one and the same matrix by getting the 
## result from cache after the first invert calculation.

## Attention! If the matrix x changes in between running the two functions,
## wrong results will be produced.

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set matrix value
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get matrix value
        getmatrix <- function() x
        ## set the value of the inverse
        setinverse <- function(solve) i <<- solve
        ## get the value of the inverse
        getinverse <- function() i
        ## store a list of functions to be used by chacheSolve
        flist <<- list(setmatrix = setmatrix, 
                       getmatrix = getmatrix,
                       setinverse = setinverse,
                       getinverse = getinverse)
}


## cacheSolve() computes the inverse of a matrix x.
## If the inverse has already been calculated it is retrieved from the cache.

cacheSolve <- function(x) {
        i <- flist$getinverse()
        ## if inverse has been calculated before, get its value from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if inverse has not yet been calculated, calculate it via solve()
        data <- flist$getmatrix()
        i <- solve(data)
        flist$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
