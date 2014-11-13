## These two functions calculate and store the inverse of a matrix
## They can be used to avoid performing the calculation multiple times 
## on the same matrix


## The makeCacheMatrix() function will store the inverse of a matrix that has been 
## calculated in the function cacheSolve().  It provides methods to store and 
## retrieve
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve() function check if inverse has previously been calculated
## and stored using methods in makeCacheMatrix(), then either retrievs the result
## if it has been stored, or calculates the inverse and stores it.  
##In both cases the matrix invers is returned


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Usage Example:
##> M = matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##> m <- makeCacheMatrix(M)
##> m$getinverse()
##NULL
##> cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> m$getinverse()
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(m)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## source("GitHub\\ProgrammingAssignment2\\cachematrix.R")