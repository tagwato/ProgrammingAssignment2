## Coursera: R Programming for Data Science
## Second programming assignment
## The following functions help to speed up potentially time-consuming matrix computations by caching results.
##
## Usage example:
##
## source('~/ProgrammingAssignment2/cachematrix.R')
## > m <- matrix(c(0, 2, 5, 0), nrow = 2, ncol = 2, byrow = TRUE)
## > m
##       [,1] [,2]
## [1,]    0    2
## [2,]    5    0
## > mobj <- makeCacheMatrix(m)
## > mobj$getinverse()
## NULL
## > cacheSolve(mobj)
##       [,1] [,2]
## [1,]  0.0  0.2
## [2,]  0.5  0.0
## > mobj$getinverse()
##       [,1] [,2]
## [1,]  0.0  0.2
## [2,]  0.5  0.0
## > mobj$get() %*% mobj$getinverse()
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## This function creates and return an object which allows the following operations on a matrix:
##   - set the matrix 
##   - get the matrix
##   - set the inverse of the matrix
##   - get the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of a matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and store it in the cache, using the setinverse function.
## Note:  The definition of a matrix's inverse is that the product of the matrix and its inverse is the identity matrix, if the inverse exists. 
##        So, this is a good way to make sure that the correct inverse of matrix 'mat' was computed:
##         round (mat %*% solve(mat) )    ##identity matrix should be returned
##
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        } else {
                inverse <- solve(x$get())
                x$setinverse(inverse)
                return (inverse)
        }
}
