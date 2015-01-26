# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The cacheSolve function returns the inverse of the matrix.
# If the inverse has alreadt been computed, it retrieves 
# the results and does not compute the inverse.  If the inverse
# has not been computed, compute the inverse and sets the value
# in the cache with "setinverse" function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Test run:
## > source("cachematrix.R")
## Sample matrix (3 is my favorite number):
## > x = rbind(c(3,1/3),c(-1/3,1)) 
## > m = makeCacheMatrix(x)
## > m$get()
##           [,1]      [,2]
## [1,]  3.0000000 0.3333333
## [2,] -0.3333333 1.0000000
## > cacheSolve(m)
##           [,1]       [,2]
## [1,] 0.3214286 -0.1071429
## [2,] 0.1071429  0.9642857
## > cacheSolve(m)
## getting cached data.
##          [,1]       [,2]
## [1,] 0.3214286 -0.1071429
## [2,] 0.1071429  0.9642857