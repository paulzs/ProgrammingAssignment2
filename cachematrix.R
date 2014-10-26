## Matrix inversion is costly and so caching the inverse of a matrix rather 
## than computing it repeatedly seems like a good idea. The following functions 
## create a matrix object that can cache its inverse and either computes the inverse 
## of a matrix or returns the inverse from the cache if it has previously been computed. 

## The makeCacheMatrix() function creates a matrix object which is a list containing a function to:
# 1) Set the value of the matrix
# 2) Get the value of the matrix
# 3) Set the value of inverse
# 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve() function either returns the inverse matrix that was previously 
## cached or computes the inverse matrix and caches it using the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cache money")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

# Test Run

# > source('cachematrix.R')
# x = rbind(c(2,3),c(2,2))

# Make the matrix object
# m = makeCacheMatrix(x)
# > m$get()
#      [,1] [,2]
# [1,]    2    3
# [2,]    2    2

# Compute the inverse (no cache)
# > cacheSolve(m)
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0

# Get inverse from cache this time
# > i = cacheSolve(m)
# getting cache money
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0

# Sanity Check
# > i%*%x
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# Yes! The two matrices are indeed inverses.
