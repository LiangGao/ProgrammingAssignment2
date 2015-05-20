##  Matrix inversion is usually a costly computation and there is benefit
##  to caching the inverse of a matrix rather than compute it 
##  repeatedly. This pair of functions can cache the inverse of a matrix:
##  1. X = makeCacheMatrix(x) creates a special "matrix" X which stores
##     the matrix (x) and caches the inverse if it's been calculated.
##  2. cacheSove(X) returns the inverse of matrix x by either calucating
##     directly or getting the cached result.

##  To use the function makeCacheMatrix, type X = makeCacheMatrix(x), where
##  x should be a square invertible matrix.
##  X$get() returens the stored matrix.
##  X$getinverse() returns the inverse of matrix x if it's been calculated.
##  X$setinverse() caches the inverse of matrix x.
##  X$set(y) sets another matrix y to X instead of x.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  To use the function cacheSolve, type cacheSolve(X), where X should be
##  previously defined using X = makeCacheMatrix(x).
##  The return value will always be the inverse of matrix x:
##  The function will calculte the inverse of matrix x and cache it to X, or
##  get the previously calculated inverse of matrix x from X.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
