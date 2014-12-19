## Put comments here that give an overall description of what your
## functions do

## The following function creates an object that can store a matrix
## and its inverse in the cache.  Four methods exists to manipulate
## the object : 'set' and 'get' to store and recover the matrix stored
## in cache; 'setinverse' and 'getinverse' to set the inverse of the
## matrix and recover it.

makeCacheMatrix <- function(A = matrix()) {
        inv <- NULL
        set <- function(y) {
                A <<- y
                inv <<- NULL
        }
        get <- function() A
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## The following function is used to recover the inverse of a matrix
## from the cache. If the inverse has not been computed yet, the
## methods call 'solve' to compute it and store the result in the
## cache for subsequent use.

## Here, the argument A must be an object created by the
## makeCacheMatrix()

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'A'
        inv <- A$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- A$get()
        inv <- solve(data, ...)
        A$setinverse(inv)
        inv
}
