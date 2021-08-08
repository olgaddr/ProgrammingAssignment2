## Below is a pair of functions that cache the inverse of a matrix

## The first function creates a special "matrix" object that can cache 
## its inverse. This "matrix" object is really a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the matrix inverse
## 4. get the matrix inverse

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


## The second function calculates the inverse of the matrix created 
## with the above function. It
## 1. checks the inverse from the cache
## 2. otherwise, it calculates the inverse from data and sets its value
## in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix m that is the inverse of 'x'
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
