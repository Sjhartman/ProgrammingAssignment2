## The following set of functions cache the inverse of
## a matrix, or retrieves the inverted matrix if it 
## is already cached

## makeCachMatrix generates a cache of a new matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachesolve searches for a cache of the matrix and 
## retrieves the inverse if it exists. Otherwise, 
## chacheSolve will caluclate and the inverse

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}

aMatrix <- makeCacheMatrix()
aMatrix$set(matrix(1:4, 2, 2))
cacheSolve(aMatrix)
aMatrix$getinverse()

