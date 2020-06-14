## The following set of functions cache the inverse of
## a matrix, or retrieves the inverted matrix if it 
## is already cached

# Mean test

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    }

myVector <- makeVector(1:15)
myVector
m

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

aVector <- makeVector()
aVector$set(30:50)
cachemean(aVector)
aVector$getmean()

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

