## Comments from the student Jan 2021: I am retaking this R Programming course 
## to refresh my skills after some time away from the data science field.
## It is not possible to fork the same repo twice, so this repo contains my work
## from when I first took the course. It is, nevertheless, my own work.
## Thanks for reviewing it.

## This pair of functions help increase computational efficiency 
## when inverting a matrix by caching it. This avoids repeated
## calculation

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
