## Inverted/Solve Matrix Cache

## Return the list (object) with caching functions
makeCacheMatrix <- function(x = matrix()) {
    # define functions to get and set caching    
    m <- NULL
    set <- function(y) {
        x <<- y
        # clear cache
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    
    # return object with functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

# Cached solve
cacheSolve <- function(x, ...) {
    # solving function using cache properties
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