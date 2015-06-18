## Method for abstracting of the caching of matrix inverses.
##
## Usage:
## > m <- makeCacheMatrix(matrix(rnorm(36), 6, 6))
## > m$get()
## [matrix...]
## > n <- cacheSolve(m)
## [n is the inverse matrix]
## > m$getinv()
## [also the inverse]
## > m$set(matrix(rnorm(36), 6, 6))
## > m$getinv()
## NULL
## > n <- cacheSolve(m)
## > m$getinv()
## [inverse of new matrix]


## Creates a object which holds a matrix an potentially its cached inverse.
## Methods:
##    $get      gets matrix value
##    $set      sets matrix value
##    $getinv   gets inverse matrix value
##    $setinv   sets inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
		# x is the matrix, m is theinverse.
		# The use of <<- is to update the variables in this scope.
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Cached version of the solve() method.
## The first parameter must be an object created by makeCacheMatrix()
## The return value is the inverse.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
