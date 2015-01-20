# These two functions are used to create special data type
# comprising a matrix and cache its inverse


# This function creates a special matrix data type that stores
# a standard matrix data type along with its cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    Matrix <- x
    Inverse <- NULL
    list(getMatrix = function() { Matrix }
        ,setMatrix = function(y) {
                         Matrix <<- y
                         Inverse <<- NULL
                     }
        ,getInverse = function() { Inverse }
        ,setInverse = function(inv) { Inverse <<- inv }
        )
}


# This function returns the cached inverse if available, else it calculates
# the inverse and caches it before returning

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        inverse
    }
    else {
        inverse <- solve(x$getMatrix())
        x$setInverse(inverse)
        inverse
    }
}