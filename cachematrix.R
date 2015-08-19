makeCacheMatrix <- function(x = numeric()) {
        
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # stored matrix
        getMatrix <- function() {
                x
        }

        # given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # cached value
        getInverse <- function() {
                cache
        }
        
        # return a list
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# function calculates the inverse of a "special" matrix created with 
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # caclulate the inverse and store it in
        # the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
