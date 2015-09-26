## Since finding the inverse of a matrix can be an expensive operation,
## the functions in this file provide a mechanism to find the inverse of
## a matrix and cache its result. The `makeCacheMatrix` should be called
## first followed by the `cacheSolve` function.

## Initializes and resets the cache variables as well as provide methods
## to interact with the cache variables.
makeCacheMatrix <- function(x = matrix()) {
	cachedMatrix <- x
	cachedInverse <- NULL
	set <- function(y) {
		cachedMatrix <<- y
		cachedInverse <<- NULL
	}
	get <- function() {
		cachedMatrix
	}
	setInverse <- function(mean) {
		cachedInverse <<- mean
	}
	getInverse <- function() {
		cachedInverse
	}
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)

}

## Solve the matrix provided in the `makeCacheMatrix` function. The result
## will be stored in the passed object to avoid calculating it again.
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()

	if (is.null(inverse)) {
		inverse <- solve(x$get(), ...)
		x$setInverse(inverse)
	} else {
		message("getting cached data")
	}

	inverse
}
