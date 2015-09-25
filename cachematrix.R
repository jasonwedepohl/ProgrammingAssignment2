## R Programming Assignment 2
## Author: Jason Wedepohl
##
## Two functions allowing for the storage of a matrix
## and the computation and caching of its inverse, showcasing the use of the '<<'
## operator to set variables that were defined in a different environment.

## makeCacheMatrix returns a list that acts as a matrix container allowing
## the caching and retrieval of its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mInverse <- NULL
	set <- function(y) {
		x <<- y
		mInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) mInverse <<- inverse
	getInverse <- function() mInverse
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

## cacheSolve uses the extended matrix supplied by makeCacheMatrix
## to cache and retrieve the matrix inverse. The inverse will only be calculated
## if the cached value of the inverse is null.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	mInverse <- x$getInverse()
	if(!is.null(mInverse)) {
		message("getting cached data")
		return(mInverse)
	}
	data <- x$get()
	mInverse <- solve(data, ...)
	x$setInverse(mInverse)
	mInverse
}
