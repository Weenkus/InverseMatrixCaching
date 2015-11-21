## A set of functions for working with matrix and caching 
## the inverse matrix for increase in computing performance

# Creates a matrix with a full set of setters and getters for the matrix
# itself and the mean of the matrix as well.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


# Allows the caching of a matrix for latter usage, thus increaing the 
# performance of a program that uses the inverse matrix value of the 
# same matrix multiple times
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
        	message("getting cahced data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
