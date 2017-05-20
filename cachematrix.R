## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## compute it repeatedly
## The next two functions can be used to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
	matr <- NULL
	set <- function(y) {
		x <<- y
		matr <<- NULL
	}
	get <- function() x
	setInverse <- function(res) matr <<- res
	getInverse <- function() matr
	list( set = set, 
	      get = get, 
              setInverse = setInverse,
	      getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	matr <- x$getInverse()
	if(!is.null(matr)) {
		message("getting cached data")
		return(matr)
	}
	data <- x$get()
	matr <- solve(data, ...)
	x$setInverse(matr)
	matr
}
