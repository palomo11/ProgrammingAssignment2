## Programming Assignment 2: Lexical Scoping - R Programming

## Two functions to cache the inverse of a matrix

## The first functions creates a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## The second function computes the inverse of the special matrix returned in the first function.
## If the matrix has already been cached, the cachesolve will retrieve the inverse. Otherwise, it will
## be calculated with the solve function

cacheSolve <- function (x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
