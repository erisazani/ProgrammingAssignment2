## cachematrix.R
##
## Functions to calculate inverse of a matrix using dynamic programming method

## makeCacheMatrix function returns a list of functions which are able to
##  access or reset the value of objects its parent environment, specifically:
## > x, the matrix object stored
## > i, the numeric vector storing the inverse value of (supposed to be) x
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function does the calculation of the inverse of object returned
##  by makeCacheMatrix function. It does checking if there is a cached inverse
##  value and returning it straightforwardly instead of recalculating the
##  inverse. This algorithm makes use of the functions at makeCacheMatrix
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
