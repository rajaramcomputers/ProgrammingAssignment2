makeCacheMatrix <- function(x = matrix()) {
	# inverse will store the cached inverse matrix
	xinv <- NULL

	#To set the value of the matrix x
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}

	#To get the value of the matrix x
	get <- function() x

	#To set the value of the inverse of x
	setinv <- function(inverse) xinv <<- inverse

	#To get the value of the inverse of x
	getinv <- function() xinv

	#Returns a matrix with newly defined functions to manipulate the cache
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: Compute the inverse of the matrix.
# If the inverse is already calculated before, it returns the cached inverse.
##################################################################

cacheSolve <- function(x, ...) {
	xinv <- x$getinv()

	# If the inverse is already calculated
	if (!is.null(xinv)) {
		message("Info - getting cached data")
		return(xinv)
	}

	# The inverse is not yet computed. Compute it here.
	data <- x$get()
	message("Info - computing inverse")
	xinv <- solve(data, ...)

	# Cache the inverse of x
	x$setinv(xinv)

	# Returns inverse of x
	xinv
}
