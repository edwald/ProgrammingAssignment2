## These functions set up matrix objects that hold their own inverses, if known,
## and return them 'from memory' rather than re-calculate if needed more than once.

## Set up a matrix object which can hold its own inverse if known

makeCacheMatrix <- function(x = numeric()) {
	i   <- NULL
	set <- function(y) {
		x <<- y
		i <<-NULL
	}
	get <- function() x
	setinv <- function(ing) i <<- ing
	getinv <- function() i
	list( set=set, get=get, setinv = setinv, getinv = getinv )
}


## return the inverse of a matrix (assumed to be invertible) either by calculating
#  or by recalling 'from memory' if we've done this before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if( !is.null(i)) {
		message("getting cached data")
		return(i)
	}
	d <- x$get()
	i <- solve(d)
	x$setinv(i)
	i
}

