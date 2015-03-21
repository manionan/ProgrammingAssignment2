## Functions to store the inverse of a matrix upon computation
## and access that stored result if the inverse is called again
## in order to save time

## makeCacheMatrix creates a vector whose entries are funtions to 
## set/get the value of the vector and set/get the value of the inverse
## of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	    x <<- y
	    m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get, 
	     setinv = setinv,
	     getinv = getinv)	    
}


## Calculates inverse of special matrix made with above function
## but checks first to see if it's in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m

}
