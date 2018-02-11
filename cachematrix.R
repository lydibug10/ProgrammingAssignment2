## Functions for Week 3 Peer-graded Assignment
##
## Lydia Cromwell

## Create a matrix that can have its inverse cached
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setcache <- function(inv) i <<- inv
	getcache <- function() i
	list (set = set, get = get, setcache = setcache, getcache = getcache)
}


## Check cache for inverse. If not, calculate and cache inverse
cacheSolve <- function(x, ...) {
	m <- x$getcache()
	if (!is.null(m)) {
		message("found cached inverse. fetching that.")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setcache(m)
	m
}
