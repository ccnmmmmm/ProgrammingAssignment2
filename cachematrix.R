## The following functions create and cache a matrix and its inverse

## This function creates a special
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- matrix()
	set <- function(y = matrix()){
		x <<- y
		m <<- matrix()
	}
	get <- function() x
	setminv <- function(minv = matrix()) m <<- minv
	getminv <- function() m
	list(set = set, get = get, setminv = setminv, getminv = getminv)
}

## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getminv()
        if(!is.na(m[1,1])) { #checks if no inverse matrix in cache
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setminv(m)
        m
}	

