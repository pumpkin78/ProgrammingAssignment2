## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix will create 4 functions associated with a matrix x:
## set: use superassignment to cache value matrix x to an enclosing environment where x is defined
## if not such an environment exists, then the global environment is used 
## get: return matrix x
## setinv: cache the inverse of x
## getinv: return the inverse of x

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invert) inv <<- invert
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve return the inverse of matrix x, given x was created by makeCacheMatrix
## It first check if the inverse matrix was cached, if yes, return it
## If not, then compute the inverse, caches it, then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
