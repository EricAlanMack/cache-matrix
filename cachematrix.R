## These functions together allow the user to cache matrix inverses 
## and retrive them in order to simplify computations. 

## The function makeCacheMatrix creates a list of functions, which we 
## can think of as a special kind of "matrix."  
## It allows the user to set matrix values, to get the value of the 
## created matrix, to set the value of its inverse, and get the value 
## of its inverse.   


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m 
	list(set = set, get = get, 
	setinverse = setinverse,
	getinverse = getinverse)
}


## This function will, given an invertable matrix, output its inverse.
## If its inverse is stored via the makeCacheMatrix$setinverse() 
## function, then it will retrieve it from there. Otherwise it will 
## compute the inverse using solve(), and cache it for later use.   

cacheSolve <- function(x, ...) {
		m <- x$getinverse()
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m 	
}
