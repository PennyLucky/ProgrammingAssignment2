## These function create cache a matrix and its inverse.

## makeCacheMatrix create a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  	m <- NULL
  	set <- function(y){
  		x <<- y
  		m <<- NULL
	}
	get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## cacheSolve  computes the inverse of a matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated it is retrieved from the cache.

cacheSolve <- function(x=matrix(), ...) {
    	m <- x$getmatrix()
    	if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    	}
    	matrix <- x$get()
    	m <- solve(matrix, ...)
    	x$setmatrix(m)
    	m
}
