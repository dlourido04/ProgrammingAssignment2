## Function to cache  the inverse of a matrix if it has already been calculated

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	
	set <- function(y){
		x <<- y
		x_inv <<- NULL
	}
	get <- function() { x }
	
	setInverse <- function(x_inverse) { x_inv <<- x_inverse}
	getInverse <- function(){ x_inv }
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieve the inverse 
## from the cache
cacheSolve <- function(x, ...) {
    x_inv <- x$getInverse()
    if (!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    
    mtx <- x$get()
    if(det(mtx) == 0){
        return(message("your matrix is not invertible"))
    }
    
    x_inv <- solve(mtx, ...) #Reverse the matrix
    x$setInverse(x_inv) 
    x_inv
}