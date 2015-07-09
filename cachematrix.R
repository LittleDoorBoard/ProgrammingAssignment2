## Cache the inverse of a matrix
 
# Create a matrix with four funtions set, get, setSolve, and getSolve
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	# Set the value of the matrix 
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    # Get the value of the matrix
    get <- function() x
    # Set the inverse of the matrix
    setSolve <- function(solve) i <<- solve
    # Get the inverse of the matrix
    getSolve <- function() i
    
    # Return a list of the functions
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


# Write a short comment describing this function
cacheSolve <- function(x, ...) {
        i <- x$getSolve()
        
        # Check if the inverse has already been calculated
        # if so, return the inverse from the cache
        if(!is.null(i)) {
        	message("getting cache data")
        	return(i)
        }
        
        # Calculate inverse of the matrix
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        
        # Return a matrix that is the inverse of 'x'
        i
}
