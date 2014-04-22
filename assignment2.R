# A pair of functions that cache the inverse of a matrix


#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()){
        # Initialize the inverse property
        i <- NULL
        
        # setting the method for matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # method for getting matrix
        get <- function() m # return the matrix
        
        # setting the method for inverse
        
        setInverse <- function(inverse) 
                i <<- inverse 
       
        # method for getting inverse
        getInverse <- function(){
                i #Return the inverse property}
                
          # Return list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache. 
        
cachesolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # return the inverse if its already set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        # set the inverse to the object
        x$setInverse(m)
        
        m # returnthe matrix
}