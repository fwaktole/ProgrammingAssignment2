# makeCacheMatrix creates list of functions (setMatrix, getMatrix,
# cacheInverse,getInverse), which is really a list 
# containing a function to store the matrix and cached value of the inverse

makeCacheMatrix <- function(x = numeric()) {
       # setting the initial cache to NULL
        cache <- NULL
        
        # set the value of a matrix
        setMatrix <- function(newValue) 
        {
        	x <<- newValue
           	# truncating the cache
                cache <<- NULL
        }

        # get the value of a matrix
        getMatrix <- function() {x}

        # get the cached inverse value
        cacheInverse <- function(solve) {cache <<- solve}

        # get the cached value
        getInverse <- function() {cache}
        
        # return list of functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# this function computes the inverse of the special "matrix" returned 
# by the above function (makeCacheMatrix)

cacheSolve <- function(y, ...) 

	{
        # cached value
        inverse <- y$getInverse()
        # check if value exists
        if(!is.null(inverse)) {
             message("returning the cached data")
             # return the value
             return(inverse)
        }
        # else return the matrix and compute the inverse
        # and cache it
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # getting the inverse
        inverse
	}