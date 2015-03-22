## Function is based on the example provided for mean
## in the assignment.
## Input x: is a square invertible matrix 
##   	  (It is assumed it's always invertible)
## Return : a list containing functions to
##              -> set the matrix
##              -> get the matrix
##              -> set the inverse
##              -> get the inverse
##        List then used as the input to cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
        ## Function is based on the example provided for mean
	  ## in the assignment.
	  ## Input x: is a square invertible matrix 
        ##   	  (It is assumed it's always invertible)
        ## Return : a list containing functions to
        ##              -> set the matrix
        ##              -> get the matrix
        ##              -> set the inverse
        ##              -> get the inverse
        ##        List then used as the input to cacheSolve() function
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <- NULL
        }
        get <-function() x
        setinv <- function(inverse) m <<- inverse 
        getinv <- function() m
        list(set=set, get = get, 
		setinv = setinv, 
		getinv = getinv)
}


## Function is based on the example provided for mean
## in the assignment.
## Input x: makeCacheMatrix() o/p
## return : original matrix inverse
cacheSolve <- function(x, ...) {

        ## Function is based on the example provided for mean
	  ## in the assignment.
        ## Input x: makeCacheMatrix() o/p
        ## return : original matrix inverse
        
        m = x$getinv()
        
        # In case inverse exist and cached
        if (!is.null(m)){
                # As output get from the cache 
                message("getting cached data")
                return(m)
        }
        
        # else, cal. the inverse 
        data = x$get()
        m = solve(data, ...)
        
        # And set the value
        x$setinv(m)
        
        m
}