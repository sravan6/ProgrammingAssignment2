## makeCacheMatrix checks if the inverse of the supplied matrix 
## exists in the cache

## cacheSolve solves for the inverse if it doesn't exist


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	## Sets the matrix to the supplied value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	## Get the matrix  
        get <- function() x

	## Sets the Inverse of the matrix to the supplied value
        setInv <- function(inv) m <<- inv

	## Gets the inverse matrix from cache
        getInv <- function() m

	## Creates a list to return  
        list(set = set, get = get,setInv = setInv,getInv = getInv)

}


## Checks whether the inverse of a matrix exists in the cache
## If exists, returns the inverse. Else calcualtes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInv()

	## If inverse exists, return the value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## Get the matrix if inverse doesn't exist
        data <- x$get()

	## Solve the matrix to get the inverse
        m <- solve(data, ...)

	## After solving set the cache
        x$setInv(m)
        m

}