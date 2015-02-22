## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a  a list containing functions to
	## set the value of the matrix
	## get the value of the matrix
	## set the inverse of the matrix 
	## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        ## variable to store the inverse of the matrix
        i <- NULL
        
        ##
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##
        get <- function() x
        
        ## set inverse with the passed in inverse matrix
        setinverse <- function(inverse) i <<- inverse
        
        ## get the value of the cached inverse
        getinverse <- function() i
        
        ## list of functions to get and set the matrix and its inverse
        list(set = set, 
        	 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##
## Creates a function that checks to see if the inverse matrix is already cached
## If it is it simply prints "Getting cached inverse matrix" and returns the cached inverse
## If it is not yet cached the function inverts the matrix (solve(matrix)), caches it and returns it
## 
## To test:
## m <- matrix(1:4, 2,2)
## > m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > solve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## cached <- makeCacheMatrix(m) 
## cached$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(cached)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Getting cached inverse matrix ## NOTE THE MESSAGE THAT SHOWS WE ARE NOW RETURNING THE CACHED VERSION
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

cacheSolve <- function(x, ...) {
   
    ## ask the cache for the inverted matrix    
    i <- x$getinverse()

	## if it exists return it
    if(!is.null(i)) {
        message("## Getting cached inverse matrix")
        return(i)
    }

	## there is no cached inverse so get the original matrix
    matrix <- x$get()

	## invert it
    inv <- solve(matrix)

	## cache it
    x$setinverse(inv)

	## return the inverted matrix
    inv
}
