## Caching the Inversemy_matrix$getInverse() of a Matrix.
## A pair of functions that cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv = NULL 				# this is where the result of inversion is stored
    set = function(y) {
        x <<- y
        xinv <<- NULL 			# it also initialises xinv to null
    }
    get = function() x 			# return the input matrix
    setInverse = function(inv) xinv <<- inv 	# set the inversed matrix
    getInverse = function() xinv 		# return the inversed matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## calculates the inverse of the makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m = x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data = x$get()
    m = solve(data)
    x$setInv(m)
    m
}
