## makeCacheMatrix() creates a list of 4 functions for the cached matrix object
## setMatrix sets the value of the cached matrix
## getMatrix returns the value of the cached matrix
## setInverse sets the value of the matrix inverse
## getInverse returns the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(mMatrix) {
                x <<- mMatrix
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(setMatrix = setMatrix,
        	 getMatrix = getMatrix,
        	 setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve() returns the matrix inverse, first by checking if the inverse is cached 
## if not it calculates the matrix inverse by calling solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("Getting cached matrix inverse")
                return(inverse)
        }
        mMatrix <- x$getMatrix()
        inverse <- solve(mMatrix)
        x$setInverse(inverse)
        inverse
}
