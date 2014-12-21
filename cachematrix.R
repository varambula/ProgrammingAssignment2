################################################################################
# This function creates a special "matrix" object that can cache its inverse.
################################################################################
makeCacheMatrix <- function(matX = matrix()) {

    # Initialize the matrix inverse.
    iMat <- NULL

    # Store the input matrix and initialize it's inverse in cache.
    set <- function(matY) {
        matX <<- matY
        iMat <<- NULL
    }# set

    # Get the input matrix from cashe.
    get <- function() {
        matX
    }# get

    # Store the matrix inverse in cache.
    setInverse <- function(inverse) {
        iMat <<- inverse
    }# setInverse

    # Get the cashed inverse.
    getInverse <- function() {
        iMat
    }# getInverse

    # Create a list of functions, set, get, setInverse, and getInverse.
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}# makeCacheMatrix


################################################################################
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above.  If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache.
################################################################################
cacheSolve  <- function(cashedMatrix, ...) {

    # Get the matrix inverse, if already computer (otherwise it is NULL).
    iMat <- cashedMatrix$getInverse()

    # If the inverse was already computed (ie, not NULL), return it from cashe.
    if (!is.null(iMat)) {
        message("Getting cached inverse matrix")
        return (iMat)
    }
    # If we got here, the inverse needs to be computed.
    # First get the data, matX.
    mat <- cashedMatrix$get()

    # Now compute the inverse of the matrix.
    iMat <- solve(mat)

    # Store the computed inverse in cashe.
    cashedMatrix$setInverse(iMat)

    # Return the matrix inverse.
    iMat

}# cacheSolve
