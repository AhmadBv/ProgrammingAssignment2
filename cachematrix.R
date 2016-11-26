##This Assignment is from Ahmad Bhimani.
##This Assignment is to test the inverse cache matrix function

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ##inital function to Null
   inverse <-NULL
   ##seting the matrix function
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    ##this get the matrix.
    get <- function() return(x)
    ##this function sets the inverse of the matrix
    setinv <- function(inv) inverse <<- inv;
    ##this gets the inverse function of the matrix.
    getinv <- function() return(inverse)
    ## Returns the matrix depending on the function call
    return(list(set = set, get = get, setinv = setinv, getinv = getin))
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
        ##Test if the inverse is empty or not.
        if(!is.null(inverse)){
            ##if not empty then display the message and return the matrix.
            message("Getting cached data...")
            return(inverse)
        }
        ##data is variable that get the matrix.
        data <-x$get()
        ##inverse is the variable that gets the inverse of the matrix
        ##using the solve function.
        inverse <- solve(data, ...)
        ##Seting the inverse of the matrix
        x$setinv(inverse)
        ##This returns the inverse of the matrix.
        return(inverse)
}
