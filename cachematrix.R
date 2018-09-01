## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #nothing is cached at first
        set <- function(y){
                x <<- y
                m <<- NULL
        } #this reserves a matrix
        get <- function() x #returns the reserved matrix
        setinv <- function(inv) m <<- inv #cache the given argument
        getinv <- function() m #gets the value that is cached
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        #this list is returned
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv #gets the cached value
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        } #this checks whether there is a cache or not and if yes, returns it
        data <- x$get #if there is not any cache, calculates it.
        inverse <- solve(data)
        x$setinv(inverse)
        inverse
}
