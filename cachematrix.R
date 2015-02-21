## These functions work together to cache the inverse of a specified matrix
## passed to makeCacheMatrix. makeCacheMatrix sets up a matrix-specific tool (a set of
## commands (methods)) that is then passed to cacheSolve, which uses the tool
## set to check for, calculuate, cache and return the inverse of the matrix. It
## will only cache the most recently calculated inverse.

## Sample Usage:
#    x <- makeCacheMatrix(matrix(1:4, nrow=2)) # store toolset in x
#    cacheSolve(x) # calculate & retrieve inverse
#    cacheSolve(x) # previously calculated inverse returned from cache

## This function accepts a invertable matrix, nullifies any prior caching, and
## defines the internal functions (set, get, setinv & getinv) in terms of the
## input matrix. Because the assignments take place inside sub-functions of the
## makeCacheMatrix function, the <<- operator is used to carry the values up to
## the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                           # erase previous value
    set <- function(y) {                # define tool to set matrix value
        x <<- y
        i <<- NULL
    }
    get <- function() x                 # define tool to return matrix value
    setinv <- function(inv) i <<- inv   # define tool to set inverse value
    getinv <- function() i              # define tool to return existing inverse
    list(set = set, get = get,          # package & return toolkit
        setinv = setinv,
        getinv = getinv)
}


## This function takes the matrix-specific toolkit (in a list) and uses it to
## access the value of the original matrix, calculate or retrieve its inverse,
## and assign thatinverse back to the toolkit.

cacheSolve <- function(x, ...) {
    i <- x$getinv()                    # use tool to retrieve exisiting inverse
    if(!is.null(i)) {                  # if it exists, return the  cached one
        message("getting cached data")
        return(i)                      
    }
    data <- x$get()                    # if cache empty, load input matrix
    i <- solve(data, ...)              # calculate that matrix's inverse
    x$setinv(i)                        # use tool to cache new inverse
    i                                  # return calculated inverse
}
