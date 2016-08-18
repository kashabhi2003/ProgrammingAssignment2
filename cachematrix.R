## To workaround costly computations when applying a function on big dataset, caching the result is useful.
## So that the tedious 'Inverse of a matrix' function becomes efficient, we write functions below to cache the result.
## It comes of use when the data does not change in several repetitions.

## The below functions follow the assignment instructions closely, to give a simple solution and structure

# Function makeCacheMatrix produces a list containing a function to
# 1. set matrix values ('Amat' matrix)
# 2. get matrix values
# 3. set inverse matrix values ('imat' matrix)
# 4. get inverse matrix values

makeCacheMatrix <- function(Amat = matrix()) {
imat <- NULL
    set <- function(y) {
        Amat <<- y
        imat <<- NULL
    }
    get <- function() Amat
    setinverse <- function(inverse) imat <<- inverse
    getinverse <- function() imat
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# The 'cacheSolve' function returns the inverse of 'Amat' matrix. It first checks if
# the inverse ('imat')has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(Amat, ...) {
        imat <- Amat$getinverse()
    if(!is.null(imat)) {
        message("getting cached data.")
        return(imat)
    }
    data <- Amat$get()
    imat <- solve(data)
    Amat$setinverse(imat)
    imat

}
