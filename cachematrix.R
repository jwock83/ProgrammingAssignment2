## Put comments here that give an overall description of what your
## functions do

## cacheSolve function calculates the inverse of a matrix that is defined in thefunction makeCacheMatrix
## by fetching it through get() function. This inverse is then set in the function setInverse()
## so anytime same inverse is required, cacheSolve gets the results through the getInverse() function
## in the cache of the program.


## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse 
    getInverse <- function() i 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)	
}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R.

cacheSolve <- function(x, ...) {

    i <- x$getInverse()
    if(!is.null(i)) {
            message("getting cached data")
            return(i)
    }

    data <- x$get()
    i <- solve(data, ...) 
    x$setInverse(i)
    i
}

