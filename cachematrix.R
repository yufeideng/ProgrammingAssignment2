## Put comments here that give an overall description of what your
## functions do

## There are two functions in the following script:
## 1. makeCacheMatrix: This function creates a special "matrix" object
##      that can cache its inverse
## 2. cachSolve: This function computes the inverse of the special "matrix"
##      returned 'makeCacheMatrix' above. If the inverse has already been
##      calculated (and the matrix has not changed), then the cachesolve 
##      should retrieve the inverse from the cache.

## Write a short comment describing this function

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x 
        setInv <- function(solve) s <<- solve
        getInv <- function() s 
        list(set = set, get = get, 
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

## The following function caculated the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the 'setInv' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInv(s)
        s
}
