## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the solve
## 4.  get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## The following function calculates the inverted Matrix of the special "vector"
## created with the above function. However, it first checks to see if the
## inverted matrix has already been calculated. If so, it `get`s the inverted matrix from the
## cache and skips the computation. Otherwise, it calculates the inversion of
## the matrix and sets the value of the matrix in the cache via the `setsolve`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
