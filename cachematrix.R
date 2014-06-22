## Put comments here that give an overall description of what your
## functions do

## This function creates a list of 4 objects.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve is a function that when called with the name of a matrix,
## returns the inverse of that function. It first checks if the inverse
## alread been calculated and return that cached value. Otherwise it
## it calculates the inverse of that matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Use and test the functions
## Setup text matrix
test = matrix(c(0,2,2,0), 2,2)

# Setup cache matrix
foo = makeCacheMatrix()

# Populate with test matrix
foo$set(test)

# Retrieve test matrix
foo$get()

# pass list  to cache function, get return of inverse
cacheSolve(foo)

# Try again to see if computation is cached
cacheSolve(foo)

# Test that return matrix is inverse by multiplying by original matrix
# should get identity matrix as result
test %*% cacheSolve(foo)
