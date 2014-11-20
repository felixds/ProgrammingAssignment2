## author: Félix Ángel Fernández Alonso
## Functions: MakeCacheMatrix and CacheSolve
## Date: 20 November 2014
## Course: DataScience Specialization
## Description: The functions belows solve different problems 
## about how calculate an inverse matrix.

## Function: MakeCacheMatrix
## Description: This function generate a special matrix as
## a parameter of MakeCacheMatrix. Inside the function
## we create different list objects as part of a list doing the next:
##  set --> set the value of the matrix
##  get --> get the value of the matrix
##  setinver --> set the value of the matrix inverse
##  getinver --> get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
        m<-NULL
        set <- function(y = matrix()) {
            x <<- y
            m<-NULL
        }
        get <- function() x
        setinver <- function(solve) m <<- solve
        getinver <- function() m
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)

}

## Function: cacheSolve
## Description: This function calculate the inverse matrix
## using the matrix created by makeCacheMatrix function above
## one of the steps is verify if the inverse matrix value already
## exist, in this case return the cache value and skip calculate it.
## if the function doesn't find the value calculate the inverse matrix
## using the function setinver.

cacheSolve <- function(x, ...) {
    
    m <- x$getinver()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinver(m)
    m
}

## with real inverse matrix works, examples are like this to test
## the function:
## mat <- matrix(1:4, 2,2)
## matObj <- makeCacheMatrix(mat)
## cachesolve(matObj)
