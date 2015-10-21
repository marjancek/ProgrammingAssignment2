## Coursera rprog-033 Course assigment 02
## Create a pair of functions that will allow to
## cache a matrix together with its inverse, in order
## to avoid the costly computation

## This functions creates a list of accessors to an object
## which will contain both the matrix and its inverse,
## once its made available
makeCacheMatrix <- function(A = matrix()) {
    Ainv <- NULL
    set <- function(B) {
        A <<- B
        Ainv <<- NULL
    }
    get <- function() A
    setinv <- function(inv) Ainv <<- inv
    getinv <- function() Ainv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This functions returs the inverse of the matrix
## stored in the makeCacheMatrix if available, otherwise
## calculates it, stores it in the cache, and returns it
## to the user.
cacheSolve <- function(cA, ...) {
    inv <- cA$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    message("generating inverse and caching it")
    A <- cA$get()
    inv <- solve(A)
    cA$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'A'
}

## Auxiliary function to test the correct calculation of the inverse, and the caching
cacheMatrixTest<-function(size){
    A<-matrix(runif(size*size), ncol=size)
    a<-makeCacheMatrix(A)
    inv<-cacheSolve(a)
    inv<-cacheSolve(a)
    print( round(A%*%inv,digits=8) )
    inv<-cacheSolve(a)
}
