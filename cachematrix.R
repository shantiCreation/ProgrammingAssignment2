## This file contains two functions:
## makeCacheMatrix and casheSolve. 
##
## makeCacheMatrix will create matrix object that have built-in methods.
## This is anolougous to creating a class with methods in java. Once the object is created,
## it has methods to set and retrieve its values/properties.
##
## casheSolve function will always check weather inverse matrix already exists,
## if it does it will return previously stored inverse matrix, if not it will 
## calculate the value of inverse matrix and sets it in memory

## makeCasheMatrix functions will build a matrix that have four inbuilt functions/methods that
## one can use to do the following:
## 1.   set the matrix with data. In this example it is always deals with square matrix that
##      could be inversed by using solve() function.
## 2.   get the value of the matrix
## 3.   setinverse  - sets inverse matrix
## 4.   getinverse  - returns inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        ## setting variable,inv, to store value of inverse matrix 
        inv <- NULL
        
        ## if y$set is called, it will set a new matrix y instead of x when we
        ## first called makeCacheMatrix function passing x matrix as an argument.
        ## At the same time we are resetting inv variable that stored inverse matrix to NULL
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
        ## returns matrix
        get <- function() x 
        
        ## this one will store inverse matrix, so it could be retrieved with getinverse method
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        
        ## setting names of the available functions within makeCasheMatrix that could be called
        ## when we build a matrix with makeCasheMatrix function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)



}


## Still working on this function...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
