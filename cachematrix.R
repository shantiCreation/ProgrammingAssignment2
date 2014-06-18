## This file contains two functions:
## makeCacheMatrix and cacheSolve. 
##
## makeCacheMatrix will create matrix object that have built-in methods.
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
        
        ## declaring and initiating inv variable to store value of inverse matrix 
        inv <- NULL
        
        ## set function will be called as x$set(y); it will set a new matrix y instead of x when we
        ## call makeCacheMatrix with x as an argument.
        ## At the same time we are re-initiating inv variable that stores inverse matrix to NULL
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
        ## returns matrix
        get <- function() x 
        
        ## stores inverse matrix, so it could be retrieved with getinverse method
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        
        ## returns list of functions with definitions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function is used to check whether inverse matrix has been already calculated and stored
## if not, then it will call solve() to calculate inverse function and return it

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## ---------- How To Test -------------------------------
## If you like to test these functions here are some steps:
## 1.Source cachematrix.R file 
## 2. In your work console create square matrix: m1 <- matrix (1:4, 2,2)
## 3. initialize a new matrix by calling makeCacheMatrix with m1 arugment:
## m2 <- makeCacheMatrix(m1)
## 4. check what is m2 by using m2$get()
## 5. calculate inverse matrix by calling cacheSolve(m2), it will print matrix
## 6. Check if you can retreive inverse matrix for m2:
## m2$getinverse()
## 
