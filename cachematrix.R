#The first function, makeCacheMatrix creates a “matrix”,
#which is a list containing a function to set the value of the matrix, get the value of the matrix, 
#set the value of the inverse matrix and to get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) a <<- inverse
    getinv <- function() a
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function will take the list of functions returned by makeCacheMatrix(). 
#First, it call getinverse(). If there is an inverse matrix stored in the object, 
#then it will return "a" as the result. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    a <- x$getinv()
    if(!is.null(a)) {
        message("cached inverse matrix found, getting the matrix...")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$setinv(a)
    a
}

#Testing the functions
m1 <- matrix(1:4, 2,2)
m1
m2<-makeCacheMatrix(m1)
m2
Inv_m2 <- cacheSolve(m2)
Inv_m2

