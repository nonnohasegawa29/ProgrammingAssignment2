## makecachematrix and cachesolve will output you with inverse of matrix inputted
## this was our assignment for R programming week3 


## make cache matrix first

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function (y) {
                x <<- y 
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## then use cacheSolve to solve the cache inverse of x

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if (!is.null(s)) {
                message("Getting Cached Data...")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
        ## Return a matrix that is the inverse of 'x'


##To use this simply run

A<- matrix(c(2,3,4,5,6,7,8,9,1),3,3)
B <- makeCacheMatrix(A)
cacheSolve(B)

