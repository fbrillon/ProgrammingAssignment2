## The goal of this assignment is to provide  a way to store a cached result 
## of the inverse of a special matrix created by makeCacheMatrix. 
##
## 2 functions are provided :
## - makeCacheMatrix - to create the special matrix object able to store cached 
## result
## - cacheSolve - using the special matrix to calculate and store the inverse 
## of a matrix. It can already retrieve a previously calculated result if 
## available.


## Create a special matrix able to store inverse in cache for faster retreival

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(ss) s <<- ss
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Inverse matrix, returns previously cached result if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
