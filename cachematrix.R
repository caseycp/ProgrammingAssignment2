## This script contains two functions: 'cacheSolve' which will solve for the inverse
## of a given Matrix and then store the answer to cache OR pull the answer from
## cache if previously solved; 'makeCacheMatrix' is the storehouse function for 
## the calculations of 'cacheSolve'.

## makeCacheMatrix takes a matrix as argument and contains 4 sub-functions: set, get, getinv, setinv.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    get <- function() x  #just returns the input matrix
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    i <- x$getinv()     
    if(!is.null(i)) {   #checks to see if inverse variable is empty, if not, spits out cached answer
        message("just a moment, Dave...")
        return(i)
    }
    data <- x$get()     #pulls cached matrix, stores as input for solve
    i <- solve(data, ...)   #the actual computation piece
    x$setinv(i)         #stores answer in 'i' and prints to console
    i
    
        
}
