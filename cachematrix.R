## cacheMatrix.R
## For Coursera R programming Assignment 2
## By Peter Lovett
## October 2014


## The makeCacheMatrix function
## This takes a matrix
## Returns a list of four functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        # Create the 'setter' function to set the value of the matrix
        set <- function(y) {
            x <<- y
            m <<- NULL
        }

        # Create the function to return the matrix
        get <- function() x

        # Create a function to set the value of the inverse
        setinv <- function(inv) m <<- inv

        # Create a function to return the value of the inverse
        getinv <- function() m

        # Return a list of the four functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }
}


## The cacheSolve function
## This takes a matrix
## Returns a list of four functions

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()     # Look for the inverted matrix in the cache
    if(!is.null(m)) {
        # Inverted matrix is in the cache; return it
        message("Getting cached data")
        return(m)
    }
    data <- x$get()     # Get the data
    m <- solve(data)    # Calculate the inverse of the matrix
    x$setinv(m)         # Store the inverse into the cache
    m                   # Return the inverted matrix
}

# End of program
