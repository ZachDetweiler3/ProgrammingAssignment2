## Below are two functions. One that takes a matrix and caches it and a second function that
## takes that matrix and calculates its inverse. Line items are detailed below.

## This function caches a matrix to be accessed by other functions

makeCacheMatrix <- function(x = matrix()) {
i <- NULL #ignoring m to set x
        set <- function(y) { #in makeVector env, making y stored in x
                x <<- y #assigns input arg to x object in parent env
                i <<- NULL #assigns value of null to m object in parent env
        }#prevents cached values from being reported for new inputs
        get <- function() x #"gets" x out of parent directory
        setinverse <- function(solve) i <<- solve #assign input arg to m object in parent env
        getinverse <- function() i #same as get, but for m
        list(set = set, get = get, #lists the stored results creating a list vector
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function finds the inverse of a matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse() #calling cached value
        if(!is.null(i)) { #if cache value is present return value
                message("getting cached data")
                return(i)
        }
        data <- x$get() #storing input as data
        i <- solve(data, ...)#inverting data and storing to i
        x$setinverse(i)#extracting inverted i
        i#printing i 
}
