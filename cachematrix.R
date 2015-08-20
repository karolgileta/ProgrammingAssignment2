#function that stoes a list of functions that we can use later (set, get, setinverse, get inverse)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { #changes vector stored in the main function
                x <<- y
                m <<- NULL
        }
        get <- function() x #returns x
        setinverse <- function(solve) m <<- solve  #stores the value of the input in a variable m into the main function makeCacheMatrix (setinverse) and returns it (getinverse)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# returns an inverse of matrix x using functions from makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #calulates inversed matrix
        x$setinverse(m) #stores the inversed matrix
        m
}
