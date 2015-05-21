## These functions when loaded into R as a script, calculate the inverse of an 
## invertible matrix(to be assumed). The script checks if the memory cache for 
## an earlier calculation of an unchanged matrix, thus saving time if inverse for
## the same matrix is required again. Else, it calculates the inverse and prints it.

## This function creates and returns a list of functions set, get, setinverse,
## getinverse. m stores the inverse of the matrix x which is defined in the 
## environment that is the list.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- matrix()
        
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function actually prints the requisite inverse. The 'if' checks if inverse
## of the same matrix has been calculated earlier. If it has, it prints that 
## inverse else it calculates the inverse and prints it.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
        if(!is.na(m)) {
                message("Getting cached data!")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        
}
