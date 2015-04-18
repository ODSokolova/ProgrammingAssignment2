## Here is a function that cache the inverse of a matrix
## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its solve (inverse variant).

## The first function creates a matrix (set/get value of the matrix and set/get value of the solve)

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
                {
                x <<- y
                m <<- NULL
                }
        get <- function() {x}
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)        
}

## The following function calculates the solve of the matrix created with the above function. 
## Firstly it checks if the solve has already been calculated. 
## If so, it gets the solve from the cache and skips the computation. 
## Otherwise, it calculates the solve of the matrix and sets the value of the solve in the cache via the setmatrix function.

cacheSolve <- function(x, ...) 
{
        m <- x$getmatrix()
        if(!is.null(m)) 
                {
                message("getting cached data")
                return(m)
                }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        return(m)        
}
