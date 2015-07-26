# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function (z) 
        {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setinversion <- function (inverse) inv <<- inverse
        getinversion <- function () inv
        
        list (set=set, get=get, setinversion=setinversion, getinversion=getinversion)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        inv_x <- x$getinversion()
        if (!is.null(inv)) 
        {
                message("getting cached")
                return(inv)
        } 
        
        else 
        {
                inv <- solve(x$get())
                x$setinversion(inv)
                
                return(inv)
        }
}
