#To really see the usefulness of these functions, try
#using a matrix that is 2000 x 2000. Then you will see
#that recomputing the inverse is substantially slower
#than pulling it from the cache.


#This function creates a CacheMatrix that is actually a list
#containing a matrix and methods to access it.

#It is designed so that once its inverse is generated, '
#the inverse can be cached so that it does not need to 
#be recomputed if the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        #The <<- operator allows x to be accessed 
        #in the parent environment of the set function,
        #which is the environment of the makeCacheMatrix
        #function
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    #The assignments are necessary here because we could have
    #defined variable names different from the function names;
    #but then, that would just be confusing.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#This function takes a CacheMatrix object and returns
#the inverse of the matrix.
#If it is already cached it pulls from the cache.
#Otherwise it recomputes the inverse.

cacheSolve <- function(x, ...) 
{
    inv <- x$getinv()
    
    if(!is.null(inv)) 
    {
        message("getting cached inverse")
        return(inv)
    }
    else
    {
        message("recomputing inverse and storing to cache")
        data <- x$get()
        inv = solve(data)
        x$setinv(inv)
        return(inv)
    }
}
