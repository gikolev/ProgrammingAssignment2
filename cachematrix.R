## The function below creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## The variable inv (the inverse of the matrix) is assigned a null value.
        ## The 'set' function set assigns a new value to the matrix and nulls the inv variable.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## The 'get' function prints the matrix.
        get <- function() x
        ## The 'setinv' function assigns a new value to the inverse of the matrix.
        setinv <- function(inverse) inv <<- inverse
        ## The 'getinv' function prints the cached inverse of the matrix.
        getinv <- function() inv
        ## The functions are gathered in a list.
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
}

## The function below computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), the function
## returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## cacheSolve assigns the cached value for the inverse of 
        ## the matrix to the inv variable.
        inv <- x$getinv()
        ## If the cached value is not null (i.e. if it is stored in the cache) 
        ## it is returned.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## If there is no inverse value in the cache, it is calculated using 
        ## the 'solve' function.
        data <- x$get()
        inv <- solve(data, ...)
        ## The inverse value is then stored in the cache.
        x$setinv(inv)
        ## The inverse value is displayed.
        inv
}
