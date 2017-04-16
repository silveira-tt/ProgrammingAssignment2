#The explanation for inverse of a matrix
#https://www.mathsisfun.com/algebra/matrix-inverse.html

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    #list of all function created...
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from 
#the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("solving inverse...")
    data <- x$get()
    #the solve() function calculate the inverse of a matrix (to check, calculate
    #in this way: > solve(data, ...) %*% data)
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
