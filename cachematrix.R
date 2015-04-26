## Matrix inversion is usually a costly computation and there may be 
##some benefit to caching the inverse of a matrix rather than compute it repeatedly

## using the provided course example (makeVector adn cachemean) we write 
##a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special MAtrix, which is really a list containing a funciton to:
##   1. set the value of the Matrix
##   2. get the value of the Matrix
##   3. set the value of the Inverse
##   4. get the value of the Inverse




makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse,
	getinverse = getinverse)
}

## The cacheSolve function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

## we assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
##Example:
## c1=c(1,1,4)
##> c2=c(0,3,1)
##> c3=c(4,4,0)
##> mat <- cbind(c1,c2,c3)
##> mat
##     c1 c2 c3
##[1,]  1  0  4
##[2,]  1  3  4
##[3,]  4  1  0
##> solve(mat)
##          [,1]        [,2]    [,3]
##c1  0.08333333 -0.08333333  0.2500
##c2 -0.33333333  0.33333333  0.0000
##c3  0.22916667  0.02083333 -0.0625
##> det(mat)
##[1] -48
##> ii <-makeCacheMatrix(mat)
##> cacheSolve(ii)
##          [,1]        [,2]    [,3]
##c1  0.08333333 -0.08333333  0.2500
##c2 -0.33333333  0.33333333  0.0000
##c3  0.22916667  0.02083333 -0.0625
##> ii$get()
##     c1 c2 c3
##[1,]  1  0  4
##[2,]  1  3  4
##[3,]  4  1  0
##> cacheSolve(ii)
##getting cached data.
##          [,1]        [,2]    [,3]
##c1  0.08333333 -0.08333333  0.2500
##c2 -0.33333333  0.33333333  0.0000
##c3  0.22916667  0.02083333 -0.0625


