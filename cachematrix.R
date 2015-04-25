## Matrix inverse computation is a costly operation, these functions/objects will
## wrap a matrix and store its computed inverse. So you'll only have to compute
## the inverse once. The next time it will get it from cache
## instead of recomputation
##
## Sample:
## mc <- makeCacheMatrix(matrix(c(2,2,2,1,0,0,0,0,1),3))
## > cacheSolve(a)
##         [,1] [,2] [,3]
##   [1,]    0  0.5    0
##   [2,]    1 -1.0    0
##   [3,]    0 -1.0    1
##
## cacheSolve(a)
## getting cached data
##         [,1] [,2] [,3]
##   [1,]    0  0.5    0
##   [2,]    1 -1.0    0
##   [3,]    0 -1.0    1
##

## Would take as input the matrix to invert and return
## an object which contains functions
## for storing and retrieve its value and inverse

makeCacheMatrix <- function(x = matrix()) {
  nvrs <- NULL
  set <- function(y) {
    x <<- y
    nvrs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) nvrs <<- inverse
  getInverse <- function() nvrs
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Would take as input a makeCacheMatrix object
## Will check if the inverse has already been computed
## if it is not it will compute the inverse and store it
## in the passed object, else will display a message and
## return the cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    nvrs <- x$getInverse()
    if(is.null(nvrs)) {
        data <- x$get()
        nvrs <- solve(data, ...)
        x$setInverse(nvrs)
    } else {
        message("getting cached data")
    }
    nvrs
}
