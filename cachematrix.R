# The functions makeCacheMatrix() and cacheSolve() can be used 
# by the following sequence:
#
# Set a matrix, for example:
# A <- matrix(c(1, 2, 4, 4), 2, 2)
#
# Call the special function makeCacheMatrix():
# B <- makeCacheMatrix(A)
#
# Then the function cacheSolve computes the inverse of the 
# matrix:
# cacheSolve(B)
#
##
#  The function makeCacheMatrix() creates a special 
#  "matrix" object that caches its inverse. It is a 
#  list which performs the following tasks:
#   - set the value of the matrix
#   - get the value of the matrix
#   - set the value of the inverse
#   - get the value of the inverse
#
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
#
#  The function cacheSolve computes the inverse of the special 
#  "matrix" returned by makeCacheMatrix. If the inverse has 
#  already been calculated (and the matrix has not changed), then 
#  the function cacheSolve retrieves the inverse from the cache.
#  
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
      }
    data <- x$get()
    m <- solve(data, ...)   # calculates the inverse
    x$setinv(m)
    m
}
