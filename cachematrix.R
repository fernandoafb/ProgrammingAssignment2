## The functions below create a cacheable matrix objet and 
## solve (e.g. find the inverse) of it, using the cache
## whenever it is valid.

## The 'makeCacheMatrix' function receives a matrix 'x'
## and creates a cacheable matrix object of it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## The 'cacheSolve' function receives a cacheable matrix
## object 'x' and solves it (e.g. finds its inverse) if
## there is no cache 'i', otherwise it uses the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i    
}
