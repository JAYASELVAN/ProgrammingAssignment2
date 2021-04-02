## Put comments here that give an overall description of what your
## functions do

## Creating a Cache Memory with 4 functions namely to get data, set data, get inverse and set inverse

makeCacheMatrix <- function(x = matrix()) {
  m_i <- NULL
  set <- function(y) {
    x <<- y
    m_i <- NULL
  }
  get <- function() x
  # Setting inverse incase of inverse is calculated by cacheSolve function
  setinv <- function(mat_inv) m_i <<- mat_inv
  # used to send inverse mat to cacheSolve function
  getinv <- function() m_i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## checking for cache for inverse, if not present calculated the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_i <- x$getinv()
  if(!is.null(m_i)) {
    message("getting cached data")
    return(m_i)
  }
  # getting matrix from makeCacheMatrix function
  data <- x$get()
  # Calculating inverse
  # inv method is avaulable in matlib library
  m_i <- inv(data, ...)
  #m_i <- solve(data, ...)
  x$setinv(m_i)
  m_i
}
