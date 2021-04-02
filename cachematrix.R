## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m_i <- NULL
  set <- function(y) {
    x <<- y
    m_i <- NULL
  }
  get <- function() x
  setinv <- function(mat_inv) m_i <<- mat_inv
  getinv <- function() m_i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_i <- x$getinv()
  if(!is.null(m_i)) {
    message("getting cached data")
    return(m_i)
  }
  data <- x$get()
  m_i <- inv(data, ...)
  x$setinv(m_i)
  m_i
}
