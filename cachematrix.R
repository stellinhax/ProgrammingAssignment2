### R Programming Assignment 2 - Data Science Specialization rprog-011

# The following script shows a pair of functions that cache and compute the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(x) {
    x <<- y;
    m <<- NULL;
  }
  # get the value of the matrix
  get <- function() return(x);
  setinverse <- function(inv) m <<- inv;
  getinverse <- function() return(m);
  # return list 
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

# The next function computes the inverse of the matrix returned by "makeCacheMatrix". 

# "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }
  data <- x$get()
  # Calculate the inverse matrix using the function solve()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
