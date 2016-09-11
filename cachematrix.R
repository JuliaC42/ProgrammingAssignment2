##Create a matrix object that can find and store 
##the inverse of the given matrix and return the 
##inverse value

##This function creates the matrix object and sets
##the value of x:

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##this is the function you call after the value of x
##has been set. It searches the cache and if the value(m) is null
##caches the inverse of the original matrix x. If not null
##the inverse is already there (calculated on a previous call). 
##The (new)value in the cache is returned:

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ##this was saying, if m is not null then it contains the answer and we can return the value in m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ##in the null case we have to perform the solve function 
  m ##we return m which is the inverse of the matrix we put in
}