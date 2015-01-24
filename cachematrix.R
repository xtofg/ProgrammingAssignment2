# Makes a matric with its cached inverse and access functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # cached inverse
  
  set <- function(y) { # set the stored matrix
    x <<- y
    inv <<- NULL # reset the inverse since the matrix may have changed
  }
  
  get <- function() x # get the stored matrix
  
  setinv <- function(i) inv <<- i # set the cached inverse
  
  getinv <- function() inv # get the cached inverse
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # returns all the functions, for the user
}

# Gets the inverse, using the cached version if available
cacheSolve <- function(x, ...) {
  # Ask for the cached inverse
  inv <- x$getinv()
  
  # Inverse already exists: no need to the compute it again
  if(!is.null(inv)) {
    return(inv)
  }
  
  # Inverse not computed yet: we need to compute it from the stored matrix, using solve
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) # Stored for later
  inv # Returns what the user asked for
}
