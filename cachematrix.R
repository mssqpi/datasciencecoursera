## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "matrix" which can be chached

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL             # A variable to store the inverse matrix
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInvers <- function(solve) x_inv <<- solve     #To set the value of the inverse of the matrix
  getInvers <- function() x_inv                    #To get the value of the inverse of the matrix
  list(set = set, get = get,                       # To list all atributes of the x object
       setInvers = seInvers,
       getInvers = getInvers)
}


## The following function calculates the inverse of the x "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInvers()
  if(!is.null(x_inv)) {                       # check if the inverse of the matrix has been calculated or changed
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)                   # calculate the inverse of the x using the solve function
  x$setInvers(x_inv)
  x_inv
}
