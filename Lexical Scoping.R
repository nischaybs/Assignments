makeCacheMatrix <- function(x = matrix()) {    ## define the argument with default mode of "matrix
  inv <- NULL                                  ## initialize 'inv' as NULL; holds value of inverse matrix
  set <- function(y) {                         ## define the set function to assign new value of matrix in parent                                                  environment
    x <<- y                                    ## assign a value y to an object x
    inv <<- NULL                                 ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                          ##get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                 ## gets the value of invr where called
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)                      ## this is inorder to refer the functions with the $ operator
}
cacheSolve <- function(x, ...) {               ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
       return(inv)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(inv)
  inv
}
