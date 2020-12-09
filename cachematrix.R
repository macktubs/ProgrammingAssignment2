makeCacheMatrix <- function(x = matrix()){
  ## This function creates a special "matrix" object that can cache its inverse
  inv <- NULL               ## initializes inv as NULL; will hold value of matrix inverse
  set <- function(y){       ## value of matrix in parent environment
    x <<- y                 ## value of matrix in parent environment
    inv <<- NULL            ## if there is a new matrix, reset inv to NULL
  }
  get <- function() {x}     ## defines the get function - returns value of the matrix argument
  setInverse <- function(inverse) {inv <<- inverse}    ## assigns value of inv in parent environment
  getInverse <- function() {inv}                       ## gets the value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## you need this in order to refer 
                                                                               ## to the functions with the $ operator
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...){
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}