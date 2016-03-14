## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             # initialize inv as NULL
  set <- function(y) {                    # define the set function to assign new 
    x <<- y                               # value of matrix in parent environment
    inv <<- NULL                          # if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     # define the get fucntion - returns value of the matrix argument
      
  setinverse <- function(inverse) inv <<- inverse  # assigns value of inv in parent environment
  getinverse <- function() inv                     # gets the value of inv where called
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                 # initialize inv to get the inverse
     
  if(!is.null(inv)) {                   # return inverse if not NULL
          message("getting cached data")
          return(inv)
      }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv                                 # return inverse
}
