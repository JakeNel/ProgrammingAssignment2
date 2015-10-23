##makeCacheMatrix works to both store the value of a matrix
##as well as it's inverse.  cacheSolve calculates the inverse of a matrix and
##stores the value in makeCacheMatrix.  

##This function stores four other functions in a list, all of which set or retrieve
##either the matrix or its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mx <<- inverse
  getinverse <- function() x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##First, this function checks to see if an inverse was already calculated, 
##in which case it returns the inverse that was previously
##calculated without recalculation.  Otherwise, it calculates the inverse and
##and stores the value in makeCacheMatrix()

##x must be set to the value of makeCacheMatrix(); x <- makeCacheMatrix()

cacheSolve <- function() {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  print(data)
  m <- solve(data) %*% data 
  x$setinverse(m)
  m
}