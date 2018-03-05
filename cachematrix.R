## MakeCacheMatrix add to the matrix the next properties: 
##  *set: A function to set the value of the matrix
##  *get: A function to get of the matrix
##  *setInvM: A function to set the value of the inverse of matrix
##  *getInvM: A function to get the value of the inverse of matrix
## cacheSolve gets the inverse of the matrix 
## if the inverse has been calculated and saved the function return the inverse 
## but if the inverse hasn't been calculated it  calculates it saves it and returns it

## The twoo function were based in the functions shown in the example "Caching the Mean of a Vector"

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvM <- function(inverseM) invM <<- inverseM
  getInvM <- function() invM
  list(set = set, get = get,
       setInvM = setInvM,
       getInvM = getInvM)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM = x$getInvM()
  if (!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  Mat <- x$get()
  invM <- solve(Mat)
  x$setInvM(invM)
  #Next line prints the inverse of the matrix
  invM
  return(invM)
}
