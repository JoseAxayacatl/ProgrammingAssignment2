#This code compute the inverse of a square matrix
#First makeCacheMatrix has 4 functions inside it: set,get,setsolve and getsolve
#makeCacheMatrix has as argument x which is a square matrix
#set can change the matrix to solve, once both fuctions (makeCacheMAtrix and cacheSolve) were run
#get retrieve the inverse matrix once both fuctions (makeCacheMAtrix and cacheSolve) were run
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

#cacheSolve are a function that take as argument x which is the makeCacheMatrix function 
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) { #if this condition is true, then print the inverse matrix
    return(m)
  }
  data <- x$get() #here data is a new object that use the get element of the list in makeCacheMatrix
  m <- solve(data, ...) #here use the fuction solve to the matrix save un data object
  x$setsolve(m) #then, the solution of the funtion solve (m) its assigned to makeCacheMatrix$setsolve
  m #finally prints m
}