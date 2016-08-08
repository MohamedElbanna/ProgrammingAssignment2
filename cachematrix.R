## Put comments here that give an overall description of what your
## functions do

## this function the function cotaining the setters and getters for the object
## holding the matrix and the setters and getters for the object holding the 
## the inverse of the matrix ( Note: when the set function is called it makes
## sure that the M variable which is treated as the cache is set to null
## in order not to have the inverse value of a previous matrix and for a new
## added  matrix and add the computed inverse value of the new matrix instead)

makeCacheMatrix <- function(x = matrix())
{
  
  m<-NULL
  
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inv) m<<-inv
  getinverse<-function() inv
}


## this function first uses the getter of the first function to 
## retrieve the cached inverse value of the matrix if the value is null (no cached
## value) then it will get the value of the matrix by calling the get function
## of the first function then applying function solve to get inverse of the 
## matrix then calling the setinverse function to add the inverse matrix value
## to the cache (m variable of the first function) else if the variable M is not 
## null (meaning there is value in the cache) then it will return its value

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
