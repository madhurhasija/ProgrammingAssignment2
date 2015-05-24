## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                               # variabel to store invertied matrix
  set<-function(y){                     #function assign the matrix from other funtciton in otehr envirnment
    x<<-y
    m<<-NULL
  }
  get<-function() x                      # used for returning the current matrix
  setmatrix<-function(solve) m<<- solve  # used for saving the invering matrix i.e. implementing cacheing requirement
  getmatrix<-function() m                # reutrned the cached matrix
  list(set=set, get=get,                 # list of all the components
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  y<- x$get()
  if(all(x == y))                           # checks if previous matrics is the same as current
  {
    m<-x$getmatrix()                         # get previous value of inverse matrix if there
    if(!is.null(m)){               
      message("getting cached data")
      return(m)
    }
  }
  matrix<-x$get()                            
  m<-solve(matrix, ...)                      # calculate the inverse matrix
  x$setmatrix(m)                             # save matrix for caching benefits
  m
  
}