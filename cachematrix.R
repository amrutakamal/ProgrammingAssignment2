##  Name: Amruta Joshi 
#Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.
##2.cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). 
##If the inverse has already been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.


## Write a short comment describing this function
## @x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
                 m<- NULL
                 set<- function(y){
                 x<<-y
                 m<<- NULL
                } 
               get <- function()x
               setmatrix<- function(solve)
                 m<<- solve
                 getmatrix<- function()m
                list(set=set, get= get,
                setmatrix=setmatrix,
                getmatrix=getmatrix)
}




## Write a short comment describing this function
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()


cacheSolve <- function(x= matrix(),...){
  m<- x$getmatrix()
                                 # if the inverse has already been calculated
  if(!is.null(m)){
                                ## get it from the cache and skips the computation.
    message("getting cached data")
    return (m)
  }
  
  matrix<-x$get()
  m<- solve(matrix,...)
  x$setmatrix(m)           # sets the value of the inverse in the cache via the setinv function.

  m
}

