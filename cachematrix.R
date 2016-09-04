## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## I create makeCacheMatrix as a list containing a function for the following activities
## 1 First set the value of a matrix
## 2 Second get the value of the matrix
## 3 Alternatively setting next the value of inverse of the matrix
## 4 and last is get the value of inverse of the matrix

makecachematrix<-function(x = matrix()) {
  inverse1 <- NULL 
  set <- function(n) {
    x <<- n
    inverse1 <<- NULL
  }
  get <- function() x
  
  setInversefun <- function(inverse) inverse1 <<- inverse
  getInversefun <- function()inverse1
  list( set = set, get = get, setInversefun=setInversefun, getInversefun=getInversefun)
}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cachesolve <- function(x, ...){
  inverse1 <- x$getInversefun() ##returns matrix that is the inverse of 'x'
  if (!is.null(inverse1)){
    message("get cache data")
    return(inverse1)
  }
  matrixnew <- x$get()
  inverse1 <- solve(matrixnew, ...)
  x$setInversefun(inverse1)
  inverse1
  
  }


##source('C:/Users/monalisa.samal/Desktop/IIMB/Module 3/Day 5/Matrix.R')

##test the matrix
testmatrix<- makecachematrix(matrix( 1:6, 2, 2))
testmatrix$get()
testmatrix$getInversefun()
cachesolve(testmatrix)

testmatrix$set(matrix(c(10,20,30,40),2,2))
testmatrix$get()
testmatrix$getInversefun()
cachesolve(testmatrix)

##another test from internet maths
testmatrix3<-matrix(c(1,3,3,1,4,3,1,3,4),3,3)
t(testmatrix3)
testmatrix4<-makecachematrix(t(testmatrix3))
testmatrix4$get()
testmatrix4$getInversefun()
cachesolve(testmatrix4)

