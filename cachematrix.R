## makeCacheMatrix Creates an R object that stores a matrix (x) and its inverse (InvM)

## makeCacheMatrix Returns a list of function : set(), get(), setInverseMatrix(), and getInverseMatrix()
## once an instance of the makeCacheMatrix is created, all theses function will be accessed
## throught the $ sign


makeCacheMatrix <- function(x = matrix()) {
  
  InvM <- NULL
  set <- function(y) {
    x <<- y
    InvM <<- NULL    ## The InvM, defined outside the current anonymous function, will be
                     ## found and accessed in the parent environnement where it was defined.
  }
  get <- function() x
  setInverseMatrix <- function(InverseMatrix) InvM <<- InverseMatrix
  getInverseMatrix <- function() InvM
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## Write a short comment describing this function

## cacheSolve takes an argument x ( x is an makeCacheMatrix object )  and return the inverse
## of the matrix either :
## - The cached one if tne inverse of matrix had been calculated (x$getInverseMatrix() in not NULL)
## - Or calculate the inverse and returning it 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvM <- x$getInverseMatrix()
  if(!is.null(InvM)) {
    message("getting cached data")
    return(InvM)
  }
  data <- x$get()
  print(class(data))
  InvM <- solve(data, ...)
  x$setInverseMatrix(InvM)
  InvM
  
  
}
