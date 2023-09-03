## The function 'makeCacheMatrix' will initialise a makeCahceMatrix object. This
## object will have an empty matrix as a default and no inverted matrix. 
## Using the 'set' and 'get' functions, users will be able to set/get to the 
## makeCacheMatrix.object. 


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y){ #sets the matrix, while initialising the inverse matrix to null
    x <<- y
    i <<- NULL
  }
  get <- function () x #gets the matrix that was set 
  setinverse <- function (inverse) i <<- inverse #sets the inverse matrix
  getinverse <- function () i #gets (returns) the inverse matrix
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  # creating a list to allow access to the functions by name using the '$' operator
}


## The function takes in an object of type makeCacheMatrix. It first check if 
## the object has an inverse matrix set. If it does, it returns a message with
## the inverse matrix. Else, it will solve for the inverse matrix. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse () #gets the value for the inverse matrix
  if (!is.null(i)){ #if the value is not NULL, returns the inverse matrix
    print("Retrieving inverse of matrix")
    return (i)
  }
  y <- x$get() #gets the matrix set in the object
  i <- solve(y, ...) #solves for the inverse matrix
  x$setinverse(i) #sets the inverse matrix to the object
  i #returns the inverse matrix
}
