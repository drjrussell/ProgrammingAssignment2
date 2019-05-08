#cachematrix.R ---> by Jamaal Russell


##This function creates a matrix as an input, sets the value of the matrix, get the value,
#set the inverse matrix and get the inverse matrix.

#the matrix input
makeCacheMatrix <- function(x = matrix()) {
    iMatrix <- NULL
    
    #set the value of the Matrix
    setMatrix <- function(y) {
        x <<- y
        iMatrix <<- NULL
    }
    
    getMatrix <- function() x
    setInverse <- function(inverse) iMatrix <<- inverse
    getInverse <- function() invMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
    setInverse = setInverse, getInverse = getInverse)
    
}


## The cacheSolve function takes the output of the makeCacheMatrix as an input then checks for
# input and checks inverse matrix.
# If inverse matrix is empty, it gets the original matrix data and set the imatrix using solve
# function


cacheSolve <- function(x, ...) {
    
    #get the value of the imatrix from the makeCacheMatrix function
    iMatrix <- x$getInverse()
    if(!is.null(iMatrix)) {
        message("Getting Cached Invertible Matrix")
        return(iMatrix)
    }
    
    #if value of the matrix is NULL then
    MatrixData <- x$getMatrix()
    iMatrix <- solve(MatrixData, ...)
    x$setInverse(iMatrix)
    return(iMatrix)
    
    
}
