## The function makeCacheMatrix and cacheSolve take a matrix and find the
## inverse of that matrix. The inverse matrix is then stored in the cache 
## for easy retrieval when the cacheSolve of the original matrix is called.
## This lessens the burden of having to calculate over and over the inverse
## of the matrix. If the matrix changes, the functions allow for the inverse
## matrix to be reset and the new inverse matrix to be calculated and stored
## in the cache.

## makeCacheMatrix has four functions within the makeCacheMatrix function. The
## first function, set, stores the matrix in the cache and resets the inverse matrix.
## The second function, get, retrieves the matrix. The third function, setmatrix, 
## stores the inverse matrix in the cache. The fourth function, getmatrix, retrieves 
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ##sets m to null
  set <- function(y) {  ## sets the matrix and resets inverse matrix, if one exists
    x <<- y
    m <<- NULL
  }
  get <- function() x  ## retrieves the matrix
  setmatrix <- function(matrix) m <<- matrix  ## stores the inverse matrix in the cache
  getmatrix <- function() m  ## retrieves the inverse matrix
  list(set = set, get = get,  ## creates a list of the functions
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve takes a matrix and solves its inverse. If the matrix has already been
## solved previously, cacheSolve retrieves the previously stored inverse matrix
## which had already been calculated. Otherwise, it calculates the inverse matrix 
## and stores the inverse matrix in the cache. cacheSolve returns a message when it 
## pulls a previously stored inverse matrix in order to let you know that it pulled
## a stored inverse matrix instead of calculating the inverse matrix. This allows
## you to quickly pull the inverse matrix of a matrix if you needed to pull it more
## than once and not have to take the time to process the calculation each time the
## inverse matrix is needed.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()  ## retrieves the inverse matrix, if exists, otherwise null
  if(!is.null(m)) {  ## if the inverse matrix already exists, return the inverse matrix
    message("getting cached data")  ## this prints a message to let you know the inverse matrix
                                    ## had previously been calculated and stored in the cache
    return(m)  ## returns the inverse matrix previously stored in the cache
  }
  data <- x$get()  ## retrieves the matrix
  m <- solve(data, ...)  ## calculates the inverse matrix from the original matrix
  x$setmatrix(m)  ## stores the inverse matrix in the cache
  m  ## returns the inverse matrix
}
