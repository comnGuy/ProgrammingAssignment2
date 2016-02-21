# Example

# source("cachematrix.R")
# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
# my_matrix$getMatrix()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# cacheSolve(my_matrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



# Caches the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

	# Contains the inverse matrix
 	inverseMatrix <- NULL

	# Sets the matrix
      setMatrix <- function(y) {
      	x <<- y
            inverseMatrix <<- NULL
      }

	# Returns the matrix
      getMatrix <- function() {
		x
	}

	# Sets the inverve matrix
      setInverseMatrix <- function(inverse) {
		inverseMatrix <<- inverse
	}

	# Returns the inverse matrix
      getInverseMatrix <- function() {
		inverseMatrix
	}
 
	# Returns the functions
      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}


# Computes the inverse matrix
cacheSolve <- function(x, ...) {
	# Tries to get the matrix
	inverseMatrix <- x$getInverseMatrix()

	# If the matrix exists, bring it back
	if (!is.null(inverseMatrix)) {
		return(inverseMatrix)
	}

	# Get the matrix
	matrix <- x$getMatrix()

	# Inverse the matrix
	inverseMatrix <- solve(matrix, ...)

	# Store the matrix
	x$setInverseMatrix(inverseMatrix)

	# Return
	inverseMatrix 
}