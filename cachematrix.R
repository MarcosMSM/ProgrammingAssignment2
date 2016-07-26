# Peer Graded Assignment: Programming Assignment 2: Lexical Scoping
# Assignment: Caching the Inverse of a Matrix

# Write the following functions:

# 1- makeCacheMatrix: This function creates a special "matrix" 
#    object that can cache its inverse.

# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the Inverse
# 4. get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) 
{
        
        Inverse <- NULL
        
        #set the value of the vector
        set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
        
        #get the value of the vector
        get <- function() x
        
        #set the value of the Inverse
        setInverseMatrix <- function(InverseMatrix) Inverse <<- InverseMatrix
        
        #get the value of the Inverse
        getInverseMatrix <- function() Inverse
        list(     
                set=set, 
                get=get, 
                setInverseMatrix = setInverseMatrix, 
                getInverseMatrix = getInverseMatrix
        )
}

# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been 
#    calculated (and the matrix has not changed), then cacheSolve should 
#    retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the cached value
        Inverse <- x$getInverseMatrix()
        
        # Return inverse of cache
        if(!is.null(Inverse)) {
                message("getting cached data.")
                return(Inverse)
        }
        
        #caculate the inverse
        data <- x$get()
        Inverse <- solve(data)
        x$setInverseMatrix(Inverse)
        
        # return the inverse
        Inverse
}

# Run ==========================================================================

mdat <- matrix(c(5,-2,-2,5), nrow = 2, ncol = 2)
mcm  = makeCacheMatrix(mdat)
mcm$get()

# > mdat <- matrix(c(5,-2,-2,5), nrow = 2, ncol = 2)
# > mcm  = makeCacheMatrix(mdat)
# > mcm$get()
# [,1] [,2]
# [1,]    5   -2
# [2,]   -2    5


# First run: No cached data ====================================================

cacheSolve(mcm)

# Result:
# > cacheSolve(mcm)
# [,1]      [,2]
# [1,] 0.2380952 0.0952381
# [2,] 0.0952381 0.2380952


# Second run: getting cached data ==============================================

cacheSolve(mcm)

# Result:
# > cacheSolve(mcm)
# getting cached data.
# [,1]      [,2]
# [1,] 0.2380952 0.0952381
# [2,] 0.0952381 0.2380952

