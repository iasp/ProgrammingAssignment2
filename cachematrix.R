## My 2 complementary functions (or wrapping my head around the concept of caching and matrix inversion)
## The first function creates a special matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        cached_matrix <- NULL
        set <- function(y) {
                x <<- y
                cached_matrix <<- NULL
        }
        get <- function() x
        setCachedMatrix <- function(cached) cached_matrix <<- cached
        getCachedMatrix <- function() cached_matrix
        list(set = set, get = get, 
             setCachedMatrix = setCachedMatrix, 
             getCachedMatrix = getCachedMatrix)
}


## The second function computes the inverse of the matrix returned above. 
## But: if the inverse has already been calculated then ... it retrieves the inverse from the cache!

cacheSolve <- function(x, ...) {
        cached_matrix <- x$getCachedMatrix()
        if (!is.null(cached_matrix)) {
                message("Getting cached data")
                return(cached_matrix)
        }
        input_matrix <- x$get()
        
        # This checks if the determinant is non-zero before attempting to solve
        det_input_matrix <- det(input_matrix)
        if (det_input_matrix == 0) {
                stop("Cannot compute the inverse.")
        }
        
        cached_matrix <- solve(input_matrix, ...)
        x$setCachedMatrix(cached_matrix)
        cached_matrix
}

# Please test if it works:
# Note: the first matrix does not work but the next two do:
matrix_one <- matrix(c(1, 2, 3, 2, 4, 1, 3, 6, 5), nrow = 3, ncol = 3)
matrix_two <- matrix(c(1, 2, 3, 2, 5, 1, 3, 6, 4), nrow = 3, ncol = 3)
another_matrix <- matrix(c(1, 2, 0, 2, 3, 1, 0, 1, 4), nrow = 3, ncol = 3)

# Using the first function:
mymat <- makeCacheMatrix(matrix_one)
mymat2 <- makeCacheMatrix(matrix_two)
mymat3 <- makeCacheMatrix(another_matrix)
mymat4 <- makeCacheMatrix(fourth_matrix)

# Lastly, using the second:
cacheSolve(mymat)
cacheSolve(mymat2)
cacheSolve(mymat3)