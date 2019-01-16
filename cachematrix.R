## Matrix inversion is usually a costly computation, and so there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. The functions
## below calculate and cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set=set, 
             get=get, 
             setInverse=setInverse, 
             getInverse=getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
## (above). If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } else {
                message("calculating inverse")
                data <- x$get()
                i <- solve(data)
                x$setInverse(i)
                return(i)
        }
}


## ---- test 1 (3x3) ----
matrix_3x3 <- matrix(c(3,0,2,2,0,-2,0,1,1), 3, 3, byrow=TRUE) # from: https://www.mathsisfun.com/algebra/matrix-inverse-row-operations-gauss-jordan.html

test_matrix <- makeCacheMatrix(matrix_3x3)
test_matrix$get()

## first time through, should get inverse w/ message: "calculating inverse"
test_matrix$setInverse(NULL)
cacheSolve(test_matrix)

## second time through, should get inverse w/ message: "calculating inverse"
cacheSolve(test_matrix)

## ---- test 3 (4x4) ----
matrix_4x4 <- matrix(c(4,0,0,0,0,0,2,0,0,1,2,0,1,0,0,1), 4, 4, byrow=TRUE) # from https://www.mathsisfun.com/algebra/matrix-inverse-row-operations-gauss-jordan.html
matrix_4x4

test_matrix <- makeCacheMatrix(matrix_4x4)
test_matrix$get()

## first time through, should get inverse w/ message: "calculating inverse"
test_matrix$setInverse(NULL)
cacheSolve(test_matrix)

## second time through, should get inverse w/ message: "calculating inverse"
cacheSolve(test_matrix)


## ---- test 5 (set function : from 4x4 to 3x3) ----
test_matrix <- makeCacheMatrix(matrix_4x4)
test_matrix$get()
test_matrix$set(matrix_3x3)
test_matrix$get()

## first time through, should get inverse (calculate)
test_matrix$setInverse(NULL)
cacheSolve(test_matrix)

## second time through, should get inverse (cache)
cacheSolve(test_matrix)