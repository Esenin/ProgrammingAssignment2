# @file cachematrix.R 
# @brief contains the functions that add caching functionality to matrices


# @brief creates vector which encapsulates working with matrix and its inverse
# @arg m raw matrix for constructor
# @returns $set() - matrix "setter", $get() - matrix getter
# $setInverseM() - setter for inversed matrix
# $getInverseM() - getter for inversed matrix
makeCacheMatrix <- function(m = matrix()) {
    mInvMatrix <- NULL
    set <- function(y) {
        m <<- y
        mInvMatrix <<- NULL
    }
    get <- function() {
        m
    }
    setInverseM <- function(inversed) {
        mInvMatrix <<- inversed
    }
    getInverseM <- function() {
        mInvMatrix
    }
    list(set = set
         , get = get
         , setInverseM = setInverseM
         , getInverseM = getInverseM)
}


# @brief computing the inverse of a square matrix 
# @arg x square matrix
# @return raw inverse
# @throws exception if the determinant of a matrix equals to zero
cacheSolve <- function(x) {
    invM <- x$getInverseM()
    if(!is.null(invM)) {
        message("info: using cached value")
        return(invM)
    }

    message("computing the inverse")
    
    matrix_ <- x$get()
    
    dimension <- dim(matrix_)
    if (dimension[[1]] != dimension[[2]]) {
        stop("Matrix must be square by requirements")
    }
    
    # assignment says: "assume that the matrix supplied is always invertible."
    invM <- solve(matrix_) # so we don't check correctness of the result
    
    x$setInverseM(invM)
    invM
}


test <- function() {
    m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
    cacheSolve(m)
    cacheSolve(m)
    m$set(matrix(rnorm(81), 9, 9))
    cacheSolve(m)
    invisible(cacheSolve(m))
}

# output messages:
# 
# computing the inverse
# info: using cached value
# computing the inverse
# info: using cached value
