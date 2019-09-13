## Functions to create and cache the inverse of a matrix


## Creates a matrix and cache its inverse
makeCacheMatrix <- function( mat1 = matrix() ) {
        
        ## Initilize inverse
        inv <- NULL
        
        ## Set matrx
        set <- function( matrix ) {
                mat1 <<- matrix
                inv <<- NULL
        }
        
        ##Get matrix
        get <- function() {
                mat1
        }
        
        ## Set the inverse
        set_inv <- function(inverse) {
                inv <<- inverse
        }
        
        ## Get the inverse
        get_inv <- function() {
                inv
        }
        
        ## List all the methods
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## Calculate inverse of matrix from "makeCacheMatrix". If inverse already exist "cachesolve"
## will retrieve the inverse
cacheSolve <- function(x, ...) {
        
        ## Return inverse of matrix 'x'
        mat2 <- x$get_inv()
        
        ## Return inverse if it already exists
        if( !is.null(mat2) ) {
                message("getting inverse from cache")
                return(mat2)
        }
        
        ## Get the matrix
        dat <- x$get()
        
        ## The inverse is calculated by matrix multiplication
        mat2 <- solve(dat) %*% dat
        
        ## Set the inverse
        x$set_inv(mat2)
        
        ## Return the new matrix
        mat2
}
