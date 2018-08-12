## This first function (makeCacheMatrix) will make an "object" of "S3 type".  
##You pass it a matrix (given name "x" within 
## the function environment) and it will store it.  Along with the 
## ability to store the inverse matrix that is calculated in the 
## next function "cacheSolve".  THIS FIRST FUNCTION DOES NOT DO THE INVERTING.
## IT IS ESSENTIALLY A SET OF STORAGE AND RETRIEVAL FUNCTIONS.

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL #calling the inverse "invs" and setting it to NULL - 
        set_matrix <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get_matrix <- function() x
        set_inverse <- function(i) invs <<-i #put passed "i" into "invs" variable
        get_inverse <- function() invs #how to retrieve the inverse matrix

#now return the four functions, each given an identifier in the list for calling
# using terminology like "makeCacheMatrix$set" 
        list(set = set_matrix, get = get_matrix,
        setinverse = set_inverse,
        getinverse = get_inverse)
}

## This second function actually *does the inverting* of the matrix.
## First, however, it sees if the matrix has already been calculated.
## If not present - it calculates the inverse and stores it to the 
## makeCacheMatrix environment with [OBJECT]$setinverse
## if "invs" is stored (i.e. is not null) then it simply retrieves it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
## if there is no stored inverted matrix we calculate it here
## and store it in the makeCacheMatrix object x
        message("no previously stored inverse matrix is available")
        message("calculating...please wait.")
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs
}

