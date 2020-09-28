##Description

## Create a special "matrix" object that can cache its inverse.
# set: stores a matrix value and initializes the variable inv in NULL in cache
# get: get the value of the matrix in cache
# setInv: assign a value to the variable inv in cache
# getInv: get the value of the inv in cache

makeCacheMatrix<- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(i) inv <<- i
        getInv <- function() inv

        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


# Check if the variable inv has an assigned value, if it does,
# returns the inverse matrix, otherwise it calculates it

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

print("------------------------------------------------------------------------------")
mtx <- makeCacheMatrix()
#square matrix: same size of columns and rows
mtx$set(matrix(c(1,8,4,2,4,5,5,1,2,2,5,4,4,1,1,2),nrow=4,,ncol=4))

#Return the matrix given in makeCacheMatrix()
print(mtx$get())
print("------------------------------------------------------------------------------")

#Return the inverse of matrix
cache<-cacheSolve(mtx)
print(cache)

print("------------------------------------------------------------------------------")

#Check that the variables mtx and inv are already store in cache typing the next command
#once you are executed the program.
print(cacheSolve(mtx))
