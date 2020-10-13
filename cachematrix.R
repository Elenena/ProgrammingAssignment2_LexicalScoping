makeCacheMatrix <- function(x = matrix()) { #x is the input matrix
    inv <- NULL                             #the inverse value is initialized to NULL
    set <- function(y) { #defining a set function that is not required within the basic run
                        #of the code, but useful whenever I want to change the stored matrix with 
                        # makeCacheMatrix$set(new_matrix)
        x <<- y         #assigning to x the set() argument value in the parent environment
        inv <<- NULL    #cleaning the cache if necessary
    }
    get <- function() x             #just retrieving the matrix value (used in cacheSolve and when testing)
    setinv <- function(inverse) inv <<- inverse #allow to set a new inverse value (when cacheSolve is called)
                                                # if it's the first time that the code is run with a given matrix
    getinv <- function() inv                    #just retrieving inverse value setted with setinv
    list(set = set,
         get = get, #creating the output objet, a list that doesn't contain on its own nor the input matrix value
         setinv = setinv, # (but the value does nonetheless exist in the function environment), nor the inverse value,
         getinv = getinv) #that will be created only with the help of cacheSolve
}
cacheSolve <- function(x) {#input has to be a list created with makeCacheMatrix
    inv <- x$getinv() #checking if the inverse value is already stored in the the cache (that's the case if
    if(!is.null(inv)) { # it's not the first time I run cacheSolve with a given matrix)
        message("getting cached data") #retrieving cached value if it already exist
        return(inv)
    }
    data <- x$get() #if it's the first time I run the code with a particular matrix, here is where the inverse is 
    inv <- solve(data)#calculated and stored into the cache via setinv()
    x$setinv(inv)
    inv #printing the output to the screen
}
#To test the function, you can run: p<-makeCacheMatrix("a square non-singular matrix")
#                                   q<-cacheSolve(p)
#                                   p$get()%*%q, which should return (with computer rounding error)
#                                               the identity matrix