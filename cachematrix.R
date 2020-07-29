## This function was written by Davide Amato on 29-07-2020

## This function create a list of 4 elements. 
##"get" and "getinverse" are respectively my matrix and its inverse
##"set" and "setinverse" are the cached versions of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function require a cached list of 4 elements.
##It looks for the inverted matrix. If the matrix wasn't solved yet,
##then it invert the matrix, save the result in the cache and give it as result.
##Instead, if the inverted matrix is present in the list, the function just get and return it
#without calcultate it again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    
    message("The matrix was already inverted! Get it!")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  message("First time I invert this matrix. Let's save it")
  m
}


##Test. Create a cached matrix. Run the cacheSolve the first time. Then a second time.
##First time inverse will be calculated and saved, the second time, inverse will be returned
##without any calculation. Using a huge matrix can reveal different timing.

mat=makeCacheMatrix(matrix(rnorm(1000000),1000,1000))
options(digits.secs = 6) # This is set so that milliseconds are displayed

for(i in 1:2){
t0<-Sys.time()
cacheSolve(mat)
t1<-Sys.time()
print(paste("Matrix inverted in",t1-t0,"seconds"))
}

##This is my test result:
# First time I invert this matrix. Let's save it
# [1] "Matrix inverted in 0.828783988952637 seconds"
# The matrix was already inverted! Get it!
# [1] "Matrix inverted in 0.00398802757263184 seconds"
