## Calculate and store in cache memory the inverse of a square matrix .[ISA-R].

## store the matrix input and some anonymous functions in their own environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(inv) m <<- inv
  get.inv <- function() m
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}


## Solve the inverse of the matrix or take the cache value if same input-matrix is supplied

cacheSolve <- function(x, ...) {
    m <- x$get.inv()
    if(!is.null(m)) {
      message("getting cached matrix")
      return(m)
    }
    mx <- x$get()
    m <- solve(mx, ...)
    x$set.inv(m)
    m
          ## Return a matrix that is the inverse of 'x'
}

#test
#make some "big" matrix (square [1000,1000] matrix)
set.seed(1970)
test.mx <- matrix(rnorm(1000000,298.143,37),1000,1000)

# set the data and the list-environment
cache.mx <- makeCacheMatrix(test.mx)

# first run, time of calculation returned
system.time(cacheSolve(cache.mx))

# second run, time = 0, value obtained from the cache 
system.time(cacheSolve(cache.mx))
