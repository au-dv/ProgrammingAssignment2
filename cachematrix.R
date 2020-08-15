## Funciones para obtener una matriz inversa 
## 

## Esta función crea una "matrix" especial donde podamos
## depositar su inversa

makeCacheMatrix <- function(x = matrix()) {
  matriz <- NULL
  set <- function(y) {
    x <<- y
    matriz <<- NULL
  }
  get <- function() x
  setInverso <- function(inverse) matriz <<- inverse
  getInverso <- function() matriz
  list(set = set, 
       get = get, 
       setInverso = setInverso, getInverso = getInverso)
}
## Con esta funcion se crea la matriz inversa

cacheSolve <- function(x, ...) {
  matriz <- x$getInverso()
  if(!is.null(matriz)) {
    message("getting cached data")
    return(matriz)
  }
  data <- x$get()
  matriz <- solve(data, ...)
  x$setInverso(matriz)
  matriz
}