#' This is the Euclidean function
#' @param a a number
#' @param b a number
#' 
#' @return the GCD of a and b.
#' @description This is a great function rarely seen in nature. See here: 
#' \url {https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=12&cad=rja&uact=8&ved=2ahUKEwj6nP7jsdHkAhVBtIsKHUfVCM8QFjALegQIABAB&url=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FEuclidean_algorithm&usg=AOvVaw2vg5R_wmq5L2pEWXWyvzB1}
#' @export

euclidean <- function(a,b){
  #' check input type
  stopifnot(is.numeric(a) && is.numeric(b))

  #'obtain maximum and minimum of the inputs
  Max <- max(abs(a),abs(b))
  Min <- min(abs(a),abs(b))
  
  #'calculate the remainder
  remainder <- Max %% Min
  r <- c(remainder)
  

  if(remainder == 0){return(Min)}
  
  #'repeat process until remainder is equal to zero
  repeat{

    Max <- Min
    Min <- remainder
    remainder <- Max %% Min
    r <- c(r,remainder)
    if(remainder == 0){break}
  }
  
  #'return one to the last remainder
  return(r[length(r)-1])
}
