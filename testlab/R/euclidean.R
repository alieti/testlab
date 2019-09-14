#' Euclidean algorithm.
#'
#' \code{euclidean(a, b)} returns greatest common divisor (GCD) of two numbers
#' in its arguments.#'
#'
#' The Euclidean algorithm is an efficient method for computing the greatest
#' common divisor (GCD) of two numbers. The GCD is the largest natural number
#' that divides both a and b without leaving a remainder. The Euclidean
#' algorithm proceeds in a series of steps such that the output of each step is
#' used as an input for the next one. For illustration, the Euclidean algorithm
#' can be used to find the greatest common divisor of a = 1071 and b = 462.
#' Steps of the algorithm would be as follows:
#'  1071 = 2 × 462 + 147./ 462 = 3 × 147 + 21./ 147 = 7 × 21 + 0.
#' Since the last remainder is zero, the algorithm
#' ends with 21 as the greatest common divisor of 1071 and 462.
#' \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @param a Numeric scalar.
#' @param b Numeric scalar.
#' @return If all inputs are integer, then the output will be an integer.
#' @export


euclidean <- function(a,b){
  stopifnot(is.numeric(a) && is.numeric(b))
  # Finding the maximum and minimum number.
  Max <- max(abs(a),abs(b))
  Min <- min(abs(a),abs(b))
  
  # Computing the remainder of division of Max by Min, then assigning it to the vector r.
  remainder <- Max %% Min
  r <- c(remainder)
  
  # If the remainder is 0, then the greatest common divisor(GCD) would be the Min.
  if(remainder == 0){return(Min)}
  
  
  # Continue searching for the GCD until the remainder is 0.
  repeat{
    # In each loop the new Max would be the previous Min,
    # and the new Min would be the previous remainder.
    
    Max <- Min
    Min <- remainder
    remainder <- Max %% Min
    r <- c(r,remainder)
    if(remainder == 0){break}
  }
  
  # r is the vector of all remainders with the length n,
  # and the (n-1)th element would be the GCD.
  return(r[length(r)-1])
}
