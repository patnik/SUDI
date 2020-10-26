
normalize <- function(x)
{
  return((x- mean(x)) /sd(x))
}


replaceMissing <- function(v) replace(v, is.na(v), 0)

expoTransform <- function(ranks) {
  
  prop_ranks <- ranks / max(ranks)
  
  expo <- -23 * log(1 - prop_ranks * (1 - exp( -100 / 23)))
  
  return(expo)
}
