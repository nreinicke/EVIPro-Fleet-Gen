# Author: Schatz Energy Research Center
# Original Version: Jerome Carman, April 2019
# Edits: Jerome Carman
# Version: 1.0
# Date: April 25, 2019
# Description: Used to determine weights for PHEVs and BEVS
# Variables
#   N = length of vector to be returned
#   M = Constant that elements of vector must sum to
# Version Changes
#   1.0: JKC adapted from https://stackoverflow.com/a/24846002

randVector <- function(N, M) {
  if(M==0) {
    return(rep(0,N))
  } else {
    vec <- runif(N,min=0.25*M,max=M) #don't allow fraction of a pev type to be less than 25% of M unless M is zero
    vec <- vec * M / sum(vec)
    return(vec)
  }
}
