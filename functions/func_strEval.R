# Author: Colin Sheppard
# Original Version: Colin Sheppard
# Edits: Jerome Carman
# Version: 1.0 (original version date 2016-02-26)
# Date: January 9th, 2019
# Description: String evaluate
#   toeval: 
# Version History
#   1.0: Downloaded from Colin Sheppard's GitHub Repository: https://github.com/colinsheppard/colinmisc

streval <- function(toeval){
  eval.parent(parse(text=toeval))
}