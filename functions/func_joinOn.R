# Author: Colin Sheppard
# Original Version: Colin Sheppard
# Edits: Jerome Carman
# Version: 1.0 (original version date 2016-02-26)
# Date: January 9th, 2019
# Description: Function that encapsulates common pre- and post- join operations with data tables.
#   dt1: The first data table in the join, all columns and rows from dt1 will be present in the result with columns from dt2 added where keys match.}
#   dt2: The second data table in the join, dt2 should be seens as contributing data to the rows of dt1.}
#   keys1: The columns form dt1 to use as keys in the join.}
#   keys2: The columns form dt2 to use as keys in the join. Leave as NULL if the keys from dt2 are identical to keys1.}
#   include.from.dt2: The columns from dt2 to add to dt1. Leave as NULL to include all columns.}
#   included.prefix: Optional prefix to append to the columns from dt2 that are added to dt1.}
# Version History
#   1.0: Downloaded from Colin Sheppard's GitHub Repository: https://github.com/colinsheppard/colinmisc

join.on <- function(dt1,dt2,keys1,keys2=NULL,include.from.dt2=NULL,included.prefix='',allow.cartesian=F){
  dt1 <- copy(dt1)
  dt2 <- copy(dt2)
  if(is.null(keys2))keys2<-keys1
  if(!all(keys1==keys2)){
    for(i in 1:length(keys1)){
      streval(pp('dt2[,',keys1[i],':=',keys2[i],']'))
    }
  }
  streval(pp('setkey(dt1,',pp(keys1,collapse=','),')'))
  streval(pp('setkey(dt2,',pp(keys1,collapse=','),')'))
  if(is.null(include.from.dt2)){
    cols.to.include <- names(dt2)[-which(names(dt2)%in%c(keys1,keys2))]
  }else{
    cols.to.include <- include.from.dt2
  }
  if(included.prefix!=''){
    for(col.to.include in cols.to.include){
      streval(pp('dt2[,',included.prefix,col.to.include,':=',col.to.include,']'))
    }
    cols.to.include <- pp(included.prefix,cols.to.include)
  }
  cols.to.include <- c(cols.to.include,keys1)
  res.dt <- streval(pp('dt2[,list(',pp(cols.to.include,collapse=','),')][dt1,allow.cartesian=',allow.cartesian,']'))
  return(res.dt)
}