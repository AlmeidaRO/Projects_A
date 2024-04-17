na_index = function(dt){
  data_x = as.data.frame(dt)
  na_dt = data.frame(Instance = numeric(), Feature = character(), Type = character())
  for(i in 1:ncol(data_x)){
    naS = grep(TRUE, is.na(data_x[, i]))
    if(length(naS) != 0){
      na_dt_temp = data.frame(Instance = naS, Feature = names(data_x)[i], Type = 'NA')
      na_dt = bind_rows(na_dt, na_dt_temp)
    }
  }
  return(na_dt)
}