outlier_index = function(dt, iqr_v){
  data_x = as.data.frame(dt)
  out_dt = data.frame(Instance = numeric(), Feature = character(), Type = character())
  for(i in 1:ncol(data_x)){
    v1 = data_x[ , i]
    naS = grep(TRUE, is.na(v1))
    if(length(naS) != 0){
      v1[naS] = rep(0, length(naS))
    }
    iqr_value = iqr_v
    q1 = quantile(v1, 0.25)
    q3 = quantile(v1, 0.75)
    iqr = IQR(v1)
    out = grep(TRUE, v1 < q1 - (iqr_value * iqr))
    out = c(out, grep(TRUE, v1 > q3 + (iqr_value * iqr)))
    if(length(out) != 0){
      out_dt_temp = data.frame(Instance = out, Feature = names(data_x)[i], Type = 'Outlier')
      out_dt = bind_rows(out_dt, out_dt_temp)
    }
  }
  return(out_dt)
}