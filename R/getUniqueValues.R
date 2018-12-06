getUniqueValues <-
function(df, x, y, z, w, ncols=dim(df)[2]){  
  u = apply(df[,1:ncols],2,unique)
  u[x] = NULL; u[y] = NULL; u[z] = NULL; u[w] = NULL; 
  return(u)
}
