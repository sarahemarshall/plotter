getFigName <-
function(str, u,x, y, z, w, numSimsCW="unknown", theory=FALSE, outpath=""){
  ulen <- length(u)
  u.char <- which(names(u) %in% c("distOn", "distOff", "processType"))
  u.numeric = setdiff(1:ulen, u.char)
  #u.char = setdiff(1:ulen, u.numeric)
 # pars = paste(names(u)[1:ulen ], u[1:ulen], 
 #              sep="", collapse=""); 
  
  #print(u)
  #print(u.char)
  if(is.null(u)){
   pars="" 
  }else{
  if(any(unlist(lapply(u, length))>1)) stop("Error in getFigName(). At least one variable has more than one value.", 
                                            lapply(u, length))
  
  pars = paste(
        paste(names(u)[u.numeric ], as.numeric(u[u.numeric]),  sep="", collapse=""),
        paste(names(u)[u.char ], (u[u.char]), sep="", collapse=""),
        sep=""); 
  
  
  pars = gsub(".", "", pars, fixed=TRUE)
  }
  nsims.name <- get.nsims.name(numSimsCW)
  
  if(theory) figname=paste(outpath, "fig", str, "_", x, y, z, "_", "theory", "_", pars, sep="")
  else figname=paste(outpath, "fig", str, "_", x, y, z, "_", nsims.name, "_", pars, sep="")
  figname = gsub(".", "dot", figname, fixed=TRUE)
  #print(figname)
  
  return(figname)  
}
