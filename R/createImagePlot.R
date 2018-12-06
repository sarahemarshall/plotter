createImagePlot <-
function(data, x, y, z, 
                           main="",
                           expbreakseq = NULL,
                           xlabel=x, ylabel=y, zlabel=z,
                           xlims = NULL, ylims= NULL, 
                           onefig=FALSE, #addPoint=list(type=FALSE), 
                           figprefix=NA,  #cexleg=1, 
                           log=TRUE,
                           plotcols=NULL,
                           # bwPlot=FALSE, 
                           # figprefix_bw="",
                           # mar.add=NULL, 
                           # subplot=FALSE,
                           # subPlotParameters=NULL,
                           # text.plot=NULL, 
                           #sig=10,
                           ...){
  
  if(!is.na(figprefix)){
    
    outname <-paste(figprefix,  ".pdf",sep="")
    endOfpath <- max(gregexpr("/", outname)[[1]])
    outname <- paste0(substr(outname,1,endOfpath),
                      substr(outname, endOfpath+1,  nchar(outname)) )
    pdf(file=outname)
    #print(outname)
  } 
  
  #Create plot
  xmat  <-as.numeric(unique(data[,x]))
  ymat  <-as.numeric(unique(data[,y]))
  smat  <- reshape(data[,c(x, y, z)], v.names = z, idvar = x,
                   timevar = y, direction = "wide")
  smat <- as.matrix(smat[,2:ncol(smat)] )
  if(is.null(expbreakseq)){
    #expbreakseq = exp(seq(log(min(data[,z])), log(max(data[,z])), length.out=10))
    expbreakseq = sort(c(
      min(data[,c(z)]), 
      0.1,  2, 5, 10, 50, 100, 500, 1000, 10000, 50000,
      1e6,
      1e7,
      1e8,
      1e9,
      1e10,
      max(data[,c(z)])
    ))
  }
  
  if(log){
    breakseq = c(log(expbreakseq))
    smat.adj <- log(smat)
  }else{
    breakseq = expbreakseq
    smat.adj <- smat
  }
  numcols = length(breakseq) - 1
  imageplotlims = c(min(breakseq), max(breakseq))
  if(onefig){
    par(pty = "s")
    par(
      cex = 0.8, cex.axis = 1.5, cex.lab = 2, mar = c(5.1,6.1,4.1,2.1)
    )
  }
  if(is.null(plotcols)){
    plotcols = jet.colors(length(breakseq)-1)
  }
  s = simage(
    xmat, ymat, smat.adj,
    col =plotcols,
    breaks = breakseq,
    lab.breaks = expbreakseq,
    legend.args = list(
      text = TeX(zlabel), cex = 2,
      line = 1, at = 2.1
    ),
    legend.mar = 8,  #width of legend label
    legend.width = 2, #width of leg colour bar
    slim = imageplotlims,
    #legend.shrink = .08,
    main= main,
    las=1,
    xlab = TeX(xlabel), ylab = TeX(ylabel), las=1
  )
  
  if(!is.na(figprefix)) dev.off()
  
}
