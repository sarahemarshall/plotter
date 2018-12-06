addSubplot <-
function(subPlotParams, x, y, z){
  
  df.subplot <- subPlotParams$df
  #z.subplot <-subPlotParams$df
  xlim.subplot <- subPlotParams$xlim
  ylim.subplot <- subPlotParams$ylim
  cex.axis.subplot <- subPlotParams$cex.axis
  text.subplot <- subPlotParams$text.subplot
  cex.points <- subPlotParams$cex.points
  x.plotSize <- subPlotParams$x.plotSize
  y.plotSize <- subPlotParams$y.plotSize
  pchs <- subPlotParams$pchs
  ltys <- subPlotParams$ltys
  cols <- subPlotParams$cols
  lwds <- subPlotParams$lwds
  

  zval = unique(df.subplot[,z])   
  
  subplot(
    by(df.subplot, df.subplot[,z], function(xx) {
      #print(unique(match(xx[,z], zval)))
      #For first z set up plot window
      if(unique(match(xx[,z], zval))==1){
        plot(NA, NA, ylab="", xlab="", 
             xlim=xlim.subplot, 
             ylim=ylim.subplot,
             cex.axis=cex.axis.subplot)
        text(text.subplot$x, text.subplot$y, text.subplot$text,
        #, 7.1, paste("n=", numSimsCW, sep=""), 
        pos=text.subplot$pos,cex=text.subplot$cex)
      }
 
      #For all z add points
      points(xx[,x],xx[,y], type="o" ,
             pch=pchs[match(xx[,z], zval)],
             cex=cex.axis.subplot,
             col=cols[match(xx[,z], zval)],
             lty=ltys[match(xx[,z], zval)],
             lwd=lwds[match(xx[,z], zval)]
      )
    })#}
    , 
    x=grconvertX(x.plotSize, from='npc'),
    y=grconvertY(y.plotSize, from='npc'),
    type='fig', 
    pars=list( mar=c(1,1,0,0)+0.1, 
               #bottom, left, top, and right. 
               #The default is c(5.1, 4.1, 4.1, 2.1)
               mgp=c(3,0.5,0)) 
    #The first value represents the location the labels (i.e. xlab and ylab in plot), 
    #the second the tick-mark labels, 
    #and third the tick marks. The default is c(3, 1, 0).
  )
  
}
