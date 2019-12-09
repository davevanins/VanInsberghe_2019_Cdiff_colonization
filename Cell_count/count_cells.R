library(EBImage)

countCells <- function(img1){
  #resize image
  img = resize(img1,(dim(img1)[1])/2)
  #img = medianFilter(img,2)
  
  # blur the image
  f = array(1, dim=c(5,5))
  f = f/sum(f)
  img_flo = filter2(img, f)
  #display(img_flo, method = "raster")
  
  # apply a threshold
  nmaskt = thresh(1.0-img_flo, w=10, h=10, offset=0.03)
  #display(nmaskt, method = "raster")#,all = TRUE)
  
  nmask = watershed( distmap(nmaskt), 2 )
  #display(colorLabels(nmask), method = "raster",all = TRUE)
  
  nucNo <- bwlabel(nmaskt[,,1])
  fts = computeFeatures.shape(nucNo)
  sarea = fts[1:nrow(fts)]
  cellNo = sum(sarea>(mean(sarea)-(sd(sarea)/2)))
  cat(i,'=', cellNo,'\n')
  return(cellNo)
}
