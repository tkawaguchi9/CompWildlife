

require(rgdal);require(raster);require(sp);require(ggplot2);require(rgeos);requires(maptools)
require(shapefiles)

##--Parameters----------
point.col.name <- 
  c("P_ID","Date","Time","Long","Lat","Acurcy","Species","Number","Sight","Tag","Note")
point.attr.col.name <- 
  c("P_ID","Date","Time","Acurcy","Species","Number","Sight","Tag","Note")

##--Read shape file----------------------------------------
if(type == "point"){
  x <- shapefile(shape_filename)
  #Attribute table
  attr <- as.data.frame(x@data)
  ## create data frame
  dat <- as.data.frame(matrix(NA,nrow = length(attr[,1]),ncol = length(point.col.name)))
  colnames(dat) <- point.col.name
  #Insert coordinates
  coords <- as.data.frame(x@coords)
  dat$Long <- coords[,1] ; dat$Lat <- coords[,2]
  #For WLC-produced data
  for(i in 1:length(point.attr.col.name)){
    tarNam <- point.attr.col.name[i]
    if(tarNam %in% names(attr)){
      dat[,tarNam] <- attr[,which(tarNam == names(attr))]}
  }
  #TO-DO : For user specified data insertation
  userDefined <- F
  if(userDefined){
    col_convert_befor <- c("GPS_DATE","GPS_TIME","MAX_PDOP", "ESPECE")
    col_convert_after <- c("Date","Time","Acurcy", "Species")
    for(i in 1:length(col_convert_befor)){
      index <- which(names(attr) == col_convert_befor[i])
      colnames(attr)[index] <- col_convert_after[i]}
  }
  #Finalized
  newdat <- dat
}
##For line - All data, except fot coordinates and section ID, will be lost.
if(type == "line"){
  x <- shapefile(shape_filename)
  #Line coordinate data
  lineDat <- x@lines
  datCoord <- list()
  for(i in 1:length(lineDat)){
    coord <- as.data.frame(lineDat[[1]]@Lines[[1]]@coords)
    colnames(coord) <- c("Long","Lat")
    dt <- data.frame(
      LS_ID = rep(i,length(coord[,1])), Point_ID = 1:length(coord[,1]))
    datCoord[[i]] <- cbind(dt, coord)
  }
  dt1 <- do.call(rbind, datCoord)
  #Line - Property data
  propDat <- x@data
  cname <- c("LS_ID","Date","START_TIME","END_TIME","Transect","Tag","Note")
  colName_match <- which(cname %in% names(propDat))
  dt2 <- as.data.frame(matrix(NA,nrow = length(propDat[,1]),ncol = length(cname)))
  colnames(dt2) <- cname 
  dt2[,1] <- 1:length(propDat[,1])
  for(i in 1:length(dt2[,1])){
    for(m in 1:length(colName_match)){
      dt2[i,cname[colName_match[m]]] <- propDat[i,cname[colName_match[m]]]
    }
  }
  #Merge
  newdat <- merge(dt1,dt2,by = "LS_ID")
}
if(type == "Polygon"){
  x <- shapefile(paste0("C:/Users/",wd,"/Desktop/PolyShp/",filePath))
  newdat <- fortify(x)
  newdat <- newdat[,c("id","order","long","lat")]
  colnames(newdat) <- c("P_groupID", "order", "long", "lat")
}

##--Write to txt file-----------------------------------------------------------
if(type == "line"){
  cName <- c("LS_ID","Date","START_TIME","END_TIME","Transect","Tag","Note")
  cNameP <- c("LS_ID","Point_ID","Long","Lat")
  dt1 <- unique(newdat[,cName])
  dt2 <- newdat[,cNameP]
  #Compile txt data into one object
  txtCompile <- "<Line Property Data>"
  txtCompile <- c(txtCompile, paste(cName,collapse = ","))
  for(i in 1:length(dt1[,1])){
    txtCompile <- c(txtCompile, paste(dt1[i,],collapse = ","))}
  txtCompile <- c(txtCompile,"<Line Property Data>")
  txtCompile <- c(txtCompile,paste(cNameP,collapse = ","))
  for(i in 1:length(dt2[,1])){
    txtCompile <- c(txtCompile, paste(dt2[i,],collapse = ","))}
  #Start writing text file
  savename <- paste0(getwd(),"/LineData.txt")
  file.create(savename)
  fileConn <- file(savename)
  writeLines(txtCompile, fileConn)
  close(fileConn)
}else{
  savename <- switch(
    type, Point = paste0(getwd(),"/PointData.txt"), Polygon = paste0(getwd(),"/PolygonData.txt"))
  write.table(newdat,savename,sep = ",",quote = F,row.names = F)
}

print(paste0("The converted file is saved in the following file path :", savename))

