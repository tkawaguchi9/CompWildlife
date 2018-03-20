## << shp to txt file COnverter - Main user interface>>

##Usage - Just run the lines below one by one
script_filename <- file.choose() # Please choose the R file named as "fileConverter_Processing"
shape_filename <- file.choose()  # Please choose shape file(.shp).
type <- select.list(c("Point","Line","Polygon"),title = "Choose data type of the shape file")
source(script_filename)



