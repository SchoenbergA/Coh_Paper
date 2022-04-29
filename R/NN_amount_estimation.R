### Data preprocessing (projection)

# set envi
wd <- "C:/Envimaster/Coh_Paper" # main folder
dat <- file.path(wd,"Data/org") # folder with data
out <- file.path(wd,"Data/output/NN_res")

# load packages
require(openxlsx)
require(mapview)
require(sp)
require(rgdal)# only for output write OGR
require(raster)
require(LinguGeo)

### data preprocessing #########################################################

# load data
maurer <- openxlsx::read.xlsx(xlsxFile =file.path(dat,"DATEN GESAMT.xlsx"))

# check if all entires have XY Coordinates
any(is.na(maurer$LONG))
any(is.na(maurer$LAT))

# get column position for conversion to spatial object
which(colnames(maurer)=="LONG")
which(colnames(maurer)=="LAT")

# check if Coordinates are numeric
class(maurer$LONG)
# as numeric
maurer$LONG <-as.numeric(maurer$LONG)
maurer$LAT <-as.numeric(maurer$LAT)

# get spatial object
maurer_wgs <- sp::SpatialPointsDataFrame(maurer[,6:7],maurer)
proj4string(maurer_wgs) <- "+proj=longlat +datum=WGS84 +no_defs"

# check spatial
mapview(maurer_wgs)

# project to utm32
maurer_utm <- spTransform(maurer_wgs,"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
mapview(maurer_utm)

# compare
plot(maurer_wgs)
plot(maurer_utm)

# add geometry to wenker
geo <- raster::geom(maurer_utm)
# write UTM geometry
maurer$utm_x <- geo[,2]
maurer$utm_y <- geo[,3]

head(maurer)
################################################################################

### Select language item

# get columns for coordinates
which(colnames(maurer)=="utm_x") # 148
which(colnames(maurer)=="utm_y") # 149

# get column for desired language item
which(grepl("Hunde",colnames(maurer))) # 32

# subset data from maurer
hunde <- maurer[,c(32,148,149)]
colnames(hunde) <- c("hunde", "X", "Y")
str(hunde)

# sort tabel (count unique values)
sort(table(hunde$hunde), decreasing = T)

# easiest way argument "user_class = F" and "trim = T"
hunde_cl <-LinguGeo::phenmn_class(data = hunde,colname = "hunde",user_class = F,trim = T,develop = F,
                              pat_exp = c("nd|nt","ng|n.g","nn|n$"),
                              cl_to   = c(  "nd" ,   "ng" ,  "nn"))

# conversion to spatila object
hunde_cl_utm <- SpatialPointsDataFrame(hunde_cl[,2:3],hunde_cl)
# set crs wgs84
proj4string(hunde_cl_utm) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# plot
mapview(hunde_cl_utm,zcol="class",cex=4)

### coherence NN smoothing effect

# set projection argument for direct output of spatial object
proj_utm <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

hunde_02 <-LinguGeo::coherenceIndex(dat = as.data.frame(hunde_cl),cl = hunde_cl$class ,xcord = 2,ycord = 3,nk=2,reverse=F,develop=T,out_spatial = T,Proj = proj_utm )
hunde_05 <-LinguGeo::coherenceIndex(dat = as.data.frame(hunde_cl),cl = hunde_cl$class ,xcord = 2,ycord = 3,nk=5,reverse=F,develop=T,out_spatial = T,Proj = proj_utm )
hunde_10 <-LinguGeo::coherenceIndex(dat = as.data.frame(hunde_cl),cl = hunde_cl$class ,xcord = 2,ycord = 3,nk=10,reverse=F,develop=T,out_spatial = T,Proj = proj_utm )

mapview(hunde_02$sP)
mapview(hunde_05$sP)
mapview(hunde_10$sP)


writeOGR(hunde_02$sP,file.path(out,"hunde_NN_02.shp"),driver="ESRI Shapefile", layer="hunde_02")
writeOGR(hunde_05$sP,file.path(out,"hunde_NN_05.shp"),driver="ESRI Shapefile", layer="hunde_05")
writeOGR(hunde_10$sP,file.path(out,"hunde_NN_10.shp"),driver="ESRI Shapefile", layer="hunde_10")

### influrence of NN amount on Global value

# function for NN influrence estimation (argumnet x= seq for i)
NN_glob_corr <- function(x){
  res <-vector()
for (i in x) {
  print(i)
  coh <-LinguGeo::coherenceIndex(dat = as.data.frame(hunde_cl),cl = hunde_cl$class ,xcord = 2,ycord = 3,nk=i,reverse=F,develop=T,out_spatial = T,Proj = proj_utm )
  print(coh$glob_corr)
  if (i==1){
    res <- coh$glob_corr
  } else {
    res <- c(res,coh$glob_corr)
  }
}
  return(res)
}

test <- NN_glob_corr(seq(2,19,1))
test
class(test)

plot(test)
lines(test,col="red")
plot(test,ylim=c(0,1))
lines(test, col="red",lwd=2)
