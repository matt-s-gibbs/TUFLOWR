library(rgdal)
library(sp)
library(gstat)
library(raster)
library(tidyverse)

#2dm file to create initial conditions for
file="C:\\TUFLOW\\HCHB_2019_Tracer\\geo\\Coorong_20190731with20190226_w_AllNS_Renumbered.2dm"

#polygon extents to interpolated separately.
#Must extend beyond all element centroids.
#Minimum value is taken for any overlaps.
#projection should match mesh
fgdb<-"Zones/Zones.shp" 

#file with point locations (column named X,Y in mesh coordinates) and initial values. 
#Any values from column 6 on are interpolated
initialvaluesfile<-"CLLMMInitialValues2019.csv" 

#convert EC to g/L? Column name is expected to be "Sal".
convertsal=TRUE 

#outputfile
outputfile<-"Initcons2019.csv"

#calculate cell centre point for each element, to then specific init cons
X<-read_lines(file)
X<-tibble(X)
nodes<-X %>% filter(str_detect(X,"ND ")) %>% 
  separate(X,c("N","NodeID","X","Y","Z"),sep=" ") %>% 
  select(NodeID,X,Y) %>% 
  mutate_all(as.numeric)

#find the element centroids
elementIDs<-c("E3T ","E6T ","E4Q ","E8Q ","E9Q ")

elements<-NULL
for(elementID in elementIDs)
{
  nnodes<-as.numeric(substr(elementID,2,2))
  intonames<-c("char","ID",paste0("N",seq(1:nnodes)),"MAT")
  #3-noded triangular element
  e<-X %>% filter(str_detect(X,elementID)) %>% 
    separate(X,intonames,sep=" ") %>% 
    select(ID,contains("N")) %>% 
    mutate_all(as.numeric) %>%
    gather("Node","NodeID",-ID) %>% 
    left_join(nodes,by=c("NodeID")) %>% 
    group_by(ID) %>% 
    summarise(X=mean(X),Y=mean(Y))
  elements<-elements %>% bind_rows(e)
}

elements<-elements %>% arrange(ID)

initvals<-read_csv(initialvaluesfile)
if(convertsal) initvals<-initvals %>% mutate(Sal=(3e-06 * Sal^2 + 0.5517 * Sal)/1000)

Wall<-readOGR(dsn=fgdb,layer="Zones")

P<-SpatialPoints(initvals[,c("X","Y")],proj4string = Wall@proj4string)
Pall<-SpatialPointsDataFrame(P,initvals)

elements_points<-elements
coordinates(elements_points)=~X+Y

for(i in 6:ncol(initvals))
{
  
var<-colnames(initvals)[i]
dat<-array(NA,length(elements_points))
for(zone in 1:length(Wall))
{
  W<-Wall[zone,]
  P<-Pall[W,]
  if(length(P)<1) next
  P@bbox<-W@bbox #change the extent of the points to the zones
  
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(makegrid(P, cellsize=50, n=50000))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  proj4string(P) <- proj4string(P) # Temp fix until new proj env is adopted
  proj4string(grd) <- proj4string(P)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  fm<-as.formula(paste0(var,"~1"))
  P.idw <- gstat::idw(fm, P, newdata=grd, idp=2.0)
  
  # Convert to raster object then clip to the zone
  r       <- raster(P.idw)
  r.m     <- mask(r, W)
  
  #extract the element results within the zone
  dz<-raster::extract(r.m,elements_points,method="bilinear")
  dat<-pmin(dat,dz,na.rm=TRUE)
}
dat<-tibble(dat)
colnames(dat)<-var
elements<-elements %>% bind_cols(dat)
}

print(summary(elements))
write_csv(elements,outputfile)
