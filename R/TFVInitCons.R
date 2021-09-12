#' Generate a TFV initial conditions file from point observations
#' @param file .2dm mesh used to generate the cell IDs and centres
#' @param fgdb Shape file (.shp and associated .prj) for zones to be preserved
#' @param initialvaluesfile csv file with observed data to interpolate
#' @param outputfile file to write the initial conditions to
#' @param cellsize size of cells to interpolate the observations onto
#' @param ncell number of cells to interpolate the observations onto
#'
#' The shapefile should be a polygon shape file, with different zones to interpolate within. This can be useful to ensure
#' interpolations are physically sensible, for example where there is a barrier between fresh and saline water, or a
#' storage above a lower river. Projections are not handled by this function, so the shapefile should be in the same projection as the mesh.
#'
#' The initialvaluesfile should have 4 columns of information:
#' Column 1 as point index
#' Column 2 and 3, columns named "X" and "Y", with the coordinates of the point observation, in the same projection as the mesh.
#' Columns 4 and 5 are reserved for point information, e.g. a station number and station name. These are not used, but are provided for documentation and if the file is used for specifying output locations
#' Columns 6 and beyond should be values and will be interpolated. Each column will be interpolated for each zone, allowing for multiple initial conditions (e.g. H, Sal, Temp, tracers)
#'
#' cellsize and ncell are used to specify the resolution of the interpolation. ncell is in units of the mesh coordinates, e.g. m or degrees
#'
#' interpolation is undertaken using inverse distance weighting from the gstat package, with the default weighting power of 2.
#'
#' @return Nothing is returned to the environment, with the initial conditions generated written to outputfile. Summary metrics are printed to screen to allows for some quick QA
#'
#' @examples
#' \dontrun{
#' file="001_RM_Wetlands_LL_Coorong_MZ.2dm"
#' fgdb<-"Zones/Zones.shp"
#' initialvaluesfile<-"CLLMMInitialValues2019.csv"
#' outputfile<-"Initcons2021.csv"
#' TFVInitCons(file,fgdb,initialvaluesfile,outputfile)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom utils write.csv
#' @export

TFVInitCons<-function(file,fgdb,initialvaluesfile,outputfile,cellsize=50,ncell=50000)
{
#calculate cell centre point for each element, to then specific init cons
X<-readr::read_lines(file)
X<-tibble::tibble(X)
nodes<-X %>% dplyr::filter(stringr::str_detect(.data$X,"ND ")) %>%
  tidyr::separate(.data$X,c("N","NodeID","X","Y","Z"),sep=" ") %>%
  dplyr::select(.data$NodeID,.data$X,.data$Y) %>%
  dplyr::mutate_all(as.numeric)

#find the element centroids
elementIDs<-c("E3T ","E6T ","E4Q ","E8Q ","E9Q ")

elements<-NULL
for(elementID in elementIDs)
{
  nnodes<-as.numeric(substr(elementID,2,2))
  intonames<-c("char","ID",paste0("N",seq(1:nnodes)),"MAT")
  #3-noded triangular element
  e<-X %>% dplyr::filter(stringr::str_detect(.data$X,elementID)) %>%
    tidyr::separate(.data$X,intonames,sep=" ") %>%
    dplyr::select(.data$ID,dplyr::contains("N")) %>%
    dplyr::mutate_all(as.numeric) %>%
    tidyr::gather("Node","NodeID",-.data$ID) %>%
    dplyr::left_join(nodes,by=c("NodeID")) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::summarise(X=mean(.data$X),Y=mean(.data$Y))
  elements<-elements %>% dplyr::bind_rows(e)
}

elements<-elements %>% dplyr::arrange(.data$ID)

initvals<-readr::read_csv(initialvaluesfile)

Wall<-rgdal::readOGR(dsn=fgdb,layer="Zones")

P<-sp::SpatialPoints(initvals[,c("X","Y")],proj4string = Wall@proj4string)
Pall<-sp::SpatialPointsDataFrame(P,initvals)

elements_points<-elements
sp::coordinates(elements_points)=~X+Y

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
  grd              <- as.data.frame(sp::makegrid(P, cellsize=cellsize, n=ncell))
  names(grd)       <- c("X", "Y")
  sp::coordinates(grd) <- c("X", "Y")
  sp::gridded(grd)     <- TRUE  # Create SpatialPixel object
  sp::fullgrid(grd)    <- TRUE  # Create SpatialGrid object

  # Add P's projection information to the empty grid
  sp::proj4string(P) <- sp::proj4string(P) # Temp fix until new proj env is adopted
  sp::proj4string(grd) <- sp::proj4string(P)

  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  fm<-stats::as.formula(paste0(var,"~1"))
  P.idw <- gstat::idw(fm, P, newdata=grd, idp=2.0)

  # Convert to raster object then clip to the zone
  r       <- raster::raster(P.idw)
  r.m     <- raster::mask(r, W)

  #extract the element results within the zone
  dz<-raster::extract(r.m,elements_points,method="bilinear")
  dat<-pmin(dat,dz,na.rm=TRUE)
}
dat<-tibble::tibble(dat)
colnames(dat)<-var
elements<-elements %>% dplyr::bind_cols(dat)
}

print(summary(elements))
write.csv(elements,outputfile)
}
