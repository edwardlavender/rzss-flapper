################################
################################
#### process_spatial.R

#### This code: 
# 1) Processes spatial data for this project:
# ... A coastline shapefile is acquired for 
# ... ... checking the accuracy of recorded locations
# ... ... in process_fights.R. 
# ... The WeStCOMS mesh is built to obtain predictions at capture locations
# ... ... in process_fights.R

# 2) Steps preceding this code:
# ... NA 


################################
################################
#### Process spatial data

################################
#### Coastline

#### Process coastline data
## Download data
download <- FALSE
if(download){
  download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_GBR_0_sp.rds",
                destfile = "./data-raw/spatial/coast/GBR_adm0.rds")
} else {
  coast <- readRDS("./data-raw/spatial/coast/GBR_adm0.rds")
}
## Crop data (for speed)
coast <- raster::crop(coast, raster::extent(-6.5, -5, 55.8, 56.8))
## Simplify data (for speed)
fol <- rgeos::gSimplify(coast, 0.001)
raster::plot(fol)
## Save processed data
saveRDS(fol, "./data/spatial/coast.rds")


################################
#### WeStCOMS mesh

#### Load mesh data
# mesh coordinates (nodes)
nodexy <- read.csv("./data-raw/spatial/mesh/mesh_x.csv")
str(nodexy)
# trinodes
trinodes <- read.csv("./data-raw/spatial/mesh/mesh_trinodes.csv")
str(trinodes)

#### Data processing
## Node coordinates 
colnames(nodexy) <- c("x", "y", "z")
nodexy$node_id   <- 1:nrow(nodexy)
nodexy           <- dplyr::select(nodexy, node_id, x, y, z)
## Node connections
colnames(trinodes)  <- c("node1", "node2", "node3")
trinodes$element_id <- 1:nrow(trinodes)
trinodes            <- dplyr::select(trinodes, element_id, node1, node2, node3)

#### Build mesh (around elements)
mesh_around_elements <- build_mesh(nodexy = nodexy,
                                   trinodes = trinodes,
                                   mesh_type = "node")

#### Save mesh 
saveRDS(nodexy, 
        "./data/spatial/mesh/mesh_nodexy.rds")
saveRDS(trinodes, 
        "./data/spatial/mesh/mesh_trinodes.rds")
saveRDS(mesh_around_elements, 
        "./data/spatial/mesh/mesh_around_elements.rds")


#### End of code. 
################################
################################