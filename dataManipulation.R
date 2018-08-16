
library(rgdal)
library(dplyr)

municipalities <- readRDS("municipalities.RDS")




### Read


## Rename files and folders from "sksAvverkAnm" to county names.

# Rename folders
#folders <- list.files("data/Counties", full.names = T)
#file.rename(folders, paste("data/Counties/", names(municipalities)[-1], sep = ""))
#
# Rename files withing folders
#folders <- dir("data/Counties", full.names = T)
#for(i in 1:length(folders)) {
#        currentCounty <- dir("data/Counties")[i]
#        filesInFolders <- list.files(folders[i], full.names = T)
#        newNames <- gsub("\\w+Avverk\\w+", currentCounty, filesInFolders)
#        file.rename(filesInFolders, newNames)
#}

## Read all county shapefiles

separateByMunicipality <- function(dsnInput, layerInput) {
        
        
        ### Read data
        lista <- list(NULL)
        data <- readOGR(dsn = dsnInput,
                        layer = layerInput)
        
        
        
        ### Manipulations
        
        data@data <- select(data@data, id = OBJECTID, year = Arendear, muni = Kommun)
        data@data <- mutate(data@data, id = as.factor(id), year = as.numeric(year))
        data@data <- mutate(data@data, year = year + 1997)
        data <- spTransform(data,  CRS("+proj=longlat +datum=WGS84"))
        data@data[] <- sapply(data@data, function(x) gsub("„", "ä", x))
        data@data[] <- sapply(data@data, function(x) gsub("”", "ö", x))
        data@data[] <- sapply(data@data, function(x) gsub("Ž", "Ä", x))
        data@data[] <- sapply(data@data, function(x) gsub("†", "å", x))
        data@data[] <- sapply(data@data, function(x) gsub("™", "Ö", x))
        data@data[] <- sapply(data@data, function(x) gsub("\u008f", "Å", x))
        
        
        ### Separate by municipality
        data$muni[is.na(data$muni)] <- paste(layerInput, "_NA_", sep = "")
        municipality <- unique(data@data$muni)
        for(i in 1:length(municipality)) {
                muniLogical <- data$muni == municipality[i]
                lista[[i]] <- data[muniLogical,]
        }
        
        
        
        ### Write files
        for(i in 1:length(lista)) {
                writeOGR(lista[[i]],
                         dsn = paste("data//Municipalities//", municipality[i], sep = ""),
                         layer = paste(municipality[i]),
                         driver = "ESRI Shapefile")
        }
        
}


list.files("data/Municipalities", full.names = T) %>%
        file.remove(list.files(., full.names = T))




separateByMunicipality(dsnInput = paste("data/Counties/", names(municipalities)[[20]], sep = ""),
                       layerInput = names(municipalities)[[20]])


for(j in 2:length(names(municipalities))) {
        separateByMunicipality(dsnInput = paste("data/Counties/", names(municipalities)[[j]], sep = ""),
                               layerInput = names(municipalities)[[j]])
}


list.files("data/Counties", full.names = T) %>%
        file.remove(list.files(., full.names = T))