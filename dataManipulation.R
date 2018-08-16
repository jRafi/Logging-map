
library(rgdal)
library(dplyr)

municipalities <- readRDS("municipalities.rds")

# Data wrangling
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
        data@data[] <- sapply(data@data, function(x) gsub("\x84", "ä", x))
        data@data[] <- sapply(data@data, function(x) gsub("\x94", "ö", x))
        data@data[] <- sapply(data@data, function(x) gsub("\x8e", "Ä", x))
        data@data[] <- sapply(data@data, function(x) gsub("\x86", "å", x))
        data@data[] <- sapply(data@data, function(x) gsub("\x99", "Ö", x))
        data@data[] <- sapply(data@data, function(x) gsub("\x8f", "Å", x))
        
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


# Old layers need to be removed before writing new ones
list.files("data/Municipalities", full.names = T) %>%
        file.remove(list.files(., full.names = T))

# Read, wrangle, and write files to disk (one for each municipality)
for(j in 2:length(names(municipalities))) {
        separateByMunicipality(dsnInput = paste("data/Counties/", names(municipalities)[[j]], sep = ""),
                               layerInput = names(municipalities)[[j]])
}

# Remove County files
list.files("data/Counties", full.names = T) %>%
        file.remove(list.files(., full.names = T))
