library(sf)
library(dplyr)

if(isTRUE(Sys.info()['sysname'] == "Windows")) {
        setwd("~/R WD/Git Local/skogskoll")
} else if(isTRUE(Sys.info()['sysname'] == "Ubuntu")) {
        setwd("../srv/shiny-server/")
}

download.file(url = "http://geodpags.skogsstyrelsen.se/geodataport/data/sksBiotopskydd.zip",
              destfile = "temp/bio/bio.zip")
unzip("temp/bio/bio.zip", exdir = "temp/bio")
file.rename(from = dir("temp/bio/", full.names = T),
            to = gsub("sksBiotopskydd", "bio", dir("temp/bio", full.names = T)))

bio <- read_sf("temp/bio/bio.shp", options = "ENCODING=latin1")

if(isTRUE(Sys.info()['sysname'] == "Windows")) {
        replaceMisencodings <- function(x) {
                Encoding(x) <- "latin1"
                x <- gsub("Â„", "ä", x)
                x <- gsub("Â”", "ö", x)
                x <- gsub("ÂŽ", "Ä", x)
                x <- gsub("Â†", "å", x)
                x <- gsub("Â™", "Ö", x)
                x <- gsub("Â\u008f", "Å", x)
                x
        }
} else{
        replaceMisencodings <- function(x) {
                x <- gsub("\x94", "ö", x)
                x <- gsub("\x84", "ä", x)
                x <- gsub("\x86", "å", x)
                x <- gsub("\x99", "Ö", x)
                x <- gsub("\x8e", "Ä", x)
                x <- gsub("\x8f", "Å", x)
                x
        }
}

bio$Lan <- replaceMisencodings(bio$Lan)
bio$Biotyp <- replaceMisencodings(bio$Biotyp)
bio$Kommun <- replaceMisencodings(bio$Kommun)
bio$Naturtyp <- replaceMisencodings(bio$Naturtyp)
bio <- st_transform(bio, crs = "+proj=longlat +datum=WGS84")

kommunlista <- sort(unique(bio$Kommun))
for(i in 1:length(kommunlista)) {
        file.remove(dir(paste("data/bio/", kommunlista[i], sep=""), full.names = T))
        filter(bio, Kommun == kommunlista[i]) %>%
                write_sf(dsn = paste("data/bio/", kommunlista[i], sep = ""),
                         layer = paste(kommunlista[i]),
                         driver = "ESRI Shapefile")
}

file.remove(dir("data/bio/60", full.names = T))
filter(bio, Datbeslut >= Sys.Date()-60) %>%
        write_sf(dsn = "data/bio/60", layer = "60", driver = "ESRI Shapefile")

