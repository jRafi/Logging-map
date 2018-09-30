library(sf)
library(dplyr)

download.file(url = "http://geodpags.skogsstyrelsen.se/geodataport/data/sksBiotopskydd.zip",
              destfile = "temp/bio/bio.zip")
unzip("temp/bio/bio.zip", exdir = "temp/bio")

if(isTRUE(Sys.info()['sysname'] == "Windows")) {
        bio <- read_sf("temp/bio/sksBiotopskydd.shp", options = "ENCODING=latin1")
} else{
        bio <- read_sf("temp/bio/sksBiotopskydd.shp")
}

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

bio$Lan <- gsub("s län", "", bio$Lan)

kommunlista <- sort(unique(bio$Kommun))



# for(i in 1:length(kommunlista)) {dir.create(paste("data/bio/", kommunlista[i], sep = ""))}
for(i in 1:length(kommunlista)) {
        file.remove(dir(paste("data/bio/", kommunlista[i], sep=""), full.names = T))
        filter(bio, Kommun == kommunlista[i]) %>%
                write_sf(dsn = paste("data/bio/", kommunlista[i], sep = ""),
                         layer = paste(kommunlista[i]),
                         driver = "ESRI Shapefile")
}

municipalities <- readRDS("municipalities.rds")

# for(i in 2:length(names(municipalities))) {dir.create(paste("data/bio/Senaste_", names(municipalities)[i], sep = ""))}

bio <- filter(bio, Datbeslut >= Sys.Date()-60)

for(i in 2:length(names(municipalities))) {
        file.remove(dir(paste("data/bio/", "Senaste_", names(municipalities)[i], sep=""), full.names = T))
        filter(bio, Lan == names(municipalities)[i]) %>%
                write_sf(dsn = paste("data/bio/", "Senaste_", names(municipalities)[i], sep = ""),
                         layer = paste("Senaste_", names(municipalities)[i], sep = ""),
                         driver = "ESRI Shapefile")
}

file.remove(dir("temp/bio", full.names = T)[-1])
