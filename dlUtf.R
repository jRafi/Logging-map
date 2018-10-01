library(sf)
library(dplyr)

download.file(url = "http://geodpags.skogsstyrelsen.se/geodataport/data/sksUtfordAvverk.zip",
              destfile = "temp/utf/utf.zip")
unzip("temp/utf/utf.zip", exdir = "temp/utf")

if(isTRUE(Sys.info()['sysname'] == "Windows")) {
        utf <- read_sf("temp/utf/sksUtfordAvverk.shp", options = "ENCODING=latin1")
} else{
        utf <- read_sf("temp/utf/sksUtfordAvverk.shp")
}

utf <- select(utf, -Lannr, -Kommunnr)

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

utf$Lan <- replaceMisencodings(utf$Lan)
utf$Avverktyp <- replaceMisencodings(utf$Avverktyp)
utf$Kommun <- replaceMisencodings(utf$Kommun)
utf$Skogstyp <- replaceMisencodings(utf$Skogstyp)
utf <- st_transform(utf, crs = "+proj=longlat +datum=WGS84")

utf$Lan <- gsub("s län", "", utf$Lan)

kommunlista <- sort(unique(utf$Kommun))
for(i in 1:length(kommunlista)) {
        file.remove(dir(paste("data/utf/", kommunlista[i], sep=""), full.names = T))
        filter(utf, Kommun == kommunlista[i]) %>%
                write_sf(dsn = paste("data/utf/", kommunlista[i], sep = ""),
                         layer = paste(kommunlista[i]),
                         driver = "ESRI Shapefile")
}

municipalities <- readRDS("municipalities.rds")

utf <- filter(utf, Avvdatum >= Sys.Date()-60)

for(i in 2:length(names(municipalities))) {
        file.remove(dir(paste("data/utf/", "Senaste_", names(municipalities)[i], sep=""), full.names = T))
        filter(utf, Lan == names(municipalities)[i]) %>%
                write_sf(dsn = paste("data/utf/", "Senaste_", names(municipalities)[i], sep = ""),
                         layer = paste("Senaste_", names(municipalities)[i], sep = ""),
                         driver = "ESRI Shapefile")
}

file.remove(dir("temp/utf", full.names = T)[-1])
