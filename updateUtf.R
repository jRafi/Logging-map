library(sf)
library(dplyr)

library(RCurl)
url <- "http://geodpags.skogsstyrelsen.se/geodataport/data/sksUtfordAvverk.zip"
newFileSize <- url.exists(url, .header=TRUE)[[1]] %>%
        as.numeric()
prevFileSize <- file.info("temp/utf/prev.zip")[[1]]
if(newFileSize ==  prevFileSize) {
        print("New and old file size is equal.")
        quit()
}


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
utf$Lan <- gsub("s län", "", utf$Lan)

utf$Avvdatum[utf$OBJECTID == "301164"] <- "2003-12-31"
utf$Avvdatum[utf$OBJECTID == "49379"] <- "2007-10-10"
utf$Avvdatum[utf$OBJECTID == "1657267"] <- "2009-12-30"
#

utf_nya <- filter(utf, Avvdatum >= Sys.Date())
if(nrow(utf_nya) > 0){
        
        kommunlista_nya <- unique(utf_nya$Kommun)
        utf_nya <- st_transform(utf_nya, crs = "+proj=longlat +datum=WGS84")
        for(i in 1:length(kommunlista_nya)) {
                x <- read_sf(dir(paste("data/utf/", kommunlista_nya[i], sep=""), full.names = T))
                y <- filter(utf_nya, Kommun == kommunlista_nya[i])
                xy <- rbind(x,y)
                write_sf(obj = xy,
                         dsn = paste("data/utf/", kommunlista_nya[i], sep = ""),
                         layer = paste(kommunlista_nya[i]),
                         driver = "ESRI Shapefile")
        }
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
file.rename(grep("zip", dir("temp/utf", full.names = T), value = T), "temp/utf/prev.zip")
