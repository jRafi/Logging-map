

library(dplyr)

### Load list of municipalities and re-encode due to cross-platform encoding weirdness
municipalities <- readRDS("municipalities.rds")
for(i in 1:length(municipalities)) {
  Encoding(municipalities[[i]]) <- "latin1" 
}

### Download files
muniList <- data.frame("name" = names(municipalities)[-1], stringsAsFactors = F)
muniList <- arrange(muniList, name)
muniList <- mutate(muniList, code = c("10", "20", "09", "21", "13", "23", "06", "08", "07", "25", "12",
                                      "01", "04", "03", "17", "24", "22", "19", "14", "18", "05"))
for(i in 1:nrow(muniList)) {
        download.file(url = paste("http://geodpags.skogsstyrelsen.se/geodataport/data/sksAvverkAnm",
                                  muniList$code[i], ".zip", sep = ""),
                      destfile = paste("temp/zip/", muniList$name[i], ".zip", sep = ""))
}
Sys.Date() %>%
        write.table(file = "date.txt")

### Unzip
toZip <- list.files("temp/zip", full.names = T)
extr <- data.frame(path = toZip)
extr$path <- gsub("\\.zip", "", extr$path)
extr$path <- gsub("zip", "data", extr$path)
extr$dir <- gsub("temp/data/", "", extr$path)

for(i in 1:length(toZip)) {
        unzip(toZip[i], exdir = extr$path[i])
}

### Rename files

for(i in 1:nrow(extr)) {
        file.rename(from = dir(extr$path[i], full.names = T),
                    to = gsub("sksAvverkAnm\\d*", extr$dir[i], list.files(extr$path[i], full.names = T)))
}

### Move files into /data
for(i in 1:nrow(extr)) {
        file.copy(from = paste("temp/data/", extr$dir[i], sep = ""),
                  to = "data/Counties", recursive = T, overwrite = T)
}

### Delete /temp
for(i in 1:nrow(extr)) {
        list.files(extr$path[i], full.names = T) %>%
                file.remove(list.files(., full.names = T))
}
file.remove(list.files("temp/zip", full.names = T))

