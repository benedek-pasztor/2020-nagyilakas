#####
library(skimr)
library(tidyverse)
library(ggmap)
library(tidyr)
library(geosphere)


csv_paths <- list.files("raw/data", full.names = T)

data <- data.frame()

for (i in 1:length(csv_paths)){
  data_i <- read.table(csv_paths[i], stringsAsFactors = F)
  data <- plyr::rbind.fill(data, data_i)
}

write.table(data, "clean/data_raw.csv")

###########################################################################x


data <- read.table("clean/data_raw.csv")

data_cl <- data %>% 
  mutate(ar_negyzetmeter_ezer = ar_millio * 1000 / alapterulet_m,
         cim = as.character(cim)) %>%
  separate(cim, c("kerulet", "cim"), ",") %>% 
  separate(kerulet, c("kerulet_torol", "kerulet", ":")) %>%
  mutate(cim = paste0(cim, "Budapest ", as.roman(kerulet)),
         kerulet = kerulet,
         cim = paste0(cim, ". kerület")) %>% 
  select(-c(':', 'kerulet_torol')) %>% 
  mutate(legkondicionalo = as.numeric(ifelse(is.na(legkondicionalo), 0, legkondicionalo)),
         tetoter = ifelse(is.na(as.character(tetoter)), 'nincs megadva', as.character(tetoter)),
         erkely = as.numeric(gsub("([0-9]+).*$", "\\1", data$erkely)),
         rezsikoltseg_huf_ho = as.numeric(gsub("([0-9]+).*$", "\\1", gsub(" ", "", data$rezsikoltseg, fixed = TRUE))),
         parkolohely_ara = str_replace_all(parkolohely_ara, fixed(" "), ""),
         parkolohely_ara_havi = ifelse(str_detect(as.character(parkolohely_ara), 'hó'), parkolohely_ara, NA),
         parkolohely_ara = ifelse(str_detect(as.character(parkolohely_ara), 'hó'), NA, parkolohely_ara),
         parkolohely_ara_num = as.numeric(gsub("[^[:digit:].]", "",  parkolohely_ara)),
         parkolohely_ara_num = ifelse(str_detect(as.character(parkolohely_ara), 'M'), parkolohely_ara_num * 1000000, 
                                      ifelse(str_detect(as.character(parkolohely_ara), '€'), parkolohely_ara_num * 320, 
                                             parkolohely_ara_num)),
         parkolohely_ara = parkolohely_ara_num,
         parkolohely_ara_havi =  as.numeric(gsub("[^[:digit:].]", "",  parkolohely_ara_havi)),
         erkely = ifelse(is.na(erkely), 0, erkely),
         rezsikoltseg = str_replace_all(rezsikoltseg, fixed(" "), "")) %>% 
  select(-parkolohely_ara_num, -rezsikoltseg)



## geocoding function using OSM Nominatim API ## details: http://wiki.openstreetmap.org/wiki/Nominatim

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  d <- data.frame()
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if (nrow(d) == 0) {return(data.frame())} else {return(data.frame(lon = as.numeric(d$lon),
                                                                   lat = as.numeric(d$lat), as.character(d$display_name)))}
}


outerfunction <-  function(address) {
  #set the elapsed time counter to 0
  t <- Sys.time()
  #calling the nominatim OSM API
  api_output <- data.frame()
  try(api_output <- nominatim_osm(address))
  #get the elapsed time
  t <- difftime(Sys.time(), t, 'secs')
  #return data.frame with the input address, output of the nominatim_osm function and elapsed time
  if (nrow(api_output) == 0){return(data.frame(address = address, elapsed_time = t))}
  else{return(data.frame(address = address, api_output, elapsed_time = t))}
}



## Manual calls to arbitrary avoid errors
geotags <- data.frame()
for (i in 1:nrow(data)){
  print(paste0('address:', i, ', geocoding ', data_cl[i, 'cim']))
  Sys.sleep(runif(1, 1, 2))
  geotags <- plyr::rbind.fill(geotags, outerfunction(data_cl[i, 'cim']))
  print('next ...')
}

geotags
skim(geotags) 

write.table(geotags, 'raw/geotags_all.csv', sep = ',', dec = '.', row.names = F)

############# Enriching data with distance from city center

geotags <- read.table('raw/geotags_all.csv', sep = ',', dec = '.', header = T, stringsAsFactors = F) %>% 
  plyr::rename(c('address' = 'cim', 'as.character.d.display_name.' = 'cim_from_geotagapi'))


data_joined <- data_cl
data_joined$lat <- geotags$lat
data_joined$lon <- geotags$lon
data_joined$cim_api <- geotags$cim_from_geotagapi



# Budapest city center coordinates Clark Ádám square
citycenter_lat <- 47.4979
citycenter_lon <- 19.0402


data_out <- data_joined %>% mutate(tavolsag_varoskozpont = as.numeric(distHaversine(cbind(citycenter_lon, citycenter_lat),
                                           cbind(data_joined$lon, data_joined$lat)))) %>% 
  mutate(nagymama = 0) %>% 
  mutate(tavolsag_varoskozpont_mod = tavolsag_varoskozpont + runif(1, -0.0001, 0.0001),
         lat_mod = lat + runif(1, -0.01, 0.01),
         lon_mod = lon + runif(1, -0.01, 0.01),
         ID = row_number())


write.table(data_out, 'clean/data_dashboard_v1.csv', sep = ',', dec = '.', row.names = F)



data_out %>% filter(cim == 'Dorottya utca Budapest V. kerület')
data_out$lat_mod

### TODO tackle problem with one address...


data_out %>% filter(cim == 'Báthory utca Budapest V. kerület')
