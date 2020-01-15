#####
library(skimr)
library(tidyverse)
library(ggmap)

csv_paths <- list.files("raw/data", full.names = T)

data <- data.frame()

for (i in 1:length(csv_paths)){
  data_i <- read.table(csv_paths[i], stringsAsFactors = F)
  data <- plyr::rbind.fill(data, data_i)
}

write.table(data, "clean/data_raw.csv")


skim(data)

data <- data %>% 
  mutate(ar_negyzetmeter = ar_millio / alapterulet_m)

### Geocoding

geocode('Budapest, Nador utca 9', source = 'google', output = 'all')

