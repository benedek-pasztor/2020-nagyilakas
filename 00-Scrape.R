library(tidyverse)
library(rvest)
library(purrr)
library(reshape2)
library(janitor)

####### Links load
k <- 2281 ## 2281 pg - hardcode :()


for (i in 1:k){
  l <- paste0("https://ingatlan.com/budapest/elado+lakas?page=", i) %>% 
    read_html() %>% 
    html_nodes('.listing__link.js-listing-active-area') %>%
    html_attr('href') %>% 
    as.data.frame()
  
  l <- l %>% 
    mutate(id = 1:nrow(l)) %>%  
    melt(id = c("id")) %>% 
    select("http" = "value")
  
  write.table(l, paste0("raw/links/", i, ".csv"))
  
}


############# Utilizing raw/links/ .csv-s

func_scrape <- function(pg){
  #Basic information - address, price etc
  print(paste0("getting basic info"))
  draw_address <- pg %>% 
    html_nodes(".photo-address") %>% 
    html_text() %>%
    as.data.frame()
  colnames(draw_address) <- "value"
  row.names(draw_address) <- "cim"
  
  
  draw_basic <- pg %>% 
    html_nodes(".parameter") %>% 
    html_text()
  
  draw_basic <- draw_basic[1:3] %>% strsplit(" ") %>% unlist() 
  draw_basic <- data.frame(gsub("[,]",".",draw_basic) %>% as.numeric() %>% na.omit())
  if (nrow(draw_basic) > 3){
    draw_basic[3, ] <- draw_basic[3, ] + draw_basic[4, ] / 2
    draw_basic <- draw_basic[1:3, ] %>% data.frame()
  }
  colnames(draw_basic) <- "value"
  row.names(draw_basic) <- c("ar_millio", "alapterulet_m", "szobak_n")
  
  #######
  print(paste0("getting parameters"))
  
  param <- pg %>% 
    html_nodes('table') %>% 
    html_table(fill = T)
  
  draw1 <- t(param[[1]])
  colnames(draw1) = draw1[1, ] # the first row will be the header
  draw1 = data.frame(draw1[-1, ])
  colnames(draw1) = "value"
  
  draw2 <- t(param[[2]])
  colnames(draw2) = draw2[1, ] # the first row will be the header
  draw2 = data.frame(draw2[-1, ])
  colnames(draw2) = "value"
  
  print(paste0("merging and basic cleansing of data..."))
  draw_i <- t(rbind(draw_basic, draw_address, draw1, draw2)) %>%
    as.data.frame() %>% 
    janitor::clean_names() %>% 
    mutate_if(is.factor, as.character)  
  
  draw <- plyr::rbind.fill(draw, draw_i)
  return(draw)
}
  

for (rawlinks_id in 87:2267){
  links <- read.table(paste0("raw/links/", rawlinks_id, ".csv"), stringsAsFactors = F)
  
  draw <- data.frame()
  for (link_id in 1:nrow(links)){
    # Scrape
    url <- paste0("https://ingatlan.com/", links[link_id, ])
    rm(pg)
    pg <-  try(read_html(url), silent = T)
    if(class(pg) != "try-error"){
      print(paste0("accessing ", url))
    
      draw <- func_scrape(pg)
    }
  }  
  print(paste0("writing data to ", "raw/data/", rawlinks_id, ".csv"))
  write.table(draw, paste0("raw/data/", rawlinks_id, ".csv"))
}


k <- read.table(paste0("raw/data/", rawlinks_id, ".csv"), stringsAsFactors = F)
