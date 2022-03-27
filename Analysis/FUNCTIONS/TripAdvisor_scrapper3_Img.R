### HTML Scrapper Trip Advisor ###
## Objective: this code is intended to search for images in current downloaded htmls 
## This is supposed to be run as a side job while the Tripadvisor_Scrapper 2 is running

require(tidyverse)
require(rvest)
require(digest)

f_0_3_PicDest <- function(x){
  ## Objective: This function takes an url address and converts it unique name in file architecture 
  
  return(paste0(getwd(),"/IMG/",digest::digest(x,algo = "md5"),".jpg"))
}

f_0_4_tryDown <- function(url_vec = p_urls$url){
  
  down <- list()
  for(z in 1:length(url_vec)){
    # if(file.exists(f_0_3_PicDest(url_vec[z])) ){next}
    
    down[z] <- try(withCallingHandlers( 
      download.file(url_vec[z],f_0_3_PicDest(url_vec[z]),mode = "wb"),
      warning = function(w) {
        my.warning <<- sub(".+HTTP status was ", "", w)
      }),
      silent = FALSE)
  }
  return(unlist(down))
}

f_3_1_saveReviews <- function(html,url){
  ## Objective: This function takes an html file and extracts relevant info into a table
  # Html: 
  # return: 'tibble with all elements 
  
  
  # # navigate
  
  html %>% 
    html_elements(xpath = ".//div[contains(@class,'cWwQK MC R2 Gi z Z BB dXjiy')]") -> reviews 
  
  
  ## Scores 
  scores <- reviews %>% 
    html_elements(xpath = ".//div[@class='emWez F1']") %>% 
    html_elements(xpath = ".//span[contains(@class,ui_bubble_rating) and contains(@class,bubble_)]") %>% 
    html_attr("class") %>% 
    gsub(".*bubble_","",.) %>% as.numeric() %>% `/`(10)
  
  ## User
  users <- reviews %>% 
    html_elements(xpath = ".//a[contains(@class,'ui_header_link bPvDb')]") %>% 
    html_text()
  
  ## Wrote on 
  date_w <- reviews %>% 
    html_elements(xpath = ".//div[@class='bcaHz']//child::span") %>% 
    html_text() %>% 
    gsub(".* wrote a review ","",.) 
  
  ## Title Description
  title_des <- reviews %>% 
    html_elements(xpath = ".//a[@class='fCitC']") %>% html_text()
  
  ## Description Body
  body_des <- reviews %>% 
    html_elements(xpath = ".//q[@class='XllAv H4 _a']") %>% html_text()
  
  ## Pictures
  Pictures <- list()
  for(i in 1:length(reviews)){
    # i <- 1
    urls_pics <- reviews %>% `[`(i) %>%  
      html_elements(xpath = ".//div[contains(@class,'faRsf _T')]") %>% 
      html_elements(xpath = ".//img[contains(@class,'bMGfJ _Q t _U s l bnegk')]") %>% 
      html_attr('src') %>% gsub("[.]jpg.*",".jpg?w=2400&h=-1&s=1",.) 
    
    
    if(length(urls_pics) == 0){Pictures[[i]] <- NA}else{
      Pictures[[i]] <- urls_pics %>% purrr::map_df(.x = ., 
                                                   ~dplyr::tribble(~url,~loc,~name,
                                                                   .x,
                                                                   f_0_3_PicDest(.x),
                                                                   digest::digest(.x,algo = "md5"))
      )
      
      # downs <- urls_pics %>% 
      #   purrr::map(.x = ., 
      #              ~download.file(.x,f_0_3_PicDest(.x),mode = "wb")) %>% unlist()
      # 
      # Pictures[[i]] %>% add_column(downloads = downs)
    }
    
  } 
  
  
  ## Helpful
  Helpful <- list()
  for(i in 1:length(reviews)){
    # i <- 1
    Helpful[[i]] <- reviews %>% `[`(i) %>%  
      html_elements(xpath = ".//span[@class='ekLsQ S2 H2 Ch bzShB']") %>% 
      html_text() %>% 
      gsub(" Helpful vo.*","",.) %>% ifelse(length(.)==0,0,.) %>% as.numeric()
  } 
  Helpful <- Helpful %>% unlist()
  
  
  ## Stay
  Stay <- list()
  for(i in 1:length(reviews)){
    # i <- 1
    Stay[[i]] <- reviews %>% `[`(i) %>%  
      html_elements(xpath = ".//span[@class='euPKI _R Me S4 H3']") %>% 
      html_text() %>% ifelse(length(.)==0,"",.) %>% 
      gsub("Data of stay:","",.)  
  } 
  Stay <- Stay %>% unlist()
  
  
  tb <- dplyr::tibble(user = users, title = title_des, 
                      body = body_des, score = scores, date_w = date_w,
                      pictures = Pictures, helpful = Helpful, 
                      stay = Stay, url = url, url_id = digest(url), uid = digest(paste0(users,title_des,body_des,date_w)))
  
  
  
  
  
  
  return(tb)
}

f_3_2_savePictures <- function(save_ij){
  for(z in 1:nrow(save_ij)){
    # z <- 2
    if(is.na(save_ij$pictures[[z]])){next
    }else{
      a <- f_0_4_tryDown(url_vec = save_ij$pictures[[z]]$url)
      save_ij$pictures[[z]]$down <- a 
    }
  }
  return(save_ij)
}



list.files("./SAVE", full.names = T) -> l1

for(i in 1:length(l1)){
  # i <- 1
  rds_i <- readRDS(l1[i]) %>% bind_rows() %>% unnest(cols = c(tb))
  for(j in 1:nrow(rds_i)){
    # j <- 2
    
    if(file.exists(paste0("./SAVE2/",digest::digest(rds_i$url[j]),".rds"))){
      print(paste0("I got this one already, going to ",j))
      next
      }else{
    
      rds_i %>% slice(j) -> rds_ij
      pull(rds_ij,file) -> rdss
      html <- rvest::read_html(readRDS(rdss))
      save_ij <- f_3_1_saveReviews(html,rds_ij$url) 
      save_ij_m <- f_3_2_savePictures(save_ij)
      
      saveRDS(save_ij_m,paste0("./SAVE2/",digest::digest(save_ij_m$url[j]),".rds"))
      } # end if
  } # end j loop

} # end i loop

closeAllConnections()







