
require(DT)
require(tidyverse)
require(DataExplorer)
require(webshot)
require(rvest)
require(digest)




f_3_3_saveReviews <- function(html){
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
      html_attr('src') %>% gsub("jpg.*","jpg?w=2400&h=-1&s=1",.) 
    
    
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
                      stay = Stay, url = 'c_url', uid = digest(paste0(users,title_des,body_des,date_w)))
  
  
  
  
  
  
  return(tb)
}

f_0_3_PicDest <- function(x){
  return(paste0(getwd(),"/IMG/",digest::digest(x,algo = "md5"),".jpg"))
}



fi <- list.files(path = "~/repos/PictureHelpfulness_API/SAVE/", full.names = T)



## Download info from the HTMLS
for(i in 1:length(fi)){
  # i <- 1
  rds_i <- readRDS(fi[i]) %>% bind_rows() %>% unnest(cols = c(tb,params))
  # out <- tryCatch({unlist(remDr$getCurrentWindowHandle())},error = function(e){return(FALSE)})
  # if(out == FALSE){remDr$open()}
  rds_i %>% select(-params) %>% distinct() -> rds_i
  
  out_fi <- list()
  for(j in 1:nrow(rds_i)){
    rds_i %>% slice(j) -> rds_ij
    pull(rds_ij,uid) -> uid_ij
    paste0(uid_ij,".rds") -> html_ij
    list.files(path = "~/repos/PictureHelpfulness_API/HTML/", pattern = html_ij, full.names = T) -> rdss
    html <- rvest::read_html(readRDS(rdss))
    save_ij <- f_3_3_saveReviews(html) 
    save_ij
    
    # download urls and pictures sites from urls
    ri <- dplyr::tibble(orig = rds_ij %>% pull(orig), uid_ij = uid_ij, save_ij)
    
    for(i in 1:length(reviews)){
      # i <- 1
      urls_pics <- html %>%  
        html_elements(xpath = ".//div[contains(@class,'faRsf _T')]") %>% 
        html_elements(xpath = ".//img[contains(@class,'bMGfJ)']") %>% 
        html_attr('src') %>% gsub("jpg.*","jpg?w=2400&h=-1&s=1",.) 
      
      }
    
    
    # Does the url has pictures
    p_urls <- ri %>%  bind_rows() %>% pull(pictures) %>% `[`(is.na(.)==FALSE) %>% bind_rows()

    
    if(length(p_urls)==0){ # if not go to next
      print(j)
      next}
  
    
    # For every Picture available try downloading
      for(z in 1:length(p_urls$url)){
        if(file.exists(f_0_3_PicDest(p_urls$url[z])) ){next}
        
        down <- try(withCallingHandlers( 
          download.file(p_urls$url[z],f_0_3_PicDest(p_urls$url[z]),mode = "wb"),
          warning = function(w) {
            my.warning <<- sub(".+HTTP status was ", "", w)
          }),
          silent = FALSE)
      }

    out_fi[[j]] <- ri
    
  }
  saveRDS(out_fi, file = paste0("~/repos/PictureHelpfulness_API/MASTER/",uid_ij,".rds"))
  print(paste("Completed file ",i, "out of ",length(fi)))

}



# ## Download images
# 
# reviews_data <- list.files(paste0("~/repos/PictureHelpfulness_API/MASTER/"),".rds",full.names = T)
# 
# ri <- readRDS(reviews_data[1])
# 
# ri <- out_fi
# 
# # Get table with all the picture urls
# p_urls <- ri %>%  bind_rows() %>% pull(pictures) %>% `[`(is.na(.)==FALSE) %>% bind_rows()
# 
# 
# Pictures[[i]] %>% add_column(downloads = downs)
# 
# 
# 
# 
