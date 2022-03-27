library("RSelenium")
library("rvest")
library("tidyverse")
library("sys")
library("digest")
library("here")
library("xml2")


#### Parameters ####
# url <- "https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html"
url <- "https://www.tripadvisor.com/Search?q=New+York&search"
cities <- c("new york","los angeles","seattle","miami","chicago","houston","phoenix","philadelphia","denver","atlanta")
categories <- c("Hotels","Restaurants")

#### Functions ####

## F_0_1 sleep and check current url
f_0_1_sleepCheck <- function(time = 2){
  ## f_0_1_sleepCheck: This function checks current Url in Rselenium driver stored in remDr and waits the given time
  # It requires a driver from Rselenium to be open and stored in function remDr
  # time: int' number of seconds to wait for before continuing
  # return: string' value with the current string
  
  
  # First wait for the DOM to load by pausing for a couple seconds.
  Sys.sleep(time) 
  
  # Get current url
  c_url <- remDr$getCurrentUrl()
  
  return(c_url)
  
}

## F_0_2 load list of review sites
f_0_2_retrieveHtmls <- function(category = "Hotels", city = "los angeles"){
  loc <- paste0("./DATA/TA_urls_",paste(category,city,sep = "_"),".rds")
  tb1 <- readRDS(loc)
  
  return(tb1)
  
}

## F_0_3 Where to save the pictures that are found
f_0_3_PicDest <- function(x){
  return(paste0(getwd(),"/IMG/",digest::digest(x,algo = "md5"),".jpg"))
}

## F_2_1 Save Navigation Parameters
f_2_1_saveNavParams <- function(remDr = remDr, url = url_rev){
  
  if(  remDr$getStatus()$ready == TRUE){
    
    # # navigate
    # remDr$navigate(url_rev)
    
    # sleep and check
    c_url <- f_0_1_sleepCheck()
    
    ## Get element that shows number of pages
    input1 <- remDr$findElements(using = "xpath", 
                                 value = ".//span[@class='cdKMr Mc _R b']")
    
    tot_rev <- input1[[1]]$getElementText() %>% unlist() %>% as.numeric()
    
    input2 <- remDr$findElements(using = "xpath", 
                                 value = ".//div[@data-test-target='HR_CC_CARD']")
    
    num_pages <- ceiling((tot_rev+2)/length(input2))
    rev_ppag <- length(input2)
    
    input3 <- remDr$findElements(using = "xpath", 
                                 value = ".//span[@class='pageNum current disabled']")
    cur_pages<- input3[[1]]$getElementText() %>% unlist() %>% as.numeric()
    
    
    params <- c(cur_pages, num_pages,  rev_ppag, tot_rev)
    
    
    
  }else{
    stop("Driver is not ready or url is not of type search1, \n 
         check that the url is provided by function output of f_1_2 and \n
         that the Rselenium driver is active")
  } # end else
  
  return(params)
}


# f_2_1_saveNavParams(remDr = remDr, url = url_rev)
# f_2_2_saveSitAbout(remDr = remDr, url = url_rev)

## F_2_2 Save Site About

f_2_2_saveSitAbout <- function(remDr = remDr, url = url_rev){
  
  if(  remDr$getStatus()$ready == TRUE){
    
    # # navigate
    # remDr$open()
    # remDr$navigate(url_rev)
    
    # sleep and check
    c_url <- f_0_1_sleepCheck()
    
    ### Get total score out of 5 stars
    html <- remDr$getPageSource()[[1]] 
    html <- read_html(html)
    html %>% 
      html_elements(xpath = ".//div[contains(@class,'cmZRz f')]") -> scores 
    
    attributes <- scores %>% html_text()
    
    
    ### Get total scores out of 5 stars for all ratings
    html %>% 
      html_elements(xpath = ".//a[contains(@class,'fbhUA q eRJGA _T Gi')]") -> overall_scores 
    
    
    
    overall <- overall_scores  %>% html_elements(xpath = ".//span[contains(@class,'ui_bubble_rating')]") %>% 
      html_attr("class") %>% 
      gsub(".*bubble_","",.) %>% as.numeric() %>% `/`(10)
    
    attributes_scores <- scores %>% html_elements(xpath = ".//span[contains(@class,'ui_bubble_rating')]") %>% 
      html_attr("class") %>% 
      gsub(".*bubble_","",.) %>% as.numeric() %>% `/`(10)
    
    
    attributes_tb <- dplyr::tibble(attributes = attributes, scores = attributes_scores)
    
    
    # site name 
    site_name <- html %>% html_element(xpath = ".//h1[contains(@class,'fkWsC b d Pn')]") %>% 
      html_text()
    
    # site description
    site_description <- html %>% html_element(xpath = ".//div[contains(@class,'pIRBV _T')]") %>% 
      html_text()
    
    # site amenities 
    site_amenities <- html %>% html_elements(xpath = ".//div[contains(@class,'bUmsU f ME H3 _c')]") %>% 
      html_text()
    
    # Good to know - Hotel 
    hotel_type <- html %>% html_elements(xpath = ".//svg[@class='TkRkB d H0']") %>% 
      html_attr(name = "title") %>% gsub(" of 5 bubbles","",.) %>% as.numeric()
    
    hotel_style <- html %>% html_elements(xpath = ".//div[@class='drcGn _R MC S4 _a H']") %>% 
      html_text() %>% `[`(!str_detect(.,",")) %>% `[`(which(nchar(x = .)>1))
    
    about <- dplyr::tribble(~url,
                            ~site_name,
                            ~site_score,
                            ~site_attributes,
                            ~site_description,
                            ~hotel_stars, 
                            ~site_amenities,
                            ~hotel_style,
                            url_rev,site_name,overall,attributes_tb,site_description, site_amenities, hotel_type,hotel_style) 
    
    
  }else{
    stop("Driver is not ready or url is not of type search1, \n 
         check that the url is provided by function output of f_1_2 and \n
         that the Rselenium driver is active")
  } # end else
  
  return(about)
  
}


## F_2_3 Extract Reviews from site 

f_2_4_getHtml <- function(remDr = remDr, url = url_rev, category = category, city = city){
  
  if(  remDr$getStatus()$ready == TRUE){
    # sleep and check
    c_url <- f_0_1_sleepCheck(.5)
    
    
    html <- remDr$getPageSource()[[1]] 
    html <- read_html(html)
    uid <- digest::digest(c_url[[1]])
    address <- here::here("HTML",paste0(uid,".rds"))
    saveRDS(as.character(html),address)
    # read_html(readRDS(address))
    tb <- dplyr::tibble(category = category, city = city, url = c_url[[1]], file = address, uid = digest::digest(c_url[[1]]))
    
    
  }else{
    stop("Driver is not ready or url is not of type search1, \n 
         check that the url is provided by function output of f_1_2 and \n
         that the Rselenium driver is active")
  } 
  
  return(tb)
  
}

# url <- I$urls %>% unlist() %>% `[`(i)
# test <- getHtml(remDr = remDr, url = urli, category = category, city = city, resume = FALSE, n_max = 200)
getHtml <- function(remDr = remDr, url = url_rev, category = category, city = city, resume = FALSE, n_max = 100){
  
  fi <- digest::digest(url)
  params_lst <- list()
  
  if(resume == TRUE & (!file.exists(here::here("SAVE",paste0(fi,".rds"))) )){resume = FALSE}
  
  if(resume == FALSE ){
    # category = "Hotels"
    # city = "los angeles"
    
    ## Navigate to url
    remDr$navigate(url)
    
    ## Uid for the site (keep always the original page as uid)
    orig <- digest::digest(url) 
    
    ## First HTML extract get info and save html
    tb <- f_2_4_getHtml(remDr = remDr, url = url, category = category, city = city)
    
    ## Process images
    html <- rvest::read_html(readRDS(tb$file))
    print("Save review")
    save_ij <- f_3_1_saveReviews(html,tb$url)
    print("Save image")
    save_ij_m <- tryCatch({f_3_2_savePictures(save_ij)},error=function(e){return(save_ij)})
    print("Save to disk")
    saveRDS(save_ij_m,paste0("./SAVE2/",digest::digest(unique(save_ij_m$url)),".rds"))      
    
    
    ## Get what page are you and number of total reviews
    params <- f_2_1_saveNavParams(remDr = remDr, url = url)
    
    ## Placeholder for navigation
    params0 <- params
    
    ## Save a record for current page location
    params_lst[[1]] <- dplyr::tribble(~orig, ~tb, ~params,orig, tb, params)
    
    ## Create an address and save to disk
    address <- here::here("SAVE",paste0(orig,".rds"))
    saveRDS(params_lst,file = address)
    
  }else{
    
    ## If resume then find uid of url
    uid <- digest::digest(url)
    
    ## define the file name where is potentially stored
    address <- here::here("SAVE",paste0(uid,".rds"))
    
    ## Load into memory the file
    param_lst <- readRDS(address)
    
    ## extract the last entry for (params <- navigation, and orig <- original id)
    orig <- param_lst %>% tail(1) %>% `[[`(1) %>% `$`(orig)
    params <- param_lst %>% tail(1) %>% `[[`(1) %>% `$`(params) %>% `[[`(1)
    lurl <- param_lst %>% tail(1) %>% `[[`(1) %>% `$`(tb) %>% `[[`(1) %>% `$`(url)
    
    
    ## Navigate to latest
    remDr$navigate(lurl)
    
    ## Placeholder for navigation
    params0 <- params
    
  } 
  
  if(n_max <= 1){n_max <- 2}
  
  for(i in (params[1]+1):min(n_max,params[2])){ ##starting in the last record
    # i <- 2
    
    ## Find the next button
    input1 <- tryCatch(expr = {remDr$findElements(using = "xpath", 
                                                  value = ".//a[contains(@class,'ui_button nav next primary')]")[[1]]},error = function(e){return(FALSE)})
    if(is.logical(input1) == TRUE){
      next
    }else{
      
      input1$clickElement() ## go to next site
      
      ## Store HTML
      tb <- f_2_4_getHtml(remDr = remDr, url = url, category = category, city = city)
      
      ## Process images
      html <- rvest::read_html(readRDS(tb$file))
      save_ij <- f_3_1_saveReviews(html,tb$url) 
      # save_ij_m <- f_3_2_savePictures(save_ij)
      save_ij_m <- tryCatch({f_3_2_savePictures(save_ij)},error=function(e){return(save_ij)})
      saveRDS(save_ij_m,paste0("./SAVE2/",digest::digest(unique(save_ij_m$url)),".rds"))      
      
      ## deduce what page you are on based on the url name
      pag <- gsub(".*-or","",tb$url) %>% gsub("-.*","",.) %>% as.numeric() %>% `/`(5) %>% `+`(1)
      
      ## update navigation placeholder
      params0[1] <- pag
      
      ## Create a new row for tracking file
      params_lst[[i]] <- dplyr::tribble(~orig, ~tb, ~params,orig, tb, params0)
      
      ## Create an address and save to disk
      address <- here::here("SAVE",paste0(orig,".rds"))
      saveRDS(params_lst,file = address)
    }
    
  }
  
  ## Return the list
  return(params_lst)
  
}


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








#####################################################################

## F_1_1 : Nav to Trip Advisor
rD <- rsDriver(browser="firefox", port=45354L, verbose=F)
remDr <- rD[["client"]]


cities <- c("new york","los angeles","seattle","miami","chicago","houston","phoenix","philadelphia","denver","atlanta")
categories <- c("Hotels","Restaurants")



### This code Runs over Categories, Cities 
for(ca in 1:length(categories)){ ## Run over Categories
  # ca <- 1
  cate <- categories[ca]
  for(h in 2:length(cities)){ ## Run over Cities
    # 1 -> h
    I <- f_0_2_retrieveHtmls(category = cate, cities[h])[1:50,]
    
    # check if window is open
    out <- tryCatch({unlist(remDr$getCurrentWindowHandle())},error = function(e){return(FALSE)})
    if(out == FALSE){remDr$open()}
    
    for(i in 216:length(I$urls %>% unlist())){ ## Over Sites
      # i <- 146
      urli <- I$urls %>% unlist() %>% `[`(i)
      cat <- categories[ca]
      cit <- cities[h]
      
      x <- getHtml(remDr = remDr, url = urli, category = cat, city = cit, resume = FALSE, n_max = 100)
      print(paste('finished with hotels in',urli,"for city:",cit,"for category:",cat)) 
    }
    
    print(paste("fully finished with city:",cities[h]))
  }
  print(paste("fully finished with category",categories[ca]))
}




remDr$close()
rD$server$stop()
closeAllConnections()

