#### HTML Scrapper Trip Advisor
## Objective: This code is intended to take a pre-scrapped html and returns all 
## review items of interest

#### Libraries ####

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


## F_1_1 : Nav to Trip Advisor
rD <- rsDriver(browser="firefox", port=45354L, verbose=F)
remDr <- rD[["client"]]


cities <- c("new york","los angeles","seattle","miami","chicago","houston","phoenix","philadelphia","denver","atlanta")
categories <- c("Hotels","Restaurants")



### This code Runs over Categories, Cities 
for(ca in 1:length(categories)){ ## Run over Categories
  # ca <- 1
  cate <- categories[ca]
  for(h in 1:length(cities)){ ## Run over Cities
  # 1 -> h
    I <- f_0_2_retrieveHtmls(category = cate, cities[h])[1:50,]
    
    # check if window is open
    out <- tryCatch({unlist(remDr$getCurrentWindowHandle())},error = function(e){return(FALSE)})
    if(out == FALSE){remDr$open()}
    
    for(i in 113:length(I$urls %>% unlist())){ ## Over Sites
      # i <- 1
      urli <- I$urls %>% unlist() %>% `[`(i)
      cat <- categories[ca]
      cit <- cities[h]
      
      getHtml(remDr = remDr, url = urli, category = cat, city = cit, resume = TRUE, n_max = 100)
      print(paste('finished with hotels in',urli,"for city:",cit,"for category:",cat)) 
    }
  
    print(paste("fully finished with city:",cities[h]))
  }
  print(paste("fully finished with category",categories[ca]))
}




remDr$close()
rD$server$stop()





#### DRAFTS:: ####

# 
# html <- remDr$getPageSource()[[1]]
# html <- read_html(html)
# 
# reviews <- html %>% # Name a variable reviews and take a look at the html
#   html_elements(".dovOW") %>% # In particular, look at the class named .cPQsENeY
#   html_text() %>% # Grab the text contained with this class
#   as_tibble() # And save it as a tibble to reviews.
# 
# helpful <- html %>% 
#   html_elements(".cWwQK") %>% 
#   html_text()
# 
# dates <- html %>%
#   html_elements("._34Xs-BQm") %>%
#   html_text() %>%
#   str_remove_all("Date of stay: ") %>% # Remove unwanted strings
#   as_tibble()
# 
# score <- parsedPage %>%
#   html_elements(".nf9vGX55") %>%
#   html_children() %>% # Look at the child of the named class
#   html_attr("class") %>% # Grab the name of the class of the child
#   str_remove_all("ui_bubble_rating bubble_") %>% # Remove unwanted strings
#   as_tibble()