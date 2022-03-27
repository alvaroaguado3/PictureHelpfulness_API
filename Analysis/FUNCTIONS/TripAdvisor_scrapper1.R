#### Image and Review Scrapper Trip Advisor
## Objective: This code is intended to search for a specific topic and download all the htmls of a particular site 
## reviews content from the website

### Libraries

library("RSelenium")
library("rvest")
library("tidyverse")
library("sys")

### Parameters 
# url <- "https://www.tripadvisor.com/Hotel_Review-g60982-d209422-Reviews-Hilton_Waikiki_Beach-Honolulu_Oahu_Hawaii.html"
url <- "https://www.tripadvisor.com/Search?q=New+York&search"

cities <- c("new york","los angeles","seattle","miami","chicago","houston","phoenix","philadelphia","denver","atlanta")
categories <- c("Hotels","Restaurants")

### Functions

## F_0_1 sleep and check current url
f_0_1_sleepCheck <- function(){
  # First wait for the DOM to load by pausing for a couple seconds.
  Sys.sleep(2) 
  
  # Get current url
  c_url <- remDr$getCurrentUrl()
  
  return(c_url)
  
}

## F_1_1 : Nav to Trip Advisor
rD <- rsDriver(browser="firefox", port=45354L, verbose=F)
remDr <- rD[["client"]]

## F_1_2 : Search for a particular category (or item) - Search1
f_1_2_searchNULL <- function(remDr = remDr, url = url, city = "New York"){
  ## Objective: This function takes current Selenium Session and searches for a city in TripAdvisor main url
  # remDr : Rselenium connection
  # url: string - with valid url
  # city : string - a city to search (could be something else)
  # output: new url with search outputs
  
  
  # check status
  if(  remDr$getStatus()$ready == TRUE){
    
    remDr$navigate(url)
    
    #sleep and check
    c_url <- f_0_1_sleepCheck()
    
    
    # # Search for the city 
    # input <- remDr$findElements(using = "xpath", 
    #                             value = ".//input[@type='search' and @placeholder='Where to?']")[[1]]
    # input$sendKeysToElement(list(city, key = "enter"))
    # 
    
    
    #sleep and check
    d_url <- f_0_1_sleepCheck()
    
    # check if correctly navigated
    # if(identical(c_url,d_url)== TRUE){
    #   stop("Error: Could not reach destination url. Check find elements query or Sys.sleep")
    # }
    
  }else{
    stop("Driver is not ready, try again")
  }
  
  # Allow redirect from url
  d_url <- gsub("true","false",d_url)
  attr(d_url,"class") <- "search1"
  
  # return destination url
  return(d_url)
}


## F_1_3 : Select search category - Search2
f_1_3_searchCategory <- function(remDr = remDr, url = destination_url, city = "New York", category = "Hotels", nav = FALSE){
  
  start <- Sys.time()
  
  if( nav == TRUE){remDr$navigate(url[1])}
  Sys.sleep(3)
  
  # check status
  if(  remDr$getStatus()$ready == TRUE & attributes(url)$class == "search1"){
    
    # Check url and wait
    c_url <- f_0_1_sleepCheck()
    # if(url != c_url){
    #   remDr$navigate(url[1])
    #   Sys.sleep(2)
    #   c_url <- f_0_1_sleepCheck()
    # }
    bck <- RSelenium::selKeys$backspace
    
    # Change Search word, location and search
    input1 <- remDr$findElements(using = "xpath", 
                                 value = ".//input[@type='search']")[[2]]
    
    # find current value in search
    val_str <- input1$getElementAttribute('value') %>% unlist()
    
    # Remove it manually and then clear content
    for(i in 1:nchar(val_str)){
      input1$sendKeysToElement(list(bck))
      Sys.sleep(.05)
    }
    input1$clearElement()
    
    # Write new query based on the category
    Sys.sleep(0.2)
    input1$sendKeysToElement(list(category))
    
    # Find current location for search
    input2 <- remDr$findElements(using = "xpath", 
                                 value = ".//input[@class='text geoScopeInput']")[[1]]
    val_str <- input2$getElementAttribute('value') %>% unlist()
    
    # Remove it manually and then clear content    
    for(i in 1:nchar(val_str)){
      input2$sendKeysToElement(list(bck))
      Sys.sleep(.05)
    }
    input2$clearElement()
    
    # Write new query based on city to search
    Sys.sleep(0.5)
    input2$sendKeysToElement(list(city))
    
    # Select the element with the continent (this avoids selecting the city)
    Sys.sleep(1)
    input3 <- remDr$findElements(using = "xpath", 
                                 value = ".//li[contains(@class,'displayItem result selected')]")[[1]]
    input3$clickElement()
    
    # Filter searches to only the category in play (hotel searches --> filter Hotels)
    Sys.sleep(1)
    inputs <- NULL
    while(length(inputs)==0){
      inputs <- remDr$findElements(using = "xpath",
                                   value = ".//*[contains(@class,'search-filter ui_tab  ')]")
      Sys.sleep(0.5)
    }
    
    
    categories <- purrr::map(.x = inputs,.f = ~ .x$getElementText()) %>% unlist()
    loc_category <- which(categories %in% category)
    
    inputs[[loc_category]]$clickElement()
    Sys.sleep(1)
    
    # Check url and wait
    d_url <- f_0_1_sleepCheck()
    
    # check if correctly navigated
    if(identical(c_url,d_url)== TRUE){
      stop("Error: Could not reach destination url. Check find elements query or Sys.sleep")
    }
    
    
  }else{
    stop("Driver is not ready or url is not of type search1, \n 
         check that the url is provided by function output of f_1_2 and \n
         that the Rselenium driver is active")
  } # end else
  
  
  # Allow redirect from url
  d_url <- gsub("blockRedirect=true","blockRedirect=false",d_url)
  attr(d_url,"class") <- "search2"
  
  time <- Sys.time() - start
  print(paste("Total elapsed time", time))
  
  # return destination url
  return(d_url)
  
} # end of function


## F_1_4 : Gather all urls for results - list()
f_1_4_gatherSearchUrls <- function(remDr = remDr, url = destination_url2){
  
  # Each search page has 30 results per page
  
  # check status
  if(  remDr$getStatus()$ready == TRUE & attributes(url)$class == "search2"){
    
    # Check url and wait
    c_url <- f_0_1_sleepCheck()
    # if(url != c_url){
    #   remDr$navigate(url[1])
    #   Sys.sleep(2)
    #   c_url <- f_0_1_sleepCheck()
    # }
    
    
    inputs <- remDr$findElements(using = "xpath",
                                 value = ".//div[contains(@class,'result-title')]")
    
    # Vectorized extraction of urls for each website    
    page_urls <- purrr::map(.x = inputs,.f = ~ .x$getElementAttribute('onclick')[[1]] %>% 
                              stringr::str_split(.,",") %>% 
                              unlist(recursive = FALSE) %>% 
                              `[`(stringr::str_detect(.,".html"))  %>% 
                              stringr::str_remove_all(.,pattern = "'") %>% 
                              stringr::str_trim(side = "both") %>% 
                              paste0("https://www.tripadvisor.com",.))
    
    
    # review_url <- stringr::str_split(inputs[[1]]$getElementAttribute('onclick')[[1]],",")[[1]] %>% 
    #   `[`(stringr::str_detect(.,".html")) %>% 
    #   stringr::str_remove_all(.,pattern = "'") %>% 
    #   stringr::str_trim(side = "both") %>% 
    #   paste0("https://www.tripadvisor.com",.)
    
    
    
  }else{
    stop("Driver is not ready or url is not of type search2, \n 
         check that the url is provided by function output of f_1_3 and \n
         that the Rselenium driver is active")
  } # end else
  
  return(page_urls)
  
}

## F_1_5 : Get current page, max number of pages and max allowed number of pages 
f_1_5_gatherPageCounter <- function(remDr = remDr, url = destination_url2, n_max = 34){
  # Each search page has 30 results per page
  
  # check status
  if(  remDr$getStatus()$ready == TRUE & attributes(url)$class == "search2"){
    
    # Check url and wait
    c_url <- f_0_1_sleepCheck()
    # if(url != c_url){
    #   remDr$navigate(url[1])
    #   Sys.sleep(2)
    #   c_url <- f_0_1_sleepCheck()
    # }
    
    
    inputs <- remDr$findElements(using = "xpath",
                                 value = ".//a[contains(@class,'pageNum')]")
    
    final <- purrr::map(inputs, ~ .x$getElementAttribute('data-page')) %>% 
      unlist(recursive = TRUE) %>% 
      tail(1)
    
    currentP <- remDr$findElements(using = "xpath",
                                   value = ".//a[contains(@class,'pageNum current')]")
    current <- purrr::map(currentP, ~ .x$getElementAttribute('data-page')) %>% 
      unlist(recursive = TRUE) %>% 
      tail(1)
    
    
    
    # Vectorized extraction of urls for each website    
    
    
    # review_url <- stringr::str_split(inputs[[1]]$getElementAttribute('onclick')[[1]],",")[[1]] %>% 
    #   `[`(stringr::str_detect(.,".html")) %>% 
    #   stringr::str_remove_all(.,pattern = "'") %>% 
    #   stringr::str_trim(side = "both") %>% 
    #   paste0("https://www.tripadvisor.com",.)
    
    
  }else{
    stop("Driver is not ready or url is not of type search2, \n 
         check that the url is provided by function output of f_1_3 and \n
         that the Rselenium driver is active")
  } # end else
  
  counterPages <- c(current,final,n_max)
  return(counterPages)
  
}

f_1_6_navigatePages<- function(remDr = remDr, url = destination_url2, counterurls = counterurls){
  
  start <- Sys.time()
  
  # check status
  if(  remDr$getStatus()$ready == TRUE & attributes(url)$class == "search2"){
    
    # Check url and wait
    c_url <- f_0_1_sleepCheck()
    # if(url != c_url){
    #   remDr$navigate(url[1])
    #   Sys.sleep(2)
    #   c_url <- f_0_1_sleepCheck()
    # }
    
    
    # Check page number
    # if(counterurls[1] <= counterurls[3]){  
    
    inputs <- remDr$findElements(using = "xpath",
                                 value = ".//a[contains(@class,'next')]")
    
    inputs[[1]]$clickElement()
    
    # Vectorized extraction of urls for each website    
    
    
    # review_url <- stringr::str_split(inputs[[1]]$getElementAttribute('onclick')[[1]],",")[[1]] %>% 
    #   `[`(stringr::str_detect(.,".html")) %>% 
    #   stringr::str_remove_all(.,pattern = "'") %>% 
    #   stringr::str_trim(side = "both") %>% 
    #   paste0("https://www.tripadvisor.com",.)
    
    Sys.sleep(2)
    d_url <- f_0_1_sleepCheck()
    
    if(identical(c_url,d_url)== TRUE){
      stop("Error: Could not reach destination url. Check find elements query or Sys.sleep")
    }
    
    # }else{
    #   
    #   return(FALSE)
    # }
    
  }else{
    stop("Driver is not ready or url is not of type search2, \n 
         check that the url is provided by function output of f_1_3 and \n
         that the Rselenium driver is active")
  } # end else
  
  
  # Allow redirect from url
  d_url <- gsub("blockRedirect=true","blockRedirect=false",d_url)
  attr(d_url,"class") <- "search2"
  
  time <- Sys.time() - start
  print(paste("Total elapsed time", time))
  
  
  # return destination URL
  return(d_url)
  
}


## F1 - Aggregator of URL searches
F1_urlCollector <- function(remDr = remDr, url = url, city = city, category = category, nav = TRUE, n_max = 34, resume = FALSE){
  
  start <- Sys.time()
  
  if(resume == TRUE & file.exists(paste0("./DATA/TA_urls_",paste(category,city,sep = "_"),".rds"))){
    
    # Load last data for this category/city
    tb1 <- readRDS(paste0("./DATA/TA_urls_",paste(category,city,sep = "_"),".rds"))
    
    # Extract last counter information
    counterurls <- tb1 %>% tail(1) %>% pull(counterurls) %>%  unlist(recursive = FALSE)
    
    # Extract destination_url
    destination_url2 <- tb1 %>% tail(1) %>% pull(desturl) %>%  unlist(recursive = FALSE)
    
    # Go to site
    remDr$navigate(destination_url2[1])
    
    
  }else{
    
    # Get to search website
    destination_url <- f_1_2_searchNULL(remDr = remDr, url = url, city = city) # NOT RUN
    destination_url2 <- f_1_3_searchCategory(remDr = remDr, url = destination_url, city = city, category = category, nav = nav) # NOT RUN
    
    # find how many websites there are in the search
    pageurls <- f_1_4_gatherSearchUrls(remDr = remDr, url = destination_url2)
    
    
    # Save what url site we are at
    counterurls <- f_1_5_gatherPageCounter(remDr = remDr, url = destination_url2,n_max = n_max)
    
    # store information in a tribble
    tb1 <- tribble(~category, ~city , ~desturl, ~urls , ~counterurls,
                   category, city, destination_url2, pageurls, counterurls) 
    
    # Save progress
    saveRDS(tb1,paste0("./DATA/TA_urls_",paste(category,city,sep = "_"),".rds"))
    
    
  } # end else
  
  
  for(i in counterurls[1]:counterurls[3]){
    
    print(paste("Page",i,"out of ",counterurls[3],"for category:",category,"and city:",city))
    
    if(i == counterurls[3] | i == counterurls[2]){break}
    
    # Move to next page
    destination_url2 <- f_1_6_navigatePages(remDr = remDr, url = destination_url2, counterurls = counterurls)
    Sys.sleep(2)
    
    # find how many websites there are in the search
    pageurls <- f_1_4_gatherSearchUrls(remDr = remDr, url = destination_url2)
    
    # Save what url site we are at
    counterurls <- f_1_5_gatherPageCounter(remDr = remDr, url = destination_url2,n_max = n_max)
    
    # Add to list
    tbi <- tribble(~category, ~city , ~desturl, ~urls , ~counterurls,
                   category, city, destination_url2, pageurls, counterurls) 
    
    tb1 <- tb1 %>% add_case(tbi)  
    
    # Save progress
    saveRDS(tb1,paste0("./DATA/TA_urls_",paste(category,city,sep = "_"),".rds"))
    
    
  }
  
  
  time <-  Sys.time() - start
  print(paste("Elapsed time was :",time))
  
  # return resulting table
  return(tb1)
  
}

# tb1 <- F1_urlCollector(remDr = remDr, url = url, city = city, category = category, nav = TRUE, n_max = 34, resume = TRUE)


##### RUN ####

## Loop over F1 searches for categories and cities
lst <- list()
start <- Sys.time()
for(i in categories[2]){
  for(j in cities[9:10]){
    
    # check if window is open
    out <- tryCatch({unlist(remDr$getCurrentWindowHandle())},error = function(e){return(FALSE)})
    if(out == FALSE){remDr$open()}
    
    lst[[i]][[j]] <- F1_urlCollector(remDr = remDr, url = url, city = j, category = i, nav = TRUE, n_max = 34, resume = TRUE)
    
  }# loop cities
}# loop categories
time <- Sys.time() - start  
print(paste("Elapsed time was :",time))


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