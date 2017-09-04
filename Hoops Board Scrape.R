require(rvest)

## set the homepage for UMass Hoops Message Board
url <- "http://www.umasshoops.com/newboard/viewforum.php?f=1&sid=bea9f02fcd63bc6e63db30d0d2b9ae04"
bball <- read_html(url)


## Figure out how many pages there are on the site based on the top pagination links
pages <-bball %>% 
          html_node(".topic-actions .pagination a") %>%
          html_text() %>%
          {gsub("Page 1 of ", "",  .)}


## create df to store all of the topic and links
threads <- data.frame(title = character(), link = character())

## create a loop to go over each of the pages and get links to each of the threads
## each page increments by 50
for(i in c(1:pages)){
  url.string <- (50*i)-50
  url <- paste0("http://www.umasshoops.com/newboard/viewforum.php?f=1&start=", as.character(url.string))
  html <- read_html(url)
  
  ## get all of the topic titles
  titles <- html %>% 
    html_nodes(".topictitle") %>%
    html_text()
  
  ## get all of the links
  links <- html %>% 
            html_nodes(".topictitle") %>%
            html_attr("href")

  ## bind them together
  df <- cbind.data.frame(titles, links)
  threads <- rbind.data.frame(threads, df)
}


## the sticky threads at the top of the page are on each site  -- de-dup these threads
## right now there are about 6,659 topics but there should be 6,284 topics
threads <- unique(threads)

# each link has a tail that isn't needed to reach the site: "....&sid=######
# strip of this tale and then grab only the unique records again

## find position of last ampersand in URL then strip out from then on
threads$links2 <- "empty"
for(i in 1:nrow(threads)){
  amp <- max(gregexpr("&", threads$links[i])[[1]])
  threads$links2[i] <- substr(threads$links[i], start = 2, stop = amp-1)
}
threads$links <- NULL

## get only the remaining unique records
threads<- unique(threads)

## figure out how many pages there are on each thread
threads$pages <- 0
counter <- 0
for(j in threads$links2){
  counter <- counter + 1
  bball <- read_html(paste0("http://www.umasshoops.com/newboard", j))
  threads$pages[counter] <-bball %>% 
  html_node(".topic-actions .pagination a") %>%
  html_text() %>%
  {gsub("Page 1 of ", "",  .)}
}

## check that the script ran through the entire list of pages
tail(threads) ##hopefully there are no zeros at this tail!

## the last 7 records are weird... get rid of them
threads <- threads[!(row.names(threads) %in% tail(row.names(threads), 7)), ]

## replace all of the NAs with zero
threads$pages <- ifelse(is.na(threads$pages), 0, threads$pages)





######################################################################################################
## Now that I have each of the threads and the number of pages in those threads--scrape all of the posts!

## create an empty table to store all of the posts
all.posts <- data.frame(title = character(), pagenum = numeric(), link.text = character(), metadata = character(), posts = character())


## loop over each thread and then loop within the thread to grab all of the posts with all of their info
count <- 0

for(urls in threads$links2[3233:nrow(threads)]){
  for(i in c(0:as.numeric(threads[threads$links2==urls, c("pages")]))){
    site <- read_html(paste0("http://www.umasshoops.com/newboard", urls, "&start=", as.character(i*15)))
    if(i==0){
      ## this will grab the topic title
      title <- site%>%
        html_node("h2") %>%
        html_text()
      pagenum <- as.numeric(threads[threads$links2==urls, c("pages")])
      link.text <- urls
    }
      ## this will get all of the authors & dates and times
      metadata <- site %>%
        html_nodes(".author") %>%
        html_text()
      
      ## this will grab all posts on a page
      posts <- site %>%
        html_nodes(".content") %>%
        html_text()
      
      all.posts <- rbind.data.frame(all.posts, cbind.data.frame(title, pagenum, link.text, metadata, posts))
  }
  count <- count+1
  print(count)
}



## need to clean up the metadata fields... brought them in as one long string
head(all.posts$metadata)

## first grab all the username
all.posts$poster <- gsub(" ", "", gsub("by","", sub('//».*', '', all.posts$metadata)))

## convert the remainder of that metadata string into a date-time field
all.posts$DateTime <- as.POSIXct("1900/1/1")
for(i in 1:nrow(all.posts)){
  all.posts$DateTime[i] <- as.POSIXct(x = trimws(x = substr(x = all.posts$metadata[i], start = gregexpr(pattern ='»',all.posts$metadata[i])[[1]][1]+1, stop = nchar(as.character(all.posts$metadata[i]))), which = "both"), format= "%a %b %d, %Y %I:%M %p", tz = "EST")
}

## done scraping save the file
write.csv(x = all.posts, file = "C:/Users/Ben/Documents/GitHub/Hoop Head/Hoop Board Data/Hoops Board Posts.csv")

## save it as an RDS file too
save(all.posts, file="C:/Users/Ben/Documents/GitHub/Hoop Head/Hoop Board Data/Hoop Board Posts.RDA")
