#https://www.youtube.com/watch?v=PYy5C9IIgp8
#http://stackoverflow.com/questions/29861117/r-rvest-scraping-a-dynamic-ecommerce-page

##Selenium
##or
##Selenium with click event

library(RSelenium)
library(rvest)
library(httr)
library(jiebaR)
library(text2vec)
library(tmcn)

##Remove leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Run selenium server
##cmd
##java -jar selenium-server-standalone-2.53.1.jar

remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName ="firefox")
##Open browser
remDr$open() 
##Check the status of browser
##remDr$getStatus()

wd <- file.path("git_workspace", "")
setwd(wd)

filename <- file.choose()
df <- read.csv(filename, stringsAsFactors = F)
names(df)

##new col
df$new_col = 0
newcolName <- "被主投數"

##Source to crawl with
ncolOfV <- 2
##Sleep time
smin <- 1
smax <- 2

##########
##Selenium
##########
for(i in 1:nrow(df)){
  tryCatch({
    ##Percent-encode characters in URL
    ##URLencode(df[i, ncolOfV])
    url <- paste0("http://www.1111.com.tw/job-bank/job-description.asp?eNo=", df[i, ncolOfV], "")
    
    ##Website to crawl
    remDr$navigate(url)
    ##Get Page's Source code
    page_source <- remDr$getPageSource()
    html_source <- read_html(page_source[[1]])
    
    tmp <- html_source %>% html_nodes("span span") %>%  html_text()
    tmp <- tmp[which(grepl("人次主動應徵", tmp))]
    tmp <- unique(tmp)
    gc()      
    
    df$new_col[i] <- tmp
    
    cat("\r「", df[i, ncolOfV], "」 => 「", tmp, "」 - ", format(round(i/nrow(df)*100, 3), nsmall = 3), " %", rep(" ", 20))

    sleep_time <- runif(1, smin, smax)
    ##print(paste0("休息", sleep_time, "sec..."))
    Sys.sleep(sleep_time)
  }
  , error=function(e){
    print(paste0("「", df[i, ncolOfV], "」 => Failed - ", format(round(i/nrow(df)*100, 3), nsmall = 3), " %", rep(" ", 20)))
    Sys.sleep(runif(1, smin, smax))
  })
}
##Rename col
names(df)[names(df)=="new_col"] <- newcolName
outputName <- paste0(format(Sys.Date(), "%Y%m%d_"), newcolName, "_", basename(filename))

write.csv(df, outputName, row.names=F)


###########################
##Selenium with click event
###########################

##http://icap.wda.gov.tw/Resources/resources_Datum_List.aspx
##職能

##Page limit
pmin <- 1
pmax <- 8

##Sleep time
smin <- 1
smax <- 2

url <- "http://icap.wda.gov.tw/Resources/resources_Datum_List.aspx"
remDr$navigate(url)
for(pagelimit in pmin:pmax){
  #get the page 
  page_source<-remDr$getPageSource()
  #parse it
  html_source <- read_html(page_source[[1]])
  links  <- html_source %>% html_nodes("td a") %>%  html_attr("href")
  fnames <- html_source %>% html_nodes("td a") %>%  html_text()
  fnames <- fnames[grepl("pdf", links)]
  links  <- links[grepl("pdf", links)]
  
  for(i in 1:length(fnames)){
    destfile <- paste0(fnames[i], "職能.pdf")
    destfile <- gsub("/", "／",destfile)
    
    url <- paste0("http://icap.wda.gov.tw/", links[i] %>% substr(., 4, nchar(links[i])))
    download.file(url, destfile, mode="wb")
    Sys.sleep(runif(1, min = smin, max = smax))
  }
  if(pagelimit < pmax){
    ##Click event
    webElem <- remDr$findElement("id","ctl00_ContentPlaceHolderNoMenu_pager_lbtn_nextpage")
    webElem$clickElement()
    Sys.sleep(runif(1, min = 3, max = 5))
  }
}
