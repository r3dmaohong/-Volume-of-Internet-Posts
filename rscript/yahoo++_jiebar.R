##yahoo
library(rvest)
library(plyr)
library(XML)
library(RCurl)
library(SnowballC)
library(cluster)   
library(XML)
library(RCurl)

yahoo_crawler_jiebar <- function(link,forum_name,min,max,start.time){
  forum_name = forum_name
  links_data_yahoo = {}
  url = link
  
  for(i in min:max){
    url <- paste(link, i, '&sort=new',sep='')
    title_css = read_html(url) %>% html_nodes("a") %>% html_attr('href')
    title_css = title_css[which(grepl('question',title_css) & !grepl('login',title_css))]
    links_data_yahoo = c(links_data_yahoo,title_css)
    gc()
    cat("\r Yahoo Answer: Page ",i) 
    Sys.sleep(runif(1,2,5))    
  }
  cat("\n ")
  links_data_yahoo <- unique(links_data_yahoo)
  
  temp_yahoo_data = {}
  ##Start to crawl out the contents...
  for(i in 1:length(links_data_yahoo)){
    tryCatch({
    #url = paste0('https://tw.answers.yahoo.com',links_data_yahoo[i])
    url = links_data_yahoo[i]
    title_css = read_html(url) %>% html_nodes("span") %>% html_text()
    temp <- iconv(title_css,'utf8')
    
    temp_yahoo_data = c(temp_yahoo_data,temp)
    ##which contains 落點
    gc() #記憶體釋放
    cat("\r Yahoo Answer Page: ",i, ' ==> ',i/length(links_data_yahoo)*100, '% completed                              ')
    Sys.sleep(runif(1,2,5))
    },error=function(e){
      
    })
  }
  cat("\n ")
  
  last = links_data_yahoo[which(grepl('qid',links_data_yahoo))]
  last = last[length(last)]
  last = gsub("[^0-9]", "",last)
  last = substr(last,1,8)
  
  recent = gsub('-','',strsplit(toString(Sys.time()),' ')[[1]][1])
  
  dir.create(paste0(".\\output\\",n,"\\raw data"), showWarnings = FALSE)
  dir.create(paste0(".\\output\\",n,"\\raw data\\",forum_name), showWarnings = FALSE)
  write.csv(lineq_data,paste0(".\\output\\",n,"\\raw data\\",forum_name,'\\',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  
  yahoo_data = temp_yahoo_data
  ##yahoo_data = read.csv(file.choose(),stringsAsFactors=F)
  ##yahoo_data = yahoo_data[,2]
  jiebar_n(forum_name,yahoo_data,recent,last)
  
}

