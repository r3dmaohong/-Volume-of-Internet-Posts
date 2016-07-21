##lineq jieba
library(rvest)

lineq_crawler_jiebar <- function(link, forum_name, min = 1,max = 9999999, start.time = paste0('unsetted_',gsub(":","_",Sys.time()))){
  links_data_lineq = {}
  url = link
  ##Get posts' urls form lists of pages.
  for(i in min:max){
    url <- paste(link, i, sep='')
    title_css = read_html(url) %>% html_nodes("p") %>% html_nodes("a") %>% html_attr('href')
    if(toString(title_css)!=''){
      links_data_lineq = c(links_data_lineq,title_css)
      gc() #Release the memory
      cat("\r LineQ Page: ",i) #,i/max*100, '% completed',paste(replicate(50, " "), collapse = ""))
      Sys.sleep(runif(1,2,5))
    }else{
      cat("\r LineQ Page: ",i, ' failed')
      break
    }

  }
  cat("\n ")
  if(max==9999999) #Means varible "max" is unsetted.
    max = i - 1 
  print(paste0('Has Accessed to the last page : Page ', max))
  ##temp_lineq_data = {}
  links_data_lineq = unique(links_data_lineq)
  
  lineq_data = data.frame('Date'=character(),'Content'=character(),stringsAsFactors=F)
  
  xrow = 1 ##Start to crawl out the contents...
  for(i in 1:length(links_data_lineq)){
    tryCatch({
      url = paste0('http://lineq.tw',links_data_lineq[i])
      total_css = read_html(url)
      content_css = c(total_css %>% html_nodes(".question_content .content_text") %>% html_text(), total_css %>% html_nodes(".reply_content .content_text") %>% html_text())
      content_utf8 <- iconv(content_css,'utf8')
      
      #Date of the post
      date_css = total_css %>% html_nodes(".header_time") %>% html_text()
      date_utf8 <- iconv(date_css,'utf8')
      
      #nowdate = substr(gsub('-','.',Sys.time()),1,unlist(gregexpr(pattern =':',Sys.time()))[length(unlist(gregexpr(pattern =':',Sys.time())))]-1)
      if(grepl('小時',date_utf8)){
        time = as.numeric(unique(unlist(regmatches(date_utf8, gregexpr("[0-9]+", date_utf8)))))
        date_utf8 = substr(gsub('-','.',Sys.time() - time*60*60),1,unlist(gregexpr(pattern =':',Sys.time() - time*60*60))[length(unlist(gregexpr(pattern =':',Sys.time() - time*60*60)))]-1)
        
      }else if(grepl('分',date_utf8)){
        time = as.numeric(unique(unlist(regmatches(date_utf8, gregexpr("[0-9]+", date_utf8)))))
        date_utf8 = substr(gsub('-','.',Sys.time() - time*60),1,unlist(gregexpr(pattern =':',Sys.time() - time*60))[length(unlist(gregexpr(pattern =':',Sys.time() - time*60)))]-1)
        
      }else if(grepl('天',date_utf8)){
        time = as.numeric(unique(unlist(regmatches(date_utf8, gregexpr("[0-9]+", date_utf8)))))
        date_utf8 = paste0(gsub('-','.',Sys.Date() - time)," 00:00")
      }else{
      }
      ##Use it as a sep.
      content_utf8 =  paste0(content_utf8,collapse=';:;:;')
      lineq_data[xrow,] = c(date_utf8,content_utf8)
      xrow = xrow + 1 
      gc()
      
      cat("\r LineQ Page ",i, ' ==> ',i/length(links_data_lineq)*100, '% completed ',paste(replicate(50, " "), collapse = ""))
      Sys.sleep(runif(1,2,5))
    },error=function(e){
      cat("\r LineQ Page ",i, ' ==> ',i/length(links_data_lineq)*100, '% failed ',paste(replicate(50, " "), collapse = ""))
    })
    
  }
  cat("\n ")
  
  title_css = total_css %>% html_nodes(".header_time") %>% html_text()
  recent <- iconv(title_css,'utf8')
  recent = strsplit(recent,' ')[[1]][1]
  recent = gsub('[.]','',recent)
  
  last = gsub('-','',strsplit(toString(Sys.time()),' ')[[1]][1])
  
  lineq_data = unique(lineq_data)
  
  dir.create(paste0(".\\output\\",n,"\\raw data"), showWarnings = FALSE)
  dir.create(paste0(".\\output\\",n,"\\raw data\\",forum_name), showWarnings = FALSE)
  write.csv(lineq_data,paste0(".\\output\\",n,"\\raw data\\",forum_name,'\\',forum_name,'_',last,'_',recent,'.csv'),row.names=F)
  
  lineq_data = lineq_data$Content
  jiebar_n(forum_name,lineq_data,recent,last)
}

##'http://lineq.tw/search/question?q=%E6%B1%82%E8%81%B7%20%E6%9C%8D%E5%8B%99%E6%A5%AD&sort=date&sel=all'
#print(iconv(read_html(url) %>% html_nodes(".description_title") %>% html_text(),'utf8'))



