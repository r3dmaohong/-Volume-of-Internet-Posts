##ptt jieba
library(rvest)

ptt_crawler_jiebar <- function(link, forum_name = paste0('ptt ',substr(link,unlist(gregexpr(pattern ='bbs',link))+4,unlist(gregexpr(pattern ='index',link))-2)) ,min=1 , max=9999999, start.time = paste0('¥¼¶ñ¼g_',gsub(":","_",Sys.time()))){
  links_data_ptt = {}
  
  ##Get posts' urls form lists of pages.
  print(forum_name)
  for(i in min:max){
    tmp <- paste(i, '.html', sep='')
    #https://www.ptt.cc/bbs/Salary/index1896.html
    #https://www.ptt.cc/bbs/ServiceInfo/index'
    url <- paste(link, tmp, sep='')
    tryCatch({
      title_css <<- read_html(url) %>% html_nodes(".title") %>% html_nodes("a") %>% html_attr('href')
    }, error = function(e) {
      title_css <<- ""

    })
    if(toString(title_css)!=''){
      links_data_ptt = c(links_data_ptt,title_css)
      gc() 
      cat("\r PTT Page: ",i)
      Sys.sleep(runif(1,2,5))
    }else{
      cat("\r PTT Page ",i, ' failed')
      break
    }
  }
  cat("\n ")
  
  if(max==9999999) #Means varible "max" is unsetted.
    max = i - 1 
  print(paste0('Has Accessed to the last page : Page ', max))
  links_data_ptt <- unique(links_data_ptt)
  
  dir.create(paste0(".\\output\\",n,"\\raw data"), showWarnings = FALSE)
  dir.create(paste0(".\\output\\",n,"\\raw data\\",forum_name), showWarnings = FALSE)
  
  ptt_data = data.frame('Url'=paste0('https://www.ptt.cc',links_data_ptt), 'Date'="", 'Content'="", 'Reply'="", 'Title'="", stringsAsFactors=F)
  
  ##Start to crawl out the contents...
  for(i in 1:length(links_data_ptt)){
    tryCatch({
      url = paste0('https://www.ptt.cc',links_data_ptt[i])
      total_css = read_html(url) 
      
      content_css       <- total_css %>% html_nodes("#main-content") %>% html_text()
      utf8_text_content <- iconv(content_css,'utf8')
      
      meta_css  <- total_css %>% html_nodes(".article-meta-value") %>% html_text()
      date_css  <- meta_css[4]
      title_css <- meta_css[3]
      reply_css <- total_css %>% html_nodes(".push-content") %>% html_text() %>% iconv(., 'utf8') %>% paste(., collapse="\n")
      
      ##Remove IDs
      id_css = total_css %>% html_nodes("span") %>% html_text()
      utf8_text_id <- iconv(id_css,'utf8')
      
      id_delete = utf8_text_id[which(grepl(': ',utf8_text_id))-1]
      id_delete = c(id_delete, utf8_text_id[1:8])
      id_delete = id_delete[which(!is.na(id_delete))]
      for(x in 1:length(id_delete)){
        utf8_text_content=gsub(id_delete[x],'',utf8_text_content)
      }
      
      #temp = content_css
      ptt_data[i,2:5] = c(date_css, utf8_text_content, reply_css, title_css)
            
      gc()
      write.csv(ptt_data,paste0(".\\output\\",n,"\\raw data\\",forum_name,'\\',forum_name,'_',min,'_',max,'.csv'),row.names=F)
      Sys.sleep(runif(1,2,5))
      cat("\r PTT article ",i, ' ==>',i/length(links_data_ptt)*100, '% completed.   ',paste(replicate(50, " "), collapse = ""))
    }, error = function(e) {
      cat("\n ")
      print(paste0(forum_name, ' PTT article ',i,' failed. ',i/length(links_data_ptt)*100,'%'))
      Sys.sleep(runif(1,2,5))
    })
  }
  cat("\n ")
  print(paste0(forum_name,' : ',nrow(ptt_data),' articles.'))
  
  ptt_data = unique(ptt_data)
  
  write.csv(ptt_data,paste0(".\\output\\",n,"\\raw data\\",forum_name,'\\',forum_name,'_',min,'_',max,'.csv'),row.names=F)
    
  ptt_data = ptt_data$Content
  jiebar_n(forum_name,ptt_data,min,max)
}

