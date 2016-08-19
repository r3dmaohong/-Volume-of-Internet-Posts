##jiebar main program.
##Include noun extraction
jiebar_n <- function(forum_name,x_data,recent,last){
  library(jiebaR)
  library(plyr)
  library(text2vec)
  library(data.table)
  
  ##x為yahoo lineq ptt等等..
  ##Import custom words library...
  tmp     <- read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__處理後公司名稱.csv',stringsAsFactors=F)
  temp    <- unique(c(tmp$company,tmp$最終比對結果))
  tmp2    <- read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\學校名稱正規化表格.csv',stringsAsFactors=F)
  temp    <-  unique(c(temp,tmp2$trim後原始,tmp2$對應表))
  temp    <- c(temp, read.csv('D:\\abc\\wjhong\\projects\\Volume-of-Internet-Posts\\terms_DB.csv',stringsAsFactors=F)[,1])
  word_DB <- tolower(temp)
  #write.table(temp, file="D:\\abc\\wjhong\\projects\\合併詞庫.txt", row.names=FALSE, col.names=FALSE)
  
  #cutter = worker(type  = "mix"，user  = "D:/somefile.xxx")
  #cutter = worker()
  cutter=worker("tag", bylines = T)
  #sapply(temp,function(x) new_user_word(cutter,x,"n"))
  for(xj in 1:length(word_DB)){
    new_user_word(cutter,word_DB[xj],"n")
  }
  
  ##Extract words which is noun.
  get_noun = function(x){
    stopifnot(inherits(x,"character"))
    index = names(x) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")
    x[index]
  }
  
  jieba_x = {}
  jieba_x_noun = {}
  x_data = tolower(x_data)
  ##瓂\xa2\026
  #remove all punctuation except comma[^[:alnum:],]
  #x_data = x_data[,2]
  x_data = gsub('[^[:alnum:]]','',x_data)
  
  print("Start using cutter...")
  jieba_x <- lapply(x_data, function(x) cutter <=x)
  jieba_x <- lapply(jieba_x, '[[', 1)
  
  jieba_x_noun <- lapply(jieba_x, function(x) get_noun(unlist(x)))
  
  #for(i in 1:length(x_data)){
  #  tryCatch({
  #    temp = segment(x_data[i], cutter)
  #    res = cutter[x_data[i]]
  #    #get_noun(res)
  #    jieba_x = c(jieba_x,temp)
  #    jieba_x_noun = c(jieba_x_noun, get_noun(res))
  #    cat("\r ",forum_name," jiebar : ",i/length(x_data) * 100, '% completed',paste(replicate(50, " "), collapse = ""))
  #  }, error = function(e) {
  #    conditionMessage(e) 
  #  })
  #}
  
  cat("\n ")
  
  ##Data extraction
  data_ep <- function(x){
    a.token <- itoken(x)
    a.vocab <- create_vocabulary(a.token, ngram=c(1, 1))
    class(a.vocab$vocab)
    a.vocab$vocab$terms <- a.vocab$vocab$terms %>% iconv(., "utf8")
    a.vocab$vocab       <- a.vocab$vocab[order(-a.vocab$vocab$terms_counts),]
    
    x_cdf <- a.vocab$vocab
    
    ##Remove words which nchar==1.
    x_cdf = x_cdf[which(nchar(x_cdf$terms)>1),]
    ##Remove words with num...(ex. IDs)
    #x = x[which(!grepl('[0-9]',x))]
    #x = tolower(x)
    
    #x_df = as.data.frame(x,stringsAsFactors=F)
    #x_cdf = data.frame(table(x), stringsAsFactors=F) ##ddply(x_df , c('x'), nrow)
    #x_cdf$x <- as.character(x_cdf$x)
    #x_cdf = x_cdf[order(-x_cdf$Freq),]
    #write.csv(x_cdf,paste0('output/x/',format(Sys.time(), "%Y_%d_%b"),'x_output_tolower_temp.csv'),row.names=F)
    
    ##tolower.. once again
    #x_cdf[,1] = tolower(x_cdf[,1])
    #x_cdf = ddply(x_cdf , c('x'), summarize, sum(V1))
    #x_cdf = x_cdf[order(-x_cdf[,2],x_cdf[,1]),]
    
    return(x_cdf)
  }
  ##用新方法處理與輸出
  ##再挑出col去做原本的剃除
  
  jieba_x_cdf = data_ep(jieba_x)
  jieba_x_n_cdf = data_ep(jieba_x_noun)
  setDF(jieba_x_cdf)
  setDF(jieba_x_n_cdf)
  
  dir.create(paste0(".\\output\\",n,"\\after jiebar"), showWarnings = FALSE)
  dir.create(paste0(".\\output\\",n,"\\after jiebar\\",forum_name), showWarnings = FALSE)
  write.csv(jieba_x_cdf,paste0(".\\output\\",n,"\\after jiebar\\",forum_name,'/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  write.csv(jieba_x_n_cdf,paste0(".\\output\\",n,"\\after jiebar\\",forum_name,'/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba名詞',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  
  tmp = tmp[,c('company','最終比對結果')]
  tmp2 = tmp2[,1:2]
  colnames(tmp) = c('before','after')
  colnames(tmp2) = c('before','after')
  tmp3 = rbind(tmp,tmp2)
  
  if(exists('jieba_x_cdf')){
    inter_list <- intersect(jieba_x_cdf[,1],word_DB)
    x2 = jieba_x_cdf[which(jieba_x_cdf[,1] %in% inter_list),]
    
    ##Word which should be removed. 
    word_remove = read.table("應剔除字串.txt")
    word_remove = word_remove[,1]
    x2 = x2[which(!(x2[,1] %in% word_remove)),]

    write.csv(x2,paste0(".\\output\\",n,"\\raw data",'\\',forum_name,'_',recent,'_',last,'交集結果.csv'),row.names=F)
    print(paste0(forum_name, ' Chinese text segmentation and keyword extraction Completed.'))
  }
  if(exists('jieba_x_n_cdf')){
    inter_list= intersect(jieba_x_n_cdf[,1],word_DB)
    x2 = jieba_x_n_cdf[which(jieba_x_n_cdf[,1] %in% inter_list),]
    
    word_remove = read.table("應剔除字串.txt")
    word_remove = word_remove[,1]
    x2 = x2[which(!(x2[,1] %in% word_remove)),]
    
    #dir.create(, showWarnings = FALSE)
    write.csv(x2,paste0(".\\output\\",n,"\\raw data",'\\',forum_name,'_',recent,'_',last,'交集結果.csv'),row.names=F)
    print(paste0(forum_name,' Chinese text segmentation and keyword extraction Completed.'))
  }
  if(!exists('jieba_x_n_cdf') & !exists('jieba_x_cdf')){
    print("jiebar failed")
  }
  
}
