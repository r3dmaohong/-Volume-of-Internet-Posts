##jiebar main program.
##Include noun extraction
jiebar_n <- function(forum_name,x_data,recent,last){
  library(jiebaR)
  library(plyr)
  ##x��yahoo lineq ptt����..
  ##Import custom words library...
  tmp = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\__�B�z�᤽�q�W��.csv',stringsAsFactors=F)
  temp = unique(c(tmp$company,tmp$�̲פ�ﵲ�G))
  tmp2 = read.csv('D:\\abc\\wjhong\\projects\\school_performence_analysis\\�ǮզW�٥��W�ƪ���.csv',stringsAsFactors=F)
  temp = unique(c(temp,tmp2$trim���l,tmp2$������))
  word_DB = tolower(temp)
  #write.table(temp, file="D:\\abc\\wjhong\\projects\\�X�ֵ��w.txt", row.names=FALSE, col.names=FALSE)
  
  #cutter = worker(type  = "mix"�Auser  = "D:/somefile.xxx")
  #cutter = worker()
  cutter=worker("tag")
  #sapply(temp,function(x) new_user_word(cutter,x,"n"))
  for(xj in 1:length(word_DB)){
    new_user_word(cutter,word_DB[xj],"n")
  }
  
  ##Extract words which is noun.
  get_noun = function(x){
    stopifnot(inherits(x,"character"))
    index = names(res) %in% c("n","nr","nr1","nr2","nrj","nrf","ns","nsf","nt","nz","nl","ng")
    x[index]
  }
  
  jieba_x = {}
  jieba_x_noun = {}
  x_data = tolower(x_data)
  ##�G\xa2\026
  #remove all punctuation except comma[^[:alnum:],]
  #x_data = x_data[,2]
  x_data = gsub('[^[:alnum:]]','',x_data)
  
  for(i in 1:length(x_data)){
    tryCatch({
      temp = segment(x_data[i], cutter)
      res = cutter[x_data[i]]
      #get_noun(res)
      jieba_x = c(jieba_x,temp)
      jieba_x_noun = c(jieba_x_noun, get_noun(res))
      cat("\r ",forum_name," jiebar : ",i/length(x_data) * 100, '% completed',paste(replicate(50, " "), collapse = ""))
    }, error = function(e) {
      conditionMessage(e) 
    })
  }
  
  cat("\n ")
  
  
  ##Data extraction
  data_ep <- function(x){
    ##Remove words which nchar==1.
    x = x[which(nchar(x)>1)]
    ##Remove words with num...(ex. IDs)
    x = x[which(!grepl('[0-9]',x))]
    
    x_df = as.data.frame(x)
    x_cdf = ddply(x_df , c('x'), nrow)
    x_cdf = x_cdf[order(-x_cdf$V1),]
    #write.csv(x_cdf,paste0('output/x/',format(Sys.time(), "%Y_%d_%b"),'x_output_tolower_temp.csv'),row.names=F)
    
    ##tolower.. once again
    x_cdf[,1] = tolower(x_cdf[,1])
    x_cdf = ddply(x_cdf , c('x'), summarize, sum(V1))
    x_cdf = x_cdf[order(-x_cdf[,2],x_cdf[,1]),]
    
    return(x_cdf)
  }
  
  jieba_x_cdf = data_ep(jieba_x)
  jieba_x_n_cdf = data_ep(jieba_x_noun)
  
  dir.create(paste0(".\\output\\",n,"\\after jiebar"), showWarnings = FALSE)
  dir.create(paste0(".\\output\\",n,"\\after jiebar\\",forum_name), showWarnings = FALSE)
  write.csv(jieba_x_cdf,paste0(".\\output\\",n,"\\after jiebar\\",forum_name,'/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  write.csv(jieba_x_n_cdf,paste0(".\\output\\",n,"\\after jiebar\\",forum_name,'/',format(Sys.time(), "%Y_%m_%d_%H_%M_%OS"),'jieba�W��',forum_name,'_',recent,'_',last,'.csv'),row.names=F)
  
  tmp = tmp[,c('company','�̲פ�ﵲ�G')]
  tmp2 = tmp2[,1:2]
  colnames(tmp) = c('before','after')
  colnames(tmp2) = c('before','after')
  tmp3 = rbind(tmp,tmp2)
  
  if(exists('jieba_x_cdf')){
    inter_list= intersect(jieba_x_cdf[,1],word_DB)
    x2 = jieba_x_cdf[which(jieba_x_cdf[,1] %in% inter_list),]
    
    ##Word which should be removed. 
    word_remove = read.table("���簣�r��.txt")
    word_remove = word_remove[,1]
    x2 = x2[which(!(x2[,1] %in% word_remove)),]

    write.csv(x2,paste0(".\\output\\",n,"\\raw data",'\\',forum_name,'_',recent,'_',last,'�涰���G.csv'),row.names=F)
    print(paste0(forum_name,'Chinese text segmentation and keyword extraction Completed.'))
  }else if(exists('jieba_x_n_cdf')){
    inter_list= intersect(jieba_x_n_cdf[,1],word_DB)
    x2 = jieba_x_n_cdf[which(jieba_x_n_cdf[,1] %in% inter_list),]
    
    word_remove = read.table("���簣�r��.txt")
    word_remove = word_remove[,1]
    x2 = x2[which(!(x2[,1] %in% word_remove)),]
    
    #dir.create(, showWarnings = FALSE)
    write.csv(x2,paste0(".\\output\\",n,"\\raw data",'\\',forum_name,'_',recent,'_',last,'�涰���G.csv'),row.names=F)
    print(paste0(forum_name,' Chinese text segmentation and keyword extraction Completed.'))
  }else{
    print("jiebar failed")
  }
  
}