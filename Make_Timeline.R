library('XML')                 
library('RCurl')

DataFr <- data.frame()
#Временной горизонт с 2014 по 2024
for(i in 2014:2024){
  t <- c()
  s <- c()
  u <- c()
  #3 вида запросов 
  fileURL[1] <-paste0("https://search.yahoo.com/search?fr2=sa-gp-search&amp;p=%D0%B1%D0%B8%D1%82%D0%BA%D0%BE%D0%B8%D0%BD+%D0%BA+",i,"&amp;fr=sfp&amp;iscqry=")
  fileURL[2] <-paste0("https://search.yahoo.com/search?p=%D0%B1%D0%B8%D1%82%D0%BA%D0%BE%D0%B8%D0%BD+%D0%BD%D0%B0+",i,"&amp;fr2=sb-top&amp;fr=sfp")
  fileURL[3] <-paste0("https://search.yahoo.com/search?p=%D0%BA+",i,"+%D0%B1%D0%B8%D1%82%D0%BA%D0%BE%D0%B8%D0%BD+&fr2=sb-top&fr=sfp")
  for(m in 1:3){
    html <- getURL(fileURL[m])
    doc <- htmlTreeParse(html, useInternalNodes = T)
    rootNode <- xmlRoot(doc)
    #находим заголовок статьи
    title <- xpathSApply(rootNode, 
                         '//a[contains(@class, " ac-algo fz-l ac-21th lh-24")]', 
                         xmlValue)
    t <- c(t,title)
    
    #находим полную ссылку на источник, берём её из атрибута href
    url <- xpathSApply(rootNode, 
                       '//a[contains(@class, " ac-algo fz-l ac-21th lh-24")]', xmlGetAttr, 'href')
    
    u <- c(u, url)
    #находим источник новости, который виден на странице выдачи поисковика
    Source <- xpathSApply(rootNode, 
                          '//span[contains(@class, "fz-ms")]', 
                          xmlValue)
    s <- c(s,Source) 
    
    Sys.sleep(0.1);
  }
  #yahoo иногда присваивает класс fz-ms ещё 1ому тегу, внутри которого "…"
  new_s <- c()
  for(j in 1:length(s)){
    if (s[j]!="…"){
      new_s <- c(new_s,s[j])
    }
  }
  
  #объединяем полученные данные 
  DF.news <- data.frame(
    Year = i,
    Header = t,
    Source = new_s,
    Url = u,
    stringsAsFactors = F)
  
  DataFr <- rbind(DataFr, DF.news)
  Sys.sleep(0.1);
}

#сохраняем данные в .csv
file_out <- './Timeline.csv'
write.csv(DataFr, file = file_out, row.names = F)




