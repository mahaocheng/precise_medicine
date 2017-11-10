# TODO: Add comment
# 
# Author: lenovo
###############################################################################
library.name <- "XML"
options(repos=structure(c(CRAN="http://cran.rstudio.com/")))
if(!suppressWarnings(require(library.name, character.only = TRUE, quietly = TRUE))) {
  cat(paste(library.name, "missing, will attempt to install\n"))
  install.packages(library.name, depend = TRUE)
}
library('XML')
if(!file.exists("d://mahaocheng//voa_download")){
  dir.create("d://mahaocheng//voa_download")
}

setwd("d://mahaocheng//voa_download")
url.list<-vector()
for (i in 1 : 107) {
  url.list[i] <- paste("http://www.51voa.com/VOA_Standard_", paste(i, ".html", sep=""), sep="")
  url.html <- htmlParse(url.list[i])
  article.xpath <- "//li/a[@href]"
  article.node <- getNodeSet(url.html, article.xpath)
  article.list <-vector()
  for (i in 1:length(article.node)) {
    article.names <-as.character(xmlSApply(article.node, xmlAttrs)[[i]][1])
    if (substr(article.names, 1, 22) == "/VOA_Standard_English/") {
      article.list <- c(article.list, paste("http://www.51voa.com", article.names,sep=""))
    }
  }
  for (i in 1:length(article.list)){
    articlename <- substr(article.list[i],nchar("http://www.51voa.com/VOA_Standard_English/")+1,nchar(article.list[i])-5)
    destfile <- paste(articlename,".mp3",sep="")
    desttxt <- paste(articlename,".txt",sep="")
    mp3.url <- xmlGetAttr(getNodeSet(htmlParse(article.list[i]), "//a[@id='mp3']")[[1]], 'href')
    article.content <- as.character(xmlSApply(getNodeSet(htmlParse(article.list[i]), "//div[@id='content']/p"), xmlValue))
    write.table(article.content, file=desttxt, sep="",row.names=FALSE,col.names=FALSE)
    try(download.file(mp3.url, destfile=destfile), silent=TRUE) 
  } 
}



#download.file("http://downdb.51voa.com/201707/what-next-in-mosul.mp3", destfile="test.mp3",quiet = TRUE)
substr(article.list[i],nchar("http://www.51voa.com/VOA_Standard_English/")+1,nchar(article.list[i])-5)
download.file("http://simg.sinajs.cn/blog7style/images/common/topbar/topbar_logo.gif", 
    destfile="C://Users//lenovo//Desktop//1.gif")
download.file("http://downdb.51voa.com/201710/north-korea-prison-camps.mp3
article.list
as.character(xmlSApply(article.node, xmlAttrs)[[74]])