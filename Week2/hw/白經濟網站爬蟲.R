library(rvest)

title=read_html("https://talkecon.com/archive-post/")   
title=html_nodes(title,"#main .entry-content a")   
title=html_text(title)  
title=iconv(title,"UTF-8")  


urls=read_html("https://talkecon.com/archive-post/") 
urls=html_nodes(urls,"#main .entry-content a")  
urls=html_attr(urls,"href")  
urls
#how to adds elements to a list in a loop
#https://stackoverflow.com/questions/26508519/how-to-add-elements-to-a-list-in-r-loop
totaldata = list()
for (i in 1:2){
  content=read_html(urls[i])
  content=html_nodes(content,".entry-content")  
  content=html_text(content, trim=F)
  totaldata[i] = content
}
totaldata
# unlist() > dataframe / matrix
library(data.table)
article <- data.table(Title = title, Link = url, Content = totaldata)


