install.packages("rvest")  
library(rvest)

title=read_html("https://womany.net/interests/empowerment?ref=s-nav")   
title=html_nodes(title,"h2")   
title=html_text(title)  
title=iconv(title,"UTF-8")  
title

url=read_html("https://womany.net/interests/empowerment?ref=s-nav") 
url=html_nodes(url,"a.article-block")  
url=html_attr(url,"href")  
url

article <- data.frame(Title = title, Link = url )
article
