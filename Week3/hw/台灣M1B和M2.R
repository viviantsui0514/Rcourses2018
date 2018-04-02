library(readr)
m<- read_csv("C:/Users/vivian/Desktop/Rcourse2018/Week3/Monetary/EF17Y01.csv")
View(m)

str(m)
library(plotly)
library(ggplot2)
#如何在ggplot2上畫兩條線
#https://blog.gtwang.org/r/r-data-exploration-and-visualization/3/
a <- ggplot(m, aes(x = m$期間)) + geom_point(aes(y = m$`貨幣總計數-Ｍ１Ｂ-原始值`),color = "orange") + geom_point(aes(y = m $`貨幣總計數-Ｍ２-原始值`), color = "blue") + theme_bw()


ggplotly(a) %>%
  layout(
    title = "台灣M1B和M2原始值 ",
    yaxis = list(
      title = "原始值(M1:藍,M2:橘)",
      zeroline = F,
      tickprefix = F
    ),
    xaxis = list(
      title = "日期",
      zeroline = F, 
      showgrid = F
    )
  ) 
#如何在ggplot2上畫兩條不同顏色的線
#http://yjliu.net/notes/2013/01/19/ggplot2-plot-2-lines-in-same-graph.html
b <- ggplot(m, aes(x = m$期間)) + geom_line(aes(y = m$`貨幣總計數-Ｍ１Ｂ-年增率`), color = "orange") + geom_line(aes(y = m$`貨幣總計數-Ｍ２-年增率`), color = "blue") + theme_classic()

ggplotly(b) %>%
  layout(
    title = "台灣M1B與M2年增率 ",
    yaxis = list(
      title = "年增率(M1:藍,M2:橘)",
      zeroline = F,
      tickprefix = F
    ),
    xaxis = list(
      title = "日期",
      zeroline = F, 
      showgrid = F
    )
  ) 


