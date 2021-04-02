# visualization-of-site-maps
『全国遺跡地図目録』や『埋蔵文化財関係統計資料』などの資料から遺跡地図の動向を可視化するためのRコード

これは、
独立行政法人国立文化財機構 奈良文化財研究所 2021 『デジタル技術による文化財情報の記録と利活用3』奈良文化財研究所研究報告27
にて収録されている「刊行物およびGISによる遺跡地図の公開状況」における図を作成するために用いたコードです。

参照するデータについては、
奈良文化財研究所「全国遺跡報告総覧」内の「全国遺跡地図総目録」（https://sitereports.nabunken.go.jp/90060）
および
「デジタル技術による文化財情報の記録と利活用3」https://sitereports.nabunken.go.jp/90271
にて公開されているものを用いています。


#遺跡地図公開状況可視化
```{r}
#パッケージ読み込み
library(tidyverse)
library(dplyr)
```


#遺跡地図刊行状況可視化、刊行年ヒストグラム
```{r}
#データ読み込み
#奈良文化財研究所「遺跡総覧」の「全国遺跡地図総目録」より、目録.csvを読み込む
remainbook <- read.csv("https://sitereports.nabunken.go.jp/files/attach/39/39770/90060_2_%E5%85%A8%E5%9B%BD%E9%81%BA%E8%B7%A1%E5%9C%B0%E5%9B%B3%E7%B7%8F%E7%9B%AE%E9%8C%B2.csv")

class(remainbook)

year <- sub("発行年不明","", remainbook$発行年月日)

#刊行年を抽出する
year2 <- substring(year, 1, 4)

#「刊行年」のデータ型を数値にする
刊行年　<- as.numeric(year2)

#ヒストグラム作成
hist(刊行年)

```


```{r}
#遺跡地図のインターネット公開状況の推移,figure5

#パッケージ読み込み
library(ggplot2)
library(reshape2)

#データ読み込み
#文化庁文化財部記念物課 2017『埋蔵文化財関係統計資料ー平成28年度ー』における関連資料の中で、遺跡地図に関する全国の刊行・公開状況が報告されている。ここから各都道府県のインターネット公開状況の推移を表にまとめた。表には、年毎の刊行物による遺跡地図刊行数も入力している。
papernet <-  read.csv("https://sitereports.nabunken.go.jp/files/attach/40/40537/90271_2_%E3%83%87%E3%82%B8%E3%82%BF%E3%83%AB%E6%8A%80%E8%A1%93%E3%81%AB%E3%82%88%E3%82%8B%E6%96%87%E5%8C%96%E8%B2%A1%E6%83%85%E5%A0%B1%E3%81%AE%E8%A8%98%E9%8C%B2%E3%81%A8%E5%88%A9%E6%B4%BB%E7%94%A83.csv")

#インターネット公開が始まった2001年からのデータのみ抽出する
since2001year <- papernet[papernet$年 > 2000, ]

#描画
p1 <- ggplot(since2001year, aes(年))+
  geom_line(aes(y = インターネット公開開始都道府県数, color = 'インターネット公開開始'), linetype = "dashed")+
  geom_line(aes(y = 累積インターネット公開都道府県数, color = '累積インターネット公開'))+
  labs(y = "都道府県数")

#x軸の目盛を調整する
p2 <- p1 + scale_x_continuous(breaks = seq(2001, 2015, by = 4))
plot(p2)
```


#刊行物としての遺跡地図の刊行と、インターネット公開の推移,figure6
```{r}
#データ読み込み
papernet <-  read.csv("https://sitereports.nabunken.go.jp/files/attach/40/40537/90271_2_%E3%83%87%E3%82%B8%E3%82%BF%E3%83%AB%E6%8A%80%E8%A1%93%E3%81%AB%E3%82%88%E3%82%8B%E6%96%87%E5%8C%96%E8%B2%A1%E6%83%85%E5%A0%B1%E3%81%AE%E8%A8%98%E9%8C%B2%E3%81%A8%E5%88%A9%E6%B4%BB%E7%94%A83.csv")

q1 <- ggplot(papernet, aes(年))+
  geom_line(aes(y = 刊行物数, color = '刊行物数'), linetype = "dashed")+
  geom_line(aes(y = 累積インターネット公開都道府県数, color = '累積インターネット公開都道府県数'))+
  labs(y = "件数")


#描画する期間をインターネット公開時期に合わせる

#1995年からのデータのみを抽出する
since1995year <- papernet[papernet$年 > 1994,] 

#描画
q2 <- ggplot(since1995year, aes(年))+
  geom_line(aes(y = 刊行物数, color = '刊行物数'), linetype = "dashed")+
  geom_line(aes(y = 累積インターネット公開都道府県数, color = '累積インターネット公開都道府県数'))+
  labs(y = "件数")

plot(q2)
```



#発掘調査件数統計,figure3
```{r}
#データ読み込み
#文化庁文化財第二課 2020『埋蔵文化財関係統計資料-令和元年度-』を参照し、年毎の発掘調査件数をまとめた表を作成し、それを読み込む。
excavation <- read.csv("https://sitereports.nabunken.go.jp/files/attach/40/40538/90271_3_%E3%83%87%E3%82%B8%E3%82%BF%E3%83%AB%E6%8A%80%E8%A1%93%E3%81%AB%E3%82%88%E3%82%8B%E6%96%87%E5%8C%96%E8%B2%A1%E6%83%85%E5%A0%B1%E3%81%AE%E8%A8%98%E9%8C%B2%E3%81%A8%E5%88%A9%E6%B4%BB%E7%94%A83.csv")

#描画
ex_p1 <- ggplot(excavation, aes(Year))+
  geom_line(aes(y = excavation$Number.of.Rescue.Excavations, color = 'Rescue Excavations'))+
  geom_line(aes(y = excavation$Number.of.Academic.Excavations, color = 'Academic Excavations'), linetype = "dashed")+
  labs(y = "Number")


plot(ex_p1)
```
