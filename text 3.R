bank<-read.csv("banks_sjis.csv")
is.na(bank)#欠損値かどうか確かめる
colSums(is.na(bank))#欠損値を合計する
# Drop rows with missing values in 'author' or 'text' columns
bank <- bank[complete.cases(bank[c('author', 'text')]), ]
# Replace NaN values in 'like' column with 0
bank$like[is.na(bank$like)] <- 0
colSums(is.na(bank))
head(bank)
bank$date <- as.POSIXct(bank$date, format="%d.%m.%Y")
str(bank)
bank$date
# Assuming 'date' column is already in POSIXct format
bank$day <- format(bank$date, "%d")
bank$month <- format(bank$date, "%m")
bank$year <- format(bank$date, "%Y")
head(bank)
bank$date <- NULL
str(bank)
install.packages("plotly")
library(plotly)
str(bank)
options(repr.plot.width=16, repr.plot.height=8)
ggplot(bank, aes(x = bank, fill = bank))+
geom_bar() +
  theme_minimal() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) #x軸の名盛りを縦にする
labs(title = "Number of Reviews by Bank",
     x = "Bank",
     y = "Number of Reviews")
options(repr.plot.width=8, repr.plot.height=5)

ggplot(bank, aes(x = star, y = ..density.., fill = ..count..)) +#評価の分布図
  geom_histogram(binwidth = 1, color = "white", alpha = 0.7) +
  geom_density(alpha=0,color="lightgreen") +
  theme_minimal() +
  labs(title = "Distribution of Star Ratings",
       x = "Star Rating",
       y = "Density") +
  scale_fill_gradient("Count", low = "skyblue", high = "skyblue")

#How do star ratings vary by the month and year of the review?
# Calculate average star ratings by month and year
avg_star_ratings <- aggregate(star ~ year + month, data = bank, mean)
# Set the plot size
options(repr.plot.width=10, repr.plot.height=6)
# Define colors for each month
month_colors <- rainbow(length(unique(avg_star_ratings$month)))
# Create a ggplot with lines for each month
ggplot(avg_star_ratings, aes(x = year, y = star, group = month, color = factor(month))) +
  geom_line() +
  labs(title = "Star Ratings by Month and Year",
       x = "Year",
       y = "Average Star Rating") +
  theme_minimal() +
  scale_color_manual(name = "Month", values = month_colors) +  # Use manual color scale
  theme(legend.position = "top") +
  theme(panel.grid = element_blank())

install.packages("tidyverse")
library(tidyverse)
#What are the most common locations for reviews?
top_locations <- bank %>%
  count(location) %>%
  arrange(desc(n)) %>%
  head(10)
# Create a ggplot with a horizontal bar plot for the top locations
ggplot(top_locations, aes(x = n, y = reorder(location, n), fill = location)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Top Locations for Reviews",
       x = "Number of Reviews",
       y = "Location") +
  theme_minimal() 

#How does the number of likes vary with star ratings?
ggplot(bank, aes(x = star, y = like, color = factor(star))) +
  geom_point(size = 3) +
  labs(title = "Likes vs. Star Ratings",
       x = "Star Rating",
       y = "Number of Likes") +
  theme_minimal()

#Who are the top authors by the number of reviews they've written?
# Create a ggplot with a bar plot for the top authors
top_authors <- bank %>%
  count(author) %>%
  arrange(desc(n)) %>%
  head(10)

# Create a ggplot with a bar plot for the top authors
ggplot(top_authors, aes(x = n, y = reorder(author, n),fill =author)) +
  geom_bar(stat = "identity", color = "white") +
  labs(title = "Top Authors by Number of Reviews",
       x = "Number of Reviews",
       y = "Author") +
  theme_minimal()

# Compute the number of reviews by year
reviews_by_year <- table(bank$year)

# Set the plot size
options(repr.plot.width=10, repr.plot.height=6)

# Create a line plot for the number of reviews over the years
plot(names(reviews_by_year), reviews_by_year, type = "o", col = "salmon",
     main = "Number of Reviews Over the Years", xlab = "Year", ylab = "Number of Reviews")


top_5_star_banks <- names(sort(table(bank$bank[bank$star == 5]), decreasing = TRUE)[1:5])

library(RColorBrewer)
# Filter the data for the top 5 banks with 5-star ratings
filtered_data <- bank[bank$bank %in% top_5_star_banks, ]

# Set the plot size
options(repr.plot.width=12, repr.plot.height=6)
color_palette <- brewer.pal(5, "Set2")
# Create a ggplot with a count plot for the top 5 banks with 5-star ratings
ggplot(filtered_data, aes(x = bank, fill = factor(star))) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Top 5 Banks with 5-Star Ratings",
       x = "Bank",
       y = "Number of Star disbution Reviews") +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Star Rating"))

ggplot(bank, aes(x = factor(star), y = like)) +
  stat_summary(fun = "mean", geom = "bar", fill = brewer.pal(5,"Oranges"), position = "dodge") +
  labs(title = "Average Number of Likes by Star Rating",
       x = "Star Rating",
       y = "Average Number of Likes") +
  theme_minimal()

library(tidyverse)
library(tidytext)
library(stringr)
library(DT)
library(igraph)
library(ggraph)
library(tm)
library(topicmodels)
library(wordcloud)
library(caret)
library(ggthemes)
library(RColorBrewer)
fillColor = "#FFA07A"
  
fillColor2 = "#F1C40F"

text<-bank$text
bankauthor<-bank$author
bankname<-bank$bank
#グループごとにクチコミ数を数えて、降順で結果で出す
topbank<-bank%>%
  group_by(bank)%>%
  tally(sort = TRUE)
ggplot(head(topbank,10),aes(x=reorder(bank,n),y=n))+
         geom_bar(stat = "identity",color="white",fill=fillColor)+
  geom_text(aes(x=bank,y=1,label=paste0("(",n,")",sep="")),
            hjust=0,vjust=0,size=6,colour="black",
            fontface="bold")+
  labs(x="name",y="Count Of Sentences",title="Ten Most Achive Charcters")+
  coord_flip()+#転置
  theme_fivethirtyeight(base_size=15)

# 4 Top 15 most Common Words
bank%>%
  select(text)%>%
unnest_tokens(word,text)%>%#textデータを単語に分割し、word列として取得
  filter(!is.na(word))%>%#欠損値でない行をフィルタリング
  filter(!word %in% stop_words$word)%>%#情報量が低い単語を削除する
  count(word,sort=TRUE)%>%#単語出現回数を数えて、出現回数で降順に出す
  ungroup()%>%#データフレームをグループ化解除
  mutate(word = factor(word, levels = rev(unique(word))))%>%#factorしてユニークな単語を逆順にした順序でファクターレベルを設定
  head(15)%>%
ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
  hjust=0, vjust=.5, size = 6, colour = 'black',
  fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
  title = 'Top 15 most Common Words') +
  coord_flip() + 
  theme_fivethirtyeight(base_size = 15)  

#wordcloudを作る
keyword<-bank%>%
  unnest_tokens(word,text)%>%
  filter(!is.na(word))%>%
  filter(!word %in% stop_words$word)%>%
  count(word,sort=TRUE)%>%
  ungroup()
keyword<-filter(keyword,!(word=="2"|word=="3"|word=="5"|word=="10"))#数字を削除
head(keyword,50)%>%
with(wordcloud(word, n, max.words = 50,colors=brewer.pal(9, "Paired")))#30サイズのwordcloudを作る


#part of speech ごとにworcloudにする
glimpse(parts_of_speech)#parts_of_speechデータセット
unique(parts_of_speech$pos)#ユニックな値をとる

#形容詞wordcloud
 bank%>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  left_join(parts_of_speech) %>%
  filter(pos == "Adjective") %>%
　count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(50) %>%
   with(wordcloud(word, n, max.words =50,colors=brewer.pal(8, "Set1")))

# Verb(transitive)wordcloud
 bank%>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   left_join(parts_of_speech)%>% 
     filter(pos =="Verb (transitive)") %>%
   count(word,sort = TRUE) %>%
   ungroup()  %>%
   head(50) %>%
   with(wordcloud(word, n, max.words =50,colors=brewer.pal(8, "Dark2")))


 # Verb(intransitive)wordcloud
 bank%>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   left_join(parts_of_speech)%>% 
    filter(pos =="Verb (intransitive)") %>%
   count(word,sort = TRUE) %>%
   ungroup()  %>%
   head(50)%>% 
   with(wordcloud(word, n, max.words =50,colors=brewer.pal(8, "Dark2")))
 rm(list=ls()) 

 # Noun wordcloud
 bank%>%
   unnest_tokens(word, text) %>%
   filter(!word %in% stop_words$word) %>%
   left_join(parts_of_speech)%>% 
   filter(pos =="Noun") %>%
   count(word,sort = TRUE) %>%
   ungroup()  %>%
   head(50)%>% 
   with(wordcloud(word, n, max.words =50,colors=brewer.pal(8, "Dark2")))
 

