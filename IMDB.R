# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 
install.packages("ggplot2")
install.packages("readr")
install.packages("plyr")
install.packages("ggthemes")

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

# Any results you write to the current directory are saved as output.

library(plyr)
library(ggthemes)
dat <- read.csv('/Users/vrushaliwalde/Documents/2017 Spring/titanic/movie_metadata.csv', header=TRUE)
df <- as.data.frame(dat)
head(df)
names(df)


# first we use plyr to calculate the mean rating and SE for each main actor
ratingdat <- ddply(df, c("actor_1_name"), summarise,
                   M = mean(imdb_score, na.rm=T),
                   SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))

ratings<-ratingdat[which(ratingdat$N>=15),]

#to get number of rows in data
nrow(ratings)

#make actor into an ordered factor, ordering by mean rating:
ratings$actor_1_name <- factor(ratings$actor_1_name)
ratings$actor_1_name <- reorder(ratings$actor_1_name, ratings$M)


ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = actor_1_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                   y = actor_1_name, yend=actor_1_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Mean rating") + ylab("First Actor")

# then we use plyr to calculate the mean rating and SE for each director
ratingdat <- ddply(df, c("director_name"), summarise,
                   M = mean(imdb_score, na.rm=T),
                   SE = sd(imdb_score, na.rm=T)/sqrt(length(na.omit(imdb_score))),
                   N = length(na.omit(imdb_score)))
ratings<-ratingdat[which(ratingdat$N>=10 & !(ratingdat$director_name=='')),]

# make director into an ordered factor, ordering by mean rating:
ratings$director_name <- factor(ratings$director_name)
ratings$director_name <- reorder(ratings$director_name, ratings$M)

ggplot(ratings, aes(x = M, xmin = M-SE, xmax = M+SE, y = director_name )) +
  geom_point() + 
  geom_segment( aes(x = M-SE, xend = M+SE,
                    y = director_name, yend=director_name)) +
  theme(axis.text=element_text(size=8)) +
  xlab("Mean rating") + ylab("Director") 



#Keyword Analysis

movies <- read.csv('/Users/vrushaliwalde/Documents/2017 Spring/IMDB/movie_metadata.csv', header=TRUE)
### Data cleaning ####

# Remove instances which have at least one NA variable
movies <- movies[complete.cases(movies), ]


# Remove instances which are duplicated (duplicated based on title)
movies <- movies[!duplicated(movies$movie_title),]
movies <- movies[, c("movie_title", "gross", "imdb_score", "plot_keywords")]
head(movies)
# Function to remove Ã, leading and trailing whitespace from movies$movie_title
movie_title_processing <- function(str){
  str <- sub(pattern = "Ã", replacement = "", str)
  str <- sub(pattern = "^//s+|//s+$", replacement ="", str)
}
# Apply previous function
movies$movie_title <- sapply(movies$movie_title, FUN = movie_title_processing)


### Keywords Analysis ***

### Plot Keywords

movies0 <- movies[movies$plot_keywords != "", ]
keywords <- c()
i <- 1
for (ins in movies0$plot_keywords){
  kw <- strsplit(ins, "[|]")
  if (length(kw) != 0){
    for (word in kw[[1]]){
      if (!(word %in% keywords)){
        keywords[i] <- word
        i = i + 1
      }
    }
  }
}
# Create a dataframe with logical values which 
# indiacte the keywords of each movie
movies0$plot_keywords <- strsplit(as.character(movies0$plot_keywords), "[|]")
keywords_idx <- movies0[, c("movie_title", "plot_keywords")]
View(movies0)
i = 1
mat <- matrix(rep(0, (dim(movies0)[1] * length(keywords))), nrow = dim(movies0)[1])

for (word in keywords_idx$plot_keywords){
  idx <- which(keywords %in% word)
  mat[i, idx] <- 1
  i = i + 1
}
colnames(mat) <- keywords
movies_and_keywords <- data.frame(mat)



# Find how many movies belong in each keyword
sum <- rep(0, length(keywords))
for (i in 1:length(keywords)){
  sum[i] <- sum(movies_and_keywords[, i])
}

keywords_sum <- data.frame(keywords = factor(keywords), sum = sum)
keywords_sum <- keywords_sum[order(sum, decreasing = FALSE),]
keywords_sum$keywords <- factor(keywords_sum$keywords, levels = keywords_sum$keywords)
#keywords_sum <- keywords_sum[keywords_sum$sum > 39, ]
keywords_sum <- keywords_sum[(dim(keywords_sum)[1]-19):dim(keywords_sum)[1] ,]


#Number of most popular keywords
  ggplot(keywords_sum, aes(x = keywords, y = sum, fill = keywords)) + 
  geom_bar(stat = "identity", colour = "black") + 
  coord_flip() +
  labs(title = "Most popular keywords", x = "", y = "") + 
  geom_text(aes(label = sum), hjust = -0.2, vjust = 0.4) + 
  theme_few() +
  theme(legend.position = "None") +
  theme(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())
  




