library(plyr)
library(car) 
library(moments)  # For skew calculations
library(lattice)  # Use trellis graphics

imdb <- read.csv("C:/Data Analysis Kaggle/IMDB/movie_metadata.csv" , header = TRUE , stringsAsFactors = FALSE)
print("Rows before cleaning")
nrow(imdb)


imdb <- na.omit(imdb)
imdb <- unique(imdb)
print("Rows after cleaning")
nrow(imdb)

averaged_Scores <- ddply(imdb, .(director_name), summarize,  Rate1=mean(imdb_score, na.rm = TRUE))
muReview <- mean(imdb$imdb_score)
sdReview <- sd(imdb$imdb_score)
#Histogram depicting the IMDB scores
hist(averaged_Scores$Rate1,
     breaks=10,
     main="",
     xlab="IMDB Scores",
     col=colors()[25:40])

abline(v=muReview, 
       col="red", lwd=2)
title(main="IMDB Scores Distribution",
      subtitle=bquote(paste(mu, .(muReview), " ",
                       sigma, .(sdReview))))



genres_data <- ddply(imdb, .(genres), summarize,  gross= mean(gross, na.rm = TRUE) / 1000000, 
                                               budget = mean(budget, na.rm = TRUE)/ 1000000)

genres_data[ ,"Profit"] <-   genres_data$gross - genres_data$budget

genres_data <- genres_data[order(-genres_data$Profit),]

genres_data <- genres_data[1:10,]


symbols( genres_data$budget, genres_data$gross, circles=genres_data$Profit, 
         fg="white", bg=colors()[417:446], xlab="Budget in 100 milion", ylab="Gross Income in 100 milion")
title("Profit Distribution across top 10 Genres")
text(genres_data$budge, genres_data$gross, genres_data$genres, cex=0.6)








