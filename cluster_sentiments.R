library(cluster)
library(fpc)

########### Must run build_sentiments.R first!! #############################


tidy_dtm_grouped_clust <- select(tidy_dtm_grouped, -country, -university, -document, -year, -month, -day, -hour, -state, -region)
scaled_data <- scale(model.matrix(~ ., data = drop_na(tidy_dtm_grouped_clust))[,-1])

k_max <- length(unique(drop_na(tidy_dtm_grouped)$university)) - 1

kmeans_out <- sapply(1:20,
                     function(k){kmeans(scaled_data, k, nstart = 50, iter.max = 100)$tot.withinss})

plot_kmeans <- plot(1:20, kmeans_out
                    , type = "b"
                    , pch = 19
                    , frame = FALSE
                    , xlab="Number of clusters K"
                    , ylab="Total within-clusters sum of squares")


# Select 13 from the above graph
kmean_obj <- kmeans(scaled_data, 13, nstart = 50, iter.max = 100)
plotcluster(drop_na(tidy_dtm_grouped_clust), kmean_obj$cluster)
