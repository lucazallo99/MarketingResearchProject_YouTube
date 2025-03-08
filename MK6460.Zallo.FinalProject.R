##################FINAL PROJECT##############
YOUTUBE_DATA <- read.csv("C:/Users/lucaz/Downloads/archive (2)/Global YouTube Statistics.csv", comment.char="#")
View(YOUTUBE_DATA)

##########DATA CLEANING AND PREPROCESSING############
str(YOUTUBE_DATA)
#drop columns
YOUTUBE_DATA$Latitude <- NULL
YOUTUBE_DATA$Longitude <- NULL
YOUTUBE_DATA$rank <- NULL
YOUTUBE_DATA$Abbreviation <- NULL
YOUTUBE_DATA$created_month <- NULL
YOUTUBE_DATA$created_date <- NULL
YOUTUBE_DATA$Title <- NULL
YOUTUBE_DATA$country_rank <- NULL
YOUTUBE_DATA$channel_type <- NULL #it is redundant and might generate multicollinearity
names(YOUTUBE_DATA)

#check for NAs and clean
colSums(is.na(YOUTUBE_DATA))
YOUTUBE_DATA$subscribers[YOUTUBE_DATA$subscribers == 0] <- NA
YOUTUBE_DATA$video.views[YOUTUBE_DATA$video.views == 0] <- NA
YOUTUBE_DATA$uploads[YOUTUBE_DATA$uploads == 0] <- NA
YOUTUBE_DATA$lowest_monthly_earnings[YOUTUBE_DATA$lowest_monthly_earnings == 0] <- NA
YOUTUBE_DATA$highest_monthly_earnings[YOUTUBE_DATA$highest_monthly_earnings == 0] <- NA
YOUTUBE_DATA$lowest_yearly_earnings[YOUTUBE_DATA$highest_yearly_earnings == 0] <- NA

#turn nans and Nans into NAs
YOUTUBE_DATA[YOUTUBE_DATA == "nan" | YOUTUBE_DATA == "NaN"] <- NA

#remove NA values
YOUTUBE_DATA <- na.omit(YOUTUBE_DATA)

#there are some channels that have few uploads, I will check descriptive statistics for that column
library(psych)
describe(YOUTUBE_DATA)

#remove rows that have less than 15 uploads (it seems unlikely). Specify that in the assumption section
YOUTUBE_DATA <- YOUTUBE_DATA[YOUTUBE_DATA$uploads >= 15, ]
options(scipen = 999)

#turning some columns into categorical
YOUTUBE_DATA$category <- as.factor(YOUTUBE_DATA$category)
YOUTUBE_DATA$Country <- as.factor(YOUTUBE_DATA$Country)
YOUTUBE_DATA$created_year <- as.factor(YOUTUBE_DATA$created_year)
# Rename the column
colnames(YOUTUBE_DATA)[colnames(YOUTUBE_DATA) == "Gross.tertiary.education.enrollment...."] <- "gross.tertiary.education.enrollment"

#turning the % into a number
YOUTUBE_DATA$gross.tertiary.education.enrollment <- YOUTUBE_DATA$gross.tertiary.education.enrollment / 100
YOUTUBE_DATA$Unemployment.rate <- YOUTUBE_DATA$Unemployment.rate / 100

#################EDA###########
names(YOUTUBE_DATA)
dim(YOUTUBE_DATA)
str(YOUTUBE_DATA)
describe(YOUTUBE_DATA)

#some graphs
# Load required libraries
library(ggplot2)
library(dplyr)

# Plot frequency distribution of subscribers
ggplot(YOUTUBE_DATA, aes(subscribers)) +
  geom_histogram(binwidth = 5000000, fill = "skyblue", color = "black") +
  labs(title = "Frequency Distribution of Subscribers",
       x = "Subscribers",
       y = "Frequency")

# Plot frequency distribution of video views
ggplot(YOUTUBE_DATA, aes(video.views)) +
  geom_histogram(binwidth = 1e10, fill = "lightgreen", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Frequency Distribution of Video Views",
       x = "Video Views",
       y = "Frequency")

# Uploads
ggplot(YOUTUBE_DATA, aes(uploads)) +
  geom_histogram(binwidth = 500, fill = "coral", color = "black") +
  labs(title = "Frequency Distribution of Uploads",
       x = "Uploads",
       y = "Frequency")

# Video views for the last 30 days
ggplot(YOUTUBE_DATA, aes(video_views_for_the_last_30_days)) +
  geom_histogram(binwidth = 1e8, fill = "lightblue", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Frequency Distribution of Video Views for the Last 30 Days",
       x = "Video Views for Last 30 Days",
       y = "Frequency")

# Lowest monthly earnings
ggplot(YOUTUBE_DATA, aes(lowest_monthly_earnings)) +
  geom_histogram(binwidth = 100000, fill = "lightgreen", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Frequency Distribution of Lowest Monthly Earnings",
       x = "Lowest Monthly Earnings",
       y = "Frequency")

# Highest monthly earnings
ggplot(YOUTUBE_DATA, aes(highest_monthly_earnings)) +
  geom_histogram(binwidth = 1000000, fill = "lightpink", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Frequency Distribution of Highest Monthly Earnings",
       x = "Highest Monthly Earnings",
       y = "Frequency")

# Lowest yearly earnings
ggplot(YOUTUBE_DATA, aes(lowest_yearly_earnings)) +
  geom_histogram(binwidth = 1000000, fill = "lightyellow", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Frequency Distribution of Lowest Yearly Earnings",
       x = "Lowest Yearly Earnings",
       y = "Frequency")

# Highest yearly earnings
ggplot(YOUTUBE_DATA, aes(highest_yearly_earnings)) +
  geom_histogram(binwidth = 1e7, fill = "lightgrey", color = "black") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Frequency Distribution of Highest Yearly Earnings",
       x = "Highest Yearly Earnings",
       y = "Frequency")

# Cross-tabulation of category and channel type
cross_tab <- table(YOUTUBE_DATA$category, YOUTUBE_DATA$Country)
cross_tab_df <- as.data.frame(cross_tab)
names(cross_tab_df) <- c("Category", "Country", "Frequency")

# Plot cross-tabulation
ggplot(cross_tab_df, aes(x = Category, y = Country, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Cross-tabulation of Category and Country",
       x = "Category",
       y = "Country",
       fill = "Frequency")

#created year vs video views
ggplot(YOUTUBE_DATA, aes(x = created_year, y = video.views)) +
  geom_point() +
  labs(title = "Relationship between Created Year and Views",
       x = "Created Year",
       y = "Views")

#created year vs subscribers
ggplot(YOUTUBE_DATA, aes(x = created_year, y = subscribers)) +
  geom_point() +
  labs(title = "Relationship between Created Year and Subscribers",
       x = "Created Year",
       y = "Subscribers")

#Correlation Matrix
# Select only numerical variables
numerical_data <- YOUTUBE_DATA[, sapply(YOUTUBE_DATA, is.numeric)]

# Calculate the correlation matrix for numerical variables
correlation_matrix <- cor(numerical_data)
library(corrplot)
corrplot(correlation_matrix, method = "pie", type = "lower", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

#we can see that there are several highly correlated variables, especially for what concerns earnings, consider using only yearly for logistic regression and such

#############CLUSTER ANALYSIS############
#Find the best number of clusters using NbClust, which provides multiple indices for determining the number of clusters 
str(YOUTUBE_DATA)
library(cluster) 
library(NbClust) 
library(factoextra)

#create a subset not including socioeconomics variables, then I will add them in another dataframe and see if there are differences in clusters
YOUTUBE_PERF_DATA <- YOUTUBE_DATA[, c("subscribers", "video.views", "uploads", "video_views_for_the_last_30_days",
                                   "lowest_monthly_earnings", "highest_monthly_earnings",
                                   "lowest_yearly_earnings", "highest_yearly_earnings")]

YOUTUBE_DATA2<- as.data.frame(scale(YOUTUBE_PERF_DATA))
NBC <-NbClust(YOUTUBE_DATA2, min.nc=2, max.nc=15, method="kmeans")
#According to the majority rule, the best number of clusters is  2 

#With the idea of 2 clusters, we run kmeans
set.seed(1234)
KM <- kmeans(YOUTUBE_DATA2,2,nstart=25)

#It is recommended to have a relative large nstart number such as 25 or 50 for initial random centroids
table(KM$cluster)

#cluster centers for each variable in each cluster
KM$centers

#merge cluster ID to the original dataset
YOUTUBE_DATA3 <-as.data.frame(cbind(YOUTUBE_DATA2, KM$cluster))
colnames(YOUTUBE_DATA3)[9]<-"clusters"

#Group means by each cluster. Then you can do further analysis based on groups
aggregate(YOUTUBE_DATA3[1:9], by=list(YOUTUBE_DATA3$clusters), FUN=mean)


#With another package, we can visually show clusters
set.seed(1234)
KM.RES <- eclust(YOUTUBE_DATA2, "kmeans", k = 2, nstart = 25, graph = FALSE)

table(KM.RES$cluster)
fviz_cluster(KM.RES, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

#Not really good 
#I will try with the socioeconomic vars as well
YT_NUM_DATA <- as.data.frame(scale(numerical_data)) 
NBC <-NbClust(YT_NUM_DATA, min.nc=2, max.nc=15, method="kmeans")
#According to the majority rule, the best number of clusters is  3 

#With the idea of 3 clusters, we run kmeans
set.seed(1234)
KM <- kmeans(YT_NUM_DATA,3,nstart=25)
table(KM$cluster)
KM$centers
YT_NUM_DATA2 <-as.data.frame(cbind(YT_NUM_DATA, KM$cluster))
colnames(YT_NUM_DATA2)[16]<-"clusters"
aggregate(YT_NUM_DATA2[1:16], by=list(YT_NUM_DATA2$clusters), FUN=mean)
set.seed(1234)
KM.RES <- eclust(YT_NUM_DATA, "kmeans", k = 3, nstart = 25, graph = FALSE)

table(KM.RES$cluster)
fviz_cluster(KM.RES, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())
#Seems a better model, therefore I will run cluster analysis with socioeconomic variables

##########HIERARCHICAL CLUSTERING##########
#Find the best number of clusters using NbClust, which provides multiple indices for determining the number of clusters. We first try the “complete” linkage approach.
numComplete <- NbClust(YT_NUM_DATA, distance="euclidean", min.nc=2, max.nc=10,
                       method="complete", index="all")
#According to the majority rule, the best number of clusters is  4
names(numComplete)
#See all the indices
numComplete$Best.nc
#We calculate distance matrix
dis <- dist(YT_NUM_DATA, method="euclidean")
#We conduct Hierarchical clustering using "hclust". Remeber we have different linkage options
HC <- hclust(dis, method="complete")
plot(HC, hang=-1,labels=FALSE, main="Complete-Linkage")
#We cut the tree into 4 clusters, but it looks very unbalanced
COMP <- cutree(HC, 4)
table(COMP)

#We will now try Ward’s linkage
NBC1<-NbClust(YT_NUM_DATA, distance="euclidean", min.nc=2, max.nc=10,
              method="ward.D2", index="all")
#According to the majority rule, the best number of clusters is 4
HCWARD <- hclust(dis, method="ward.D2")
plot(HCWARD, labels=FALSE, main="Ward's-Linkage")
WARD <- cutree(HCWARD, 4)
table(WARD)
#The 4-cluster solution seems to more equally segment the subjects
#Check the cross table of the two solutions
table(COMP, WARD)

#single linkage
NBC2<-NbClust(YT_NUM_DATA, distance="euclidean", min.nc=2, max.nc=10,
              method="single", index="all")
#According to the majority rule, the best number of clusters is  2 
HCSINGLE <- hclust(dis, method="single")
plot(HCSINGLE, labels=FALSE, main="Single Linkage")
SINGLE <- cutree(HCSINGLE, 2)
table(SINGLE) #terrible


#Next step: how do we identify what the differences are for the interpretation
#A good way to compare is to use the aggregate() summarizing on a statistic such as the mean or median

#We check group means by each cluster
aggregate(YT_NUM_DATA,list(COMP),mean)
aggregate(YT_NUM_DATA,list(WARD),mean)

#merge cluster ID to the original dataset
YT_NUM_DATA3<-as.data.frame(cbind(YT_NUM_DATA2, COMP, WARD))

table(YT_NUM_DATA3$clusters, YT_NUM_DATA3$COMP,YT_NUM_DATA3$WARD)
library(clValid)

CLMETHODS <- c("hierarchical","kmeans","pam")
intern <- clValid(YT_NUM_DATA, nClust = 2:6,
                  clMethods = CLMETHODS, validation = "internal", )

#“Pam” is another method that is good for clustering mixed data (both continuous and categorical variables)


# Summary, it shows that Hierarchical" may be the better choice, but I believe 3 clusters could better describe the data
summary(intern)
str(YT_NUM_DATA3)
# Visualize clusters from hierarchical clustering (Complete Linkage)
library(factoextra)
data_to_plot <- YT_NUM_DATA3[, !(names(YT_NUM_DATA3) %in% c("clusters", "WARD"))]
dev.off()

# Visualize clusters for COMP
fviz_cluster(list(data = data_to_plot, cluster = YT_NUM_DATA3$COMP), 
             geom = "point", ggtheme = theme_minimal(),
             main = "Clusters Visualized by COMP")
data_to_plot1 <- YT_NUM_DATA3[, !(names(YT_NUM_DATA3) %in% c("clusters", "COMP"))]

# Visualize clusters for WARD
fviz_cluster(list(data = data_to_plot, cluster = YT_NUM_DATA3$WARD), 
             geom = "point", ggtheme = theme_minimal(),
             main = "Clusters Visualized by WARD")


#############WITH MIXED DATA############
str(YOUTUBE_DATA)
NUM_VARS <- c("subscribers", "video.views", "uploads", "video_views_rank", "channel_type_rank", 
              "video_views_for_the_last_30_days", "lowest_monthly_earnings", "highest_monthly_earnings",
              "lowest_yearly_earnings", "highest_yearly_earnings", "subscribers_for_last_30_days", 
              "gross.tertiary.education.enrollment", "Population", "Unemployment.rate", "Urban_population")
CAT_VARS <- c("category", "Country", "created_year")
selected_variables <- c(NUM_VARS, CAT_VARS)


subset_data <- YOUTUBE_DATA[selected_variables]
dummy_data <- model.matrix(~ . - 1, data = subset_data[, CAT_VARS])
final_data <- cbind(subset_data[, NUM_VARS], dummy_data)
scaled_data <- scale(final_data)
scaled_data <- as.data.frame(scaled_data)

disMat = daisy(scaled_data, metric="gower")

#find the best number of clusters using Silhouette approach: a measure to estimate the dissimilarity between clusters (2:10 are the range of search for proper number of clusters that we decide)
sil_width <- c(NA)
for(i in 2:10){  
  pam_fit <- pam(disMat, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

set.seed(123)
pamFit <- pam(disMat, k=3)
table(pamFit$clustering)
library(compareGroups)
YOUTUBE_MIXED_DATA_CLUSTERED <-as.data.frame(cbind(scaled_data, pamFit$clustering))
dim(YOUTUBE_MIXED_DATA_CLUSTERED)
colnames(YOUTUBE_MIXED_DATA_CLUSTERED)[20]<-"categoryFilm&Animation"
colnames(YOUTUBE_MIXED_DATA_CLUSTERED)[90]<-"clusters"

group <- compareGroups(clusters~., data=YOUTUBE_MIXED_DATA_CLUSTERED)
#We can check the group means and standard deviations for each cluster on each variable
createTable(group, digits=3, show.p.overall = FALSE)
str(YOUTUBE_MIXED_DATA_CLUSTERED)

p1 <- aggregate(. ~ clusters, data = YOUTUBE_MIXED_DATA_CLUSTERED, FUN = mean)
p1
str(p1)
library(reshape)
p1.long <- melt(p1, id="clusters")
library(ggplot2)
numerical_vars <- p1[, 2:grep("Urban_population", names(p1))]
numerical_vars$clusters <- p1$clusters
numerical_melted <- reshape2::melt(numerical_vars, id.vars = "clusters")
numerical_plot <- ggplot(numerical_melted, aes(x = variable, y = value, fill = as.factor(clusters))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free", labeller = label_wrap_gen(width = 10)) +
  labs(x = "Variable", y = "Mean Percentage", fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
print(numerical_plot)
library(tidyr)

# Select the categorical variables
categorical_vars <- p1[, 17:grep("created_year2022", names(p1))]
str(categorical_vars)
categorical_vars$clusters <- p1$clusters

library(reshape2)
library(ggplot2)


#Ask the professor for visualization suggestions

##################PCA####################
library(FactoMineR)
library(factoextra)
str(YOUTUBE_DATA)

youtube_pca_data <- YOUTUBE_DATA[, c("Youtuber", "subscribers", "video.views", "uploads", 
                                     "video_views_for_the_last_30_days", 
                                     "lowest_monthly_earnings", "highest_monthly_earnings", 
                                     "lowest_yearly_earnings", "highest_yearly_earnings", 
                                     "subscribers_for_last_30_days", 
                                     "gross.tertiary.education.enrollment", 
                                     "Population", "Unemployment.rate", "Urban_population")]
cor_matrix <- cor(youtube_pca_data)
cor_matrix
# Assuming 'video_views_for_the_last_30_days' as the representative for all views and earnings related data
youtube_pca_data_revised <- youtube_pca_data[c("subscribers", "uploads", "video_views_for_the_last_30_days", 
                                               "subscribers_for_last_30_days", "gross.tertiary.education.enrollment",
                                               "Population", "Unemployment.rate", "Urban_population")]
dim(youtube_pca_data_revised)
w.pca <- PCA(youtube_pca_data_revised, scale.unit = TRUE,  graph = TRUE)
#Show output of the function PCA
print(w.pca)

#Get eigenvalues which indicates the variance of the variables explained by each component
eig.val <- get_eigenvalue(w.pca)
eig.val
#Dim.1: The first dimension explains about 33.13% of the variance in the data. Its eigenvalue is the highest among all components, indicating it is the most significant single component.
#Dim.2: explains 24.63% of the variance in the data. 
#Dim.3 etcc

#Graph the eigenvalues 
fviz_eig(w.pca, addlabels = TRUE, ylim = c(0, 40))

#factoextra package has the list of matrices containing all the results for the active variables
var <- get_pca_var(w.pca)
var

# Coordinates. We can see them on the PCA graph produced above.
head(var$coord)

# Correlations between input variables and components
head(var$cor)

# Cos2: variables' representation on the factor map
# we can see that cos2=cor x cor
head(var$cos2)

# Variables' Contributions to the principal components (in percentage)
# We can see that contrib is the percentage of each variable's cos2's in the sum of all cos2s for each dimension
head(var$contrib)

#Plot variables' positions on the maps formed dimensions

#Dim 1 and 2
fviz_pca_var(w.pca, col.var = "red", axes=1:2)

#Dim 2 and 3
fviz_pca_var(w.pca, col.var = "red", axes=2:3)

#Dim 1 and 3
fviz_pca_var(w.pca, col.var = "red", axes=c(1,3))

#Dim 1 and 4
fviz_pca_var(w.pca, col.var = "red", axes = c(1, 4))

#Dim 3 and 4
fviz_pca_var(w.pca, col.var = "red", axes = c(3, 4))

#Dim 2 and 4
fviz_pca_var(w.pca, col.var = "red", axes = c(2, 4))


#In these plots, variables with positive correlations tend to position together
#Variables with negative correlations tend to position on opposite sides of the origin
#The distance between the variable (the arrow heads) and the origin represents the quality of variables on the map
#If the arrow head rests on the circle, that means the variables are fulled accounted for by the two components

#Visualize cos2
library(corrplot)
corrplot(var$cos2, is.corr=FALSE)
#We can see that health care and arts are very high on Dim.1

#Barplot showing Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(w.pca, choice = "var", axes = 1:2)
fviz_cos2(w.pca, choice = "var", axes= c(2,3))
fviz_cos2(w.pca, choice = "var", axes= c(3,4))
#A high cos2 indicates a good representation of the variable on the principal component
#For a given variable, the sum of the cos2 on all the principal components is equal to one
# Color by cos2 values: the variables that are related to the selected components (Dim.1 and Dim.2 in this case)
# Plot for Dimension 1 and 2
fviz_pca_var(w.pca, col.var = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE,
             axes = c(1, 2))

# Plot for Dimension 2 and 3
fviz_pca_var(w.pca, col.var = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE,
             axes = c(2, 3))

# Plot for Dimension 3 and 4
fviz_pca_var(w.pca, col.var = "cos2",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE,
             axes = c(3, 4))


#dimdesc() function shows most significantly associated variables with a given principal component
w.desc <- dimdesc(w.pca, axes = c(1:4), proba = 0.05)

# Highest associated variables to PC1
w.desc$Dim.1
w.desc$Dim.2
w.desc$Dim.3
w.desc$Dim.4


#ask the professor if there are any additional steps to be done

################MBA##############
#there is some redundancy between views and subscribers as they are highly correlated. I will use subscribers as it is an important metric for Youtubers
#breaks <- quantile(YOUTUBE_DATA$video.views, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
#YOUTUBE_DATA$View.Cat <- cut(YOUTUBE_DATA$video.views, breaks = breaks,
                                  #labels = c("Low", "Medium", "High"), include.lowest = TRUE)

breaks1 <- quantile(YOUTUBE_DATA$subscribers, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
YOUTUBE_DATA$Subs.Cat <- cut(YOUTUBE_DATA$subscribers, breaks = breaks1,
                             labels = c("Low", "Medium", "High"), include.lowest = TRUE)
summary(YOUTUBE_DATA$View.Cat)
summary(YOUTUBE_DATA$Subs.Cat)
MBA_data <- YOUTUBE_DATA[, c("category", "Country", "created_year", "Subs.Cat")]
str(MBA_data)
dim(MBA_data)

#Distribution of Videos by Category
ggplot(MBA_data, aes(x = category)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 
  labs(title = "Distribution of Videos by Category",
       x = "Category",
       y = "Count")
#Distribution of Videos by Country
ggplot(MBA_data, aes(x = Country)) +
  geom_bar(fill = "lightgreen", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) + 
  labs(title = "Distribution of Videos by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Distribution of Videos by Created Year
ggplot(MBA_data, aes(x = created_year)) +
  geom_bar(fill = "lightcoral", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Videos by Created Year",
       x = "Created Year",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Distribution of Videos by Subscription Category
ggplot(MBA_data, aes(x = Subs.Cat)) +
  geom_bar(fill = "lightpink", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Videos by Subscription Category",
       x = "Subscription Category",
       y = "Count")

library(arules)
library(arulesViz)

# Convert the data into transactions
transactions <- as(MBA_data, "transactions")

# Generate association rules using the Apriori algorithm
DATA.BASKET <- apriori(transactions, parameter = list(supp = 0.09, conf = 0.4, target = "rules"))
summary(DATA.BASKET)
#Try with different supp and conf values
DATA.BASKET1 <- apriori(transactions, parameter=list(supp=0.05, conf=0.3, target="rules"))
DATA.BASKET2 <- apriori(transactions, parameter=list(supp=0.03, conf=0.3, target="rules"))
DATA.BASKET3 <- apriori(transactions, parameter=list(supp=0.08, conf=0.3, target="rules"))

#Or you can sort it
inspect(DATA.BASKET)
inspect(sort(DATA.BASKET1, by = "lift")[1:5])
inspect(sort(DATA.BASKET2, by = "lift")[1:5])
inspect(sort(DATA.BASKET3, by = "lift")[1:5])
# We further visualize the rules and we can observe how the items are related in a "big picture"
# First we get the top 5 rules by "lift". You may also use "confidence" and then we plot the map
Rules_top1 <- head(DATA.BASKET1, n = 5, by = "lift")
Rules_top2 <- head(DATA.BASKET2, n = 5, by = "lift")
Rules_top3 <- head(DATA.BASKET3, n = 5, by = "lift")
inspect(Rules_top1)
inspect(Rules_top2)
inspect(Rules_top3)
plot(Rules_top1, method = "graph")
plot(Rules_top2, method = "graph")
plot(Rules_top3, method = "graph")

plot(Rules_top1, method = "grouped")
plot(Rules_top2, method = "grouped")
plot(Rules_top3, method = "grouped")
#Or make it more interpretable
plot(Rules_top1, method = "graph",  engine = "htmlwidget")
plot(Rules_top2, method = "graph",  engine = "htmlwidget")
plot(Rules_top3, method = "graph",  engine = "htmlwidget")

#Similarly, check the results by lift or confidence
options(digits=3)

#We sort the results by lift from the largest to the smallest
DATA.BASKET1 <- sort(DATA.BASKET1, by="lift", decreasing=TRUE)
inspect(DATA.BASKET1[1:5])
DATA.BASKET2 <- sort(DATA.BASKET2, by="lift", decreasing=TRUE)
inspect(DATA.BASKET2[1:5])
DATA.BASKET3 <- sort(DATA.BASKET3, by="lift", decreasing=TRUE)
inspect(DATA.BASKET3[1:5])
#We sort the results by confidence from the largest to the smallest
DATA.BASKET1 <- sort(DATA.BASKET1, by="confidence", decreasing=TRUE)
inspect(DATA.BASKET1[1:5])
DATA.BASKET2 <- sort(DATA.BASKET2, by="confidence", decreasing=TRUE)
inspect(DATA.BASKET2[1:5])
DATA.BASKET3 <- sort(DATA.BASKET3, by="confidence", decreasing=TRUE)
inspect(DATA.BASKET3[1:5])

# Combine datasets
combined_rules <- c(DATA.BASKET1, DATA.BASKET2, DATA.BASKET3)

# Sort combined rules by lift
combined_rules <- sort(combined_rules, by = "lift", decreasing = TRUE)

# Select top rules (e.g., top 10)
top_rules <- combined_rules[1:10]
# Remove duplicates
unique_top_rules <- unique(top_rules)

# Inspect the unique top rules
inspect(unique_top_rules)

##################PREDICTIVE MODELS#############
str(YOUTUBE_DATA)
correlation_matrix <- cor(numerical_data)
correlation_matrix
library(corrplot)
corrplot(correlation_matrix, method = "pie", type = "lower", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

#for logistic regression I will focus on the number of subscribers since it is an important metric for Youtubers.
percentile_65 <- quantile(YOUTUBE_DATA$subscribers, 0.65, na.rm = TRUE)
YOUTUBE_DATA$Top_35_Subscribers <- ifelse(YOUTUBE_DATA$subscribers >= percentile_65, "Yes", "No")
YOUTUBE_DATA$Top_35_Subscribers <- as.factor(YOUTUBE_DATA$Top_35_Subscribers)
YOUTUBE_DATA$Subs.Cat <- NULL
YOUTUBE_DATA$View.Cat <- NULL
str(YOUTUBE_DATA)

#building a first model
YT_LOG_DATA <- YOUTUBE_DATA[, !(names(YOUTUBE_DATA) %in% c("Youtuber", "subscribers"))]
#video.views is too good of a predictor
YT_MODEL <-  glm(Top_35_Subscribers ~ video.views + category + uploads + Urban_population + highest_yearly_earnings +
                   subscribers_for_last_30_days + created_year, data = YOUTUBE_DATA, family = binomial)

#Check the model results
library(car)
vif(YT_MODEL)
summary(YT_MODEL)
YT_LOG_DATA$pred <- predict(YT_MODEL, newdata = YT_LOG_DATA,
                    type = "response")
YT_LOG_DATA$pred
YT_LOG_DATA$pred1[YT_LOG_DATA$pred >=0.5]<-"Yes"
YT_LOG_DATA$pred1[YT_LOG_DATA$pred <0.5]<- "No"
tb1<-table(YT_LOG_DATA$pred1, YT_LOG_DATA$Top_35_Subscribers)
tb1
mean(YT_LOG_DATA$pred1 == YT_LOG_DATA$Top_35_Subscribers)
confusionMatrix(factor(YT_LOG_DATA$pred1), factor(YT_LOG_DATA$Top_35_Subscribers), positive = "Yes")
set.seed(1234)

# Determine the number of rows for the training set, I had to make the train and test more similar in size due to instances in which a level of category 
#didn't appear. I also had to use the variable Urban_population for the same reasons
train_size <- floor(0.7 * nrow(YT_LOG_DATA))
train_indices <- sample(seq_len(nrow(YT_LOG_DATA)), size = train_size)
train_data <- YT_LOG_DATA[train_indices, ]
test_data <- YT_LOG_DATA[-train_indices, ]
dim(train_data)
dim(test_data)

#Fitting the model
fit <- glm(Top_35_Subscribers ~ video.views + category + uploads + Urban_population + highest_yearly_earnings +
             subscribers_for_last_30_days + created_year, data = YOUTUBE_DATA, family = binomial)
summary(fit)
test_data$pred <- predict(fit, newdata = test_data,
                        type = "response")
test_data$pred1[test_data$pred <0.5]<- "No"
test_data$pred1[test_data$pred >=0.5]<- "Yes"
tb2<-table(test_data$pred1, test_data$Top_35_Subscribers)
tb2
mean(test_data$pred1 == test_data$Top_35_Subscribers)
library(caret)
cm <- confusionMatrix(factor(test_data$pred1), factor(test_data$Top_35_Subscribers), positive = "Yes")
cm
library(MASS)
fit <- glm(Top_35_Subscribers ~ video.views + category + uploads + Urban_population + highest_yearly_earnings +
             subscribers_for_last_30_days + created_year, data = YOUTUBE_DATA, family = binomial)
step.model <- stepAIC(fit, trace = FALSE)
summary(step.model)
test_data$pred <- predict(step.model,test_data, type="response")
test_data$pred1[test_data$pred <0.5]<-"No"
test_data$pred1[test_data$pred >=0.5]<-"Yes"
confusionMatrix(factor(test_data$pred1), factor(test_data$Top_35_Subscribers), positive = "Yes")

#Similar results. There is probably perfect separation within the model. Let's try with Decision Tree and Random Forest
###################DECISION TREE################
library(rpart)
Top35.rp <- rpart(Top_35_Subscribers ~ video.views + category + uploads + Urban_population + highest_yearly_earnings +
                    subscribers_for_last_30_days + created_year, data=train_data)
Top35.rp
printcp(Top35.rp)
plotcp(Top35.rp)
summary(Top35.rp)
plot(Top35.rp)
text(Top35.rp, all=TRUE, use.n = TRUE)
test_data$predictions <- predict(Top35.rp, test_data, type="class")
confusionMatrix(test_data$Top_35_Subscribers, test_data$predictions, positive = "Yes")

#solid model, it does not seem to need pruning

###############RANDOM FOREST###############
library(randomForest)
Top35.rf <- randomForest(Top_35_Subscribers ~ video.views + category + uploads + Urban_population + highest_yearly_earnings +
                           subscribers_for_last_30_days + created_year, data = train_data, importance = T)
Top35.rf
Top35.rf.prediction <- predict(Top35.rf, test_data)
confusionMatrix(test_data$Top_35_Subscribers, Top35.rf.prediction, positive = "Yes")
plot(Top35.rf)
importance(Top35.rf)
varImpPlot(Top35.rf)

#despite a lower accuracy, the random forest seems the better model
