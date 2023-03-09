#### Text analysis in R using hierarchical clustering and LDA

#install all necessary packages

install.packages("quanteda")
install.packages("dplyr")
install.packages("cld2")
install.packages("tidyverse")
install.packages("Rcolorbrewer")
install.packages("lares")
install.packages("cluster")
install.packages("factoextra")
install.packages("dendextend")
install.packages("gt")
install.packages("ldatuning")
install.packages("topicmodels")


# load packages
library(dplyr)
library(tidyverse)
library(cld2)
library(lares)

# load original data
reviews.db <- read.csv("C:/Users/UX435E/Desktop/Business project/Airbnb_london/10 sept 22/reviews.csv")
View(reviews.db)

#overview of dataset
dim(reviews.db)
str(reviews.db)
df_str(reviews.db, return = "plot")

#### EDA #########
# reviewer-wise freq. plot of reviews
cnt_name <- head(arrange(count(reviews.db, reviewer_name), desc(n)), 10) 
ggplot(cnt_name, aes(x = reorder(reviewer_name,n), y=n)) + 
  geom_col(fill = "skyblue3")+
  coord_flip()+
  geom_text(aes(label = n), 
            hjust= -0.07,
            vjust= 0.5) +
  labs(x = "reviewer name", 
       y = "count") +
  ggtitle("Top 10 reviewers by reviews count") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"))

# listing-wise freq. plot of reviews
cnt_id <- head(arrange(count(reviews.db, listing_id), desc(n)), 10) 
ggplot(cnt_id, aes(x = reorder(listing_id,n), y=n)) + 
  geom_col(fill = "skyblue3")+
  coord_flip()+
  geom_text(aes(label = n), 
            hjust= -0.07,
            vjust= 0.5) +
  labs(x = "listing ID", 
       y = "count") +
  ggtitle("Top 10 listings by review count") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"))

#removing null values if any
colSums(is.na(reviews.db))
reviews.db <- na.omit(reviews.db)

# extracting year from the date column
reviews.db$date <- as.Date(reviews.db$date, format="%Y-%m-%d")
reviews.db$year <- format(reviews.db$date, "%Y")
reviews.db %>%
  group_by(year) %>%
  summarise(mean_var = count(reviewer_id))

reviews.db %>% freqs(year, plot = T, results = F)

# year-wise freq. plot of reviews
cnt_yr <- head(count(reviews.db, year), 15) 
ggplot(cnt_yr, aes(x = year, y=n)) + 
  geom_col(fill = "skyblue4")+
  coord_flip()+
  geom_text(aes(label = n), 
            hjust= -0.05,
            vjust= 0.5) +
  labs(x = " ",
       y = "count") +
  ggtitle("Frequency of reviews by year") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"))

# Detecting languages of reviews
reviews.lang <- reviews.db %>%
  mutate(language = cld2::detect_language(comments))
View(reviews.lang) 

cnt_lan <- head(arrange(count(reviews.lang, language), desc(n)), 10) 
ggplot(cnt_lan, aes(x = reorder(language,n), y=n)) + 
  geom_col(fill = "skyblue4")+
  coord_flip()+
  geom_text(aes(label = n), 
            hjust= -0.07,
            vjust= 0.5) +
  labs(x = "languages code", 
       y = "count") +
  ggtitle("Top languages with most reviews") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"))

#removing all non-English reviews
reviews.en <- filter(reviews.lang, language=="en")
nrow(reviews.en)
sum(is.na(reviews.en))

#selecting relevant columns for the analysis
cleaned <- reviews.en%>%
  select(2,7,6)

#randomly selecting 1 million odd observations out of 10,70,000
cleaned <- sample_n(cleaned, 1000000)
dim(cleaned)

#save the transformed file
write.csv(cleaned, "C:/Users/UX435E/Desktop/Business project/Airbnb_london/10 sept 22/cleaned.csv")


-------------------------------------------------------------------------------------------------------------
  
  
  
library(quanteda)

#read the transformed file
reviews.cleaned <- read.csv("C:/Users/UX435E/Desktop/Business project/Airbnb_london/10 sept 22/cleaned.csv")
View(reviews.cleaned)
sum(is.na(reviews.cleaned))


##### First we will apply statistical analysis on whole data 
####  and then we will apply main analysis on the sample data 
####  due to computational limitations


#################### statistical analysis on whole dataset ###############################


# create a corpus of comments
reviews.corp1 <- corpus(
  reviews.cleaned,
  docid_field = "id",
  text_field = "comments",
  unique_docnames = TRUE)
summary(reviews.corp1)

#create token matrix and clean the data
reviews.token1 <- tokens(reviews.corp1,
                         what = "word",
                         split_hyphens = TRUE,
                         split_tags = TRUE,
                         remove_numbers = TRUE,
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_url = TRUE,
                         remove_separators = TRUE)
head(reviews.token1)

#converting all tokens to lowercase
reviews.token1 <- tokens_tolower(reviews.token1)

#joining words that can potentially cause misleading outputs
reviews.token1 <- tokens_compound(reviews.token1, pattern = phrase("not *"))
reviews.token1 <- tokens_compound(reviews.token1, pattern = phrase("very *"))
reviews.token1 <- tokens_compound(reviews.token1, pattern = phrase("highly *"))
reviews.token1 <- tokens_compound(reviews.token1, pattern = phrase("really *"))

#lemmatizing the tokens using the lexicon package
reviews.token1 <- tokens_replace(reviews.token1, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)

#removing stopwords from the tokens
reviews.token1 <- tokens_remove(reviews.token1, stopwords("english"))

sum(is.na(reviews.token1))
summary(reviews.token1)
head(reviews.token1)


#cretae dtm and cleaning it a bit more
reviews.dfm1 <- dfm(reviews.token1) 
head(reviews.dfm1)
reviews.dfm1 <- reviews.dfm1 %>% 
  dfm_tolower() %>% 
  dfm_remove(pattern = stopwords("en")) %>% 
  dfm_remove("london") %>%
  dfm_keep(min_nchar = 4)

#checking the sparsity and reducing it by trimming the DFM
sparsity(reviews.dfm1)

#trimming the DFM to reduce sparsity
reviews.dfm.trim11 <- dfm_trim(reviews.dfm1, min_termfreq = 20, termfreq_type = "count", min_docfreq = 20, docfreq_type = "count") 
# reviews.dfm.trim21 <- dfm_trim(reviews.dfm1, sparsity = 0.93)
sparsity(reviews.dfm.trim11)

#viewing the top featuers of trimmed DFM
View(cbind(topfeatures(reviews.dfm.trim11, 60)))

View(reviews.dfm.trim11)

#loading statistical packages
library(quanteda.textstats)
library(quanteda.textplots)

#wordcloud
textplot_wordcloud(
  reviews.dfm.trim11,
  random_order = FALSE,
  random_color = T,
  ordered_color = T,
  comparison = F,
  rotation = FALSE
)

#simple frequency analysis
tstat_freq <- textstat_frequency(reviews.dfm.trim11, n = 20)
head(tstat_freq, 35)

#term frequency plot by documents
reviews.dfm.trim11 %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, docfreq), y = docfreq)) +
  geom_bar(stat='identity', fill = 'skyblue3') +
  geom_text(aes(label=docfreq),vjust=0.5, hjust= -0.1)+
  coord_flip() +
  labs(x = NULL, y = "count") +
  ggtitle("Top 20 words by document frequency") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", size=11, colour = "black"))

#term frequency plot
reviews.dfm.trim11 %>% 
  textstat_frequency(n = 20) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_bar(stat='identity', fill = 'skyblue4') +
  geom_text(aes(label=frequency),vjust=0.5, hjust= -0.1)+
  coord_flip() +
  labs(x = NULL, y = "count") +
  ggtitle("Top 20 words by frequency") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", size=11, colour = "black"))


#keyness
tstat_key <- textstat_keyness(reviews.dfm.trim11, target = reviews.dfm1$year >= 2020)
head(tstat_key)
textplot_keyness(tstat_key)




#################### main analysis on sample dataset ###############################




#sampling
set.seed(801)
reviews.sam <- sample_n(reviews.cleaned, 10000)
head(reviews.sam)

# create a corpus of comments
reviews.corp <- corpus(
  reviews.sam,
  docid_field = "id",
  text_field = "comments",
  unique_docnames = TRUE)
summary(reviews.corp)

#create token matrix and clean the data
reviews.token <- tokens(reviews.corp,
                        what = "word",
                        split_hyphens = TRUE,
                        split_tags = TRUE,
                        remove_numbers = TRUE,
                        remove_punct = TRUE,
                        remove_symbols = TRUE,
                        remove_url = TRUE,
                        remove_separators = TRUE)
head(reviews.token)

#converting all tokens to lowercase
reviews.token <- tokens_tolower(reviews.token)

#joining words that can potentially cause misleading outputs
reviews.token <- tokens_compound(reviews.token, pattern = phrase("not *"))
reviews.token <- tokens_compound(reviews.token, pattern = phrase("very *"))
reviews.token <- tokens_compound(reviews.token, pattern = phrase("highly *"))
reviews.token <- tokens_compound(reviews.token, pattern = phrase("really *"))

#lemmatizing the tokens using the lexicon package
reviews.token <- tokens_replace(reviews.token, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)

#removing stopwords from the tokens
reviews.token <- tokens_remove(reviews.token, stopwords("english"))

sum(is.na(reviews.token))
summary(reviews.token)
head(reviews.token)


#cretae dtm and cleaning it a bit more
reviews.dfm <- dfm(reviews.token) 
head(reviews.dfm)
reviews.dfm <- reviews.dfm %>% 
  dfm_tolower() %>% 
  dfm_remove(pattern = stopwords("en")) %>% 
  dfm_remove("london") %>%
  dfm_keep(min_nchar = 4)

#checking the sparsity and reducing it by trimming the DFM
sparsity(reviews.dfm)
set.seed(830)
reviews.dfm.trim1 <- dfm_trim(reviews.dfm, min_termfreq = 15, termfreq_type = "count", min_docfreq = 30, docfreq_type = "count") 
# reviews.dfm.trim2 <- dfm_trim(reviews.dfm, sparsity = 0.93)
sparsity(reviews.dfm.trim1)

#viewing the top featuers of trimmed DFM
View(cbind(topfeatures(reviews.dfm.trim1, 20)))

# creating tf-idf
set.seed(842)
reviews.dfm.tfidf <- dfm_tfidf(reviews.dfm.trim1)
mat.tfidf <- convert(reviews.dfm.tfidf, to="matrix")
View(mat.tfidf)

# creating a dataframe of dfm and tf-idf
df.dfm <- convert(reviews.dfm.trim1, to="data.frame")
df.tfidf <- convert(reviews.dfm.tfidf, to="data.frame")
View(df.dfm)
df.dfm <- na.omit(df.dfm)
str(df)



############## Hierarchical clustering #############

library(factoextra)
library(cluster)
library(dendextend)

#scaling the dfm dataframe
h.df <- df.dfm %>%
  select(2:813)
s <- scale(h.df)
View(s)

# Dissimilarity matrix
d <- dist(s, method = "euclidean")
View(d)

# Hierarchical clustering using ward Linkage
hc1 <- hclust(d, method = "ward.D" )
str(hc1)

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -5)
rect.hclust(hc1, k=2, border="red")
hc2 <- agnes(d, method = "ward")
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc2, k=2, border="red")
hc2$ac

#determinig optimal number of clusters

op <- fviz_nbclust(h.df, FUN = hcut, method = "wss")
op$coordinates
dev.off() 
#or
gap_stat <- clusGap(h.df, FUN = hcut, nstart = 25, nboot = 50)
fviz_gap_stat(gap_stat)


# cutting dendrogram into 2 clusters
cluster_n <- cutree(hc1, k=2)


#visualizing the dendogram
dend_obj <- as.dendrogram(hc1)
col_dend <- color_branches(dend_obj, h = 6000, col=c("skyblue4","red4"))
plot(col_dend)
abline(h = 6000, col = 'black', lty=5)
title(main="Dendogram for n=2")

#explore clusters
cluster_n
table(cluster_n)

#add cluster to original data set
df_final <- cbind(h.df, cluster_n)%>%group_by(cluster_n)
View(df_final)

#calculate segment size in percentages
proportions <- table(df_final$cluster_n)/length(df_final$cluster_n)
percentages <- proportions*100
percentages

#Grouping the sums of all feature counts by clusters
segments <- df_final %>%
  group_by(cluster_n) %>%
  summarise_all(list(sum))
segments <- t(segments)
seg.df <- as.data.frame(segments)

#top features in cluster 1
clus1 <- seg.df %>%
  select(1)
View(clus1 )
write.csv(clus2, "C:/Users/UX435E/Desktop/Business project/Airbnb_london/10 sept 22/cluster1.csv")

#top features in cluster 2
clus2 <- seg.df %>%
  select(2)
View(clus2)
write.csv(clus2, "C:/Users/UX435E/Desktop/Business project/Airbnb_london/10 sept 22/cluster2.csv")





######### Topic modelling using LDA #############

library(topicmodels)
library(ldatuning)

reviews.top <- convert(reviews.dfm.trim1, to="topicmodels")

lda_tops <- FindTopicsNumber(
  reviews.top,
  topics = 2:20,
  metrics =  c("Arun2010", "CaoJuan2009", "Griffiths2004","Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 75),
  mc.cores = 2L,
)
numb <- as.matrix(lda_tops$Deveaud2014)
View(numb)
FindTopicsNumber_plot(lda_tops)

#LDA for topics 
set.seed(21)
ldm <- LDA(reviews.top, k=8, method='Gibbs', )
View(ldm)

summary(ldm)
terms(ldm,5)
View(topics(ldm))


r_beta <- tidy(ldm, matrix = "beta")
r_beta

top_terms <- r_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol=4) +
  scale_y_reordered() +
  labs(x = "beta score") +
  ggtitle("Top 10 attributes in each topic") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"))


#storing results of topics, terms and doc probabilities
top.df <- as.data.frame(topics(ldm))
View(top.df)
top.df$docs <- rownames(top.df)
colnames(top.df) <- c('topics','docnames')

term.df <- as.data.frame(terms(ldm, 1000))
View(term.df)
write.csv(term.df, "C:/Users/UX435E/Desktop/Business project/Airbnb_london/10 sept 22/lda_terms.csv")

#doc probabilities
topicprob <- as.data.frame(ldm@gamma) 
View(topicprob)

r_gamma <- tidy(ldm, matrix = "gamma", document_names=rownames(reviews.dfm.trim1))
r_gamma$topic %>%
  ggplot(aes(gamma, fill = topic)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol=4) +
  labs(x = "gamma score") +
  ggtitle("Probability distribution of documents in each topic") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"))

gam <- as.data.frame(r_gamma)

topsss <- gam %>%
  group_by(topic) %>%
  summarise(gamma=mean(gamma))

topsss <- topsss %>%
  ggplot(aes(x=reorder(topic, gamma),y=gamma)) +
  geom_bar(stat='identity', fill = "skyblue4") +
  geom_text(aes(label=gamma),vjust=0.5, hjust= -0.1)+
  coord_flip() +
  labs(x = "gamma score") +
  ggtitle("Document probabilities per topic") +
  theme_minimal()+
  theme(axis.text.y = element_text(face="bold", colour = "black"), plot.margin = margin(1, 1, 2, 2, "cm")) 


#heatmap for year and topics
docs = docvars(reviews.dfm.trim1)[match(rownames(reviews.top),docnames(reviews.dfm.trim1)),]
df_year = aggregate(posterior(ldm)$topics, by=list(docs), mean)
View(df_year)
rownames(df_year) = df_year$Group.1
colnames(df_year) <- c('year', 'proximity', 'host + satisfaction', "physical", 'expectation + feel', 'hospitality','location','experience','booking')
heatmap(as.matrix(df_year[-1]), Rowv = NA, Colv = NA, margins = c(8,8), cexRow = 1, cexCol=1, col = colorRampPalette(brewer.pal(8, "Blues"))(25))





############################################# End of the code ###############################################




