##### ------------------------------------------------------ Dependencies
source("libraries.R")
source("functions.R")
source("variables.R")

##### ------------------------------------------------------ Set paths etc.
wd <- toString(getwd())
inputF <- glue("{path_prefix}input/schatsi_terms.csv")

### Load data
dataTerm <- read.csv(inputF, header = TRUE, sep=";")
names(dataTerm)[names(dataTerm) == "term.count"] <- "termCount" #TO-DO: Needs to be changed in predecessor docker

##### ------------------------------------------------------ [B] Data Preparation
# [B.001] initialize parameters and custom settings
tryCatch({ 
  customWords <- read.csv("./schatsi_custom_words.csv")
  remWords <- customWords[2]
  
  df <- read.csv("./schatsi_lda_params.csv")
  paramFreq <- df[1,3]
  paramLen <- df[2,3]
  paramk <- df[3,3]
  paramkmin <- df[4,3]
  paramkmax <- df[5,3]
  paramn <- df[6,3]
}, 
error=function(e) {
})

stop_en <- data.frame(word = stopwords::stopwords("en"), stringsAsFactors = FALSE)
stop_de <- data.frame(word = stopwords::stopwords("de"), stringsAsFactors = FALSE)

# [B.002] create two datasets of which dataSet0 will not be processed and dataSet1 will be processed
dataTerm <- subset(dataTerm, dataTerm[3] >= paramFreq)
dataSet0 <- dataTerm # basis for reference

dataSet1 <- dataTerm # data set which is wrangled
##### Very detailed dumbo comment: Stopwords library column name is "word", however 
##### input dataframe column name is "term" and therefore the stopword removal by a join  
##### will not find the column. Therefore need to rename the colnames to "term".
colnames(stop_en) <- c("term")
colnames(stop_de) <- c("term")
dataSet1 <-  dataSet1 %>% 
  anti_join(stop_en, by = c("term")) %>%
  anti_join(stop_de, by = c("term"))

dataSet1$Length <- stringr::str_count(dataSet1$term)
dataSet1.ParamLen <- subset(dataSet1, dataSet1$Length <= paramLen) # for checking what has been excluded by paramLen
dataSet1 <- subset(dataSet1, dataSet1$Length > paramLen) # filter words that are too short and therefore possible no words
dataSet1 <- dataSet1[!grepl(paste(remWords, collapse="|"), dataSet1$term),] # remove custom words
dataSet1 <- dataSet1 %>% # remove digits in term array
  filter(!grepl("[[:digit:]]", dataSet1$term))
documents <- unique(dataTerm[1])
x <- nrow(documents)

##### ------------------------------------------------------ [C] Modelling 
# [C.001] part 1: create wordclouds
# create wordcloud without any cleansing
dataSet0WC <- dataSet0[1:3]
dataSet0WC <- aggregate(dataSet0WC[3], by=list(Category=dataSet0WC$term), FUN=sum)
wordCloudPlain0 <- wordcloud2::wordcloud2(data=dataSet0WC, size=1.6, color=rep_len( c("steelblue","slategrey"), nrow(dataSet0WC)))
wordCloudPlainFile0 <- paste(wd, "wordCloudPlain0", ".html", sep ="", collpase=NULL)
htmlwidgets::saveWidget(wordCloudPlain0,wordCloudPlainFile0,selfcontained = F)

dataSet1WC <- dataSet1[1:3]
dataSet1WC <- aggregate(dataSet1WC[3], by=list(Category=dataSet1WC$term), FUN=sum)

# create wordcloud with cleansing
wordCloudPlain1 <- wordcloud2::wordcloud2(data=dataSet1WC, size=1.6, color=rep_len( c("steelblue","slategrey"), nrow(dataSet0WC)))
wordCloudPlainFile1 <- paste(wd, "wordCloudPlain1", ".html", sep ="", collpase=NULL)
htmlwidgets::saveWidget(wordCloudPlain1,wordCloudPlainFile1,selfcontained = F)

# make for every single document a single wordcloud
for (i in 1:nrow(documents)){
  x <- i
  value <- as.character(documents[x,1])
  currentValue <- dataSet1[dataSet1$filename == value,]
  dataSet1WC <- currentValue[1:3]
  dataSet1WC <- aggregate(dataSet1WC[3], by=list(Category=dataSet1WC$term), FUN=sum)
  wordCloudPlain1 <- wordcloud2::wordcloud2(data=dataSet1WC, size=1.6, color=rep_len( c("steelblue","slategrey"), nrow(dataSet0WC)))
  value <- substr(value, 1, 10)
  string <- glue("wordcloudPlain1_{value}")
  wordCloudPlainFile1 <- paste(wd, string, ".html", sep ="", collpase=NULL)
  htmlwidgets::saveWidget(wordCloudPlain1,wordCloudPlainFile1,selfcontained = F)
}

# [C.002] part 2: conduct topicmodelling with lda
# in the following a topicmodeling using lda-algorithm will be executed
# create document term matrix 
data_frame <- setNames(dataSet1, c("doc_id","term","freq"))
dtm <- udpipe::document_term_matrix(data_frame)

# derive optimized paramk using Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014 metrics
paramkdf <- data.frame(c(paramk))
paramkdf[,2] <- paramkdf
tryCatch({
  result <- ldatuning::FindTopicsNumber(
    dtm,
    topics = seq(from = paramkmin, to = paramkmax, by = 1),
    metrics = c("Griffiths2004"),
    #metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
  ) # "Arun2010", "Deveaud2014" seem to have other dependencies for docker
  
  file <- glue("{wd}SCHATSI_ML_ldaTuning.csv")
  write.table(result, file=file,sep = ";", col.names = T, row.names = F)
  ldatuning::FindTopicsNumber_plot(result)
  file <- glue("{wd}SCHATSI_ML_ldaTuning.png")
  ggsave(file, width = 16, height = 10, units = "cm")
  
  paramCaoJuan2009 <- result[which.min(result[,3]),1]
  paramArun2010 <- result[which.min(result[,4]),1]
  paramGriffiths2004 <- result[which.max(result[,2]),1]	
  paramDeveaud2014 <- result[which.max(result[,5]),1]	
  
  #paramkList <- c(paramk,paramCaoJuan2009,paramArun2010,paramGriffiths2004,paramDeveaud2014)
  paramkList <- c(paramk,paramCaoJuan2009,paramGriffiths2004)
  #paramkdf <- data.frame(c("paramk","paramCaoJuan2009","paramArun2010","paramGriffiths2004","paramDeveaud2014"))
  paramkdf <- data.frame(c("paramk","paramCaoJuan2009","paramGriffiths2004"))
  paramkdf[,2] <- paramkList
}, 
error=function(e) {
})

# conduct latent dirichlet allocation (lda) for text clustering
paramkdf <- setNames(paramkdf, c("Metric","Value"))
n <- nrow(paramkdf)

for (i in 1:n){
  r <- i
  param <- paramkdf[r,2]
  paramName <- paramkdf[r,1]
  
  ap_lda <- topicmodels::LDA(dtm, param, control = list(seed = 1234))
  ap_topics <- tidytext::tidy(ap_lda, matrix = "beta")
  
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    slice_max(beta, n = paramn) %>% 
    ungroup() %>%
    arrange(topic, -beta)
  
  ap_top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered()
  
  file <- glue("{wd}schatsi_ML_topTerms_{paramName}.png")
  ggsave(file, width = 16, height = 10, units = "cm")
  
  ap_top_terms[3] <- ap_top_terms[3] %>% dplyr::mutate_each(funs(myfun))
  file <- glue("{wd}schatsi_ML_topTerms_{paramName}.csv")
  write.table(ap_top_terms, file=file,sep = ";", col.names = T, row.names = F)
  
  ap_documents <- tidytext::tidy(ap_lda, matrix = "gamma")
  ap_documents[3] <- ap_documents[3] %>% mutate_each(funs(myfun))
  file <- glue("{wd}schatsi_ML_topicAllocation_{paramName}.csv")
  write.table(ap_documents, file=file,sep = ";", col.names = T, row.names = F)
}

selectedParam <- paramkdf[which.min(paramkdf$Value),]
param <- selectedParam[1,2]
paramName <- selectedParam[1,1]

ap_lda <- topicmodels::LDA(dtm, param, control = list(seed = 1234))
ap_topics <- tidytext::tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = paramn) %>% 
  ungroup() %>%
  arrange(topic, -beta)

topTerms <- ap_top_terms
topics <- unique(topTerms[1])
topics$count <- 9
colnames(topics) <- c("id","count")
# with all documents:
#data1 <- ap_documents
data1_nodes <- documents
colnames(data1_nodes) <- "id"
#data1 <- subset(data1, data1[3] != "0%")
#data1_nodes <- data1[1]
data1_nodes$count <- 3
data_nodes <- data1_nodes
# with top n terms per document:
topTermsDoc <- dataSet1 %>%
  group_by(filename) %>%
  slice_max(`termCount`, n = paramn) %>% 
  ungroup() %>%
  arrange(filename, -`termCount`)

topTermsDoc2 <- topTermsDoc[2]
topTermsDoc2 <- unique(topTermsDoc[2])
topTermsDoc2$count <- 1
colnames(topTermsDoc2) <- c("id", "count")
data_nodes2 <- rbind(topics, data_nodes, topTermsDoc2)

# create links list
other_links <- topTerms[1:2]
colnames(other_links) <- c("from", "to")
data_links <- topTermsDoc[1:2]
data_links <- data_links[,c(2,1)]
colnames(data_links) <- c("from", "to")
data_links2 <- unique(data_links)
data_links2 <- rbind(other_links, data_links2)

# initialize visual network
vis.nodes <- unique(data_nodes2)
vis.links <- data_links2

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- FALSE # Nodes will drop shadow
#vis.nodes$title  <- vis.nodes$media # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$size   <- vis.nodes$count # Node size
vis.nodes$borderWidth <- 2 # Node border width
vis.nodes$color.background <- c("darkred", "orange", "slategray")[nodes$quality]
vis.nodes$color.border <- "black"
#vis.nodes$color.background <- rgb(1, 33, 105, maxColorValue=255)
vis.nodes$color.highlight.background <- rgb(0, 75, 155, maxColorValue=255)
vis.nodes$color.highlight.border <- rgb(134, 188, 37, maxColorValue=255)
vis.nodes$group <- nodes$quality

vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- TRUE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow

# build visual networks
visnet <- visNetwork(vis.nodes, vis.links, width = "100%", hight = "100%")  %>% 
  visEdges(arrows = "from") %>% 
  visHierarchicalLayout()

visnet <- visLegend(visnet, main="Legend", position="right", ncol=1) 

htmlwidgets::saveWidget(visnet, 
                        file = "C:/Users/do-khac/Desktop/Neuer Ordner/lit_net.html", 
                        selfcontained = TRUE,
                        title = glue("Networkvisualization with {paramName}"))

visnet2 <- visNetwork(vis.nodes, vis.links, legend = TRUE) %>%
  visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction( navigationButtons = TRUE) %>%
  visPhysics( maxVelocity = 35)

visnet2 <- visLegend(visnet2, main="Legend", position="right", ncol=1) 

htmlwidgets::saveWidget(visnet2, 
                        file = "C:/Users/do-khac/Desktop/Neuer Ordner/lit_net.html", 
                        selfcontained = TRUE,
                        title = glue("Networkvisualization with {paramName} 2"))
