# Network �м�
#install.packages("RmecabKo")
#install_mecab("mecab")
#install.packages("tm")
#install.packages("KoNLP")
library(tm)
library(KoNLP)
library("RmecabKo")

# ������ �ε�

docs <- as.matrix(read.delim("./docs.txt",header=FALSE))

# ���¼� �м�

for (i in 1:length(docs)){
  doc_nouns <- as.vector(unlist(nouns(as.character(docs[i]))))
  docs[i] <- as.character(paste(doc_nouns,collapse = " "))
}

#Corpus ����
corp<-VCorpus(VectorSource(docs[,1]))

#Ư������ ����
corp <- tm_map(corp, removePunctuation)

#Ư�� �ܾ� ����
corp <- tm_map(corp, removeWords, c("¥��",))

#���Ǿ� �� �ҿ�� ó��
for (j in seq(corp))
{
  #corp[[j]] <- gsub("kei", "�ѱ�ȯ����å�򰡿�����", corp[[j]])
}


#Document Term Matrix ���� (�ܾ� Length�� 2�� ����)
dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

## �ѱ��� �ܾ� �����ϱ� ##
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]
#Sparse Terms ����
dtm <- removeSparseTerms(dtm, as.numeric(0.997))

# DTM ���� ����
dtm <- as.matrix(dtm)

##Remove low tf-idf col and row
#term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
#new_dtm <- dtm[,term_tfidf >= 0]

# �������� ���絵 ���

library(proxy)

c_mat <- as.matrix(dist(dtm, method = "cosine"))
diag(c_mat) <- 0

# ���� 40%���� ���
thres <- quantile(c_mat,0.6)
c_mat[c_mat>thres] <- 1
c_mat[c_mat<=thres] <- 0

# �׷��� �ð�ȭ
library(igraph)
g <- graph.adjacency(c_mat, mode="undirected", weighted=NULL) 

#plot(g)
# length(docs) = 7

# ��� �÷� ����
node_col <- rep("orange",length(docs))
node_col[2] <- "green"
V(g)$color <- node_col

# ���̺� ���� 
label <- V(g)$name
label[3] <- "H"
V(g)$label <- label


# ��� ũ�� ���� (�߽ɼ�)
V(g)$size<- degree(g)$vector*30

degree(g)
betweeness(g)
closeness(g)
eigen_centrality(g)$vector

plot(g)
