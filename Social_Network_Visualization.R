# Network 분석
#install.packages("RmecabKo")
#install_mecab("mecab")
#install.packages("tm")
#install.packages("KoNLP")
library(tm)
library(KoNLP)
library("RmecabKo")

# 데이터 로드

docs <- as.matrix(read.delim("./docs.txt",header=FALSE))

# 형태소 분석

for (i in 1:length(docs)){
  doc_nouns <- as.vector(unlist(nouns(as.character(docs[i]))))
  docs[i] <- as.character(paste(doc_nouns,collapse = " "))
}

#Corpus 생성
corp<-VCorpus(VectorSource(docs[,1]))

#특수문자 제거
corp <- tm_map(corp, removePunctuation)

#특정 단어 삭제
corp <- tm_map(corp, removeWords, c("짜증",))

#동의어 및 불용어 처리
for (j in seq(corp))
{
  #corp[[j]] <- gsub("kei", "한국환경정책평가연구원", corp[[j]])
}


#Document Term Matrix 생성 (단어 Length는 2로 세팅)
dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))

## 한글자 단어 제외하기 ##
colnames(dtm) = trimws(colnames(dtm))
dtm = dtm[,nchar(colnames(dtm)) > 1]
#Sparse Terms 삭제
dtm <- removeSparseTerms(dtm, as.numeric(0.997))

# DTM 내용 보기
dtm <- as.matrix(dtm)

##Remove low tf-idf col and row
#term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
#new_dtm <- dtm[,term_tfidf >= 0]

# 문서사이 유사도 계산

library(proxy)

c_mat <- as.matrix(dist(dtm, method = "cosine"))
diag(c_mat) <- 0

# 상위 40%값만 사용
thres <- quantile(c_mat,0.6)
c_mat[c_mat>thres] <- 1
c_mat[c_mat<=thres] <- 0

# 그래프 시각화
library(igraph)
g <- graph.adjacency(c_mat, mode="undirected", weighted=NULL) 

#plot(g)
# length(docs) = 7

# 노드 컬러 조정
node_col <- rep("orange",length(docs))
node_col[2] <- "green"
V(g)$color <- node_col

# 레이블 조정 
label <- V(g)$name
label[3] <- "H"
V(g)$label <- label


# 노드 크기 조정 (중심성)
V(g)$size<- degree(g)$vector*30

degree(g)
betweeness(g)
closeness(g)
eigen_centrality(g)$vector

plot(g)

