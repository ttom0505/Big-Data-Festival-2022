library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)
library(tidytext)

install.packages("stringr")
library(stringr)
library(dplyr)
library(lubridate)
library(tm)
install.packages("devtools")

library(topicmodels)
library(readr)

install.packages("multilinguer")
library(multilinguer)
#install_jdk()

library(rJava)
install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type="binary")

install.packages("remotes")
library(remotes)

remotes::install_github('haven-jeon/KoNLP', upgrade="never", INSTALL_opts=c("--no-multiarch"))
remotes::install_github('haven-jeon/KoNLP', upgrade="never", force=TRUE,INSTALL_opts=c("--no-multiarch"))

install.packages("KoNLP", 
                 repos = "https://forkonlp.r-universe.dev",
                 dependencies = TRUE,
                 INSTALL_opts = c("--no-multiarch"))

install.packages("https://cran.r-project.org/src/contrib/Archive/KoNLP/KoNLP_0.80.2.tar.gz", repos = NULL, type="source", INSTALL_opts = c('--no-lock'))
             
install.packages('RcppMeCab')
library(RcppMeCab)
library(KoNLP)
##extractNoun('고려대학교 정치외교학과') ##KoNLP 확인용
useSejongDic()
useNIADic()

# 검색결과 크롤링

Nblog.lda1 <- c()

#여기서 검색 기간과 키워드를 변경해야함
for (i in 1:50){
  httr::GET(url   = "https://section.blog.naver.com/ajax/SearchList.nhn",
            query = list("countPerPage" = "7",
                         "currentPage"  = i,
                         "endDate"      = "2022-11-23",
                         "keyword"      = "사랑",
                         "orderBy"      = "sim",
                         "startDate"    = "2021-01-01",
                         "type"         = "post"),
            add_headers("referer" = "https://section.blog.naver.com/Search/Post.nh")) %>% httr::content(as = "text") %>% str_remove(pattern = '\\)\\]\\}\',') %>% jsonlite::fromJSON() -> naverBlog
  
  data <- naverBlog$result$searchList
  Nblog.lda1 <- dplyr::bind_rows(Nblog.lda1, data)
  
  cat(i, "번째 페이지 정리 완료 \n")
  Sys.sleep(time=0.2)
}

#Nblog 구조확인 - ncol =15, nrow=7*페이지수 맞는지 확인
dplyr::glimpse(Nblog.lda1)


#필요한 컨텐츠만 추출(내용 제외) - ncol=7, nrow=7*페이지수
Nblog.lda1 <- Nblog.lda1 %>% dplyr::select(1, 2, 4, 6, 9) %>% 
  dplyr::rename(id      = domainIdOrBlogId,
                no      = logNo,
                posturl = postUrl,
                title   = noTagTitle,
                name    = blogName) %>% dplyr::mutate(url        = stringr::str_glue("http://blog.naver.com/PostView.nhn?blogId={id}&logNo={no}"),contents   = NA)

dplyr::glimpse(Nblog.lda1)


#내용물 크롤링(에러 표시 코드 포함)
for(i in 1:nrow(Nblog.lda1)){
  tryCatch({
    Nblog.lda1$contents[i] <- httr::GET(url = Nblog.lda1$url[i]) %>%     xml2::read_html() %>% rvest::html_nodes(css = "div.se-main-container")%>% html_text(trim = TRUE)
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 0.1)
  }, 
  error = function(e) cat(' --> 에러\n'))
}


##View(Nblog.lda1) ##크롤링 한거 표 형식으로 보여


#에러 제거 파트(없으면 생략 가능)
#에러가 있는 contents
which(is.na(Nblog.lda1$contents)) -> naData.lda1

# css 다른 것 수정
for(i in naData.lda1){
  tryCatch({
    Nblog.lda1$contents[i] <- httr::GET(url = Nblog.lda1$url[i]) %>% xml2::read_html() %>% rvest::html_nodes(css = "div#postViewArea") %>% html_text(trim = TRUE)
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 0.2)
  }
  ,error = function(e) cat(' --> 에러\n'))
}

#비공개글 제거
na.omit(Nblog.lda1) -> Nblog.lda1

Nblog.lda1[1,] #자연어 처리가 전혀 안된 상태

#결측치 확인
is.na(Nblog.lda1) %>% sum()

# 중복값 없음
duplicated(Nblog.lda1) %>% sum()

# 한 블로그 콘텐츠별 최대 최소 글자수
Nblog.lda1$contents %>% nchar() %>% range()
boxplot(Nblog.lda1$contents %>% nchar())

"""
#한글만 남기기
Nblog.lda1$contents <- Nblog.lda1$contents %>% stringr::str_remove_all(pattern = "[^가-힣]")


Nblog.lda1$contents[1]
"""
library(NLP4kec)
library(tm)
library(RWeka)

"""
#엑셀 처리 X
#자연어처리 1단계
data.lda1 <- NLP4kec::r_parser_r(contentVector = Nblog.lda1$contents, language = "ko")
data.lda1[1]
View(data.lda1)

data.lda1.1 <- strsplit(data.lda1, split=" ")
data.lda1.1 <- unlist(data.lda1.1)


#자연어처리 2단계 - 불용어 제거
ko.stopword <- read.table(file="한국어불용어600.txt",sep = "\t", fileEncoding = "UTF-8")
head(ko.stopword)
ko.stopword <- ko.stopword[,1] %>% as.character()


data.lda1.2 <- data.lda1.1[!data.lda1.1 %in% ko.stopword]
data.lda1.2 <- unlist(data.lda1.2)
class(data.lda1.2)
head(data.lda1.2)
head(sort(table(data.lda1.2), decreasing = T), 200)
head(sort(table(data.lda1.1), decreasing = T), 200)


data.lda1.3 <- data.lda1.2[!data.lda1.2 %in% data.lda1.2[str_detect(data.lda1.2, '다$')]]
head(sort(table(data.lda1.3), decreasing = T), 100)

"""


#엑셀로 저장
install.packages("writexl")
library(writexl)
write.csv(Nblog.lda1, file="LDA1.csv")

install.packages("readxl")
library(readxl)
normal.blog <- read.csv("LDA1.csv", header=T)
normal.blog$contents <- iconv(normal.blog$contents, "utf8", "UTF-8")

#자연어처리 2번째 방법 1단계 - 한글, 숫자, 영어, 공백만 남기기 + 제외하고 싶은 단어들 빈칸으로 대체
normal.blog$contents1 <- str_replace_all(normal.blog$contents, pattern="\r", replacement=" ") %>% str_replace_all(pattern="\t", replacement=" ") %>% 
    str_replace_all(pattern="\n", replacement=" ") %>% 
    str_replace_all(pattern="[\u3000]", replacement=" ") %>% 
    str_replace_all(pattern="[  ]{2}", replacement=" ") %>% 
    str_replace_all(pattern="[[:punct:]]", replacement=" ") %>% 
    str_replace_all(pattern="사랑", replacement="") %>% 
    str_remove_all(pattern = "[^A-Za-z0-9가-힣 ]") %>%
    str_replace_all(pattern="  ", replacement=" ")


head(normal.blog$contents1,1)

#자연어처리2 2단계 - 불용어 제거
ko.stopword <- read_lines("한국어불용어600.txt")
head(ko.stopword)
ko.stopword <- gsub("\uFEFF", "", ko.stopword)
for(i in seq_along(ko.stopword)){
  normal.blog$contents1 <- stringr::str_replace_all(normal.blog$contents1, fixed(ko.stopword), " ")
}
head(normal.blog$contents1, 2)
?seq_along

#자연어처리2 3단계 - 체언, 용언 추출
normal.blog.ko.words <- function(text){
  pos <- str_split(text, ";") ##띄어쓰기를 기주능로 한 문장을 여러 단어로 나눔
  pos1 <- paste(SimplePos22(pos))
  extracted <- str_match(pos1, "([가-힣a-zA-Z0-9]+)/[NC]")

  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}
normal.blog.ko.words("고려대학교")
class(normal.blog$contents1)

#LDA - TDM
options(mc.cores=1)
normal.Corpus <- VCorpus(VectorSource(normal.blog$contents1))
normal.tdm <- TermDocumentMatrix(normal.Corpus, control=list(tokenize=normal.blog.ko.words, removePunctuation=T, removeNumbers=T, wordLengths=c(2,10)))

nTerms(normal.tdm) #단어 수
nDocs(normal.tdm) #문서 수
head(Terms(normal.tdm))

#TDM 정제
normal.tdm <- removeSparseTerms(normal.tdm, sparse=0.99)
wordFreq <- slam::row_sums(normal.tdm)
wordFreq <- sort(wordFreq, decreasing=T)
head(wordFreq)

#DTM
dtm <- as.DocumentTermMatrix(normal.tdm)
rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals>0, ]
dtm <- dtm.new

#LDA - k = 토픽 수, 25 = 토픽별 상위 25개 단어
library(topicmodels)
lda <- LDA(dtm, k=5, control=list(seed=12345))
term <- terms(lda, 20)

term

#시각화
x <- posterior(lda)$terms
y <- data.frame(t(x[,apply(x,2,max)>0.009]))
z <- data.frame(type=paste("Topic", 1), keyword=rownames(y), posterior=y[,1])


for (i in 2:5){
  z <- rbind(z, data.frame(type=paste("Topic", i), keyword=rownames(y), posterior=y[,i]))
}

ggplot(z, aes(keyword, posterior, fill=as.factor(keyword))) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip() + facet_wrap(~type, nrow=1) +
  theme(legend.position = "none")
 