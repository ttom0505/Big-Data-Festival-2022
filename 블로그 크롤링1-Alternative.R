install.packages("httr")
install.packages("rvest")
install.packages("jsonlite")
install.packages("tidyverse")

library(httr)
library(rvest)
library(jsonlite)
library(tidyverse)
library(stringr)


# 검색결과 크롤링

Nblog.A <- c()

for (i in 1:20){
  httr::GET(url   = "https://section.blog.naver.com/ajax/SearchList.nhn",
            query = list("countPerPage" = "7",
                         "currentPage"  = i,
                         "endDate"      = "2022-07-21",
                         "keyword"      = "일상",
                         "orderBy"      = "sim",
                         "startDate"    = "2021-02-23",
                         "type"         = "post"),
          add_headers("referer" = "https://section.blog.naver.com/Search/Post.nh")) %>% httr::content(as = "text") %>% str_remove(pattern = '\\)\\]\\}\',') %>% jsonlite::fromJSON() -> naverBlog
  
  data <- naverBlog$result$searchList
  Nblog.A <- dplyr::bind_rows(Nblog.A, data)
  
  cat(i, "번째 페이지 정리 완료 \n")
  Sys.sleep(time=0.2)
}

#Nblog 구조확인
dplyr::glimpse(Nblog.A)


#필요한 컨텐츠만 추출(내용 제외)
Nblog.A <- Nblog.A %>% dplyr::select(1, 2, 4, 6, 9) %>% 
  dplyr::rename(id      = blogId,
                no      = logNo,
                posturl = postUrl,
                title   = noTagTitle,
                name    = blogName) %>% dplyr::mutate(url        = stringr::str_glue("http://blog.naver.com/PostView.nhn?blogId={id}&logNo={no}"),contents   = NA)

dplyr::glimpse(Nblog.A)


#내용물 크롤링(에러 표시 코드 포함)
for(i in 1:nrow(Nblog.A)){
  tryCatch({
    Nblog.A$contents[i] <- httr::GET(url = Nblog.A$url[i]) %>%     xml2::read_html() %>% rvest::html_nodes(css = "div.se-main-container")%>% html_text(trim = TRUE)
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 0.1)
  }, 
  error = function(e) cat(' --> 에러\n'))
}


View(Nblog.A)


#에러 제거 파트(없으면 생략 가능)
#에러가 있는 contents
which(is.na(Nblog.A$contents)) -> naData.A

# css 다른 것 수정
for(i in naData.A){
  tryCatch({
    Nblog.A$contents[i] <- httr::GET(url = Nblog.A$url[i]) %>% xml2::read_html() %>% rvest::html_nodes(css = "div#postViewArea") %>% html_text(trim = TRUE)
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 0.2)
  }
  ,error = function(e) cat(' --> 에러\n'))
}

#비공개글 제거
na.omit(Nblog.A) -> Nblog.A


#Wordcloud용 패키지
install.packages("rJava")
install.packages("C:/Users/user/AppData/Local/R/win-library/4.2/NLP4kec_1.4.0.zip", repos=NULL)
install.packages("tm")
install.packages("RWeka")
install.packages("wordcloud2")
install.packages("RColorBrewer")

library(rJava)
library(NLP4kec)
library(tm)
library(RWeka)
library(wordcloud2)
library(RColorBrewer)

##KoNLP
rJava::.jinit()
install.packages("KoNLP", 
                 repos = "https://forkonlp.r-universe.dev",
                 dependencies = TRUE,
                 INSTALL_opts = c("--no-multiarch"))
install.packages("Sejong")
library(KoNLP)
library(Sejong)

#결측치 확인
is.na(Nblog.A) %>% sum()

# 중복값 없음
duplicated(Nblog.A) %>% sum()

# 한 블로그 콘텐츠별 최대 최소 글자수
Nblog.A$contents %>% nchar() %>% range()
boxplot(Nblog.A$contents %>% nchar())


"""
#이상치 제거(옵션)
dplyr::filter(Nblog, nchar(contents) > 8000) %>% dplyr::select(url)
"""

#한글만 남기기
Nblog.A$contents <- Nblog.A$contents %>% stringr::str_remove_all(pattern = "[^가-힣]")

Nblog.A$contents[1]


#자연어처리 1단계
data.A <- NLP4kec::r_parser_r(contentVector = Nblog.A$contents, language = "ko")
data.A[1]
View(data.A)


#그냥 워드클라우드만
data.A.1 <- strsplit(data.A, split=" ")
data.A.1[1]
head(table(data.A.1), 100)

data.A.1 <- unlist(data.A.1)
table(data.A.1)
length(data.A.1)

wordcount.A.1 <- head(sort(table(data.A.1), decreasing = T), 500)
wordcloud2::wordcloud2(wordcount.A.1, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")



#자연어처리 2단계 - 불용어 제거
ko.stopword <- read.table(file="한국어불용어600.txt",sep = "\t", fileEncoding = "UTF-8")
head(ko.stopword)
ko.stopword <- ko.stopword[,1] %>% as.character()



#subgroup방식
data.A.2 <- data.A.1[!data.A.1 %in% ko.stopword]
data.A.2 <- unlist(data.A.2)
head(sort(table(data.A.2), decreasing = T), 200)
head(sort(table(data.A.1), decreasing = T), 200)


data.A.3 <- data.A.2[!data.A.2 %in% data.A.2[str_detect(data.A.2, '다$')]]
head(sort(table(data.A.3), decreasing = T), 100)



#여러 글 워드클라우드
wordcount.A.2 <- head(sort(table(data.A.2), decreasing = T), 500)
wordcloud2::wordcloud2(wordcount.A.2, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")

#여러글 워드클라우드 - 명사만
wordcount.A.3 <- head(sort(table(data.A.3), decreasing = T), 300)
wordcloud2::wordcloud2(wordcount.A.3, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")

wordcount.A.3.top <- head(sort(table(data.A.3), decreasing = T), 50)
wordcloud2::wordcloud2(wordcount.A.3.top, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")
