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

Nblog <- c()

for (i in 1:20){
  httr::GET(url   = "https://section.blog.naver.com/ajax/SearchList.nhn",
  query = list("countPerPage" = "7",
               "currentPage"  = i,
               "endDate"      = "2022-07-19",
               "keyword"      = "우울증 일기",
               "orderBy"      = "sim",
               "startDate"    = "2021-02-23",
               "type"         = "post"),
  add_headers("referer" = "https://section.blog.naver.com/Search/Post.nh")) %>% httr::content(as = "text") %>% str_remove(pattern = '\\)\\]\\}\',') %>% jsonlite::fromJSON() -> naverBlog

  data <- naverBlog$result$searchList
  Nblog <- dplyr::bind_rows(Nblog, data)
  
  cat(i, "번째 페이지 정리 완료 \n")
  Sys.sleep(time=0.2)
}

#Nblog 구조확인
dplyr::glimpse(Nblog)


#필요한 컨텐츠만 추출(내용 제외)
Nblog <- Nblog %>% dplyr::select(1, 2, 4, 6, 9) %>% 
  dplyr::rename(id      = blogId,
                no      = logNo,
                posturl = postUrl,
                title   = noTagTitle,
                name    = blogName) %>% dplyr::mutate(url        = stringr::str_glue("http://blog.naver.com/PostView.nhn?blogId={id}&logNo={no}"),contents   = NA)

dplyr::glimpse(Nblog)


#내용물 크롤링(에러 표시 코드 포함)
for(i in 1:nrow(Nblog)){
  tryCatch({
    Nblog$contents[i] <- httr::GET(url = Nblog$url[i]) %>% 
    xml2::read_html() %>% rvest::html_nodes(css = "div.se-main-container")%>% html_text(trim = TRUE)
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 0.1)
    }, 
    error = function(e) cat(' --> 에러\n'))
}


View(Nblog)


#에러 제거 파트(없으면 생략 가능)

#에러가 있는 contents
which(is.na(Nblog$contents)) -> naData

# css 다른 것 수정
for(i in naData){
  tryCatch({
    Nblog$contents[i] <- httr::GET(url = Nblog$url[i]) %>% xml2::read_html() %>% rvest::html_nodes(css = "div#postViewArea") %>% html_text(trim = TRUE)
    cat(i, "번째 블로그 글 내용 취합 완료\n")
    Sys.sleep(time = 0.2)
    }
  ,error = function(e) cat(' --> 에러\n'))
}

#비공개글 제거
na.omit(Nblog) -> Nblog


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
is.na(Nblog) %>% sum()

# 중복값 없음
duplicated(Nblog) %>% sum()

# 한 블로그 콘텐츠별 최대 최소 글자수
Nblog$contents %>% nchar() %>% range()
boxplot(Nblog$contents %>% nchar())


"""
#이상치 제거(옵션)
dplyr::filter(Nblog, nchar(contents) > 8000) %>% dplyr::select(url)
"""

#한글만 남기기
Nblog$contents <- Nblog$contents %>% stringr::str_remove_all(pattern = "[^가-힣]")

Nblog$contents[1]


#자연어처리 1단계
data.1 <- NLP4kec::r_parser_r(contentVector = Nblog$contents, language = "ko")
data.1[1]
View(data.1)


#그냥 워드클라우드만
data.1.1 <- strsplit(data.1, split=" ")
data.1.1[1]
head(table(data.1.1), 100)

data.1.1 <- unlist(data.1.1)
table(data.1.1)
length(data.1.1)

wordcount.1.1 <- head(sort(table(data.1.1), decreasing = T), 500)
wordcloud2::wordcloud2(wordcount.1.1, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")



#자연어처리 2단계 - 불용어 제거
ko.stopword <- read.table(file="한국어불용어600.txt",sep = "\t", fileEncoding = "UTF-8")
head(ko.stopword)
ko.stopword <- ko.stopword[,1] %>% as.character()


"""
#tm 패키지로 말뭉치 처리 - 실패
data.2 <- data.1 %>% tm::VectorSource() %>% tm::VCorpus()

View(data.2)
data.2.1 <- tm::tm_map(data.2, FUN = removeWords, words = c(ko.stopword))

inspect(data.2.1[1])
data.2.1
"""


#subgroup방식
data.2.2 <- data.1.1[!data.1.1 %in% ko.stopword]
data.2.2 <- unlist(data.2.2)
head(sort(table(data.2.2), decreasing = T), 200)
head(sort(table(data.1.1), decreasing = T), 200)


data.2.3 <- data.2.2[!data.2.2 %in% data.2.2[str_detect(data.2.2, '다$')]]
head(sort(table(data.2.3), decreasing = T), 100)


"""
#KoNLP
library(KoNLP)
library(Sejong)
library(RSQLite)
 #useSejongDic() - 에러
useNIAdic()
data.2.3 <- extract_noun(data.1.1)
"""


#여러 글 워드클라우드
wordcount.2.2 <- head(sort(table(data.2.2), decreasing = T), 500)
wordcloud2::wordcloud2(wordcount.2.2, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")

#여러글 워드클라우드 - 명사만
wordcount.2.3 <- head(sort(table(data.2.3), decreasing = T), 300)
wordcloud2::wordcloud2(wordcount.2.3, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")

wordcount.2.3.top <- head(sort(table(data.2.3), decreasing = T), 50)
wordcloud2::wordcloud2(wordcount.2.3.top, 
                       color = "random-light",
                       backgroundColor="black",
                       fontFamily = "NanumGothic")




