# 필요한 패키지 로드
library(KoNLP)
library(openxlsx)
library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)
library(reshape2)
library(scales)
library(ggplot2)
library(wordcloud2)
library(htmlwidgets)

# abstracts and title.csv 파일 읽기
abstracts <- read.csv("D:/LDAtopicmodeling/abstracts and title.csv", header = TRUE, stringsAsFactors = FALSE)

# KoNLP를 사용하여 명사만 추출
useNIADic()
extract_morphemes <- function(text) {
  paste(extractNoun(text), collapse = " ")
}

# 형태소 분석 적용 (content 열 사용)
result <- sapply(abstracts$content, extract_morphemes)

# 유의어 목록 불러오기
synonyms <- readLines("D:/LDAtopicmodeling/synonyms.txt", encoding = "UTF-8")

# 유의어를 매핑하는 함수 생성
create_synonym_map <- function(synonyms) {
  synonym_map <- list()
  for (line in synonyms) {
    parts <- unlist(strsplit(line, ":"))  # ':'로 구분
    key <- parts[1]  # 기준 단어
    values <- unlist(strsplit(parts[2], ","))  # 유의어 목록
    for (value in values) {
      synonym_map[[value]] <- key  # 유의어를 기준 단어로 매핑
    }
  }
  return(synonym_map)
}

# 유의어 매핑 생성
synonym_map <- create_synonym_map(synonyms)

# 텍스트에서 유의어를 대체하는 함수
replace_synonyms <- function(text, synonym_map) {
  words <- unlist(strsplit(text, " "))
  replaced_words <- sapply(words, function(word) {
    if (word %in% names(synonym_map)) {
      return(synonym_map[[word]])  # 유의어를 기준 단어로 대체
    } else {
      return(word)  # 유의어가 아니면 그대로 반환
    }
  })
  return(paste(replaced_words, collapse = " "))
}

# 형태소 분석 결과에 유의어 처리 적용
result_with_synonyms <- sapply(result, replace_synonyms, synonym_map = synonym_map)

# 불용어 목록 불러오기
stopwords <- readLines("D:/LDAtopicmodeling/stopwords.txt", encoding = "UTF-8")

# 불용어 및 한 글자 단어 제거 함수
remove_stopwords <- function(text) {
  morphemes <- unlist(strsplit(text, " "))
  filtered <- morphemes[!morphemes %in% stopwords & nchar(morphemes) > 1]
  paste(filtered, collapse = " ")
}

# 각 국문 초록에서 불용어 제거
result_final <- sapply(result_with_synonyms, remove_stopwords)

# 결과를 데이터 프레임으로 결합
comparison_df <- data.frame(
  ID = abstracts$id,
  Title = abstracts$title,
  Original = abstracts$content,
  Final = result_final
)

# 결과를 Excel 파일로 저장
write.xlsx(comparison_df, file = "D:/LDAtopicmodeling/comparison_result_with_titles.xlsx")

### 1. 주제어 빈도 분석
corpus <- Corpus(VectorSource(result_final))
tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(2, Inf)))
word_freq <- rowSums(as.matrix(tdm))

# 데이터 프레임 변환
word_freq_df <- data.frame(term = names(word_freq), freq = word_freq) %>% arrange(desc(freq))

# 빈도 막대그래프 저장
freq_plot <- ggplot(head(word_freq_df, 20), aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "상위 20개 주제어 빈도 분석", x = "단어", y = "빈도") +
  theme_minimal()
ggsave("D:/LDAtopicmodeling/word_frequency_plot.png", freq_plot, width = 10, height = 6, dpi = 300)

# 워드클라우드 생성 및 저장
wordcloud_plot <- wordcloud2(head(word_freq_df, 100), size = 0.5, color = "random-light", backgroundColor = "white")
saveWidget(wordcloud_plot, "D:/LDAtopicmodeling/wordcloud.html", selfcontained = TRUE)

### 2. TF-IDF 분석
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))

# TF-IDF 가중치 부여
tfidf_matrix <- weightTfIdf(dtm)
tfidf_values <- as.matrix(tfidf_matrix)

# 단어 기준 TF-IDF 합산
tfidf_scores <- colSums(tfidf_values)

# TF-IDF 데이터 프레임 변환
tfidf_df <- data.frame(term = names(tfidf_scores), tfidf = tfidf_scores) %>%
  arrange(desc(tfidf))

# TF-IDF 결과 저장
write.xlsx(tfidf_df, file = "D:/LDAtopicmodeling/tfidf_result.xlsx")

# TF-IDF 상위 단어 막대그래프 저장
tfidf_plot <- ggplot(head(tfidf_df, 20), aes(x = reorder(term, tfidf), y = tfidf)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "상위 20개 TF-IDF 단어 분석", x = "단어", y = "TF-IDF 값") +
  theme_minimal()
ggsave("D:/LDAtopicmodeling/tfidf_plot.png", tfidf_plot, width = 10, height = 6, dpi = 300)

### 3. LDA 토픽 모델링
lda_model <- LDA(dtm, k = 7, method = "Gibbs", control = list(seed = 1234, burnin = 1000, thin = 100, iter = 5000))

# 토픽별 상위 단어 추출
term_topic <- tidy(lda_model, matrix = "beta")

top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 토픽별 상위 단어 시각화 저장
topic_plot <- ggplot(top_term_topic, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  labs(title = "Top 10 Terms per Topic", x = "Terms", y = "Beta") +
  theme_minimal()
ggsave("D:/LDAtopicmodeling/topic_terms_plot.png", topic_plot, width = 12, height = 8, dpi = 300)

# 문서-토픽 확률 추출
doc_topics <- tidy(lda_model, matrix = "gamma")

# abstracts 데이터의 id와 title을 준비
abstracts$id <- as.character(abstracts$id)  # id를 문자형으로 변환
abstracts_titles <- abstracts$title         # title 열 추출

# doc_topics의 document를 문자형으로 변환
doc_topics$document <- as.character(doc_topics$document)

# title 열 추가: 전체 제목 리스트를 7번 반복하여 삽입
k <- 7  # 토픽 개수
doc_topics$title <- rep(abstracts_titles, times = k)

# 결과를 Excel 파일로 저장
write.xlsx(doc_topics, file = "D:/LDAtopicmodeling/doc_topics_with_titles.xlsx", overwrite = TRUE)

print("doc_topics 데이터가 title 열과 함께 doc_topics_with_titles.xlsx 파일로 저장되었습니다.")

### 4. 각 문서별 gamma 최대값 추출
# doc_topics_with_titles.xlsx 파일 읽기
doc_topics <- read.xlsx("D:/LDAtopicmodeling/doc_topics_with_titles.xlsx")

# 각 문서별로 gamma 값이 최대인 행만 남기기
doc_topics_max_gamma <- doc_topics %>%
  group_by(document) %>%
  filter(gamma == max(gamma)) %>%
  ungroup()

# 결과를 새로운 Excel 파일로 저장
write.xlsx(doc_topics_max_gamma, file = "D:/LDAtopicmodeling/doc_topics_max_gamma.xlsx", overwrite = TRUE)

print("각 문서별 gamma 최대값만 남긴 결과가 doc_topics_max_gamma.xlsx 파일로 저장되었습니다.")