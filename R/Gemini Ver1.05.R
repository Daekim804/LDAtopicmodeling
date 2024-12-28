# 필요한 패키지 설치 및 로드
if(!require(NLP)) install.packages("NLP")
if(!require(tm)) install.packages("tm")
if(!require(topicmodels)) install.packages("topicmodels")
if(!require(LDAvis)) install.packages("LDAvis")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr") # 데이터 전처리
if(!require(lubridate)) install.packages("lubridate") # 시계열 분석
if(!require(pdftools)) install.packages("pdftools") #pdf텍스트 추출
if(!require(KoNLP)) install.packages("KoNLP")
if(!require(htmlwidgets)) install.packages("htmlwidgets")
library(htmlwidgets)
library(pdftools)
library(NLP)
library(tm)
library(topicmodels)
library(LDAvis)
library(ggplot2)
library(dplyr)
library(lubridate)
library(KoNLP)
library(stringi)

# KoNLP 사전 설정
useNIADic()

# 1. 논문 수집 및 형태소 분석
pdf_files <- list.files(path = "G:/내 드라이브/2. 연구 자료/박사 과정/0. 연구 파일/메타버스/메타버스 논문 분석(22.~)", pattern = "*.pdf", full.names = TRUE) # PDF 파일 경로 지정
papers <- lapply(pdf_files, pdf_text) # 모든 PDF 파일에서 텍스트 추출
papers <- lapply(papers, paste, collapse = "\n") # 페이지별 텍스트를 하나의 문자열로 결합

# 형태소 분석 (명사 추출)
parsed_papers <- lapply(papers, function(paper) {
  nouns <- extractNoun(paper)
  return(nouns)
})

# 2. 텍스트 전처리
preprocess_text <- function(nouns) {
  nouns <- gsub("\\s+", " ", nouns) # 여러 공백을 하나의 공백으로 치환
  nouns <- gsub("[[:punct:]]", "", nouns)
  nouns <- gsub("[a-zA-Z]+", "", nouns)
  nouns <- gsub("[0-9]+", "", nouns)
  nouns <- gsub("[[:cntrl:]]", "", nouns)
  nouns <- gsub("[^가-힣\\s]", "", nouns) # 한글과 공백 이외 문자 제거
  nouns <- unique(nouns)
  nouns <- nouns[nouns != ""]
  nouns <- nouns[!grepl("^[가-힣a-zA-Z]$", nouns)]
  nouns <- nouns[!grepl("^[0-9]+$", nouns)]
  return(nouns)
}

# 불용어 목록
add_stopwords <- c("교육종합연구제권", "제호", "김용훈", "학습자강옥", "원광", "대학교", "교육학과", "박사과정", "변관", "신진숙", "있다김동일", "있다박찬선", "류인혜", "느끼며주은미", "최승", "한다박찬선", "장세", "필요하다김근하", "다류인혜", "그만두었다정희정", "다이금진", "넘었다허은", "평가되었다정희정", "이재연", "부족하다김태은", "가진다강옥", "때문이다고예일유정민황이", "김상균", "있다변문경", "존재한다황경화정주연권오병", "있다유진희", "있다유진희", "있다샤넬디파수필이현정", "어렵고김지숙", "살펴보았는데표", "보면이가영한", "한정혜원종윤", "있었다홍희경", "백란이성아", "백란이성아연구는", "이인숙은", "정미숙은", "홍희경은", "이가영한송이", "하지", "하게", "들이", "필요", "였으나", "후속", "지기", "해지", "수록", "라는", "하였", "만큼", "아니다", "우리", "사람들", "때문이다", "의", "에", "가", "및", "을", "를", "은", "는", "에", "의", "가", "으로", "로", "와", "과", "에서", "에게", "부터", "까지", "에게서", "에서부터", "때", "때문", "통해", "따라", "대한", "관련", "대해", "도록", "뿐", "또한", "역시", "만", "더", "덜", "더욱", "훨씬", "너무", "매우", "아주", "정말", "많이", "적게", "모두", "전부", "전체", "일부", "각", "다른", "같은", "어떤", "이러한", "그러한", "저러한", "모든", "하나", "두", "세", "네", "다섯", "여섯", "일곱", "여덟", "아홉", "열", "경우", "것으로", "것이다", "것을", "것에", "것과", "수", "및", "등", "점", "중", "및", "것", "때", "후", "전", "내", "외", "안", "밖", "전반", "상", "하", "내적", "외적", "요소", "통한", "위해", "따른", "따라서", "결과", "본", "고", "저", "제", "권", "호", "약", "더", "전")
stopwords_all <- unique(c(stopwords_kr, add_stopwords)) # 중복 제거

# 불용어 제거 함수
remove_stopwords <- function(nouns, stopwords) {
  nouns <- nouns[!nouns %in% stopwords]
  return(nouns)
}

# 지정어/동의어 처리 함수
replace_words <- function(nouns, replacements) {
  for (i in seq_along(replacements)) {
    nouns[nouns == names(replacements)[i]] <- replacements[[i]]
  }
  return(nouns)
}

# 치환 목록
replacements <- list("메타 버스" = "메타버스", "메타-버스" = "메타버스", "메타플랫폼" = "메타버스 플랫폼") # "플랫폼" 단독 사용 빈도가 높을 경우, "메타버스 플랫폼"으로 치환 고려


# 모든 논문에 전처리 적용
cleaned_papers <- lapply(parsed_papers, preprocess_text)
cleaned_papers <- lapply(cleaned_papers, remove_stopwords, stopwords = stopwords_kr)
cleaned_papers <- lapply(cleaned_papers, replace_words, replacements = replacements)


# 3. DTM 생성
corpus <- VCorpus(VectorSource(lapply(cleaned_papers, function(paper) {
  paste(paper, collapse = " ") # 전처리된 명사 벡터를 공백으로 연결
})))

dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(2, Inf),
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stopwords = stopwords_all,
  minDocFreq = 2,
  maxDocFreq = 0.9
))

# DTM 생성 후 추가적인 희소 용어 제거
dtm_removed <- removeSparseTerms(dtm, 0.98)
dim(dtm) # 필터링 전 차원 확인
dim(dtm_removed) # 필터링 후 차원 확인
inspect(dtm_removed[1:2, 1:10])


# 4. LDA 기반 토픽 모델링 및 최적 토픽 수 결정
# 최적의 토픽 개수(k) 결정
ks <- c(2, 3, 4, 5, 6, 7) # 시도해볼 k 값의 범위
perplexity_values <- numeric(length(ks)) # 각 k 값에 대한 perplexity를 저장할 벡터

for (i in 1:length(ks)) {
  k <- ks[i]
  lda_model <- LDA(dtm_removed, k = k, control = list(seed = 1234))
  perplexity_values[i] <- perplexity(lda_model, dtm_removed)
  print(paste("k =", k, "Perplexity =", perplexity_values[i]))
}

# Perplexity 그래프 그리기
plot(ks, perplexity_values, type = "b", xlab = "Number of Topics (k)", ylab = "Perplexity", main = "Perplexity for Different k Values")

# 그래프를 보고 적절한 k 값을 선택. 예를 들어, perplexity가 급격히 감소하다가 완만해지는 지점을 선택할 수 있음
best_k <- ks[which.min(perplexity_values)] # perplexity가 가장 낮은 k 선택

# 선택된 k 값으로 최종 LDA 모델 학습
lda_model <- LDA(dtm_removed, k = best_k, control = list(seed = 1234))

# LDAvis를 이용한 시각화
lda_vis <- createJSON(
  phi = posterior(lda_model)$terms,
  theta = posterior(lda_model)$topics,
  doc.length = rowSums(as.matrix(dtm_removed)),
  vocab = colnames(dtm_removed),
  term.frequency = colSums(as.matrix(dtm_removed))
)
serVis(lda_vis)

# 토픽별 주요 단어 확인
terms(lda_model, 10) # 각 토픽별 상위 10개 단어 출력

# 문서별 토픽 분포 확인
topic_distribution <- as.data.frame(posterior(lda_model)$topics)
head(topic_distribution) # 문서별 토픽 분포 확인

