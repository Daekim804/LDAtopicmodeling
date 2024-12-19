# 1단계: 필요한 패키지 설치 및 로드, PDF 파일 목록 가져오기

# 필요한 패키지 로드
library(pdftools)
library(dplyr)
library(stringr)
library(KoNLP)
library(tm)
library(topicmodels)
library(stopwords)
useNIADic()

# 2단계: PDF 파일 텍스트 추출 및 전처리

# PDF 파일이 있는 폴더 경로 설정
pdf_folder <- "G:/내 드라이브/2. 연구 자료/박사 과정/0. 연구 파일/메타버스/메타버스 논문 분석(22.~)"

# 폴더 내의 모든 PDF 파일 목록 가져오기
pdf_files <- list.files(pdf_folder, pattern = "*.pdf", full.names = TRUE)

# 모든 PDF 파일의 텍스트를 저장할 빈 데이터 프레임 생성
all_text <- data.frame(text = character(), stringsAsFactors = FALSE)

# 각 PDF 파일에 대해 텍스트 추출 및 전처리 반복
for (pdf_file in pdf_files) {
  pdf_text <- tryCatch({
    pdf_text(pdf_file)
  }, error = function(e) {
    message(paste("Error reading PDF:", pdf_file))
    return(NULL)
  })

  if (is.null(pdf_text)) next

  pdf_text_combined <- paste(pdf_text, collapse = " ")

  # 불필요한 문자 제거 (개선된 정규 표현식)
  pdf_text_cleaned <- gsub("[[:punct:]]|[0-9]|\\n|\\t|\\r|\\s+", " ", pdf_text_combined)
  pdf_text_cleaned <- trimws(pdf_text_cleaned)

  all_text <- rbind(all_text, data.frame(text = pdf_text_cleaned, stringsAsFactors = FALSE))
}

# 3단계: 텍스트 전처리 (토큰화, 명사 추출, 불용어 처리, 영어 제거)

# 명사 추출 함수
extract_nouns_split <- function(text, chunk_size = 5000) {
  text_len <- nchar(text)
  nouns <- character()
  for (i in seq(1, text_len, by = chunk_size)) {
    chunk <- substr(text, i, min(i + chunk_size - 1, text_len))
    tryCatch({
      chunk_nouns <- extractNoun(chunk)
      nouns <- c(nouns, chunk_nouns)
    }, error = function(e) {
      message(paste("Error in chunk:", substr(text, i, min(i+100, text_len))))
    })
  }
  nouns <- nouns[nchar(nouns) > 1]
  nouns <- unique(nouns)
  return(nouns)
}

all_text$nouns <- sapply(all_text$text, extract_nouns_split)

# 영어 단어 제거 (정규 표현식 사용)
all_text$nouns <- lapply(all_text$nouns, function(x) x[!grepl("^[a-zA-Z]+$", x)])

# 불용어 목록 생성 (stopwords-iso, 사용자 정의, 추가)
korean_stopwords <- stopwords("ko", source = "stopwords-iso")
additional_stopwords <- c("있다", "됩니다", "입니다", "었습니다", "때문", "때문에", "그러한", "이러한", "그러므로", "때로는", "또한", "그리고", "그러나", "하지만", "따라서", "더", "좀", "약", "개", "등의", "통한", "따른") # 추가 불용어 수정
custom_stopwords <- c("하다", "되다", "것", "수", "때", "및", "등", "통해", "대한", "관련", "따라", "도록", "위해", "연구", "분석", "제공", "사용", "적용", "방법", "경우", "교육", "학습", "기반", "기존", "진행", "과정", "내용", "문제", "부분", "정보", "설계", "수행")
all_stopwords <- unique(c(korean_stopwords, custom_stopwords, additional_stopwords))

# 명사 리스트에서 불용어 제거
all_text$nouns <- lapply(all_text$nouns, function(x) x[!x %in% all_stopwords])


# 4단계: 문서-용어 행렬(DTM) 생성 및 LDA 토픽 모델링

# 명사 리스트를 문자열 벡터로 변환
all_text$nouns_string <- sapply(all_text$nouns, paste, collapse = " ")

# Corpus 생성
corpus <- Corpus(VectorSource(all_text$nouns_string))

# DTM 생성 (전처리 추가)
dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(2, Inf),
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE
))

# 희소 용어 제거 (추가 조정 가능)
dtm <- removeSparseTerms(dtm, 0.9)

# DTM이 비어있는지 확인 후 처리
if (dtm$nrow > 0 && dtm$ncol > 0) {
  # LDA 토픽 모델링
  num_topics <- 5
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

  # 결과 출력
  print(terms(lda_model, 10))
  topic_distribution <- posterior(lda_model)$topics
  dim(topic_distribution)
  most_probable_topic <- apply(topic_distribution, 1, which.max)
  table(most_probable_topic)
} else {
  print("DTM이 비어있어 LDA를 수행할 수 없습니다. 전처리 과정을 확인하세요.")
}
