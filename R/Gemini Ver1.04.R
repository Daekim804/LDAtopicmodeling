# 1단계: 필요한 패키지 설치 및 로드, PDF 파일 목록 가져오기

# 필요한 패키지 로드
if(!require(pdftools)){install.packages("pdftools")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(stringr)){install.packages("stringr")}
if(!require(KoNLP)){install.packages("KoNLP")}
if(!require(tm)){install.packages("tm")}
if(!require(topicmodels)){install.packages("topicmodels")}
if(!require(stopwords)){install.packages("stopwords")}
if(!require(LDAvis)){install.packages("LDAvis")}
if(!require(servr)){install.packages("servr")}
if(!require(ggplot2)){install.packages("ggplot2")}
library(pdftools)
library(dplyr)
library(stringr)
library(KoNLP)
library(tm)
library(topicmodels)
library(stopwords)
library(LDAvis)
library(servr)
library(ggplot2)
useNIADic()

# 2단계: PDF 파일 텍스트 추출 및 전처리

# PDF 파일이 있는 폴더 경로 설정
pdf_folder <- "G:/내 드라이브/2. 연구 자료/박사 과정/0. 연구 파일/메타버스/메타버스 논문 분석(22.~)"

# 폴더 내의 모든 PDF 파일 목록 가져오기
pdf_files <- list.files(pdf_folder, pattern = "*.pdf", full.names = TRUE)

# 모든 PDF 파일의 텍스트를 저장할 리스트 생성
all_text <- list()

# 각 PDF 파일에 대해 텍스트 추출 및 전처리 반복
for (pdf_file in pdf_files) {
  pdf_text <- tryCatch({
    pdf_text(pdf_file)
  }, error = function(e) {
    message(paste("Error reading PDF:", pdf_file, e))
    return(NULL)
  })

  if (is.null(pdf_text)) next

  pdf_text_combined <- paste(pdf_text, collapse = " ")

  # 불필요한 문자 제거 (개선된 정규 표현식)
  pdf_text_cleaned <- gsub("[[:punct:]]|[0-9]|\\n|\\t|\\r|\\s+", " ", pdf_text_combined)
  pdf_text_cleaned <- trimws(pdf_text_cleaned)

  all_text <- c(all_text, pdf_text_cleaned)
}

# 3단계: 텍스트 전처리 (토큰화, 명사 추출, 불용어 처리, 영어 제거, 동의어/지정어 처리)

# 명사 추출 함수 (기존 함수 유지)
extract_nouns_split <- function(text, chunk_size = 5000) {
  # ... (기존 코드와 동일)
}

# 모든 텍스트에 대해 명사 추출
all_nouns <- lapply(all_text, extract_nouns_split)

# 영어 단어 제거 (기존 코드 유지)
all_nouns <- lapply(all_nouns, function(x) x[!grepl("^[a-zA-Z]+$", x)])

# 불용어 목록 생성 (stopwords-iso, 사용자 정의, 추가) (기존 코드 수정)
korean_stopwords <- stopwords("ko", source = "stopwords-iso")
additional_stopwords <- c("있다", "됩니다", "입니다", "었습니다", "때문", "때문에", "그러한", "이러한", "그러므로", "때로는", "또한", "그리고", "그러나", "하지만", "따라서", "더", "좀", "약", "개", "등의", "통한", "따른") # 추가 불용어 수정
custom_stopwords <- c("하다", "되다", "것", "수", "때", "및", "등", "통해", "대한", "관련", "따라", "도록", "위해", "연구", "분석", "제공", "사용", "적용", "방법", "경우", "교육", "학습", "기반", "기존", "진행", "과정", "내용", "문제", "부분", "정보", "설계", "수행", "활용")
all_stopwords <- unique(c(korean_stopwords, custom_stopwords, additional_stopwords))

# 명사 리스트에서 불용어 제거 (기존 코드 유지)
all_nouns <- lapply(all_nouns, function(x) x[!x %in% all_stopwords])

# 동의어 및 지정어 처리 (수정)
all_nouns <- lapply(all_nouns, function(nouns) {
  # 동의어 치환
  nouns <- sapply(nouns, function(noun) {
    if (noun %in% names(synonyms)) {
      return(synonyms[[noun]])
    } else {
      return(noun)
    }
  })

  # 중복 제거 (동의어 치환 후)
  nouns <- unique(nouns)

  # 지정어 추가 (필요한 경우 특정 조건 하에 추가하도록 수정 가능)
  #예를 들어, 특정 토픽 분석에 필요한 경우에만 추가하는 것이 좋습니다.
  #if (특정 조건) {
  #    nouns <- c(nouns, specified_words)
  #}
  return(nouns)
})

# 예시: "가상현실" -> "VR", "메타버스" -> "가상세계"
synonyms <- list(
  "가상현실" = "VR",
  "메타버스" = "가상세계",
  "증강현실" = "AR"
)
specified_words <- c("교수자", "학습자", "콘텐츠", "플랫폼")

all_nouns <- lapply(all_nouns, function(nouns) {
  nouns <- sapply(nouns, function(noun) {
    if (noun %in% names(synonyms)) {
      return(synonyms[[noun]])
    } else {
      return(noun)
    }
  })
  nouns <- unique(c(nouns, specified_words))
  return(nouns)
})

# 4단계: 문서-용어 행렬(DTM) 생성 및 LDA 토픽 모델링
# 명사 리스트를 문자열 벡터로 변환 (기존 코드 유지)
all_nouns_string <- sapply(all_nouns, paste, collapse = " ")

# Corpus 생성
corpus <- Corpus(VectorSource(all_nouns_string))

# DTM 생성 및 전처리 (대폭 수정)
dtm <- DocumentTermMatrix(corpus, control = list(
  wordLengths = c(2, Inf),
  tolower = FALSE,
  removeNumbers = TRUE,
  removePunctuation = TRUE
))

# DTM 행과 열의 합 계산 및 비어있는 행/열 제거 (중요!)
rowTotals <- apply(dtm , 1, sum) # 각 행의 합
dtm.new   <- dtm[rowTotals> 0, ] # 합이 0보다 큰 행만 선택

colTotals <- apply(dtm.new , 2, sum) # 각 열의 합
dtm.new   <- dtm.new[, colTotals> 0] # 합이 0보다 큰 열만 선택

# DTM이 비어있는지 확인 후 처리 (수정)
if (dtm.new$nrow > 1 && dtm.new$ncol > 1) { # 행과 열이 1보다 커야 함

  # 희소 용어 제거 (수정: DTM이 충분히 큰 경우에만 실행)
  if(dtm.new$ncol > 100){ # 단어 수가 100개 이상일 때만 희소 용어 제거
    dtm.new <- removeSparseTerms(dtm.new, 0.9)
  }

  # 최적 토픽 수 결정 (HM 사용, 대폭 수정)
  harmonicMean <- function(logLikelihoods) {
    if (any(is.na(logLikelihoods))) return(NA)
    ll <- logLikelihoods[logLikelihoods > -Inf]
    if (length(ll) == 0) return(NA)
    harmonic_mean <- length(ll)/sum(1/ll)
    return(harmonic_mean)
  }

  k_values <- 2:min(10, dtm.new$nrow-1, dtm.new$ncol-1) # k 값 범위 조정 (중요!)
  if(length(k_values) < 2){
    print("k value range is too small. Cannot perform topic modeling.")
    optimal_k <- NA
  }else{
    harmonic_means <- sapply(k_values, function(k) {
      tryCatch({
        lda_temp <- LDA(dtm.new, k = k, control = list(seed = 1234, verbose = TRUE)) # verbose 추가
        print(lda_temp) # lda_temp 객체 정보 출력
        logLikelihoods <- lda_temp@logLiks[-length(lda_temp@logLiks)]
        hm <- harmonicMean(logLikelihoods)
        return(hm)
      }, error = function(e) {
        print(paste("Error for k =", k, ":"))
        print(e) # 오류 객체 전체 내용 출력 (핵심!)
        return(NA)
      })
    })

    # NA 제거 후 HM 값 및 k 값 선택 (수정)
    harmonic_means_valid <- harmonic_means[!is.na(harmonic_means)]
    k_values_valid <- k_values[!is.na(harmonic_means)]

    if (length(harmonic_means_valid) > 0) {
      optimal_k <- k_values_valid[which.max(harmonic_means_valid)]
      print(paste("Optimal number of topics (HM):", optimal_k))
      plot(k_values_valid, harmonic_means_valid, type = "b",
           xlab = "Number of Topics (K)", ylab = "Harmonic Mean of Log-Likelihoods",
           main = "Harmonic Mean vs. Number of Topics")

      # LDA 토픽 모델링 (최적 k 사용)
      lda_model <- LDA(dtm.new, k = optimal_k, control = list(seed = 1234))

      # 결과 출력 및 해석
      terms_result <- terms(lda_model, 10) # 10개 단어 출력
      print("Topic 별 주요 단어:")
      print(terms_result)
    } else {
      print("All HM values are NA. Cannot determine optimal k.")
      optimal_k <- NA
    }
  }
} else {
  print("DTM is too small or empty. Cannot perform topic modeling.")
  optimal_k <- NA
}

# optimal_k 값 최종 확인 (if문 밖에서)
print(paste("Final Optimal number of topics (HM):", optimal_k))
