
# 필요한 패키지 설치 및 로드
packages <- c("KoNLP", "openxlsx", "tm", "topicmodels", "dplyr", "tidytext", "reshape2", "scales", "ggplot2", "wordcloud2", "htmlwidgets", "Rmpfr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# 사용자 사전 설정
useNIADic()

# 데이터 불러오기
abstracts <- read.csv("D:/LDAtopicmodeling/abstracts_cleaned.csv", header = FALSE, stringsAsFactors = FALSE)

# 형태소 분석
extract_morphemes <- function(text) {
  paste(extractNoun(text), collapse = " ")
}
result <- sapply(abstracts$V1, extract_morphemes)

# 불용어 처리
stopwords <- readLines("D:/LDAtopicmodeling/stopwords.txt", encoding = "UTF-8")
remove_stopwords <- function(text) {
  morphemes <- unlist(strsplit(text, " "))
  filtered <- morphemes[!morphemes %in% stopwords & nchar(morphemes) > 1]
  paste(filtered, collapse = " ")
}
result_final <- sapply(result, remove_stopwords)

# DTM 생성
corpus <- Corpus(VectorSource(result_final))
dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))

# Harmonic Mean 계산 함수
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  ll_m <- median(logLikelihoods)
  hmean <- ll_m - log(mean(exp(-mpfr(logLikelihoods - ll_m, precBits = precision))))
  return(as.numeric(hmean))
}

# 여러 k 값에 대해 LDA 수행 및 HM 계산
k_list <- 2:20
hm_values <- numeric(length(k_list))

for (i in seq_along(k_list)) {
  k <- k_list[i]
  cat("LDA 실행 중... k =", k, "\n")
  
  lda_model <- LDA(dtm, k = k, method = "Gibbs",
                   control = list(burnin = 1000, iter = 5000, keep = 100))
  
  log_likelihood <- lda_model@logLiks
  hm_values[i] <- harmonicMean(log_likelihood)
  
  cat("k =", k, "의 조화평균 =", hm_values[i], "\n")
}

# 결과 시각화
hm_df <- data.frame(k = k_list, harmonic_mean = hm_values)

hm_plot <- ggplot(hm_df, aes(x = k, y = harmonic_mean)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "토픽 수에 따른 조화평균 (HM)", x = "토픽 수 (k)", y = "Harmonic Mean") +
  theme_minimal()

ggsave("D:/LDAtopicmodeling/harmonic_mean_plot.png", hm_plot, width = 8, height = 5, dpi = 300)

# 최댓값 기준 k
optimal_k_hm <- hm_df$k[which.max(hm_df$harmonic_mean)]
cat("HM 최댓값 기준 최적의 토픽 수는 k =", optimal_k_hm, "\n")

# 엘보우 포인트 기반 자동 추정
delta_hm <- diff(hm_values)
k_diff <- k_list[-1]
elbow_k <- k_diff[which.min(abs(diff(delta_hm)))]
cat("엘보우 포인트 기반 최적 토픽 수는 k =", elbow_k, "\n")
