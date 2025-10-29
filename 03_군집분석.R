#*[데이터 준비]
load("./tourist_density.rdata")
load("./emission_region.rdata")
load("./merged_data.rdata")
head(tourist_density) #관광 과밀도
head(emission_region) #환경부화도
head(merged_data)
#───────────────────────────────────────────────────────────────────────────────
#*[군집분석]

# 표준화
cluster_data_scaled <- scale(cluster_data)

wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(cluster_data_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Clusters Sum of Squares",
     main = "Elbow Method for Optimal K")

set.seed(123)  # 재현 가능성을 위해 고정
k_result <- kmeans(cluster_data_scaled, centers = 4, nstart = 25)
head(k_result)

# 결과 확인
table(k_result$cluster)

merged_data$cluster <- as.factor(k_result$cluster)
head(merged_data)

# 첫 두 주성분 기준으로 시각화
fviz_cluster(k_result, data = cluster_data_scaled,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


# 각 클러스터별 평균 계산
cluster_summary <- aggregate(cluster_data, 
                             by = list(cluster = merged_data$cluster), 
                             FUN = mean)

View(cluster_summary)  # 혹은 print(cluster_summary)

# 클러스터별 지역 
merged_data %>%
  group_by(cluster) %>%
  summarise(지역목록 = paste(시도명, collapse = ", "))







library(factoextra)

# 2️⃣ 숫자형 변수 선택
cluster_data <- merged_data[, c("관광객밀도", "총배출량", "TFI")]

# 3️⃣ NA/NaN/Inf 제거
valid_rows <- complete.cases(cluster_data) & apply(cluster_data, 1, function(x) all(is.finite(x)))
cluster_data_clean <- cluster_data[valid_rows, ]

# 4️⃣ 스케일링
cluster_data_scaled <- scale(cluster_data_clean)

# 5️⃣ Elbow Method (최적 K 확인)
wss <- numeric(10)
for (k in 1:10) {
  wss[k] <- sum(kmeans(cluster_data_scaled, centers = k, nstart = 25)$withinss)
}

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Optimal K")

# 6️⃣ K-means 수행 (예: K=3)
set.seed(123)
k_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)

# 7️⃣ 원본 데이터에 군집 번호 추가
merged_data$cluster <- NA
merged_data$cluster[which(valid_rows)] <- k_result$cluster

# 8️⃣ fviz_cluster 시각화
fviz_cluster(k_result, data = cluster_data_scaled,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())
