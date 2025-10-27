#*[데이터 준비]
load("./tourist_density.rdata")
load("./emission_region.rdata")
head(tourist_density) #관광 과밀도
head(emission_region) #환경부화도

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