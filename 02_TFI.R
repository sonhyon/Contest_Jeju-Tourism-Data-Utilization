library(dplyr)

load("./tourist_density.rdata")
load("./busstop_sum.rdata")
load("./emission_region.rdata")

head(tourist_density) #관광 과밀도
head(busstop_sum) #생활 불편도
head(emission_region) #환경부화도

# 1. 컬럼 이름 맞추기
emission_region <- emission_region %>%
  rename(지역명 = 읍면동명)

# 2. 두 데이터 합치기 (지역 기준)
merged_data <- tourist_density %>%
  left_join(emission_region, by = "지역명")

# 결과 확인
head(merged_data)


merged_data <- merged_data %>%
  mutate(TFI = 0.4 * 관광객밀도 + 0.3 * 총배출량)

head(merged_data)

save(merged_data, file = "./merged_data.rdata")
