library(readxl) ; library(dplyr) ; library(tidygeocoder) ; library(stringr)
library(purrr)

getwd()

#*[관광객수]
tourist_2401 <- read_excel("./데이터/지역별 관광객 현황_202401.xlsx")
tourist_2402 <- read_excel("./데이터/지역별 관광객 현황_202402.xlsx")
tourist_2403 <- read_excel("./데이터/지역별 관광객 현황_202403.xlsx")
tourist_2404 <- read_excel("./데이터/지역별 관광객 현황_202404.xlsx")
tourist_2405 <- read_excel("./데이터/지역별 관광객 현황_202405.xlsx")
tourist_2406 <- read_excel("./데이터/지역별 관광객 현황_202406.xlsx")
tourist_2407 <- read_excel("./데이터/지역별 관광객 현황_202407.xlsx")
tourist_2408 <- read_excel("./데이터/지역별 관광객 현황_202408.xlsx")
tourist_2409 <- read_excel("./데이터/지역별 관광객 현황_202409.xlsx")
tourist_2410 <- read_excel("./데이터/지역별 관광객 현황_202410.xlsx")
tourist_2411 <- read_excel("./데이터/지역별 관광객 현황_202411.xlsx")
tourist_2412 <- read_excel("./데이터/지역별 관광객 현황_202412.xlsx")

# 월 리스트 만들기
months <- sprintf("%02d", 1:12)

# 데이터 불러오기
for (m in months) {
  assign(paste0("tourist_24", m),
         read_excel(paste0("./데이터/지역별 관광객 현황_2024", m, ".xlsx")))
}

# 전처리 및 합계 계산
for (m in months) {
  df <- get(paste0("tourist_24", m))
  
  # 1행 삭제 + 지역명 컬럼명 지정
  df_clean <- df[-1, ] %>% 
    rename(지역명 = ...1)
  
  # 행별 합계 계산
  df_sum <- df_clean %>%
    mutate(across(-지역명, ~ suppressWarnings(as.numeric(.)))) %>%
    rowwise() %>%
    mutate(total_tourists = sum(c_across(-지역명), na.rm = TRUE)) %>%
    ungroup() %>%
    select(지역명, total_tourists)
  
  # 결과 저장
  assign(paste0("tourist_24", m, "_clean"), df_clean)
  assign(paste0("tourist_24", m, "_sum"), df_sum)
}

# 확인: _sum 데이터프레임 목록 출력
ls(pattern = "tourist_24[0-9]{2}_sum")


# 1️⃣ 12개 sum 데이터프레임 목록 불러오기
sum_list <- mget(ls(pattern = "tourist_24[0-9]{2}_sum"))

# 2️⃣ 각 데이터프레임의 total_tourists 열 이름을 월로 변경
months <- sprintf("2024%02d", 1:12)
sum_list <- Map(function(df, m) {
  df %>% rename(!!m := total_tourists)
}, sum_list, months)

# 3️⃣ 지역명을 기준으로 병합 (full join 반복)
tourist_all_wide <- reduce(sum_list, full_join, by = "지역명")

# 4️⃣ 결과 확인
head(tourist_all_wide)

tourist_all_wide <- tourist_all_wide %>%
  mutate(연간총합 = rowSums(across(starts_with("2024")), na.rm = TRUE))

head(tourist_all_wide)


#*[면적]
area <- read_excel("./데이터/제주_지역별_면적.xlsx")
head(area)

#*[관광객수-면적]
head(tourist_all_wide)
head(area)

# 지역 기준 병합 + 관광객밀도 계산
tourist_density <- tourist_all_wide %>%
  left_join(area, by = c("지역명" = "지역")) %>%
  mutate(관광객밀도 = 연간총합 / `면적(㎢)`) %>%
  select(지역명, 관광객밀도)

# 결과 확인
head(tourist_density)

save(tourist_density, file = "./tourist_density.rdata")
#───────────────────────────────────────────────────────────────────────────────
busstop <- read.csv("./데이터/국토교통부_전국 버스정류장 위치정보.csv", fileEncoding = "CP949")

head(busstop)
busstop_jeju <- busstop %>%
  filter(도시명 == '제주특별자치도') %>%
  select(정류장명, 위도, 경도, 도시명)
head(busstop_jeju)

# 역지오코딩
#busstop_jeju_address <- busstop_jeju %>%
  reverse_geocode(lat = 위도, long = 경도, method = 'osm')
head(busstop_jeju_address)

# 제주 내 지역 추출
busstop_jeju_address <- busstop_jeju_address %>%
  mutate(행정구역 = str_extract(address, "(?<=,\\s)[^,]+(?=,\\s*제주시)")) %>%
  select(-address)
head(busstop_jeju_address)

save(busstop_jeju_address, file = "./busstop_jeju_address.rdata")
load("./busstop_jeju_address.rdata")

busstop_sum <- busstop_jeju_address %>%
  group_by(행정구역) %>%
  summarise(n = n())
head(busstop_sum)

str(busstop_sum)

save(busstop_sum, file = "./busstop_sum.rdata")
#───────────────────────────────────────────────────────────────────────────────
emission <- read.csv("./데이터/제주특별자치도_유동인구에 따른 음식물 쓰레기 배출량 예측으로 제주도 내 클린하우스 관리 시스템 최적화_매쉬업결과.csv", fileEncoding = "CP949")
head(emission)

emission$일자 <- as.Date(emission$일자)
emission_2019 <- emission[format(emission$일자, "%Y") == "2019", ]
head(emission_2019)

emission_region <- emission_2019 %>%
  group_by(읍면동명) %>%
  summarise('총배출량' = sum(배출량))
head(emission_region)

save(emission_region, file = "./emission_region.rdata")
