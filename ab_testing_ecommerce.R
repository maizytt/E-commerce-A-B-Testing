# ---- import libaries
library(boot)
library(corrplot)
library(tidyverse)
library(janitor)
library(ggplot2)
library(lmPerm)
library(dplyr)
# ---- load data
data_ab_test_commerce <- read_csv(file = "datasets/ab_test_commerce.csv",na = c("", "NA", "N/A"))
data_ab_test_commerce <- data_ab_test_commerce |> clean_names()

# ---- EDA
# Explore data
glimpse(data_ab_test_commerce)
data_ab_test_commerce|>distinct(converted)
data_ab_test_commerce|>distinct(con_treat)
data_ab_test_commerce|>distinct(page)
# Kiểm tra dữ liệu bị thiếu
missing_values <- sum(is.na(data_ab_test_commerce))
print(paste("Tổng số giá trị bị thiếu:", missing_values))
# Time
data_ab_test_commerce <- data_ab_test_commerce |>
  mutate(time_hour= hour(time),
         time_minures = minute(time),
         time_seconds = second(time))
data_ab_test_commerce|>distinct(time_seconds)
data_ab_test_commerce <- data_ab_test_commerce |> mutate(access_time = hour(time) * 60 + minute(time) + second(time))


# Hiệu chỉnh sai khác 
data_ab_test_commerce|>filter(con_treat == "control" & page != "old_page") |> nrow()
data_ab_test_commerce|>filter(con_treat == "treatment" & page != "new_page") |> nrow()
data_clean <- data_ab_test_commerce |>  mutate(page = case_when(
                                          con_treat == "control" & page != "old_page" ~ "old_page",
                                          con_treat == "treatment" & page != "new_page" ~ "new_page",
                                          TRUE ~ page ))
data_clean|>filter(con_treat == "control" & page != "old_page") |> nrow()
data_clean|>filter(con_treat == "treatment" & page != "new_page") |> nrow()


# Thống kê mô tả 
data_clean |> group_by(page, converted) |>
  summarise(n = n(), tb =mean(access_time), sd = sd(access_time))
data_clean |> group_by(page) |>
  summarise(n = n(), tb = mean(access_time), sd = sd(access_time))
# Tính tỷ lệ giao dịch thành công 
data_clean |> summarise(
          new_page_success_rate = sum(data_clean$converted[data_clean$page == "new_page"]) / sum(data_clean$page == "new_page"),
          old_page_success_rate = sum(data_clean$converted[data_clean$page == "old_page"]) / sum(data_clean$page == "old_page"))

## Nhận thấy rằng, xét về thời gian truy cập trung bình của 2 trang web là tương đương nhau.
## Tuy nhiên tỷ lệ giao dịch thành công(p) của old_page có phần cao hơn một chút so với new_page.
# Đặt giả thuyết: với A là old_page và B là new_page
## H0: p_A = p_B 
## H1: p_A > p_B 

rate_permutation <- function(x, y, R,p_A, p_B, alter){
  data <- split(x,y)
  n <-  length(x)
  nA <- length(data[[1]])
  nB <- length(data[[2]])
  mean_diff <- numeric(R)
  for (i in 1:R){
    idx_a <- sample(x = 1:n, size = nA)
    idx_b <- setdiff(x = 1:n, y = idx_a)
    mean_diff[i] <- sum(x[idx_a])/nA -  sum(x[idx_b])/nB
  }
  
  
  if (alter == "left"){
    p_values <- mean(mean_diff < (p_A - p_B))
  }
  else if (alter == 'right'){
    p_values <- mean(mean_diff > (p_A - p_B))
  }
  else{
    p_values <- mean(abs(mean_diff) > (p_A - p_B)) 
  }
  return (list(res_perm = mean_diff,rate_diff = p_A - p_B, p_value = p_values))
}
new_page_success_rate <- sum(data_clean$converted[data_clean$page == "new_page"]) / sum(data_clean$page == "new_page")
old_page_success_rate <- sum(data_clean$converted[data_clean$page == "old_page"]) / sum(data_clean$page == "old_page")
set.seed(21)
result <- rate_permutation(data_clean$converted,data_clean$page, R = 1000, p_A=old_page_success_rate, p_B = new_page_success_rate,alter = 'left')

p_value = result[3]

# Ta có p_value = 0.894 > 0.05 kết quả cho thấy Giả thuyết là không thể bị bác bỏ. 
# Do đó, việc thời tỷ lệ giao dịch thành công của new_page thấp hơn old_page là không có ý nghĩa thống kê, hay chỉ là kết quả của sự ngẫu nhiên.
# Điều này có nghĩa là trang web mới không có hiệu quả nhiều so với page cũ. 


