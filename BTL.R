library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(caret)

# Đọc flie
videogamesales <- read.csv("E:/R/vgsales.csv")
# Xử lý dữ liệu
videogamesales <- videogamesales[!(videogamesales$Year %in% c("N/A", "2017", "2020")),]
videogamesales <- videogamesales %>% gather(Region, Revenue, 7:10) 
videogamesales$Region <- factor(videogamesales$Region)
colnames(videogamesales) <- gsub("\\.", "", colnames(videogamesales))
videogamesales$Global_Sales <- as.numeric(gsub(";", "", videogamesales$Global_Sales))
# Hàm để vẽ biểu đồ plots dễ nhìn
mytheme_1 <- function() {
  
  return(theme(axis.text.x = element_text(angle = 90, size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}
mytheme_2 <- function() {
  
  return(theme(axis.text.x = element_text(size = 10, vjust = 0.4), plot.title = element_text(size = 15, vjust = 2),axis.title.x = element_text(size = 12, vjust = -0.35)))
  
}
# Biểu đồ số lượng game phát hành theo năm
ggplot(videogamesales, aes(Year)) + 
  geom_bar(fill = "blue") +
  mytheme_1() +
  ggtitle("Số lượng game theo năm")

revenue_by_year <- videogamesales %>% 
  group_by(Year) %>%
  summarize(Revenue = sum(Global_Sales))

# Biểu đồ doanh thu game theo năm
ggplot(revenue_by_year, aes(Year, Revenue)) + 
  geom_bar(fill = "maroon", stat = "identity") +
  mytheme_1() +
  ggtitle("Doanh thu game theo năm")

# Tính doanh thu cho từng loại
top_1 <- videogamesales %>% 
  group_by(Year, Genre) %>% 
  summarize(Revenue = sum(Global_Sales)) %>%
  top_n(1)

datatable(top_1)

# Biểu đồ thể loại có doanh thu cao nhất theo từng năm
ggplot(top_1, aes(Year, Revenue, fill = Genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Thể loại có doanh thu cao nhất") +
  mytheme_1() +
  theme(legend.position = "top")

# Tính tổng doanh thu của từng game theo từng năm
top_games <- videogamesales %>%
  group_by(Year, Name) %>%
  summarize(Revenue = sum(Global_Sales)) %>%
  arrange(desc(Revenue)) %>%
  top_n(1)

datatable(top_games)

# Biểu đồ game có doanh thu cao nhất từng năm
ggplot(top_games, aes(Year, Revenue, fill = Name)) + 
  geom_bar(stat = "identity") +
  mytheme_1() +
  ggtitle("Total Games by Revenue each year") +
  theme(legend.position = "top")

# Số lượng nhà phát triển
length(unique(videogamesales$Publisher))
# Lấy 10 nhà phát hành có số lượng lớn nhất
by_publishers <- videogamesales %>% group_by(Publisher) %>% summarize(Total = n()) %>% arrange(desc(Total)) %>% head(10)
by_publishers$Percentage <- by_publishers$Total/dim(videogamesales)[1] * 100
by_publishers$Publisher <- factor(by_publishers$Publisher)

# Cột đầu số lượng game, cột 2 chiếm bao nhiêu %
datatable(by_publishers, filter = "none")

# Biểu đồ những nhà phát hành có lượng game lớn
ggplot(by_publishers, aes(reorder(Publisher, Total), Total, fill = Publisher)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Top những nhà phát hành có số lượng lớn nhất") + 
  theme(legend.position = "none") + 
  xlab("Publisher") +
  mytheme_2() +
  coord_flip()

# Hồi quy tuyến tính để dự đoán doanh thu của game trong những năm tới

# 1.Loại bỏ các giá trị NA
videogamesales <- videogamesales %>%
  select(Year, Genre, Publisher, Platform, Global_Sales) %>%
  na.omit()

videogamesales <- videogamesales %>%
  filter(Year != "N/A") %>%
  mutate(Year = as.numeric(Year),  
         Global_Sales = as.numeric(Global_Sales),
         Genre = factor(Genre),
         Publisher = factor(Publisher),
         Platform = factor(Platform))
# 2.Chia dữ liệu thành tập huấn luyện và kiểm tra
set.seed(123) 
# Chia dữ liệu thành 80% train và 20% test
train_index <- createDataPartition(videogamesales$Global_Sales, p = 0.8, list = FALSE)

train_data <- videogamesales[train_index, ]
test_data <- videogamesales[-train_index, ]

# 3.Huấn luyện mô hình
# Xây dựng mô hình hồi quy tuyến tính
lm_model <- lm(Global_Sales ~ Year + Genre + Publisher + Platform, data = train_data)

# Xem tóm tắt mô hình
summary(lm_model)

# 4.Đánh giá mô hình
# Dự đoán giá trị trên tập kiểm tra
predicted_revenue <- predict(lm_model, newdata = test_data)

# Đánh giá mô hình bằng cách so sánh với giá trị thực tế
actual_vs_predicted <- data.frame(Actual = test_data$Global_Sales, Predicted = predicted_revenue)
print(head(actual_vs_predicted))

# Tính sai số RMSE(Sai số chung bình)
rmse <- sqrt(mean((actual_vs_predicted$Actual - actual_vs_predicted$Predicted)^2))
cat("Root Mean Square Error (RMSE):", rmse, "\n")

mean_sales <- mean(test_data$Global_Sales)
cat("Giá trị trung bình của doanh thu game:", mean_sales, "\n")
# RMSE gấp khoảng 2.4 lần doanh thu trung bình mô hình vẫn chưa chính xác lắm

# 5.Dự báo doanh thu game năm 2025 - 2030
future_years <- data.frame(
  Year = 2025:2030,  
  Genre = factor(rep("Action", 6), levels = levels(videogamesales$Genre)),  
  Publisher = factor(rep("Nintendo", 6), levels = levels(videogamesales$Publisher)),  
  Platform = factor(rep("PS4", 6), levels = levels(videogamesales$Platform))
)

# Dự đoán doanh thu game cho các năm 2025 - 2030(Triệu bản)
future_years$Predicted_Sales <- predict(lm_model, newdata = future_years)

# Xem kết quả dự báo
print(future_years)

# Vẽ biểu đồ xu hướng doanh thu game từ 2025 - 2030
ggplot(future_years, aes(x = Year, y = Predicted_Sales)) +
  geom_line(color = "blue", size = 1.2) +   
  geom_point(color = "red", size = 3) +     
  geom_text(aes(label = round(Predicted_Sales, 2)), vjust = -0.5, size = 5) + 
  ggtitle("Dự đoán Doanh thu trung bình của game ") +
  xlab("Năm") +
  ylab("Doanh thu trung bình (triệu bản)") +
  scale_x_continuous(breaks = seq(2025, 2030, 1)) +  
  theme_minimal() + mytheme_2()
  

