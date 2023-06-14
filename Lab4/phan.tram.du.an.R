library(readr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(stringr)

filename <- "fdi_provinces_vi.csv"
df <- read_csv(filename)

columns <- colnames(df)
for (i in 3:9) {
  df <- df %>% mutate(!!columns[i] := as.numeric(!!sym(columns[i])))
}
df[is.na(df)] <- 0

df_2022 <- filter(df, df$Năm == 2022)
df_2022 <- data.frame(df_2022$`Đối tác`, df_2022$`Vốn đăng ký cấp mới (triệu USD)`) %>%
  summarise(`Đối tác` = df_2022$`Đối tác`, `Vốn đăng ký cấp mới (triệu USD)` = df_2022$`Vốn đăng ký cấp mới (triệu USD)`)

# Economy Zone 1 - Trung du và miền núi phía Bắc
economy_zone_1 <- c("Hà Giang", "Cao Bằng", "Bắc Kạn", "Tuyên Quang", "Lào Cai", "Điện Biên", "Lai Châu", "Sơn La", "Yên Bái", "Hoà Bình", "Thái Nguyên", "Lạng Sơn", "Bắc Giang", "Phú Thọ")

# Economy Zone 2 - Đồng bằng sông Hồng
economy_zone_2 <- c("Hà Nội", "Quảng Ninh", "Vĩnh Phúc", "Bắc Ninh", "Hải Dương", "Hải Phòng", "Hưng Yên", "Thái Bình", "Hà Nam", "Nam Định", "Ninh Bình")

# Economy Zone 3 - Bắc Trung Bộ và Duyên Hải miền Trung
economy_zone_3 <- c("Thanh Hoá", "Nghệ An", "Hà Tĩnh", "Quảng Bình", "Quảng Trị", "Thừa Thiên Huế", "Đà Nẵng", "Quảng Nam", "Quảng Ngãi", "Bình Định", "Phú Yên", "Khánh Hoà", "Ninh Thuận", "Bình Thuận")

# Economy Zone 4 - Tây Nguyên
economy_zone_4 <- c("Kon Tum", "Gia Lai", "Đắk Lắk", "Đắk Nông", "Lâm Đồng")

# Economy Zone 5 - Đông Nam Bộ
economy_zone_5 <- c("Bình Phước", "Tây Ninh", "Bình Dương", "Đồng Nai", "Bà Rịa. Vũng Tàu", "TP Hồ Chí Minh")

# Economy Zone 6 - Đồng bằng sông Cửu Long
economy_zone_6 <- c("Long An", "Tiền Giang", "Bến Tre", "Trà Vinh", "Vĩnh Long", "Đồng Tháp", "An Giang", "Kiên Giang", "Cần Thơ", "Hậu Giang", "Sóc Trăng", "Bạc Liêu", "Cà Mau")

df_zone1 <- filter(df_2022, df_2022$`Đối tác` %in% economy_zone_1)
df_zone2 <- filter(df_2022, df_2022$`Đối tác` %in% economy_zone_2)
df_zone3 <- filter(df_2022, df_2022$`Đối tác` %in% economy_zone_3)
df_zone4 <- filter(df_2022, df_2022$`Đối tác` %in% economy_zone_4)
df_zone5 <- filter(df_2022, df_2022$`Đối tác` %in% economy_zone_5)
df_zone6 <- filter(df_2022, df_2022$`Đối tác` %in% economy_zone_6)
#split data
labels <- c("Trung du và miền núi phía Bắc", "Đồng bằng sông Hồng", "Bắc Trung Bộ và Duyên Hải miền Trung",
            "Tây Nguyên", "Đông Nam Bộ", "Đồng bằng sông Cửu Long")
values <- c(sum(df_zone1$`Vốn đăng ký cấp mới (triệu USD)`), sum(df_zone2$`Vốn đăng ký cấp mới (triệu USD)`),
            sum(df_zone3$`Vốn đăng ký cấp mới (triệu USD)`), sum(df_zone4$`Vốn đăng ký cấp mới (triệu USD)`),
            sum(df_zone5$`Vốn đăng ký cấp mới (triệu USD)`), sum(df_zone6$`Vốn đăng ký cấp mới (triệu USD)`))
#piechart
percentage <- percent(values/sum(values))
pie3D(values, labels = percentage, explode = 0.1, main = "TỈ LỆ TỔNG SỐ VỐN ĐĂNG KÝ CẤP MỚI GIỮA CÁC VÙNG KINH TẾ NĂM 2022", labelcex = 0.8)
legend("topright", legend = labels, fill = rainbow(length(labels)), cex = 0.8)
#barchart
bar_data = data.frame(labels, values)

ggplot(bar_data, aes(x = str_wrap(labels, width = 20), y = values)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black", alpha = 0.5) +
  geom_text(aes(label = values), vjust = -0.5) +
  labs(x = "", y = "Vốn đăng ký cấp mới (triệu USD)"
       , title = "TỔNG SỐ VỐN ĐĂNG KÝ CẤP MỚI GIỮA CÁC VÙNG KINH TẾ NĂM 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  
