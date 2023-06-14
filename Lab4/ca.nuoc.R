library(readr)
library(dplyr)
library(ggplot2)

filename <- "fdi_provinces_vi.csv"
df <- read_csv(filename)

columns <- colnames(df)
for (i in 3:9) {
  df <- df %>% mutate(!!columns[i] := as.numeric(!!sym(columns[i])))
}
df[is.na(df)] <- 0

#split dataframe
grouped_df <- group_by(df, Năm)
total_df <- unique(summarize(grouped_df, 
                      Tổng.số.dự.án = sum(`Số dự án cấp mới`),
                      Tổng.số.vốn.đăng.ký.cấp.mới = sum(`Vốn đăng ký cấp mới (triệu USD)`),
                      Năm = Năm))
print(total_df)

#define axis
x_axis = total_df$Năm
y1 = total_df$Tổng.số.dự.án
y2 = total_df$Tổng.số.vốn.đăng.ký.cấp.mới

#plot data
ggplot(total_df, aes(x_axis)) +
  geom_line(aes(y = y2, color= "Tổng số vốn đăng kí cấp mới"), size = 1)+
  geom_bar(aes(y = y1 * 5, fill = "Tổng số dự án"), stat = "identity", alpha = 0.5)+
  scale_fill_manual(values = "lightblue") +
  scale_color_manual(values = "red") +
  labs(title = "TỔNG SỐ DỰ ÁN VÀ VỐN ĐĂNG KÍ CẤP MỚI QUA CÁC NĂM CỦA CẢ NƯỚC",x = "Năm", y = "Số dự án", color = "Số vốn đăng ký cấp mới")+
  theme_minimal() +
  theme(axis.text.y.right = element_text(color = "black"),
        axis.title.y.right = element_text(color = "black")) +
  scale_y_continuous(
    name = "Số vốn đăng ký cấp mới(Triệu USD)",
    sec.axis = sec_axis(~./5, name = "Số dự án")
  )
  
