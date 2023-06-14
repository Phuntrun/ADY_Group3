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
head(df)

plot_bar_data <- function(dataframe, city_name) {
  x_axis <- dataframe[[columns[9]]]
  y1 <- dataframe[[columns[3]]]
  y2 <- dataframe[[columns[5]]]
  
  bar_data <- data.frame(x_axis, y1, y2)
  
  # Calculate the width for each set of bars
  width <- 0.35
  
  # Calculate the x-axis positions for each set of bars
  x_pos1 <- x_axis - width/2
  x_pos2 <- x_axis + width/2
  
  ggplot() +
    geom_bar(data = bar_data, aes(x = x_pos1, y = y1, fill = "Số dự án cấp mới"), stat = "identity", alpha = 0.5, width = width) +
    geom_bar(data = bar_data, aes(x = x_pos2, y = y2, fill = "Số lần điều chỉnh"), stat = "identity", alpha = 0.5, width = width) +
    geom_text(aes(x = x_pos1, y = y1, label = y1), vjust = -0.5, color = "black") +
    geom_text(aes(x = x_pos2, y = y2, label = y2), vjust = -0.5, color = "black") +
    labs(title = paste("Số dự án theo từng năm của ", city_name), x = "Năm", y = "Số dự án") +
    theme_minimal() +
    scale_fill_manual(values = c("Số dự án cấp mới" = "lightblue", "Số lần điều chỉnh" = "lightgreen"), 
                      labels = c("Số dự án cấp mới", "Số lần điều chỉnh")) +
    labs(fill = "Chú thích", color = "black")
}

df_HCM <- df[df[[columns[2]]] == "TP. Hồ Chí Minh", ]
plot_bar_data(df_HCM, "thành phố Hồ Chí Minh")

df_HN <- df[df[[columns[2]]] == "Hà Nội", ]
plot_bar_data(df_HN, "Hà Nội")
