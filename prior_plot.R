
library(readxl)
library(dplyr)

data <- read_xlsx("F:\\0-熊琦博士论文\\熊琦\\大论文\\小论文\\单层结构方程模型\\prior\\combinations.xlsx")  # 1#桩

# 计算每个c值对应的Ux均值
df_mean_c_Ux <- data %>%
  dplyr::group_by(c) %>%
  dplyr::summarise(avg=mean(Ux))

# plot(df_mean_c_Ux$c,df_mean_c_Ux$avg)

df_mean_phi_Ux <- data %>%
  dplyr::group_by(phi) %>%
  dplyr::summarise(avg=mean(Ux))

df_mean_Es_Ux <- data %>%
  dplyr::group_by(Es) %>%
  dplyr::summarise(avg=mean(Ux))

df_mean_v_Ux <- data %>%
  dplyr::group_by(v) %>%
  dplyr::summarise(avg=mean(Ux))

df_mean_gammasat_Ux <- data %>%
  dplyr::group_by(gammasat) %>%
  dplyr::summarise(avg=mean(Ux))


library(patchwork)
library(ggplot2)
library(tidyverse)


p11 = ggplot(data, aes(c,Ux)) +
  geom_point(shape=21, color="blue",fill="purple",alpha = 1/10) + 
  scale_y_continuous(labels = scales::scientific) + # 使用科学计数法格式化y轴标签
  labs(x = "Cohesion[kPa]", y = "Ux[m]", title = "[a] Scatter Plot with Ux-Cohesion") +
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),  # 设置横坐标轴标签字体大小和样式
    axis.title.y = element_text(size = 10, face = "bold"),  # 设置纵坐标轴标签字体大小和样式
    axis.text.x = element_text(size = 10),  # 设置横坐标轴刻度字体大小、旋转角度
    axis.text.y = element_text(size = 10)  # 设置纵坐标轴刻度字体大小
  )
 # geom_boxplot(outlier.size = 0, aes(fill=factor(c)),
               #position = position_dodge(0.1),size=0.2)

# 将X列转换为分类变量
data$Cohesion_Value <- factor(data$c, levels = unique(data$c), 
                        labels = c("c1=9.178", "c2=11.089", "c3=13.000", "c4=14.911", "c5=16.822"))
# 自定义颜色
colors <- c("red", "blue", "green", "orange", "purple")
#colors <-  c("#E69F00", "#56B4E9", "#009E73", "#F0E442")  # 可以自定义颜色
pp <- ggplot(data, aes(Ux, fill = Cohesion_Value)) +
  geom_density(aes(color = Cohesion_Value), alpha = 0.1) +  # 使用分类变量为密度曲线的填充和线条颜色
  scale_fill_manual(values = colors) +  # 设置填充颜色
  scale_color_manual(values = colors) +  # 设置线条颜色
  labs(title = "[b] Density Plots with Ux",
       x = "Ux[m]",
       y = "Density") +
  scale_x_continuous(labels = scales::scientific)

mean_data <- data %>%
  group_by(Cohesion_Value) %>%
  summarise(Mean_Ux = mean(Ux))

pp1 <- pp + geom_vline(data = mean_data, aes(xintercept = Mean_Ux, color = Cohesion_Value), linetype = "dashed") + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.1),
         color = guide_legend(title.position = "top", title.hjust = 0.1)) + 
  theme(legend.position=c(0.65, 0.45),
        legend.justification=c(0.2, 0.2),
        legend.text = element_text(size = 8, color = "blue"),  # 自定义图例标签的字体样式
        legend.title = element_text(size = 8, color = "red") ) +
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),  # 设置横坐标轴标签字体大小和样式
    axis.title.y = element_text(size = 10, face = "bold"),  # 设置纵坐标轴标签字体大小和样式
    axis.text.x = element_text(size = 10),  # 设置横坐标轴刻度字体大小、旋转角度
    axis.text.y = element_text(size = 10) , # 设置纵坐标轴刻度字体大小
    # 缩小图例项的大小
    legend.key.size = unit(0.5, "cm")
    # 缩小图例文本的大小
    #legend.text.size = 8
  )


library(ggpmisc)
pp2 <- ggplot(df_mean_c_Ux, aes(avg,c)) +
  geom_point(shape = "diamond", size = 6, color = "red") +  # 绘制散点图
 #scale_y_continuous(labels = scales::scientific) +
  geom_smooth(formula = y ~ x,method = "lm",  se = T, size = 1.2, color="blue",
              fill="lightgray",
              alpha=.5) +  # 添加拟合曲线
  labs(x = "Ux[m]", y = "Cohesion[kPa]", title = "[c] Fitted Line Plot with R-squared") +
  scale_x_continuous(labels = scales::scientific) + 
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),  # 设置横坐标轴标签字体大小和样式
    axis.title.y = element_text(size = 10, face = "bold"),  # 设置纵坐标轴标签字体大小和样式
    axis.text.x = element_text(size = 10),  # 设置横坐标轴刻度字体大小、旋转角度
    axis.text.y = element_text(size = 10)  # 设置纵坐标轴刻度字体大小
  )
  # stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
              #formula = y ~ x, parse = TRUE)    # 添加公式和R2
 # theme_minimal()  # 使用简洁主题  无背景颜色


p_fin <- p11 | (pp1/pp2)
p_fin
# 自定义保存图片的清晰度、尺寸和文件格式
ggsave("prior_c_ux.png", plot = p_fin, 
       width = 8, height = 6, units = "in", dpi = 900)





