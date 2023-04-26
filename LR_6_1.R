
library(factoextra)
library(ggplot2) # для визуализации данных
library(dplyr) # для манипуляций с данными

df <- read.csv("StudentsPerformance.csv", sep=",", header=T, fileEncoding="UTF-8")
str(df)
summary(df)

ggplot(df, aes(x=gender)) +
  geom_bar(fill="#69b3a2") +
  labs(title="Распределение по полу", x="Пол", y="Количество студентов")

ggplot(df, aes(x=race_ethnicity)) +
  geom_bar(fill="#69b3a2") +
  labs(title="Распределение по расе/этнической группе", x="Раса/этническая группа", y="Количество студентов")

ggplot(df, aes(x=parental_level_of_education)) +
  geom_bar(fill="#69b3a2") +
  labs(title="Распределение по уровню образования родителей", x="Уровень образования", y="Количество студентов")

ggplot(df, aes(x=lunch)) +
  geom_bar(fill="#69b3a2") +
  labs(title="Распределение по типу обеда", x="Тип обеда", y="Количество студентов")

ggplot(df, aes(x=test_preparation_course)) +
  geom_bar(fill="#69b3a2") +
  labs(title="Распределение по курсу подготовки к экзамену", x="Курс подготовки", y="Количество студентов")

ggplot(df, aes(x=math_score)) +
  geom_histogram(fill="#69b3a2", binwidth=5) +
  labs(title="Распределение оценок по математике", x="Оценка", y="Количество студентов")

ggplot(df, aes(x=reading_score)) +
  geom_histogram(fill="#69b3a2", binwidth=5) +
  labs(title="Распределение оценок по чтению", x="Оценка", y="Количество студентов")

ggplot(df, aes(x=writing_score)) +
  geom_histogram(fill="#69b3a2", binwidth=5) +
  labs(title="Распределение оценок по письму", x="Оценка", y="Количество студентов")

ggplot(df, aes(x=math_score, y=reading_score)) +
  geom_point(color="#69b3a2") +
  labs(title="Диаграмма рассеяния для оценок по математике и чтению", x="Оценка по математике", y="Оценка по чтению")

# Выбираем переменные для кластеризации
cluster_vars <- c("race_ethnicity", "parental_level_of_education", "math_score", "reading_score", "writing_score")

# Создаем матрицу расстояний на основе выбранных переменных
d <- dist(df[, cluster_vars], method = "euclidean")

# Выполняем кластеризацию методом "полного" (complete) сцепления
hc <- hclust(d, method = "complete")

# Строим дендрограмму для визуализации кластеров
plot(hc, cex = 0.6, hang = -1, labels = df$race_ethnicity, main = "Dendrogram of StudentsPerformance data")

# Выбираем переменные для кластеризации
cluster_vars <- c("race_ethnicity", "parental_level_of_education", "math_score", "reading_score", "writing_score")

# Создаем матрицу расстояний на основе выбранных переменных
d <- dist(df[, cluster_vars], method = "euclidean")

# Выполняем кластеризацию методом "полного" (complete) сцепления
hc <- hclust(d, method = "complete")

# Строим график для метода локтя
wss <- (nrow(df[, cluster_vars]) - 1) * sum(apply(df[, cluster_vars], 2, var))
for (i in 2:10) wss[i] <- sum(kmeans(d, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups sum of squares")

rect.hclust(hc,k = 4, border="red")

plot(1:(nrow(df)-1), hc$height, type='b', main='Каменная осыпь')
  
# df$average_score <- (df$math_score + df$reading_score + df$writing_score)/3
# library(ggplot2)
# ggplot(df, aes(x = parental_level_of_education, y = average_score)) +
#   geom_boxplot(fill = "lightblue") +
#   labs(title = "Зависимость между образованием родителей и успеваемостью студентов",
#        x = "Образование родителей", y = "Средняя оценка")