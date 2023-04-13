# 1. Выполнить учебный импорт любых таблиц данных из csv-файла и xls-таблицы.
df <- read.csv("Лучший мультфильм.csv", sep=";", header=T,
               fileEncoding="cp1251")

df[15, ] <- df[16, ]
df <- df[-c(16), ]

library(readxl)
test_data <- read_excel("test.xls", sheet = "test"); test_data


# 2. Выполнить дескриптивный анализ данных из ЛР №2.
num_idx <- which(sapply(df, is.numeric))

hist(df$Тачки, main="Гистограмма по столбцу Тачки", xlab="Оценки", 
     ylab="Частота", col = rainbow(6))
boxplot(df[, num_idx], main='Коробчатые диаграммы мультфильмов',
        xlab='Фильмы', ylab='Оценки', col=rainbow(11))
summary(df)

# 3. Выполнить сортировку наборов данных по выбранному признаку.
df_sorted <- df[order(df$Тачки, decreasing = T), ]; View(df_sorted)

# 4.	Сформировать отдельные наборы данных по одинаковому признаку
#(например, составить subdataset, из студентов, отдавших предпочтение по 
# шкале > 7 определенной книге), вывести результат,  выполнить подсчет 
# размерностей новых таблиц, снова выполнить их анализ –
#гистограмма, боксплот, серединные меры
cars_more_than_6 <- subset(df, Мадагаскар > 6); cars_more_than_6
dim(cars_more_than_6)
attach(cars_more_than_6)

hist(Мадагаскар, main="Гистограмма по столбцу Мадагаскар, оценки > 6", xlab="Оценки", 
     ylab="Частота", col = rainbow(4))
boxplot(Мадагаскар, main='Коробчатая диаграмма фильма Мадагаскар',
        xlab='Мадагаскар', ylab='Оценки', col=rainbow(1))

mean(Суперсемейка)
median(Суперсемейка)

detach(cars_more_than_6)