# Создайте фрейм данных из N = 20 записей со следующими полями: Nrow – номер записи,
# Name – имя сотрудника, BirthYear – год рождения, EmployYear – год приема на работу,
# Salary – зарплата. Заполните данный фрейм данными так, что Nrow изменяется от 1 до N,
# Name задается произвольно, BithYear распределен равномерно (случайно) на отрезке [1960,
#                                                                                   1985], 
# EmployYear распределен равномерно на отрезке [BirthYear + 18, 2006], Salary для
# работников младше 1975 г.р. определяется по формуле Salary = (ln(2007 − EmployYear) + 1)
# ∗8000, для остальных Salary = (log2(2007 − EmployY ear) + 1) ∗ 8000
# Подсчитайте число сотрудников с зарплатой, большей 15000 Добавьте в таблицу поле,
# соответствующее суммарному подоходному налогу (ставка 13%), выплаченному
# сотрудником за время работы в организации, если его зарплата за каждый год начислялась
# согласно формулам для Salary, где вместо 2007 следует последовательно подставить
# каждый год работы сотрудника в организации.

n <- 20
BirthDate <- c(sample(1960:1985 ,n,replace = TRUE))
names <- c('Султан','Артём','Валя','Иван','Олег','Константин','Арсений','Анна','Илья')
DF <- data.frame('nrow'=1:n, 'name'=sample(names, n, replace=TRUE),
                 'BirthYear'=BirthDate, 'EmployYear' = integer(20))

for(i in 1:n) {
  DF[i, 'EmployYear'] <- sample((DF[i, 'BirthYear']+18):2006,1)
}

DF <- transform(
  DF, 'Salary'= ifelse(BirthYear>=1975, (log(2007-EmployYear) + 1) * 8000,
                       (log2(2007-EmployYear) + 1) * 8000))
DF

count <- 0
count <- which(DF[, 'Salary'] > 15000 )
length(count)

vec = double(n)
cur = 2023
for(i in 1:n) {
  vec[i] <- ifelse('BirthYear'>=1975, (log(cur - DF[i,'EmployYear']) + 1) * 8000,
                            (log2(cur - DF[i,'EmployYear']) + 1) * 8000)
  
}

vec * 0.13

# for(i in 1:n) {
#   if (DF[i, 'Salary'] > 15000 ) {
#     count = count + 1
#   }
# }
# count




