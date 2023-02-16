x <- c(170, 160, 165, 162, 155, 153, 144, 143 )
x
pol <- c('male', 'female','male', 'female','male', 'female','male', 'female')
is.character(pol)
pol.f <- factor(pol)
pol.f
is.factor(pol.f)
is.character(pol.f)

plot(pol.f)

w <- c(80,88, 44, 40, 55, 54, 66, 65)
m<-c("L","S","XL","XXL","S","M","L")
m.f<-factor(m)
m.f

m.o<-ordered(m.f,levels=c("S","M","L","XL","XXL"))
m.o
plot(x,w,pch = as.numeric(pol.f),col = as.numeric(pol.f))
legend('topleft', pch = 1, col = 1, legend = levels(pol.f))

df <- read.csv("Лучший мультфильм.csv", sep=";", header = T,fileEncoding = "cp1251")
View(df)
getwd()