Time <- rep(c(0,10))
Num <- 1:10
RES <- double(10)

#t0 <- strptime(Sys.time(), "%Y-%m-%d%H:%M:%S")
#t0

#1

t0 <- Sys.time()
xA <- seq(100,200,5)
RES[1] <- sum(xA)
RES[1]

t1 <- Sys.time()
Time[1] <- difftime(t1,t0,units = "mins")

#2

col <- length(xA)
RES[2] <- col
RES[2]
t2 <- Sys.time()
Time[2] <- difftime(t2,t1,units = "mins")


RES[3] <- mean(xA)
RES[3]
t3 <- Sys.time()
Time[3] <- difftime(t3,t2,units = "mins")

#3

norm_vec <- rnorm(col+7, mean = 5)
RES[4] <- round(sd(norm_vec))
RES[4]
t4 <- Sys.time()
Time[4] <- difftime(t4,t3,units = "mins")

#4

arr <- array(xA,dim = c(5,round(col/5)))
RES[5] <- round(sum(sin(arr)),4)
RES[5]
t5 <- Sys.time()
Time[5] <- difftime(t5,t4,units = "mins")


#5
matr <- matrix(data = xA[1:20],nrow = 5)
matr <- matr[-c(2,5),]
RES[6] <- nrow(matr) + ncol(matr)
t6 <- Sys.time()
Time[6] <- difftime(t6,t5,units = "mins")

#6

re <- list(rep(c(T,F), each =5),rep(c(T,F), each =5),rep(c(T,F), each =5))
RES[7] <- sum(re[[1]]) > 0
t7 <- Sys.time()
Time[7] <- difftime(t7,t6,units = "mins")

#7

RES[8] <- identical(arr,matr)
t8 <- Sys.time()
Time[8] <- difftime(t8,t7,units = "mins")

#8

arr_new <- array(arr,dim(matr))
RES[9] <- identical(arr,matr)
t9 <- Sys.time()
Time[9] <- difftime(t9,t8,units = "mins")

#9

t10 <- sum(Time); Time[10] <-t10
KR <- data.frame(Num, RES, Time)
View(KR)



