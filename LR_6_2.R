library(klaR)

ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))

hc_train <- data.frame(hc, clusters)
hc_train <- hc_train[ind == 1, ]
hc_train$clusters <- as.factor(hc_train$clusters)

hc_test <- hc[ind == 2, ]
clusters_test <- clusters[ind == 2]
hc_test <- data.frame(hc_test, clusters=clusters_test)


naive_crashes <- NaiveBayes(clusters~., data=hc_train)
naive_crashes$tables


par(mfrow=c(3,1))
plot(naive_crashes,lwd = 2, legendplot=FALSE)


pred_b <- predict(naive_crashes, hc_train[, -4])$class
table(Факт = hc_train$clusters, Прогноз = pred_b)

acc_b <- mean(pred_b == hc_train$clusters)
paste0("Точность = ", round(100*acc_b, 2), "%")



library(party)
myFormula <- clusters ~ .
crashes_ctree <- ctree(myFormula, data=hc_train)


pred_t <- predict(crashes_ctree, hc_train[, -4])
table(Факт = hc_train$clusters, Прогноз = pred_t)

acc_t <- mean(pred_t == hc_train$clusters)
paste0("Точность = ", round(100*acc_t, 2), "%")


dev.off()
plot(crashes_ctree)



library(randomForest)
rf <- randomForest(clusters ~ ., data=hc_train, ntree=100, proximity=TRUE)

pred_rf <- predict(crashes_ctree, hc_train[, -4])
table(Факт = hc_train$clusters, Прогноз = pred_rf)

acc_rf <- mean(pred_rf == hc_train$clusters)
paste0("Точность = ", round(100*acc_rf, 2), "%")