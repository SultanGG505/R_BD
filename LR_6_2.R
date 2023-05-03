library(klaR)

ind <- sample(2, nrow(df), replace=TRUE, prob=c(0.7, 0.3))

df_train <- data.frame(df[6:8], clusters)
df_train <- df_train[ind == 1, ]
df_train$clusters <- as.factor(df_train$clusters)

df_test <- df[ind == 2, ]
clusters_test <- clusters[ind == 2]
df_test <- data.frame(df_test, clusters=clusters_test)


naive_crashes <- NaiveBayes(clusters~., data=df_train)
naive_crashes$tables


par(mfrow=c(3,1))
plot(naive_crashes,lwd = 2, legendplot=FALSE)


pred_b <- predict(naive_crashes, df_train[, -4])$class
table(Факт = df_train$clusters, Прогноз = pred_b)

acc_b <- mean(pred_b == df_train$clusters)
paste0("Точность = ", round(100*acc_b, 2), "%")



library(party)
myFormula <- clusters ~ .
crashes_ctree <- ctree(myFormula, data=df_train)


pred_t <- predict(crashes_ctree, df_test[, -4])
table(Факт = df_test$clusters, Прогноз = pred_t)

acc_t <- mean(pred_t == df_test$clusters)
paste0("Точность = ", round(100*acc_t, 2), "%")


dev.off()
plot(crashes_ctree)



library(randomForest)
rf <- randomForest(clusters ~ ., data=df_train, ntree=100, proximity=TRUE)

pred_rf <- predict(crashes_ctree, df_train[, -4])
table(Факт = df_train$clusters, Прогноз = pred_rf)

acc_rf <- mean(pred_rf == df_train$clusters)
paste0("Точность = ", round(100*acc_rf, 2), "%")