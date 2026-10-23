library(rpart)
library(rpart.plot)
library(randomForest)
dir.create("outputs", showWarnings = FALSE)

soil_raw <- read.csv("MultiFactor_data.csv")
factor_names <- c("P", "C", "L", "N", "A", "I", "SU", "FU", "H", "M", "S", "D")
soil_once <- soil_raw[soil_raw$remark %in% c("CT", "1", "2", "5", "8"), ]

nrow(soil_raw)
nrow(soil_once)
head(soil_once[, c("Decom", factor_names)])
table(as.matrix(soil_once[, factor_names]), useNA = "ifany")
all(rowSums(soil_once[, factor_names]) == soil_once$Lv)

keep <- complete.cases(soil_once[, c("Decom", factor_names)])
excluded_ids <- soil_once$X[!keep]
excluded_ids
model_data <- soil_once[keep, c("Decom", factor_names)]
rownames(model_data) <- paste0("record_", soil_once$X[keep])
dim(model_data)

set.seed(505)
train_rows <- sample(seq_len(nrow(model_data)), size = floor(0.75 * nrow(model_data)))
train_data <- model_data[train_rows, ]
test_data <- model_data[-train_rows, ]
nrow(train_data)
nrow(test_data)

tree_model <- rpart(
  Decom ~ .,
  data = train_data,
  method = "anova",
  control = rpart.control(maxdepth = 3, minbucket = 10, cp = 0.01, xval = 0)
)
print(tree_model)

rpart.plot(tree_model, type = 2, extra = 1, yesno = 2, fallen.leaves = TRUE,
           digits = 3, roundint = FALSE, box.palette = "GnBu",
           main = "CART: predicted litter mass loss")



first_cart_pred <- predict(tree_model, newdata = test_data[, factor_names])
first_cart_rmse <- sqrt(mean((test_data$Decom - first_cart_pred)^2))
100 * first_cart_rmse

set.seed(505)
cv_folds <- sample(rep(1:5, length.out = nrow(model_data)))
table(cv_folds)
tree_cv <- rpart(
  Decom ~ .,
  data = model_data,
  method = "anova",
  control = rpart.control(maxdepth = 3, minbucket = 10, cp = 0.01, xval = cv_folds)
)
printcp(tree_cv)
print(tree_cv)

best_row <- which.min(tree_cv$cptable[, "xerror"])
best_cp <- tree_cv$cptable[best_row, "CP"]
best_cp
tree_cv_pruned <- prune(tree_cv, cp = best_cp)
print(tree_cv_pruned)

rpart.plot(tree_cv_pruned, type = 2, extra = 1, yesno = 2, fallen.leaves = TRUE,
           digits = 3, roundint = FALSE, box.palette = "GnBu",
           main = "CART: tree size selected by five-fold CV")

cart_cv_pred <- xpred.rpart(tree_cv, xval = cv_folds)[, best_row]





set.seed(505)
rf_model <- randomForest(
  x = model_data[, factor_names],
  y = model_data$Decom,
  ntree = 25,
  mtry = 4,
  nodesize = 5,
  importance = TRUE,
  nPerm = 50
)
print(rf_model)

rf_oob_rmse <- sqrt(tail(rf_model$mse, 1))
100 * rf_oob_rmse

perm_importance <- importance(rf_model, type = 1, scale = FALSE)
importance_table <- data.frame(
  factor = rownames(perm_importance),
  delta_mse = as.numeric(perm_importance[, 1]),
  row.names = NULL
)

importance_table <- importance_table[order(-importance_table$delta_mse), ]
print(importance_table, row.names = FALSE)

par(mar = c(5, 5, 3, 1))
barplot(rev(importance_table$delta_mse),
        names.arg = rev(importance_table$factor),
        horiz = TRUE, las = 1, col = "#39745b", border = NA,
        xlab = "Increase in OOB MSE (proportion squared)",
        main = "Random forest: permutation importance")
abline(v = 0, col = "grey40", lty = 2)
