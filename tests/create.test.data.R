library("gbm3")
library("data.table")
library("gbm3")

a.dt <- data.table(mtcars)

set.seed(123)

a.dt[, rvar := runif(nrow(a.dt))]
a.dt[, y := mpg]
a.dt[, weight := 1]
a.dt[, offset := 0]
for (i in 1:5) a.dt <- rbind(a.dt, a.dt)
train.dt <- a.dt[rvar < 0.7, ]
setorder(train.dt, rvar)

yvar <- "mpg"
pvar <- "model.pred"
wvar <- "weight"
uvar <- xvar <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
train.fraction <- 0.5
n.trees <- 10
shrinkage <- 0.01
interaction.depth <- 2
cv.folds <- 3
formula <- stats::as.formula(paste("y", paste(xvar, collapse = "+"), sep = "~offset(offset)+"))
modelfile <- file.path("inst", "test.data", "model.rds")
pdffile <- file.path("inst", "test.output", "model.pdf")

model <- gbm(
  formula = formula,
  data = train.dt,
  weights = train.dt[, weight],
  distribution = "gaussian",
  train.fraction = train.fraction,
  n.trees = n.trees,
  shrinkage = shrinkage,
  interaction.depth = interaction.depth,
  cv.folds = cv.folds,
  keep.data = FALSE,
  verbose = FALSE
)

best.trees.test <- gbm.perf(model, plot.it = FALSE, method = "test")
a.dt[, pvar := predict(model, a.dt, best.trees.test, type = "response")]
setnames(a.dt, "pvar", pvar)
a.dt[, y := NULL]
train.dt <- a.dt[rvar < 0.7, ]
train.a.dt <- train.dt[rvar < 0.35, ]
train.b.dt <- train.dt[rvar >= 0.35, ]
test.dt <- a.dt[rvar >= 0.7, ]
model$train.dt <- train.dt
model$train.a.dt <- train.a.dt
model$train.b.dt <- train.b.dt
model$test.dt <- test.dt
# model$var.names <- uvar
model$uvar <- uvar
model$yvar <- yvar
model$pvar <- pvar
model$wvar <- wvar
model$pdffile <- pdffile
model$modelfile <- modelfile
print(model)
saveRDS(model, modelfile)

################################################################################
# model with no cv
cv.folds <- 1
modelfile <- file.path("inst", "test.data", "model.no.cv.rds")
pdffile <- file.path("inst", "test.output", "model.no.cv.pdf")
a.dt[, y := mpg]
train.dt <- a.dt[rvar < 0.7, ]
setorder(train.dt, rvar)

model <- gbm(
  formula = formula,
  data = train.dt,
  weights = train.dt[, weight],
  distribution = "gaussian",
  train.fraction = train.fraction,
  n.trees = n.trees,
  shrinkage = shrinkage,
  interaction.depth = interaction.depth,
  cv.folds = cv.folds,
  keep.data = FALSE,
  verbose = FALSE
)

best.trees.test <- gbm.perf(model, plot.it = FALSE, method = "test")
a.dt[, pvar := predict(model, a.dt, best.trees.test, type = "response")]
setnames(a.dt, "pvar", pvar)
a.dt[, y := NULL]
train.dt <- a.dt[rvar < 0.7, ]
train.a.dt <- train.dt[rvar < 0.35, ]
train.b.dt <- train.dt[rvar >= 0.35, ]
test.dt <- a.dt[rvar >= 0.7, ]
model$train.dt <- train.dt
model$train.a.dt <- train.a.dt
model$train.b.dt <- train.b.dt
model$test.dt <- test.dt
# model$var.names <- uvar
model$uvar <- uvar
model$yvar <- yvar
model$pvar <- pvar
model$wvar <- wvar
model$pdffile <- pdffile
model$modelfile <- modelfile
print(model)
saveRDS(model, modelfile)
