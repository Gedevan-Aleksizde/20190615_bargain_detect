pacman::p_load(mlr)

##### FlexMix #####
# https://github.com/mlr-org/mlr/blob/master/R/RLearner_classif_xgboost.R
makeRLearner.regr.flexmix = function() {
  makeRLearnerRegr(
    cl = "regr.flexmix",
    package = "flexmix",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="k", default=2L, lower=1L),
      makeUntypedLearnerParam(id="cluster", default=NULL),
      makeUntypedLearnerParam(id="model", default=NULL)
    ),
    properties = c("numerics", "factors", "weights"),
    name = "Flexibile Mixuture Model",
    short.name = "flexmix",
    note = ""
  )
}

trainLearner.regr.flexmix = function (.learner, .task, .subset, .weights = NULL, ...) 
{
  f <- getTaskFormula(.task)
  flexmix::flexmix(f, data=getTaskData(.task, .subset), ...)
}

predictLearner.regr.flexmix = function (.learner, .model, .newdata, ...) 
{
  predict(.model$learner.model, newdata = .newdata)
}

registerS3method("makeRLearner", "regr.flexmix",
                 makeRLearner.regr.flexmix)
registerS3method("trainLearner", "regr.flexmix", 
                 trainLearner.regr.flexmix)
registerS3method("predictLearner", "regr.flexmix", 
                 predictLearner.regr.flexmix)

##### quantregForest #####
# 設定が不完全
makeRLearner.regr.quantregForest = function() {
  makeRLearnerRegr(
    cl = "regr.quantregForest",
    package = "quantregForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="nthreads", default=1L, lower=1L),
      makeIntegerLearnerParam(id="ntree", default=500L, lower=1L),
      makeLogicalLearnerParam(id="replace", defaul=T),
      makeUntypedLearnerParam(id="strata", default=NULL, tunable=F),
      makeIntegerVectorLearnerParam(id="sampsize", lower=1),
      makeIntegerLearnerParam(id="nodesize", default=5, lower=1L, upper=Inf),
      makeIntegerLearnerParam(id="maxnodes", lower=1L, upper=Inf),
      makeLogicalLearnerParam(id="importance", default=F),
      makeLogicalLearnerParam(id="localImp", default=F),
      makeIntegerLearnerParam(id="nPerm", default=1L, lower=1L),
      makeLogicalLearnerParam(id="proximity", default=F, tunable=F),
      makeLogicalLearnerParam(id="do.trace", default=F, tunable=F),
      makeLogicalLearnerParam(id="keep.forest", default=T, tunable=F),
      makeLogicalLearnerParam(id="keep.inbag", default=F, tunable=F)
    ),
    properties = c("numerics", "factors", "weights"),
    name = "quantile regression random forest",
    short.name = "quantregForest",
    note = ""
  )
}
makeRLearner.regr.quantregForest = function() {
  makeRLearnerRegr(
    cl = "regr.quantregForest",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id="num.trees", default=500L, lower=1L),
      makeIntegerLearnerParam(id="mtry", defaul=NULL, special.vals=list(NULL), lower=1L),
      makeDiscreteLearnerParam(id="importance", values=c("none", "impurity", "impurity_corrected", "permutations"), default="none"),
      makeIntegerLearnerParam(id="seed", default=NULL, special.vals=list(NULL), tunable=F),
      makeIntegerLearnerParam(id="num.threads", lower=1L, when="both", tunable=FALSE)
    ),
    par.vals = list(num.threads = 1L),
    properties = c("numerics", "factors", "weights"),
    name = "quantile regression random forest by ranger",
    short.name = "qrf",
    note = ""
  )
}


trainLearner.regr.quantregForest = function (.learner, .task, .subset, .weights = NULL, ...) 
{
  f <- getTaskFormula(.task)
  ranger::ranger(f, data=getTaskData(.task, .subset), quantreg=T, keep.inbag=T, ...)
}

predictLearner.regr.quantregForest = function (.learner, .model, .newdata, ...) 
{
  # TODO: .5 以外の分位点を返すオプションはいるか?
  predict(.model$learner.model, newdata = .newdata, type="quantiles")$predictions[, 2]
}

registerS3method("makeRLearner", "regr.quantregForest",
                 makeRLearner.regr.quantregForest)
registerS3method("trainLearner", "regr.quantregForest", 
                 trainLearner.regr.quantregForest)
registerS3method("predictLearner", "regr.quantregForest",
                 predictLearner.regr.quantregForest)
