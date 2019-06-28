pacman::p_load(mlr)
##### FlexMix #####
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
    short.name = "earth",
    note = ""
  )
}

trainLearner.regr.flexmix = function (.learner, .task, .subset, .weights = NULL, ...) 
{
  f = getTaskFormula(.task)
  flexmix::flexmix(f, data = getTaskData(.task, .subset), ...)
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