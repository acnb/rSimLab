objFromEnv <- function(env){
  nms <- ls(envir = env, sorted = FALSE)
  obj.ls <- mget(nms, envir = env)
  res <- lapply(obj.ls, function(o)
    ifelse(is.environment(o) |is.function(o), NA, o))
  res[!is.na(res)]
}

