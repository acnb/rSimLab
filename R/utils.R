objFromEnv <- function(env){
  nms <- ls(envir = env, sorted = FALSE)
  # fix for R CMD check of examples ('T' and 'F' in Roxygen example
  # environments are confused with 'T'/'F' TRUE/FALSE)
  nms <- nms[nms != 'T' & nms != 'F']
  obj.ls <- mget(nms, envir = env)
  res <- lapply(obj.ls, function(o)
    ifelse(is.environment(o) | is.function(o), NA, o))
  res[!is.na(res)]
}

