#For internal uses only
checkMat = function(mat){
  if(!is.matrix(mat) | !is.numeric(mat)){
    stop("Input data must be numeric matrices!")
  } else{
    na.fail(mat)
  }
}

checkIdx = function(idx, N){
  if(!is.numeric(idx)){
    stop("idx must be a numeric vector!")
  } else{
    idx = as.integer(idx)
    if(max(idx) > N | min(idx) < 1){
      na.fail(idx)
      stop("idx not in range [1,N]!")
    }
  }
  return(idx)
}


checkDist = function(dist){

  if(!inherits(dist, "dist")) {
    stop("dist must be a 'dist' object!")
  }
}

checkBool = function(bool){

  if(length(bool) != 1 | !is.logical(bool)){
    stop("Invalid (diag, upper) option!")
  }

}

checkMethod  = function(method){
  if(length(method) != 1 |!is.character(method)){
    stop("Invalid method!")
  }
}

checkP = function(method){
  if(length(p) != 1 |!is.numeric(p)){
    stop("Invalid p!")
  } else{
    p = as.integer(p)
    if(p < 0){
      stop("p >= 1 is required!")
    }
  }
  return(p)
}


