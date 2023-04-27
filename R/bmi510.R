#' @title 
#' Apply functions over a vector or dataframe (tibble) 
#' @description
#' Returns a specified samples or rows from a vector or data frame.
#' @usage
#' rando(X, n, replace = TRUE)
#' @param X An atomic vector, data frame, or tibble
#' @param n n samples will be sampled from X
#' @param replace default TRUE, this allows sampling with replacement
#' @examples
#' X = c(1,2,3)
#' rando(X, 2, replace = F)
#' @returns 
#' vector or data frame
#'@details 
#' This function take a vector or dataframe. If a vector is the input, this function returns a specified number of random samples from this vector. If a data frame is the imput, this function picks a specified number of random rows and returns a data frame with these rows. 

rando = function(X, n ,replace= T){
    # Determine atomic vector or dataframe or tidy 
    if(is.vector(X)){
        res = sample(X, n, replace)
        return(res)
    }else if(sum(class(X) %in% c("tbl_df","tbl","data.frame")>0)){
        # sample ramdom rows from the X dataframe or tibble
        rows = sample(c(1:nrow(X)), n, replace)
        res = X[rows,]
        return(res)
    }else if(is.matrix(X)){
        # sample ramdom rows from the X dataframe or tibble
        rows = sample(c(1:nrow(X)), n, replace)
        res = X[rows,]
        return(res)
    }else{
        stop("X needs to be eight an atomic vector or data frame/tibble")
    }    
} 






#' Identify the minimum and max value
#' @import tidyverse
#' @usage 
#' is_min(x, na.rm=T)
#' is_max(x, na.rm=T)
#' @param x A vector
#' @examples
#' x = c(1,2,3)
#' is_min(x, na.rm=T)
#' # returns
#' # [1] TRUE, FALSE, FALSE
#' @returns
#' A logical vector

is_min <- function(x, na.rm=T){
    if(is.vector(x)){
        min_ = min(x, na.rm=na.rm)
        x==min_    
    }else{
        stop("X is not a vector")
    }
}


is_max <- function(x, na.rm=T){
    if(is.vector(x)){
        max_ = max(x, na.rm=na.rm)
        x==max_    
    }else{
        stop("X is not a vector")
    }
}











#' Function to return a detaframe or matrix where each original row is replicated.
#' @import tidyverse
#' @usage 
#' rep_mat(X, M=1, N=1)
#' @param X A data frame or matrix
#' @param M The number of replication for the rows
#' @param N The number of replication for the columns
#' @examples
#' X = matrix(c(1:10), ncol = 5)
#' rep_mat(X, M= 2, N=2)
#' # This returs
#' #  
#' @returns
#' A data frame or matrix



rep_mat <- function(X, M=1, N=1){
    if(sum(class(X) %in% c(c("tbl_df","tbl","data.frame")))>0 | is.matrix(X)==T){
        # replicate for rows
        temp = X[rep(1:nrow(X), M), ]
        # replciate for columns
        temp1 = temp[, rep(1:ncol(temp), N)]
        return(temp1)
    }else{
        stop("X is not data frame or matrix")
    }
}





#' classes
#' @usage 
#' classes(x)
#' @param x data frame
#' @examples 
#' b = data.frame(x = c(1,2,3),
#'           y = c(4,5,6),
#'           z = c(7,8,9),
#'           e = c("a", "b","d"))
#' classes(b)
#' # return
#' "numeric"   "numeric"   "numeric"   "character"
#' @returns
#' vector


classes <- function(x){
    if(sum(class(x) %in% c("tbl_df","tbl","data.frame")>0)){
        res = unname(unlist(lapply(x, class)))
        return(res)
    }else{
        stop("X is not a dataframe")
    }
}




