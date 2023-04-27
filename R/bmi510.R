
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
#' 3,2
rando = function(X, n ,replace= T){
    # Determine atomic vector or dataframe or tidy 
    if(is.vector(X)){
        res = sample(X, n, replace)
        return(res)
    }
    else if(sum(class(X) %in% c("tbl_df","tbl","data.frame")>0)){
        # sample ramdom rows from the X dataframe or tibble
        rows = sample(c(1:nrow(X)), n, replace)
        res = X[rows,]
        return(res)
    }else{
        stop("X needs to be eight an atomic vector or data frame/tibble")
    }
    
} 
