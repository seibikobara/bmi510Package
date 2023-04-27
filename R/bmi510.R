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









#' @title 
#  Identify the minimum and max value
#' @importFrom tidyverse
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










#' @title 
#' Function to return a detaframe or matrix where each original row is replicated.
#' @import tidyverse
#' @description
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
#' @details 


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











#' @title 
#' classes
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 



classes <- function(x){
    if(sum(class(x) %in% c("tbl_df","tbl","data.frame")>0)){
        res = unname(unlist(lapply(x, class)))
        return(res)
    }else{
        stop("X is not a dataframe")
    }
}






#' @title 
#' df_scale
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 

df_scale <- function(x, center = T, scale = T){
    # keep only numeric variables
    numerics = x[, classes(x)=="numeric"]
    # convert to matrix
    numerics = as.matrix(numerics)
    res = scale(numerics, center, scale)[,]
    return(res)
}







#' @title 
#' log_likelihood_norm
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 


log_likelihood_norm <- function(x, mean, sd){
    # density function
    sum(dnorm(x, mean, sd, log =TRUE))
}










#' @title 
#' log_likelihood_unif
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 


log_likelihood_unif <- function(x, min, max){
    # density function
    sum(dunif(x, min, max, log = TRUE))
}





#' @title 
#' log_likelihood_chisq
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 


log_likelihood_chisq <- function(x, df){
    # density function
    sum(dchisq(x, df, log = TRUE))
}







#' @title 
#' log_likelihood_f
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 


log_likelihood_f<- function(x, df1, df2){
    # density function
    sum(df(x, df1, df2, log = TRUE))
}






#' @title 
#' log_likelihood_t
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 


log_likelihood_t <- function(x, df){
    # density function
    sum(dt(x, df, log = TRUE))
}






#' @title 
#' sensitivity/spe/pre/recall/acc/f1
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 
#' 
sensitivity = function(pred, truth){
    # if statement for logical vector
    # if the input vector is logical, this vector is convered to numerical
    if(class(pred)=="logical"){
        pred = ifelse(pred==T, 1, 0)
        truth= ifelse(truth==T, 1, 0)
    }
    # extract true positives
    temp = tibble(pred = pred,
                truth = truth)
    true_positive = temp %>% filter(truth == 1)
    # sensitivity is a proportion of observed posivie among them 
    n_observed_positive = true_positive %>% filter(pred ==1) %>% nrow()
    # total true positive
    n_true_positive = nrow(true_positive)
    # sensitivity 
    sensi = n_observed_positive/n_true_positive
    return(sensi) 
}






specificity = function(pred, truth){
    # if statement for logical vector
    # if the input vector is logical, this vector is convered to numerical
    if(class(pred)=="logical"){
        pred = ifelse(pred==T, 1, 0)
        truth= ifelse(truth==T, 1, 0)
    }
    # extract true negatives
    temp = tibble(pred = pred,
                truth = truth)
    true_negative = temp %>% filter(truth == 0)
    # specificity is a proportion of observed negative among them 
    n_observed_negative = true_negative %>% filter(pred ==0) %>% nrow()
    # total true negative
    n_true_negative = nrow(true_negative)
    # specificity
    speci = n_observed_negative/n_true_negative
    return(speci) 
}




precision = function(pred, truth){
    # if statement for logical vector
    # if the input vector is logical, this vector is convered to numerical
    if(class(pred)=="logical"){
        pred = ifelse(pred==T, 1, 0)
        truth= ifelse(truth==T, 1, 0)
    }
    # extract observed positives
    temp = tibble(pred = pred,
                truth = truth)
    observed_positive = temp %>% filter(pred == 1)
    # precision is a proportion of true positive among them 
    n_true_positive = observed_positive %>% filter(truth ==1) %>% nrow()
    # total true negative
    n_observed_positive = nrow(observed_positive)
    # precision 
    preci = n_true_positive/n_observed_positive
    return(preci) 
}





recall = function(pred, truth){
    # if statement for logical vector
    # if the input vector is logical, this vector is convered to numerical
    if(class(pred)=="logical"){
        pred = ifelse(pred==T, 1, 0)
        truth= ifelse(truth==T, 1, 0)
    }
    # extract true positives
    temp = tibble(pred = pred,
                truth = truth)
    true_positive = temp %>% filter(truth == 1)
    # recall is a proportion of observed posivie among them 
    n_observed_positive = true_positive %>% filter(pred ==1) %>% nrow()
    # total true positive
    n_true_positive = nrow(true_positive)
    # recall 
    recall = n_observed_positive/n_true_positive
    return(recall) 
}




accuracy <- function(pred, truth){
    # if statement for logical vector
    # if the input vector is logical, this vector is convered to numerical
    if(class(pred)=="logical"){
        pred = ifelse(pred==T, 1, 0)
        truth= ifelse(truth==T, 1, 0)
    }
    # extract true positives
    temp = tibble(pred = pred,
                truth = truth)
    # TP
    true_positive = temp %>% filter(truth == 1)
    TP = nrow(true_positive)
    # TN
    true_negative = temp %>% filter(truth == 0)
    TN = nrow(true_negative)
    # FP
    false_positive = temp %>% filter(truth == 0) %>% filter(pred==1)
    FP = nrow(false_positive)
    # FN
    false_negative = temp %>% filter(truth == 1) %>% filter(pred==0)
    FN = nrow(false_negative)
    # accuracy
    acc = (TP + TN)/(TP + TN + FP + FN)
    return(acc)  
}





F1 = function(pred, truth){
    precision_ = precision(pred, truth)
    recall_    = recall(pred, truth)
    f1 = 2*(precision_*recall_)/(precision_ + recall_)
    return(f1)
}






#' @title 
#' minimum_n_per_group 
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 
#' 
minimum_n_per_group <- function(d, power = 0.8){
    # simulate 100 standard deviation
    sd_ = runif(n=5, min=0, max= 100)
    # temporal storage for the minimum sample size
    res_list = c()
    for(i in 1:length(sd_)){
        sd = sd_[i]
        delta = sd*d
        res = power.t.test(n=NULL, delta = delta, sd = sd, power = power) 
        minimumSampleSize = res[["n"]]
        res_list[i] = minimumSampleSize
    }
    sampleSize = mean(res_list)
    return(sampleSize)
}






#' @title 
#' r2, adjusted r2
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 
#' 
#' 
r2 <- function(pred, truth){
    SSE = sum((pred-truth)^2)
    SSR = sum((mean(truth)-pred)^2)
    SST = SSE + SSR
    res = 1 - SSE/SST
    return(res)
}




#' @title 
#' r2, adjusted r2
#' @description
#' @usage 
#' @param 
#' @examples 
#' @returns
#' @details 
#' 
#' 

adj_R2 <- function(pred, truth, n_p){
    SSE = sum((pred-truth)^2)
    SSR = sum((mean(truth)-pred)^2)
    SST = SSE + SSR
    n = length(truth)
    df_t = n-1
    df_e = n-1-n_p
    res = 1 - (SSE/df_e)/(SST/df_t)
    return(res)
}
