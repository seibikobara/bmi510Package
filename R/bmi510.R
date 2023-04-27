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
#' @export

rando <- function(X, n ,replace= T){
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
#' @aliases is_min
#' @aliases is_max
#' @export is_min is_max
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
#' @export
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





#' Determining a class for each column vector in a data frame
#' @export
#' @usage 
#' classes(x)
#' @param x a data frame
#' @examples 
#' b = data.frame(x = c(1,2,3),
#'           y = c(4,5,6),
#'           z = c(7,8,9),
#'           e = c("a", "b","d"))
#' classes(b)
#' # return
#' "numeric"   "numeric"   "numeric"   "character"
#' @returns
#' a character vector


classes <- function(x){
    if(sum(class(x) %in% c("tbl_df","tbl","data.frame")>0)){
        res = unname(unlist(lapply(x, class)))
        return(res)
    }else{
        stop("X is not a dataframe")
    }
}




#' df_scale
#' @export
#' @usage 
#' df_scale(x, center = T, scale = T)
#' @param x a data frame
#' @examples 
#' x = tibble(x = c(1,2,3),
#'            y = c(4,5,6),
#'            z = c(7,8,9)
#'           )
#' df_scale(x)
#' # returns
#'       x  y  z
#' [1,] -1 -1 -1
#' [2,]  0  0  0
#' [3,]  1  1  1
#' @returns
#' A matrix 

df_scale <- function(x, center = T, scale = T){
    # keep only numeric variables
    numerics = x[, classes(x)=="numeric"]
    # convert to matrix
    numerics = as.matrix(numerics)
    res = scale(numerics, center, scale)[,]
    return(res)
}






#' Sum of log likelihood for a particular probability distribution 
#' @export
#' @aliases log_likelihood_norm
#' @aliases log_likelihood_unif
#' @aliases log_likelihood_chisq
#' @aliases log_likelihood_f
#' @aliases log_likelihood_t
#' 
#' @usage 
#' # Normal distribition
#' log_likelihood_norm(x, mean, sd)
#' 
#' # Uniform distribution
#' log_likelihood_unif(x. min, max)
#' 
#' # Chi square distribution
#' log_likelihood_chisq(x, df)
#' 
#' # F distribution
#' log_likelihood_f(x, df1, df2)
#' 
#' # t distribution
#' log_likelihood_t(x, df)
#'
#' @param x a vector
#' @examples
#' x = c(10,11,12,13)
#' log_likelihood_norm(x, mean=11, sd = 1)
#' # returns
#' -6.675754
#' @returns
#' The sum of the log likelihood



log_likelihood_norm <- function(x, mean, sd){
    # density function
    sum(dnorm(x, mean, sd, log =TRUE))
}


log_likelihood_unif <- function(x, min, max){
    # density function
    sum(dunif(x, min, max, log = TRUE))
}


log_likelihood_chisq <- function(x, df){
    # density function
    sum(dchisq(x, df, log = TRUE))
}


log_likelihood_f<- function(x, df1, df2){
    # density function
    sum(df(x, df1, df2, log = TRUE))
}



log_likelihood_t <- function(x, df){
    # density function
    sum(dt(x, df, log = TRUE))
}










#' Calculate the sensitivity, specificity, precision, recall, accuracy, and F1 scores
#' @export
#' @usage 
#' sensitivity(pred, truth)
#' specificity(pred, truth)
#' precision(pred, truth)
#' recall(pred, truth)
#' accuracy(pred, truth)
#' f1(pred, truth)
#' @aliases specificity
#' @aliases precision
#' @aliases recall
#' @aliases accuracy
#' @aliases f1
#' 
#' @param pred A vector of predicted binary or boolean values
#' @param truth A vector of true binary or boolean values 
#' @examples
#' pred = c(rep(1,40),rep(0,10), rep(1,45),rep(0,5))
#' truth = c(rep(1, 50),rep(0, 50))
#' sensitivity(pred, truth)
#' # returns
#' 0.8
#' @returns
#' A value 
#' 
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
    true_positive = temp |> filter(truth == 1)
    # sensitivity is a proportion of observed posivie among them 
    n_observed_positive = true_positive |> filter(pred ==1) |> nrow()
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
    true_negative = temp |> filter(truth == 0)
    # specificity is a proportion of observed negative among them 
    n_observed_negative = true_negative |> filter(pred ==0) |> nrow()
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
    observed_positive = temp |> filter(pred == 1)
    # precision is a proportion of true positive among them 
    n_true_positive = observed_positive |> filter(truth ==1) |> nrow()
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
    true_positive = temp |> filter(truth == 1)
    # recall is a proportion of observed posivie among them 
    n_observed_positive = true_positive |> filter(pred ==1) |> nrow()
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
    true_positive = temp |> filter(truth == 1)
    TP = nrow(true_positive)
    # TN
    true_negative = temp |> filter(truth == 0)
    TN = nrow(true_negative)
    # FP
    false_positive = temp |> filter(truth == 0) |> filter(pred==1)
    FP = nrow(false_positive)
    # FN
    false_negative = temp |> filter(truth == 1) |> filter(pred==0)
    FN = nrow(false_negative)
    # accuracy
    acc = (TP + TN)/(TP + TN + FP + FN)
    return(acc)  
}




f1 = function(pred, truth){
    precision_ = precision(pred, truth)
    recall_    = recall(pred, truth)
    f1 = 2*(precision_*recall_)/(precision_ + recall_)
    return(f1)
}







#' Determine the minimum sample based on t distribution 
#' @export
#' @usage 
#' minimum_n_per_group(d, power)
#' @param d cohen's effect size
#' @param power power
#' @examples
#' minimum_n_per_group(1.0, power=0.8)
#' # returns
#' 17 
#' @returns
#' A integer
#' 
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
    rounded = round(sampleSize)
    return(rounded)
}




#' @export
temp <- function(){
    print("Hello!!!")
}




#' Calculate the R square
#' @export
#' @usage 
#' r2(pred, truth)
#' @param pred A vector of predicted numerical values
#' @param truth A vector of observed numerical values
#' @examples
#' pred = c(1,2,3,4,5)
#' truth = c(1.2, 2.2, 3.0, 3.8, 5.5)
#' r2(pred, truth)
#' # returns
#' 0.9646542
#' @returns
#' A numerical value
#' 
#' 


r2 <- function(pred, truth){
    SSE = sum((pred-truth)^2)
    SSR = sum((mean(truth)-pred)^2)
    SST = SSE + SSR
    res = 1 - SSE/SST
    return(res)
}




#' Calculate the adjusted R square
#' @export
#' @usage 
#' adj_R2(pred, truth, n_p)
#' @param pred A vector of predicted numerical values
#' @param truth A vector of observed numerical values
#' @param n_p The number of parameters to estimate except the intercept
#' @examples
#' pred = c(1,2,3,4,5)
#' truth = c(1.2, 2.2, 3.0, 3.8, 5.5)
#' adj_R2(pred, truth, 2)
#' # returns
#' 0.9293084
#' @returns
#' A numerical value
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
