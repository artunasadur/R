#q1 is used to mention column index number Sepal.Length = 1 Petal.Length=3 
#alpha is used to mention for  different alpha values
q1 <- c(1,3)
alpha <- c(0.10, 0.05, 0.025, 0.02, 0.01)

len_q1 <- length(q1)
len_q2 <- length(alpha)

for(l in  1:len_q1){
  for(n in 1:len_q2){
    mydata <- iris[,c(q1[l],5)]
    
    mydata<- mydata[order((mydata[,1])),] #to order the table
    list1 <- sort(unique(mydata[,1]))
    len_list1 <- length(list1)
    
    labels1 = c("setosa", "versicolor", "virginica")
    alpha_val <- alpha[n]
    
    #tresholds <- c(qchisq(1-alpha,2), qchisq(1-alpha,2), qchisq(1-alpha,2), qchisq(1-alpha,2))
    tresholds_val <- qchisq(1-alpha_val,2)
    
    #chi square function
    chi_sqr_cal <- function(observed){
      n_row <- nrow(observed)
      n_col <- ncol(observed)
      row_sum <- rowSums(observed)
      col_sum <- colSums(observed)
      total_sum <- sum(observed)
      x <- matrix(0, nrow = n_row , ncol = n_col)
      for ( i in 1:nrow(observed)) {
        for(j in 1:ncol(observed)) {
          if(sum(row_sum[i]) > 0){
            x[i,j] <- ((observed[i,j]-((row_sum[i]*col_sum[j])/ total_sum))^2)/((row_sum[i]*col_sum[j])/ total_sum)
          }else{
            x[i,j] <- 0 
          }
        }
      }
      sum_chi= sum(x,na.rm = TRUE)
      return (sum_chi)
    }
    
    #define mid_points between numbers 
    mid_points <- numeric()
    interval<- numeric()
    for(i in 1:len_list1-1){
      mid_points[i] <- (list1[i] + list1[i+1])/2
    }
    interval <- mid_points
    mid_points = c(min(list1), mid_points, max(list1))
    
    #prepare table and as a frequency table
    x <- cut(mydata[,1], breaks =mid_points,include.lowest = TRUE)
    ola <- table(x,mydata$Species)
    chi_sqr_cal(ola)
    
    ncol(ola)
    min_chi <-0
    
    # while min_chi_list >
    while (min_chi < tresholds_val ) {
      chi_list <- numeric()
      len_x <- nrow(ola)
      #len_x <- length(sort(unique(ola[,1])))
      #calculation of chi_square value for the each adjascent row
      for(g in 1:(len_x-1)){
        chi_list[g] <- chi_sqr_cal(ola[g:(g+1),])  
      } 
      
      
      k <- which.min(chi_list)
      min_chi <-chi_list[k]
      # check chi value each time before merge
      if(min_chi<tresholds_val){
        #merge two row which has the minimum chi_square value
        ola[k+1,]<- ola[k,] + ola[k+1,]
        # remove the row which 
        ola <- ola[-k,]
        interval <- interval[-k]
      }
      else
        break
    }
    interval_final <- c(min(list1),interval, max(list1))
    table(cut(mydata[,1], breaks =interval_final,include.lowest = TRUE), mydata$Species)
    #print(interval)
    
    cat("Column Name:", colnames(iris[q1[l]]), ", Alpha Value:", alpha_val,"\n","Intervals:", interval)
    print(table(cut(mydata[,1], breaks =interval_final,include.lowest = TRUE), mydata$Species))
    cat( "\n\n")
  }
}

