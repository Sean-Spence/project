#' Two sample t Test
#'
#' @description Runs a two sample t test to see if the average heights of male is the same as females.
#' @param file_name The name of the excel file that will be read
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#' @importFrom methods show
#' @importFrom stats quantile IQR t.test
#' @importFrom rlang .data
#' @examples
hypothesis_t <- function(file_name){
  cat("*R is going to read the file and analyse", file_name, "and conduct a two
        sample t-test on the data. The assumption is that the file containing
        the data set is a csv file with the same headings so this test can be repeated
        with different data set. .*\n\n\n", sep=" ")
  import2 <- read.csv(file_name)
  name2 <- names(import2)
  cat("HYPOTHESIS\nThe Null hypothesis H0 is meu1=meu2 where the means in the
        height of male and female are the same and the alternate hypothesis H1 is
        meu1 does not equal meu2 where the height of male does not equal to height
        of female.\n\n", sep="")
}

assumptions_and_t_test <- function(file_name){
  cat("ASSUMPTIONS\nThere are three assumptions for the t-test which need to be
        confirmed before we can move on. 1) Making sure the data has no outlier. 2)
        Assume equal variance between male and female heights. 3) Check that the
        data is normally distributed between the two categorical variables.\n")


  import2 <- read.csv(file_name)
  #removing outliers - split data into male and female, get rid of outliers then join them back together
  import_male <- import2 %>% filter (.data$gender == "Male")
  import_male_Q <- quantile(import_male$height, probs = c(0.25, 0.75), na.rm = FALSE)
  iqr_male <- IQR(import_male$height)
  up_male <- import_male_Q[2] + 1.5*iqr_male
  low_male <- import_male_Q[1] - 1.5*iqr_male
  import_male_final <- subset(import_male, import_male$height > (import_male_Q[1] - 1.5*iqr_male) & import_male$height < (import_male_Q[2] + 1.5*iqr_male))

  import_female <- import2 %>% filter (.data$gender == "Female")
  import_female_Q <- quantile(import_female$height, probs = c(0.25, 0.75), na.rm = FALSE)
  iqr_female <- IQR(import_female$height)
  up_female <- import_female_Q[2] + 1.5*iqr_female
  low_female <- import_female_Q[1] - 1.5*iqr_female
  import_female_final <- subset(import_female, import_female$height > (import_female_Q[1] - 1.5*iqr_female) & import_female$height < (import_female_Q[2] + 1.5*iqr_female))

  import_noout <- rbind(import_male_final, import_female_final)

  #Test to see if male and female or normal distributed
  normtest <- import_noout %>%
    group_by(.data$gender)
  normtest <- rstatix::shapiro_test(data=normtest, vars="height")
  plot <- ggpubr::ggqqplot(import_noout, x = "height", facet.by = "gender") +
    ggtitle("Check to see if data is normally distributed")
  show(normtest)
  show(plot)

  #Testing
  name2 <- names(file_name)
  t_test_male <- import_noout %>% filter(.data$gender == 'Male')
  t_test_female <- import_noout %>% filter(.data$gender == 'Female')
  t_test <- t.test(t_test_male$height, t_test_female$height, var.equal = TRUE,
                   alternative = "two.sided", mu=0)
  pval_t <- round(t_test$p.value, 217)
  cat("\nT-Test\n")
  cat("p_value =", pval_t, "\n")
  myfit2 <- list(pval_t = round(t_test$p.value, 217))
}

decision_t <- function(x, ...){
  UseMethod("decision_t_test")
}

decision_t_test <- function(myfit2){
  cat("DECISION\n")
  p_value <- myfit2$pval_t
  if (p_value>=0.05) {
    decision <- "Do not reject NULL hypothesis"
  } else {
    decision <- "Reject NULL hypothesis"
  }
  cat(decision, "\n\n")
}

conclusion_t <- function(x, ...){
  UseMethod("conclusion_t_test")
}

conclusion_t_test <- function(myfit2){
  p_value <- myfit2$pval_t
  cat("CONCLUSION\n")
  if (p_value >= 0.05) {
    conclusion <- glue::glue("There is no evidence that the mean of male and
                               female height are the different at a 5% signficance
                               level")
  } else{conclusion <- glue::glue("There is evidence that the mean for male and female
                              height are different at a 5% signifance level.")
  }
  cat(conclusion, "\n\n")
}

mytest2 <- function(file_name){
  hypothesis_t(file_name)
  myfit2 <- assumptions_and_t_test(file_name)
  decision_t_test(myfit2)
  conclusion_t_test(myfit2)
}


