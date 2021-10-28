#' Chi Square Test
#'
#' @description Runs a Chi-square statistical test to see whether there is a relationship between the gender and level of physical activity.
#' @param file_name The name of the excel file that will be read
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#' @importFrom stats chisq.test qchisq
#' @importFrom rlang .data
#' @examples
hypothesis_chi <- function(file_name){
  cat("*R is going to read the file and analyse", file_name, "and conduct a chi
        square test on the data. The assumption is that the file containing
        the data set is a csv file with the same headings so this test can be repeated
        with different data set. .*\n\n\n", sep=" ")
  import <- read.csv(file_name)
  name <- names(import)
  cat("HYPOTHESIS\nThe Null Hypothesis H0 is where there is no relationship betweeen
        gender and the level of physical activity. The alternate hypothesis H1 is
        there is a relationship between gender and the amount of physical activity.\n\n",
      sep="")
}

assumption_chi <- function(file_name){
  cat("ASSUMPTIONS\nThere are several assumptions for the Chi-square test of
        independence test. Some of them include
        1)Each study group must be independent,
        2) The value of the expected value cells should be 5 or more and no less than
        1, and
        3)The categories of the variables are mutually exclusive\n")
}

beans <- function(file_name){
  import3 <- read.csv(file_name)
  #Filter and create table to do the chi-square test

  #males that do not exercise
  project_malenone <- import3 %>% filter (.data$gender == "Male", .data$phys == "None")

  #males that have moderate exercise
  project_malemoderate <- import3 %>% filter (.data$gender == "Male", .data$phys == "Moderate")


  #males that have intense exercise
  project_maleintense <- import3 %>% filter (.data$gender == "Male", .data$phys == "Intense")

  #females that do not exercise
  project_femalenone <- import3 %>% filter (.data$gender == "Female", .data$phys == "None")

  #females that have moderate exercise
  project_femalemoderate <- import3 %>% filter (.data$gender == "Female", .data$phys == "Moderate")

  #females that have intense exercise
  project_femaleintense <- import3 %>% filter (.data$gender == "Female", .data$phys == "Intense")


  chi_square_matrix <- matrix(c(nrow(project_malenone), nrow(project_femalenone),
                                nrow(project_malemoderate),
                                nrow(project_femalemoderate), nrow(project_maleintense),
                                nrow(project_femaleintense)), nrow = 2)

  show(chi_square_matrix)
  #Issue
  chisq.test(chi_square_matrix)
  pval_chi <- round(chisq.test(chi_square_matrix)$p.value, 3)
  statistic <- chisq.test(chi_square_matrix)$statistic
  df <- chisq.test(chi_square_matrix)$parameter
  critical_valueq <- qchisq(p = 0.05, df, lower.tail = FALSE)
  cat("\nCHI SQUARE VALUES\n")
  cat("p_value = ", pval_chi, "\n")
  cat("chi_sq_statistic =", statistic, "\n")
  cat("critical_value =", critical_valueq, "\n")
  myfit3 <- list(p_value = round(chisq.test(chi_square_matrix)$p.value, 3),
                 chi_sq_statistic = chisq.test(chi_square_matrix)$statistic,
                 critical_value = qchisq(p = 0.05, df, lower.tail = FALSE))
}

decision_chi <- function(x, ...){
  UseMethod("decision_chi_test")
}

decision_chi_test <- function(myfit3){
  cat("DECISION\n")
  p_value <- myfit3$p_value
  if (p_value>=0.05) {
    decision <- "Do not reject NULL hypothesis"
  } else {
    decision <- "Reject NULL hypothesis"
  }
  cat(decision, "\n\n")
}

conclusion_chi <- function(x, ...){
  UseMethod("conclusion_chi_test")
}

conclusion_chi_test <- function(myfit3){
  p_value <- myfit3$p_value
  cat("CONCLUSION\n")
  if (p_value >= 0.05) {
    conclusion <- glue::glue("There is no evidence that there is a relationship
                               between gender and the level of physical activity
                               at a 5% signficance
                               level.")
  } else{conclusion <- glue::glue("There is evidence that there is a relationship
                                  between gender and the level of physical activity
                                    at a 5% signifance level.")
  }
  cat(conclusion, "\n\n")
}

mytest3 <- function(file_name){
  hypothesis_chi(file_name)
  assumption_chi(file_name)
  myfit3 <- beans(file_name)
  decision_chi_test(myfit3)
  conclusion_chi_test(myfit3)
}



