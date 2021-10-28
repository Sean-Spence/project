#' Test if there is a linear relationship
#'
#' @description Conducts a linear test to see if there is a relationship between heights and weight.
#' @param file_name The name of the excel file that will be read
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#' @importFrom utils read.csv
#' @importFrom stats lm
#' @export
hypothesis_linear <- function(file_name){
  cat("*R is going to read the file and analyse", file_name, "and conduct a linear
      regression with custom output. The assumption is that the file containing
      the data set is a csv file with the same headings so this test can be repeated
      with different data set. .*\n\n\n", sep=" ")
  import <- read.csv(file_name)
  name <- names(import)
  fit1 <- lm(height ~ weight, data = import)
  cat("HYPOTHESIS\nThe null hypothesis H0 is beta = 0, and the alternative hypothesis
       H1 is beta does not equal to 0. Beta is the true slope parameter as in the
       model ",name[3]," = alpha + beta ", name[4], " + epsilon.\n\n", sep="")
}

#' Test if there is a linear relationship
#'
#' @description Conducts a linear test to see if there is a relationship between heights and weight.
#' @param file_name The name of the excel file that will be read
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#' @import ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom stats predict residuals
#' @importFrom rlang .data
assumptions_linear <- function(file_name){
  cat("Assumptions\nThere are three assumptions which can be seen in the plot. 1)
      plotting height vs weight we can see if there are any linear relationship. 2)
      The fitted values vs residuals need to be scattered randomly and have no pattern.
      3) The distribution of residuals  needs to be a normal distribution.\n")
  import <- read.csv(file_name)
  name <- names(import)
  fit1 <- lm(height~weight, data = import)
  # drawing the scatter plot
  plot_scatter <- ggplot(import, aes(x = .data$weight, y = .data$height)) +
    geom_point(colour = 'dodgerblue4') +
    ggtitle(("Scatterplot of height vs weight")) +
    xlab(name[4]) + ylab(name[3])

  # drawing residual plot
  import$fitted <- predict(fit1)
  import$residuals <- residuals(fit1)
  plot_resid <- ggplot(import, aes(x = .data$fitted, y = .data$residuals)) +
    geom_point(colour = 'dodgerblue4') +
    ggtitle("Scatterplot of fitted values vs residuals") +
    xlab("Fitted values") + ylab("Residuals")
  #drawing the histogram
  plot_hist <- ggplot(import) +
    geom_histogram(aes(x = .data$residuals), colour = 'dodgerblue4') +
    ggtitle("Histogram of residuals") +
    xlab("Residuals")
  #TIDY TIME!
  patchwork::wrap_plots(plot_scatter, plot_resid, plot_hist, ncol = 1)
}

#' Test if there is a linear relationship
#'
#' @description Conducts a linear test to see if there is a relationship between heights and weight.
#' @param file_name The name of the excel file that will be read
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#' @importFrom utils read.csv
#' @importFrom stats lm
#' @importFrom rlang .data
#' @import dplyr
fit_linear <- function(file_name){
  import1 <- read.csv(file_name)
  name1 <- names(file_name)
  fit1 <- lm(height~weight, data = import1)
  sum_fit1 <- broom::tidy(fit1, conf.int = TRUE) %>%
    dplyr::filter(.data$term == "weight")
  cat("\nFIT - Linear Regression\n")
  cat("beta_hat = ", sum_fit1$estimate, "\n")
  cat("p_value = ", sum_fit1$p.value, "\n")
  myfit1 <- list(beta_hat = sum_fit1$estimate, p_value = sum_fit1$p.value)
}


decision_lm <- function(x, ...){
  UseMethod("decision_linear")
}

#' Test if there is a linear relationship
#'
#' @description Conducts a linear test to see if there is a relationship between heights and weight.
#' @param myfit1 the beta and p value from linear regression model
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#'
decision_linear <- function(myfit1){
  cat("DECISION\n")
  p_value <- myfit1$p_value
  if (p_value>=0.05) {
    decision <- "Do not reject NULL hypothesis"
  } else {
    decision <- "Reject NULL hypothesis"
  }
  cat(decision, "\n\n")
}

conclusion_lm <- function(x, ...){
  UseMethod("conclusion_linear")
}



#' Test if there is a linear relationship
#'
#' @description Conducts a linear test to see if there is a relationship between heights and weight.
#' @param myfit1 the beta and p value from linear regression model
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#'
conclusion_linear <- function(myfit1){
  beta_hat <- myfit1$beta_hat
  p_value <- myfit1$p_value
  cat("CONCLUSION\n")
  if (p_value >= 0.05) {
    conclusion <- glue::glue("There is no evidence that the slope (beta) is
                             different than 0. There is no significant linear
                             relationship between {name[3]} and {name[4]}.")
  } else{
    in_decrease <- ifelse(beta_hat > 0, "increases", "decreases")
    conclusion <- glue::glue("There is evidence that the slope (beta) is different
                             than 0. There is a significant linear relationship
                             between height and weight. For each unit-increse
                             in weight, height {in_decrease} by
                             {round(abs(beta_hat),4)}.")
  }
  cat(conclusion, "\n\n")
}


#' Test if there is a linear relationship
#'
#' @description Conducts a linear test to see if there is a relationship between heights and weight.
#' @param file_name The name of the excel file that will be read
#'
#' @return A statistic test with the hypothesis, tests assumptions, test statistics, a decision and conclusion.
#'
#' @export
#' @example
#' mytest1("project.csv")
mytest1 <- function(file_name){
  hypothesis_linear(file_name)
  print(assumptions_linear(file_name))
  myfit1 <- fit_linear(file_name)
  decision_linear(myfit1)
  conclusion_linear(myfit1)
}



