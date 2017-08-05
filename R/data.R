#' Measurement Instrument for the Mobile Phone Industry
#'
#' The data set is used as measurement instrument for the european customer satisfaction
#' index (ECSI) adapted to the mobile phone market, see Tenenhaus et al. (2005).
#'
#' @format A data frame with 250 rows and 24 variables:
#' \describe{
#'   \item{CUEX1}{Expectations for the overall quality of “your mobile phone provider” at the moment you became customer of this provider}
#'   \item{CUEX2}{Expectations for “your mobile phone provider” to provide products and services to meet your personal need}
#'   \item{CUEX3}{How often did you expect that things could go wrong at “your mobile phone provider}
#'   \item{CUSA1}{Overall satisfaction}
#'   \item{CUSA2}{Fulfillment of expectations}
#'   \item{CUSA3}{How well do you think “your mobile phone provider” compares with your ideal mobile phone provider?}
#'   \item{CUSCO}{You complained about “your mobile phone provider” last year. How well, or poorly, was your most recent complaint handled or You did not complain about “your mobile phone provider” last year. Imagine you have to complain to “your mobile phone rovider” because of a bad quality of service or product. To what extent do you think that “your mobile phone provider” will care about your complaint?}
#'   \item{CUSL1}{If you would need to choose a new mobile phone provider how likely is it that you would choose “your provider” again?}
#'   \item{CUSL2}{Let us now suppose that other mobile phone providers decide to lower their fees and prices, but “your mobile phone provider” stays at the same level as today. At which level of difference (in percentage) would you choose another mobile phone provider?}
#'   \item{CUSL3}{If a friend or colleague asks you for advice, how likely is it that you would recommend “your mobile phone provider”?}
#'   \item{IMAG1}{It can be trusted what it says and does}
#'   \item{IMAG2}{It is stable and firmly established}
#'   \item{IMAG3}{It has a social contribution to society}
#'   \item{IMAG4}{It is concerned with customers}
#'   \item{IMAG5}{It is innovative and forward looking}
#'   \item{PERQ1}{Overall perceived quality}
#'   \item{PERQ2}{Technical quality of the network}
#'   \item{PERQ3}{Customer service and personal advice offered}
#'   \item{PERQ4}{Quality of the services you use}
#'   \item{PERQ5}{Range of services and products offered}
#'   \item{PERQ6}{Reliability and accuracy of the products and services provided}
#'   \item{PERQ7}{Clarity and transparency of information provided}
#'   \item{PERV1}{Given the quality of the products and services offered by “your mobile phone provider” how would you rate the fees and prices that you pay for them?}
#'   \item{PERV2}{Given the fees and prices that you pay for “your mobile phone provider” how would you rate the quality of the products and services offered by “your mobile phone provider”?}
#' }
#'
#' @details The data frame mobi contains the observed data for the model specified by ECSImobi.
#' @references Tenenhaus, M., V. E. Vinzi, Y.-M. Chatelin, and C. Lauro (2005) PLS path modeling. Computational Statistics & Data Analysis 48, 159-205.
#' @examples data("mobi")
#'
"mobi"
