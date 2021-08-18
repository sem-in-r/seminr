#' Measurement Instrument for the Mobile Phone Industry
#'
#' The data set is used as measurement instrument for the european customer satisfaction
#' index (ECSI) adapted to the mobile phone market, see Tenenhaus et al. (2005).
#'
#' @format A data frame with 250 rows and 24 variables:
#' \describe{
#'   \item{CUEX1}{Expectations for the overall quality of "your mobile phone provider" at the moment you became customer of this provider}
#'   \item{CUEX2}{Expectations for "your mobile phone provider" to provide products and services to meet your personal need}
#'   \item{CUEX3}{How often did you expect that things could go wrong at "your mobile phone provider}
#'   \item{CUSA1}{Overall satisfaction}
#'   \item{CUSA2}{Fulfillment of expectations}
#'   \item{CUSA3}{How well do you think "your mobile phone provider" compares with your ideal mobile phone provider?}
#'   \item{CUSCO}{You complained about "your mobile phone provider" last year. How well, or poorly, was your most recent complaint handled or You did not complain about "your mobile phone provider" last year. Imagine you have to complain to "your mobile phone rovider" because of a bad quality of service or product. To what extent do you think that "your mobile phone provider" will care about your complaint?}
#'   \item{CUSL1}{If you would need to choose a new mobile phone provider how likely is it that you would choose "your provider" again?}
#'   \item{CUSL2}{Let us now suppose that other mobile phone providers decide to lower their fees and prices, but "your mobile phone provider" stays at the same level as today. At which level of difference (in percentage) would you choose another mobile phone provider?}
#'   \item{CUSL3}{If a friend or colleague asks you for advice, how likely is it that you would recommend "your mobile phone provider"?}
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
#'   \item{PERV1}{Given the quality of the products and services offered by "your mobile phone provider" how would you rate the fees and prices that you pay for them?}
#'   \item{PERV2}{Given the fees and prices that you pay for "your mobile phone provider" how would you rate the quality of the products and services offered by "your mobile phone provider"?}
#' }
#'
#' @details The data frame mobi contains the observed data for the model specified by ECSImobi.
#' @references Tenenhaus, M., V. E. Vinzi, Y.-M. Chatelin, and C. Lauro (2005) PLS path modeling. Computational Statistics & Data Analysis 48, 159-205.
#' @examples data("mobi")
#'
"mobi"

#' Measurement Instrument for the Corporate Reputation Model
#'
#' The data set is used as measurement instrument for corporate reputation.
#'
#' @format A data frame with 344 rows and 46 variables:
#' \describe{
#'   \item{serviceprovider}{A categorical variable for the service provider: 1, 2, 3, or 4.}
#'   \item{servicetype}{A categorical variable for the service type: 1=Prepaid plan (n=125); 2=Contract plan (n=219).}
#'   \item{csor_1}{The company behaves in a socially conscious way.}
#'   \item{csor_2}{The company is forthright in giving information to the public.}
#'   \item{csor_3}{The company has a fair attitude toward competitors.}
#'   \item{csor_4}{The company is concerned about the preservation of the environment.}
#'   \item{csor_5}{The company is not only concerned about the profits.}
#'   \item{csor_global}{Please assess the extent to which the company acts in socially conscious ways 0 (not at all) to 7 (definitely).}
#'   \item{attr_1}{The company is succesful in attracting high-quality employees.}
#'   \item{attr_2}{I could see myself working at the company.}
#'   \item{attr_3}{I like the physical appearance of the company/buildings/shops, etc.}
#'   \item{attr_global}{Please assess the company’s overall attractiveness; 0=very low; 7=very high.}
#'   \item{perf_1}{The company is a very well managed company.}
#'   \item{perf_2}{The company is an economically stable company.}
#'   \item{perf_3}{The business risk for the company is modest compared to its competitors.}
#'   \item{perf_4}{The company has growth potential.}
#'   \item{perf_5}{The company has a clear vision about the future of the company.}
#'   \item{perf_global}{Please assess the company’s overall performance; 0=very low; 7=very high.}
#'   \item{qual_1}{The products/services offered by the company are of high quality.}
#'   \item{qual_2}{The company is an innovator, rather than an imitator with respect to industry.}
#'   \item{qual_3}{The company's services/products offer good quality for money.}
#'   \item{qual_4}{The services the company offered are good.}
#'   \item{qual_5}{Customer concerns are held in high regard at the company.}
#'   \item{qual_6}{The company is a reliable partner for customers.}
#'   \item{qual_7}{The company is a trustworthy company.}
#'   \item{qual_8}{I have a lot of respect for the company.}
#'   \item{qual_global}{Please assess the overall quality of the company’s activities; 0=very low; 7=very high.}
#'   \item{like_1}{The company is a copany that I can better identify with than other companies.}
#'   \item{like_2}{The company is a company that I would regret more not having if it no longer existed than other companies.}
#'   \item{like_3}{I regard the company as a likeable company.}
#'   \item{comp_1}{The company is a top competitor in its market.}
#'   \item{comp_2}{As far as I know, the company is recognized worldwide.}
#'   \item{comp_3}{I believe that the company performs at a premium level.}
#'   \item{cusl_1}{I would recommend the company to friends and relatives.}
#'   \item{cusl_2}{If I had to choose again, I would choose the company as my mobile phone services provider.}
#'   \item{cusl_3}{I will remain a customer of the company in the future.}
#'   \item{cusa}{I am satisfied with company.}
#'   \item{age}{?}
#'   \item{education}{Categorical for education.}
#'   \item{occupation}{Categorical for type of occupation.}
#'   \item{nphh}{?}
#'   \item{sample_type}{?}
#'   \item{mga_1}{Multi Group Analysis 1.}
#'   \item{mga_2}{Multi Group Analysis 2.}
#'   \item{mga_3}{Multi Group Analysis 3.}
#'   \item{mga_4}{Multi Group Analysis 4.}
#'   \item{switch_1}{It takes me a great deal of time to switch to another company.}
#'   \item{switch_2}{It costs me too much to switch to another company.}
#'   \item{switch_3}{It takes a lot of effort to get used to a new company with its specific "rules" and practices.}
#'   \item{switch_4}{In general, it would be a hassle switching to another company.}
#' }
#'
#' @details The data frame mobi contains the observed data for the model specified by Corporate Reputation.
#' @references Hair, J. F., Hult, G. T. M., Ringle, C. M., and Sarstedt, M. (2017). A Primer on Partial Least Squares Structural Equation Modeling (2nd ed.). Thousand Oakes, CA: Sage.
#' @examples data("corp_rep_data")
#'
"corp_rep_data"

#' A Second Measurement Instrument for the Corporate Reputation Model
#'
#' The data set is used as measurement instrument for corporate reputation.
#'
#' @format A data frame with 347 rows and 49 variables:
#' \describe{
#'   \item{servicetype}{A categorical variable for the service type: 1=Postpaid plan; 2=Prepaid plan.}
#'   \item{serviceprovider}{A categorical variable for the service provider: 1, 2, 3, or 4.}
#'   \item{cusa}{If you consider your experiences with "company", how satisfied are you with "company"?}
#'   \item{cusl_1}{I would recommend "the company" to friends and relatives.}
#'   \item{cusl_2}{If I had to choose again, I would choose "the company" as my mobile phone services provider.}
#'   \item{cusl_3}{I will remain a customer of "the company" in the future.}
#'   \item{qual_1}{The products/services offered by "the company" are of high quality.}
#'   \item{qual_2}{"The company" is an innovator, rather than an imitator with respect to the mobile phone service industry.}
#'   \item{qual_3}{"The company's" services/products offer good quality for money.}
#'   \item{qual_4}{The services "the company" offers are good.}
#'   \item{qual_5}{Customer concerns are held in high regard at "the company".}
#'   \item{qual_6}{"The company" is a reliable partner for customers.}
#'   \item{qual_7}{"The company" is a trustworthy company.}
#'   \item{qual_8}{I have a lot of respect for "the company".}
#'   \item{perf_1}{"The company" is a very well managed company.}
#'   \item{perf_2}{"The company" is an economically stable company.}
#'   \item{perf_3}{The business risk of "the company" is reasonable compared to its competitors.}
#'   \item{perf_4}{The growth of "the company" is promising.}
#'   \item{perf_5}{"The company" has a clear vision about the future of the company.}
#'   \item{csor_1}{"The company" behaves in a socially conscious way.}
#'   \item{csor_2}{"The company" is honest in giving information to the public.}
#'   \item{csor_3}{"The company" competes fairly in the indsutry.}
#'   \item{csor_4}{"The company" cares for the preservation of the environment.}
#'   \item{csor_5}{"The company" is doing more than just making profits.}
#'   \item{attr_1}{"The company" is succesful in attracting high-quality employees.}
#'   \item{attr_2}{I could see myself working at "the company".}
#'   \item{attr_3}{I like the physical appearance of "the company" (company/buildings/shops, etc.).}
#'   \item{comp_1}{"The company" is a top competitor in its market.}
#'   \item{comp_2}{As far as I know, "the company" is recognized worldwide.}
#'   \item{comp_3}{I believe that "the company" performs at a premium level.}
#'   \item{like_1}{"The company" is a company that I can better identify with than other companies.}
#'   \item{like_2}{When comparing with other companies, "The company" is the company I would regret more if it no longer existed.}
#'   \item{like_3}{I regard "the company" as a likeable company.}
#'   \item{qual_global}{Please assess the general quality of "the company".}
#'   \item{perf_global}{Please assess the general performance of "the company".}
#'   \item{csor_global}{Please assess the extent to which "the company" acts in socially conscious ways.}
#'   \item{attr_global}{Please assess the attractiveness of "the company".}
#'   \item{switch_1}{It takes me a great deal of time to switch to another mobile phone services provider.}
#'   \item{switch_2}{It costs me too much to switch to another mobile phone services provider.}
#'   \item{switch_3}{It takes a lot of effort to get used to a new mobile phone services provider with its specific "rules" and practices.}
#'   \item{switch_4}{In general, it would be a hassle switching to another mobile phone services provider.}
#' }
#'
#' @details The data frame mobi contains the observed data for the model specified by Corporate Reputation.
#' @references Sarstedt, M., Hair Jr, J. F., Cheah, J. H., Becker, J. M., & Ringle, C. M. (2019). How to specify, estimate, and validate higher-order constructs in PLS-SEM. Australasian Marketing Journal (AMJ), 27(3), 197-211.
#' @examples data("corp_rep_data2")
#'
"corp_rep_data2"

#' Measurement Instrument for the Influencer Model
#'
#' The data set is used as measurement instrument for the Influencer Model which is used in
#' Partial Least Squares Structural Equation Modeling (PLS-SEM) Using R - A Workbook (2021)
#' Hair, J.F. (Jr), Hult, T.M., Ringle, C.M., Sarstedt, M., Danks, N.P., and Ray, S.
#'
#' @format A data frame with 250 rows and 24 variables:
#' \describe{
#'
#'   \item{sic_1}{The influencer reflects who I am.}
#'   \item{sic_2}{I can identify with the influencer.}
#'   \item{sic_3}{I feel a personal connection to the influencer.}
#'   \item{sic_4}{I (can) use the influencer to communicate who I am to other people.}
#'   \item{sic_5}{I think the influencer (could) help(s) me become the type of person I want to be.}
#'   \item{sic_6}{I consider the influencer to be "me".}
#'   \item{sic_7}{The influencer suits me well.}
#'   \item{sic_global}{My personality and the personality of the influencer relate accordingly to one another.}
#'   \item{pq_1}{The product has excellent quality.}
#'   \item{pq_2}{The product looks to be reliable and durable.}
#'   \item{pq_3}{The product will have fewer problems.}
#'   \item{pq_4}{The product has excellent quality features.}
#'   \item{pl_1}{I dislike the product (reverse coded).}
#'   \item{pl_2}{The product is appealing to me.}
#'   \item{pl_3}{The presented product raises a positive feeling in me.}
#'   \item{pl_4}{The product is interesting to me.}
#'   \item{pi_1}{It is very likely that I will purchase this product.}
#'   \item{pi_2}{I will purchase this product the next time I need it.}
#'   \item{pi_3}{I would definitely try the product out.}
#'   \item{pi_4}{I would recommend this product to my friends.}
#'   \item{pi_5}{I am willing to purchase this product.}
#'   \item{pic_1}{The influencer is qualified.}
#'   \item{pic_2}{The influencer is competent.}
#'   \item{pic_3}{The influencer is an expert.}
#'   \item{pic_4}{The influencer is experienced.}
#'   \item{pic_5}{The influencer is knowledgeable.}
#'   \item{wtp}{Please state your willingness to pay (in Euro) for the presented product.}
#'   \item{influencer_group}{A binary variable indicating which group the influencer belongs to.}
#' }
#'
#' @details The data frame influencer_data contains the observed data for the model specified in the Influencer Model.
#' @examples data("influencer_data")
#'
"influencer_data"
