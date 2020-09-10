library(DBI)
db <- 'csv_database'  #provide the name of your db
host_db <- '192.168.1.24' #i.e. # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'  
db_port <- '5432'  # or any other port specified by the DBA
db_user <- 'python'
db_password <- 'python'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password) 
#dbListTables(con) 
treas2010 <- dbGetQuery(con, "Select agency_id, awarding_agency_code, awarding_agency_name, 
                        awarding_sub_tier_agency_c, awarding_sub_tier_agency_n, awardee_or_recipient_legal, 
                        period_of_performance_star, period_of_performance_curr, naics, naics_description, 
                        base_and_all_options_value, action_date, type_of_contract_pric_desc, contract_award_type_desc,
                        place_of_performance_state, place_of_perfor_state_desc, place_of_perform_country_c, 
                        place_of_perf_country_desc, contracting_officers_deter, contracting_officers_desc, 
                        extent_competed, extent_compete_description, purchase_card_as_payment_m, 
                        purchase_card_as_paym_desc, multi_year_contract, product_or_service_code,
                        product_or_service_co_desc, solicitation_procedures, solicitation_procedur_desc, 
                        type_set_aside, type_set_aside_description, type_of_contract_pricing, 
                        type_of_contract_pric_desc from Transaction_FPDS 
                        WHERE naics in ('541511','541512','541513','541519') and  awarding_agency_code = '020' 
                        and (action_date::date >= '2010-01-01' and action_date::date <= '2010-12-31')
                        and contract_award_type in ('A', 'B', 'C', 'D') 
                        and base_and_all_options_value::numeric > 1000 
                        and place_of_perform_country_c = 'USA'
                        ORDER BY awarding_sub_tier_agency_n")
dbDisconnect(con)
save(treas2010, file = "treas2010.RData")
load("treas2010.RData")





boxplot(as.numeric(base_and_all_options_value)~naics, data = dod2010)
boxplot(log(as.numeric(base_and_all_options_value))~naics, data = dod2010)
boxplot(log10(as.numeric(base_and_all_options_value))~naics, data = dod2010)
boxplot(log10(as.numeric(base_and_all_options_value))/10^6~naics, data = dod2010)
boxplot(as.numeric(base_and_all_options_value)/10^6~naics, data = dod2010)
boxplot(as.numeric(base_and_all_options_value)/10^12~naics, data = dod2010)
boxplot(as.numeric(base_and_all_options_value)/10^12~naics, data = dod2010)
boxplot(log(as.numeric(base_and_all_options_value))~naics, data = dod2010)
boxplot(log(as.numeric(base_and_all_options_value))~naics, data = dod2010)
hist(log2(as.numeric(dod2010$base_and_all_options_value)))
> hist(log10(as.numeric(dod2010$base_and_all_options_value)))
> hist(log2(as.numeric(dod2010$base_and_all_options_value)))
> hist(log(as.numeric(dod2010$base_and_all_options_value)))
> hist(log10(as.numeric(dod2010$base_and_all_options_value)))
> sd(log10(as.numeric(dod2010$base_and_all_options_value)))
[1] 0.949723
> sd((as.numeric(dod2010$base_and_all_options_value)))
[1] 5439270
> mean((as.numeric(dod2010$base_and_all_options_value)))
[1] 657472.3
> max((as.numeric(dod2010$base_and_all_options_value)))
[1] 583892230
> min((as.numeric(dod2010$base_and_all_options_value)))
[1] 0.01
> median((as.numeric(dod2010$base_and_all_options_value)))
[1] 50000
> boxplot(log(as.numeric(base_and_all_options_value))~naics, data = dod2010)
> boxplot(log10(as.numeric(base_and_all_options_value))~naics, data = dod2010)
> boxplot(log10(as.numeric(base_and_all_options_value))~psc, data = dod2010)
Error in eval(predvars, data, env) : object 'psc' not found
> boxplot(log10(as.numeric(base_and_all_options_value))~product_or_service_code, data = dod2010)
> boxplot(product_or_service_code~log10(as.numeric(base_and_all_options_value)), data = dod2010)
Error in x[floor(d)] + x[ceiling(d)] : 
  non-numeric argument to binary operator
> boxplot(product_or_service_code~log10(as.numeric(base_and_all_options_value)), data = dod2010)
Error in x[floor(d)] + x[ceiling(d)] : 
  non-numeric argument to binary operator
> 
  > boxplot(log10(as.numeric(base_and_all_options_value))~product_or_service_code, data = dod2010)
> boxplot(log10(as.numeric(base_and_all_options_value))~naics, data = dod2010)
> boxplot(log10(as.numeric(base_and_all_options_value))~awarding_agency_code, data = dod2010)




boxplot(dod2010value, treas2010value,
        main = "DOD & TREAS 2010 Base and All Options",
        xlab = "Log10 of Contract Value",
        ylab = "Agencies",
        names = c("DOD", "TREAS"),
        col = c("red","blue"),
        border = "black",
        horizontal = TRUE
)

boxplot(log10(as.numeric(base_and_all_options_value))~naics, 
        main = "DOD 2010 NAICS Vs Base and All Options",
        xlab = "Log10 of Contract Value",
        ylab = "NAICS",
        col = c("red"),
        border = "black",
        horizontal = TRUE,
        data = dod2010)



boxplot(log10(as.numeric(base_and_all_options_value))~naics, 
        main = "DOD 2010 NAICS Vs Base and All Options",
        ylab = "Log10 of Contract Value",
        xlab = "NAICS",
        col = c("red"),
        border = "black",
        horizontal = FALSE,
        data = dod2010)

boxplot(log10(as.numeric(base_and_all_options_value))~naics, 
        main = "TREASURY 2010 NAICS Vs Base and All Options",
        ylab = "Log10 of Contract Value",
        xlab = "NAICS",
        col = c("red"),
        border = "black",
        horizontal = FALSE,
        data = treas2010)