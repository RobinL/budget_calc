assets_liabilities <- readr::read_csv("assets_liabilities.csv")
income_spending <- readr::read_csv("income_spending.csv")

get_moving_cost <- function(input) {
  buy_price <- input$buy_price
  stamp_duty = c(0,0,0)
  stamp_duty[1] <- max(0, (min(250000, buy_price)- 125000 )) * 0.02
  stamp_duty[2] <- max(0,(min(925000, buy_price)- 250000 )) * 0.05
  stamp_duty[3] <- max(0,buy_price - 925000) * 0.05
  
  sell_estate_agent <- input$sell_price * 0.0125
  sell_legal <- 2000
  buy_legal <- 2000
  
  sum(stamp_duty) + sell_estate_agent + sell_legal + buy_legal
}

get_total_assets <- function(my_env, assets_liabilities) {
  
  my_env$total_assets <- sum(assets_liabilities$value)
  
  my_env
  
}


environment_generator <- function(localinput, output, session, input) {

  reactive_assets_liabilities <- reactive({

    # Override any values in the input files
    for (n in names(input)) {
      if (n %in% assets_liabilities$asset_id) {
        assets_liabilities[assets_liabilities$asset_id == n, "value"] <- input[[n]]
      }
    }

    assets_liabilities

  })
  
  reactive_income_spending <- reactive({
    
    # Override any values in the input files
    for (n in names(input)) {
      if (n %in% income_spending$income_spend_id) {
        income_spending[income_spending$income_spend_id == n, "value"] <- input[[n]]
      }
    }
    
    # Convert monthly to annual
    
    income_spending[income_spending$period == "year", "value"] <- income_spending[income_spending$period == "year", "value"]/12
    income_spending$period <- "month"
    
    income_spending
    
  })
  
  

  my_env <- reactive({
    
  
    assets_liabilities <- reactive_assets_liabilities()
    
    my_env <- list()
    
    my_env <- add_table_to_env(my_env, assets_liabilities, "asset_id", "value")
    
    for (n in names(input)) {
      my_env[[n]] <- input[[n]] 
    }
    
    my_env <- get_total_assets(my_env, assets_liabilities)
    my_env$moving_cost <- get_moving_cost(input)
    
    my_env$total_morgage_needed  <- my_env$buy_price + my_env$bank_money_after_purchase + my_env$moving_cost - my_env$total_assets 
    
    my_env$monthly_mortgage_payments <- mortgage(P=my_env$total_morgage_needed, I=(input$interest_rate*100)+1.35, L=36)
    
    income_spending <- reactive_income_spending()
    
    my_env$income_spending <- income_spending

    my_env <- add_incomes_to_env(my_env, input)
    
    my_env <- add_costs_to_env(my_env)
    
    assets_liabilities$value_fmt <- fmtc(assets_liabilities$value)
    my_env$assets_liabilities <- assets_liabilities[,c("asset_id", "value_fmt")]
    
    income_spending$value_fmt <- fmtc(income_spending$value)
    my_env$income_spending <- income_spending[,c("income_spend_id", "value_fmt")]
    
    my_env
    
  })
  
  
  return(my_env)

}


add_table_to_env <- function(my_env, df, id_col, val_col) {
  
  for (i in 1:nrow(df)) {
    id <- df[[i, id_col]]
    val <- df[[i, val_col]]
    my_env[id]  <- val
  }
  
  my_env
  
}

add_costs_to_env <- function(my_env) {
  
  my_env$monthly_spending_nonmortgage <- -sum(my_env$income_spending$value)
  my_env
  
}

# Compute after tax salary
get_after_tax_salary <- function(gross, pension_contribution) {
  
  tax = c(0,0,0)
  tax[1] <- max(0, (min(45000, gross)- 11500 )) * 0.2
  tax[2] <- max(0,(min(150000, gross)- 45000 )) * 0.40
  tax[3] <- max(0,gross - 150000) * 0.45
  
  ni <- c(0,0)
  ni[1] <- max(0, (min(866*52, gross)- 155*52 )) * 0.12
  ni[2] <- max(0,gross - 866*52) * 0.02
  
  pension <- gross * pension_contribution
  
  after <- gross - sum(tax) - sum(ni) - pension
  
  after
}

add_incomes_to_env <- function(my_env, input) {
  
  my_env$robin_before_tax <- input$robin_salary * input$robin_work_prop
  my_env$robin_after_tax <- get_after_tax_salary(my_env$robin_before_tax, pension_contribution = 0.05)
  
  my_env$gemma_before_tax <- input$gemma_salary * input$gemma_work_prop
  my_env$gemma_after_tax <- get_after_tax_salary(my_env$gemma_before_tax, pension_contribution = 0.05)
  
  my_env
}



