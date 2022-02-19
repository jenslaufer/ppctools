cost_per_conversion <- function(cpc, cr) {
  cpc / cr
}

num_of_conversions_for_ad_budget <- function(ad_budget, cpc, cr) {
  ad_budget / cost_per_conversion(cpc, cr)
}

ad_budget_for_num_of_conversions <-
  function(num_of_conversions, cpc, cr) {
    num_of_conversions / cr * cpc
  }

revenue_for_ad_budget <-
  function(ad_budget, net_product_price, cpc, cr) {
    num_of_conversions_for_ad_budget(ad_budget, cpc, cr) * net_product_price
  }

profit_for_ad_budget <-
  function(ad_budget, net_product_price, cpc, cr) {
    revenue_for_ad_budget(ad_budget, net_product_price, cpc, cr) - ad_budget
  }

ad_budget_for_profit <-
  function(profit, net_product_price, cpc, cr) {
    profit * cost_per_conversion(cpc, cr) / (net_product_price - cost_per_conversion(cpc, cr))
  }

volume_for_profit <-
  function(profit, net_product_price, cpc, cr, ctr) {
    ad_budget_for_profit(profit, net_product_price, cpc, cr) / (ctr * cpc)
  }

num_of_conversions <- function(volume, cr, ctr) {
  volume * ctr * cr
}

product_price_for_profit <- function(profit, volume, cpc, cr, ctr) {
  num_of_conversions <- num_of_conversions(volume, cr, ctr)
  ad_cost <- cost_per_conversion(cpc, cr) * num_of_conversions
  
  gross <- profit + ad_cost
  
  gross / num_of_conversions
}

profit_for_specified_volume <-
  function(volume, cpc, net_product_price, cr, ctr) {
    num_of_conversions <- num_of_conversions(volume, cr, ctr)
    ad_cost <- volume * ctr * cpc
    
    num_of_conversions * net_product_price - ad_cost
  }

net_product_price <- function(profit_ad_cost_ratio, cpc, cr) {
  ad_budget <- 1 / profit_ad_cost_ratio
  (1 + ad_budget) / num_of_conversions_for_ad_budget(ad_budget, cpc, cr)
}
