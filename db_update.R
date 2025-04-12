# load settings
source("global.R")

# update db (<5 min - compared to 10-15min in R)
run_db_update() 

# breadth animation 
animate_breadth(2020)


### update tables on anchor date changes ###
# # price anchor 1&2
# add_view_price_anchor("1", anchor_1)
# add_view_price_anchor("2", anchor_2)
# 
# # avwap anchor 1&2
# add_view_avwap("1", anchor_1)
# add_view_avwap("2", anchor_2)
# 
# # avwap anchor ytd
# add_view_avwap("ytd", "2025-01-01")
#
# # avwap anchor specific
# add_view_avwap_specific("high", "2024-11-01")
# add_view_avwap_specific("low", "2025-01-01")


### Every Quarter - SP Companies ###
# pull data from stockcharts - get sector & industry
# now is pre-built as example scan
# [group is SP500] OR [group is SP400] OR [group is SP600]

# other stocks not in SP1500 - requires a little cleanup (rm na's)
# [type is stock] AND [group is not etf] AND
# [group is not SP500] AND [group is not SP400] AND [group is not SP600] AND 
# [[exchange is NYSE  ] OR [exchange is NASDAQ]] AND
# [ market cap > 1000 ] 
