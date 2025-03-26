import time
import datetime
import duckdb
import pandas as pd
import yfinance as yf

# pull data from db already saved
def get_price_db(year, db = "data/stock_prices.db"):
  query = "SELECT * FROM prices WHERE date >= '{yr}-01-01'".format(yr = year)
  db_conn = duckdb.connect(db)

  out = (None, None, None)
  try:
    df = pd.read_sql(query, db_conn)
    tickers = df.ticker.unique().tolist() 
    query_date0 = df.groupby("ticker")["date"].max().min() 
    query_date = (query_date0 - datetime.timedelta(weeks = 5)).strftime("%Y-%m-%d") # to calc rsi
    d_old = df[df.date >= datetime.datetime.strptime(query_date, "%Y-%m-%d").date()][["ticker", "date"]]
    d_old["date"] = d_old.date.astype(str)
    out = (d_old, tickers, query_date)
    print("Data Extracted")
  finally:
    db_conn.close()
    
  return out
  
# pull data from yfinance
def query_ticker_data(tickers, query_date):
  start = time.time()
  d0 = yf.download(
      tickers = tickers,
      start = query_date,
      auto_adjust = False,
      threads = True,
      group_by = 'ticker',
      multi_level_index = False
  )
  d1 = d0.reset_index().melt(id_vars = [('Date', '')])
  d1.columns = ["date", "ticker", "name", "value"]
  d1.name = d1.name.str.lower()
  d2 = d1.pivot(index = ["date", "ticker"], columns = "name", values = "value")
  df = d2.drop('adj close', axis = 1).reset_index()
  print("Run Time {}s".format(round(time.time() - start, 0)))

  return df

# stagger calls to api so not rate limited
def get_updated_prices(tickers, query_date):

  d1 = query_ticker_data(tickers[:800], query_date)
  time.sleep(180) # rest for api call
  d2 = query_ticker_data(tickers[800:], query_date)

  return pd.concat([d1, d2])


# run
yr_3m_ago = str((datetime.datetime.now() - datetime.timedelta(weeks = 12)).year)
d_old, tickers, query_date = get_price_db(yr_3m_ago)
d_out = get_updated_prices(tickers, query_date)
