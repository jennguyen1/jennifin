
table_creation <- c(
stocks = "
CREATE TABLE stocks(
  ticker TEXT NOT NULL,
  company TEXT NOT NULL,
  sector TEXT NOT NULL,
  industry TEXT NOT NULL,
  size TEXT NOT NULL,
  PRIMARY KEY(ticker)
);
",
etfs = "
CREATE TABLE etfs(
  ticker TEXT NOT NULL,
  description TEXT NOT NULL,
  e_type TEXT NOT NULL,
  category TEXT NOT NULL,
  category2 TEXT NOT NULL,
  PRIMARY KEY(ticker)
);
",
prices = "
CREATE TABLE prices(
  ticker TEXT NOT NULL,
  date DATE NOT NULL,
  open DOUBLE,
  high DOUBLE,
  low DOUBLE,
  close DOUBLE,
  volume HUGEINT,
  rsi DOUBLE,
  UNIQUE(ticker, date)
);
",
price_index = "
CREATE UNIQUE INDEX prices_index ON prices(ticker, date);
",
sectors = "
CREATE TABLE sectors(
  sector TEXT NOT NULL,
  category TEXT NOT NULL,
  PRIMARY KEY(sector)
);
"
)

view_creation <- c(
# MA, 52w Low/High, trailing months, ytd
price_stats = "
CREATE VIEW price_stats AS(
  WITH stats_pre AS (
    SELECT 
      *, year(date) as year,
      -- MA
      AVG(close) OVER (PARTITION BY ticker ORDER BY ticker, date ROWS BETWEEN 19 PRECEDING AND CURRENT ROW) as ma_20_pre,
      AVG(close) OVER (PARTITION BY ticker ORDER BY ticker, date ROWS BETWEEN 49 PRECEDING AND CURRENT ROW) as ma_50_pre,
      AVG(close) OVER (PARTITION BY ticker ORDER BY ticker, date ROWS BETWEEN 199 PRECEDING AND CURRENT ROW) as ma_200_pre,
      -- 52W Low/High
      MIN(low) OVER (PARTITION BY ticker ORDER BY ticker, date ROWS BETWEEN 251 PRECEDING AND CURRENT ROW) as lo_52w_pre,
      MAX(high) OVER (PARTITION BY ticker ORDER BY ticker, date ROWS BETWEEN 251 PRECEDING AND CURRENT ROW) as hi_52w_pre,
      -- Prices from prev time
      FIRST(close) OVER (PARTITION BY ticker ORDER by ticker, date ROWS BETWEEN 21 PRECEDING AND CURRENT ROW) as price_1mm_pre,
      FIRST(close) OVER (PARTITION BY ticker ORDER by ticker, date ROWS BETWEEN 63 PRECEDING AND CURRENT ROW) as price_3m_pre,
      FIRST(close) OVER (PARTITION BY ticker ORDER by ticker, date ROWS BETWEEN 126 PRECEDING AND CURRENT ROW) as price_6m_pre,
      FIRST(close) OVER (PARTITION BY ticker ORDER by ticker, date ROWS BETWEEN 252 PRECEDING AND CURRENT ROW) as price_12m_pre,
      -- Counter
      ROW_NUMBER() OVER (PARTITION BY ticker ORDER BY ticker, date) as i
    FROM prices
  ),
  ytd AS(
    SELECT DISTINCT
    ticker, year(date)+1 as year, 
    FIRST(close) OVER (PARTITION BY ticker, year(date) order by ticker, date DESC) as price_ytd
    FROM prices
  )
  SELECT 
    p.ticker, p.date, p.open, p.high, p.low, p.close, p.volume, p.rsi,
    CASE WHEN i BETWEEN 1 AND 19 THEN NULL ELSE p.ma_20_pre END as price_20d,
    CASE WHEN i BETWEEN 1 AND 49 THEN NULL ELSE p.ma_50_pre END as price_50d,
    CASE WHEN i BETWEEN 1 AND 199 THEN NULL ELSE p.ma_200_pre END as price_200d,
    CASE WHEN i BETWEEN 1 AND 251 THEN NULL ELSE p.lo_52w_pre END as price_52w_lo,
    CASE WHEN i BETWEEN 1 AND 251 THEN NULL ELSE p.hi_52w_pre END as price_52w_hi,
    CASE WHEN i BETWEEN 1 AND 21 THEN NULL ELSE p.price_1mm_pre END as price_1m,
    CASE WHEN i BETWEEN 1 AND 63 THEN NULL ELSE p.price_3m_pre END as price_3m,
    CASE WHEN i BETWEEN 1 AND 126 THEN NULL ELSE p.price_6m_pre END as price_6m,
    CASE WHEN i BETWEEN 1 AND 262 THEN NULL ELSE p.price_12m_pre END as price_12m,
    ytd.price_ytd
  FROM stats_pre as p
  LEFT JOIN ytd
  ON p.ticker = ytd.ticker AND p.year = ytd.year
);
",
# OB/OS days since
since_os = "
CREATE VIEW since_os AS(
  WITH last_o AS(
    SELECT ticker, MAX(date) as dto 
    FROM prices 
    WHERE rsi < 30
    GROUP BY ticker
  )
  SELECT prices.ticker, count(*) as days_since_os
  FROM prices
  LEFT JOIN last_o
  ON prices.ticker = last_o.ticker
  WHERE prices.date > last_o.dto
  GROUP BY prices.ticker
);
",
since_ob = "
CREATE VIEW since_ob AS(
  WITH last_o AS(
    SELECT ticker, MAX(date) as dto 
    FROM prices 
    WHERE rsi >70
    GROUP BY ticker
  )
  SELECT prices.ticker, count(*) as days_since_ob
  FROM prices
  LEFT JOIN last_o
  ON prices.ticker = last_o.ticker
  WHERE prices.date > last_o.dto
  GROUP BY prices.ticker
);
",
# OB/OS Perc
obos = "
CREATE VIEW obos AS(
  SELECT 
    prices.date,
    AVG(CASE WHEN prices.rsi IS NULL THEN NULL WHEN prices.rsi <= 30 THEN 1 ELSE 0 END)*100 as os,
    AVG(CASE WHEN prices.rsi IS NULL THEN NULL WHEN prices.rsi >= 70 THEN 1 ELSE 0 END)*100 as ob,
    COUNT(prices.ticker) as count
  FROM prices 
  RIGHT JOIN stocks
    ON stocks.ticker = prices.ticker
  GROUP BY date
);
",
# AVWAP
avwap = "
CREATE VIEW avwap_anchor_{dt_name} AS(
  WITH tab AS(
    SELECT 
      ticker, date, '{dt}' as anchor,
      SUM((open+high+low+close)/4 * volume) OVER(PARTITION BY ticker ORDER BY ticker, date) as num,
      SUM(volume) OVER(PARTITION BY ticker ORDER BY ticker, date) as den,
      num / den as avwap
    FROM prices
    WHERE date >= '{dt}'
  )
  SELECT ticker, date, anchor, avwap FROM tab
);
",
# Price anchor
price_anchor = "
CREATE VIEW price_anchor_{dt_name} AS(
  SELECT ticker, date, close
  FROM prices
  WHERE date = '{dt}'
);
"
)

assemble_data <- "
WITH d1d AS(
  SELECT 
    a1.ticker, a1.close as price_anchor_1, a2.close as price_anchor_2,
    ob.days_since_ob, os.days_since_os
  FROM price_anchor_1 a1
  FULL JOIN price_anchor_2 as a2
    ON a1.ticker = a2.ticker
  FULL JOIN since_ob as ob
    ON a1.ticker = ob.ticker
  FULL JOIN since_os as os
    ON a1.ticker = os.ticker
)
SELECT 
  {specific_columns},
  p.date, p.open, p.high, p.low, p.close as price, p.volume, p.rsi, 
  p.price_20d, p.price_50d, p.price_200d, 
  p.price_52w_lo, p.price_52w_hi, p.price_ytd,
  p.price_1m, p.price_3m, p.price_6m, p.price_12m,
  ava1.avwap as price_avwap_anchor_1, ava2.avwap as price_avwap_anchor_2, 
  avay.avwap as price_avwap_ytd,
  d1d.price_anchor_1, d1d.price_anchor_2, 
  d1d.days_since_ob, d1d.days_since_os
FROM {table} 
LEFT JOIN price_stats as p
  ON {table}.ticker = p.ticker
LEFT JOIN avwap_anchor_1 as ava1
  ON p.ticker = ava1.ticker AND p.date = ava1.date
LEFT JOIN avwap_anchor_2 as ava2
  ON p.ticker = ava2.ticker AND p.date = ava2.date
LEFT JOIN avwap_anchor_ytd as avay
  ON p.ticker = avay.ticker AND p.date = avay.date
LEFT JOIN d1d
  ON p.ticker = d1d.ticker
WHERE p.date = '{pull_date}'
;
"
