from pytrends.request import TrendReq
import pandas as pd
from datetime import date

as_of_date = date.today()

pytrends = TrendReq(hl='en-US', tz=360) 

search_words = ["generative ai","chatgpt", "llm"] 

pytrends.build_payload(search_words, cat=0, timeframe='today 12-m') 

data = pytrends.interest_over_time() 
data = data.reset_index() 


data.to_csv(f"../historic-output/gtrends_{as_of_date}.csv", index = False)

data.to_csv(f"../current-output/gtrends.csv", index = False)