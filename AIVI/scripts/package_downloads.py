import pypistats
import pandas as pd
from datetime import date, datetime

as_of_date = date.today()


langchain_df = pypistats.overall("langchain", total=True, format="pandas",   start_date="2021-01-01")

huggingface_df = pypistats.overall("huggingface", total=True, format="pandas", start_date="2021-01-01")

openai_df = pypistats.overall("OpenAI", total=True, format="pandas",start_date="2021-01-01")


llamaindex_df = pypistats.overall("llama-index", total=True, format="pandas", start_date="2021-01-01")



langchain_df.to_csv(f"../historic-output/package_download_langchain_{as_of_date}.csv", index = False)
huggingface_df.to_csv(f"../historic-output/package_download_huggingface_{as_of_date}.csv", index = False)
openai_df.to_csv(f"../historic-output/package_download_openai_{as_of_date}.csv", index = False)
llamaindex_df.to_csv(f"../historic-output/package_download_llamaindex_{as_of_date}.csv", index = False)

langchain_df.to_csv(f"../current-output/package_download_langchain.csv", index = False)
huggingface_df.to_csv(f"../current-output/package_download_huggingface.csv", index = False)
openai_df.to_csv(f"../current-output/package_download_openai.csv", index = False)
llamaindex_df.to_csv(f"../current-output/package_download_llamaindex.csv", index = False)

