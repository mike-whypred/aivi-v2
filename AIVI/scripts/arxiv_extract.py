from pyarxiv import query, download_entries
from pyarxiv.arxiv_categories import ArxivCategory, arxiv_category_map
from datetime import date, datetime, timedelta
import pandas as pd
import requests
import os
from dotenv import load_dotenv 





# Path

as_of_date = date.today()

#load_dotenv()

#api_key = os.getenv("chatPDFKey")

# list of topics 

topics = ["Large Language Model", "Generative AI", "GPT"]



# creating dataframe of papers 


dfList = []

maxPaperReturned = 1500

for topic in topics:
    
    results = query(abstract= topic, categories=[ArxivCategory.cs_AI, ArxivCategory.cs_LG, ArxivCategory.cs_CL ], max_results= maxPaperReturned)
    
    titles = map(lambda x: x['title'], results)
    authors = map(lambda x: x['author'], results)
    updated = map(lambda x: x['updated'], results)
    links = map(lambda x: x['link'], results)
    tag1 = map(lambda x:x['tags'][0]['term'], results)


    # Create empty dataframe called "papers"

    papers = pd.DataFrame()

    # Insert columns into "papers" from the previously created lists

    papers['Title'] = pd.Series(titles)
    papers['Author'] = pd.Series(authors)
    papers['Updated'] = pd.Series(updated)
    papers['Link'] = pd.Series(links)
    papers['Tag'] = pd.Series(tag1)
    papers['Search'] = topic

    # Slice HH:MM:SS off of each row in date column

    papers['Updated'] = papers['Updated'].str.slice(stop = 10)
    
    papers["Updated"] = pd.to_datetime(papers["Updated"]).dt.date
    
    papers = papers.sort_values(by = 'Updated', ascending = False).reset_index(drop = True)

    # Reformat URL string to take user to the PDF of the paper

    papers['Link'] = papers['Link'].str.replace("abs", "pdf", case = True)

    # Strip paper ID from Link URL and put it in its own column called "ID"

    papers['ID'] = pd.Series(papers['Link'].str.rsplit("/", n=1, expand=True)[1])
    
    dfList.append(papers)


papersComplete = pd.concat(dfList)


papersComplete.drop_duplicates(subset=['Title'], keep='first',inplace=True)

papersComplete.to_csv("../current-output/papers_list.csv", index = False)

papersComplete.to_csv(f"../historic-output/papers_list_{as_of_date}.csv", index = False)
