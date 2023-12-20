
require(tidyverse)
require(ggplot2)
require(RColorBrewer)
require(alphavantager)
require(fmpcloudr)
require(gtrendsR)
require(purrr)
require(dotenv)
######
###UDF


replace_strings <- function(x) {
  
  str_replace_all(x, "<1", "0") # Replace "St" with "Street"
  
  }


process_downloads <- function(df, new.name){
  
  ## Date to date format
  ## take only with mirrors
  ##change column name 
  
  df <- df %>% mutate(date = ymd(date))
  
  df <- df %>% filter(category == "with_mirrors") %>% select(c(date, downloads)) %>% arrange(date)
  
  colnames(df)[2] <- new.name
  
  return(df)
  
}



create_date_seq_df <- function(start.date = ymd('2022-01-01'), end.date = Sys.Date()){
  
  
  date.seq <- seq(start.date, end.date, by = "days")
  
  
  date.df <- data.frame(date = date.seq)
  
  return(date.df)
}



pcp <- function(x, n =1 ){
  
  return(  x/lag(x) - 1 )
  
  
}



pcpdiff <- function(x, n =1){
  
  return(    (x - lag(x,n)) / 100 )
  
  
}

###########################


master.date <- create_date_seq_df()


## ai stocks

load_dot_env(file = ".env")

fmpc_set_token(Sys.getenv("fmp_api_key"))

#fmpc_set_token("84168111cf22fcb71abc64719574da2f")

ai.stocks <- c("MSFT", "NVDA", "META", "GOOGL", "AMZN")





ai.stocks.df <- fmpc_price_history(
  symbols = ai.stocks,
  startDate = Sys.Date() - 10000,
  endDate = Sys.Date()
)


ai.stocks.df.sub <- ai.stocks.df %>% select( c(date, symbol, adjClose)) %>% filter(date > ymd("2020/01/01"))


#ai.stocks.df.sub.wide <- ai.stocks.df.sub %>% spread(symbol, changePercent) %>% drop_na()


ai.stocks.df.sub <- ai.stocks.df.sub %>% mutate(week_n = paste(year(date), isoweek(date), sep ="_"))

ai.stocks.df.sub.week <- ai.stocks.df.sub  %>% 
                            group_by(symbol, week_n) %>% summarise(return = last(adjClose) / first(adjClose) - 1) 

ai.stocks.df.sub.week.wide <- ai.stocks.df.sub.week %>% spread(symbol, return)


#################################
## google trends


# gai.trends <- gtrends(c("generative ai"), time = "today+5-y",onlyInterest = T)[["interest_over_time"]]
# 
# Sys.sleep(20)
# gpt.trends <- gtrends(c("chatgpt"), time = "today+5-y",onlyInterest = T)[["interest_over_time"]]
# Sys.sleep(20)
# llm.trends <- gtrends(c("large language models"), time = "today+5-y",onlyInterest = T)[["interest_over_time"]]
# 
# 
# 
# gai.trends.sub <- gai.trends %>% rename(gai.hits = hits) %>% select(c(date, gai.hits))
# gpt.trends.sub <- gpt.trends %>% rename(gpt.hits = hits) %>% select(c(date, gpt.hits))
# llm.trends.sub <- llm.trends %>% rename(llm.hits = hits) %>% select(c(date, llm.hits))

##############################
## broken gtrends package

comb.trends <- read.csv("../inputs/comb_trends.csv")

#################################

#comb.trends <- gai.trends.sub %>% left_join(gpt.trends.sub, by = c("date")) %>% left_join(llm.trends.sub, by = c("date"))

#######################################
comb.trends <- comb.trends %>% mutate_all(replace_strings)

#comb.trends <- comb.trends %>% mutate(date = ymd(date))

comb.trends <- comb.trends %>% mutate(date = dmy(date))


comb.trends <- comb.trends %>% mutate_at(vars(-date), as.numeric)


comb.trends.delta <- comb.trends %>% arrange(date) %>% mutate_at(vars(-date), pcpdiff)


comb.trends.delta <- comb.trends.delta %>% rename(last_date = date)


comb.trends.delta <- comb.trends.delta %>% mutate(week_n = paste(year(last_date), isoweek(last_date), sep ="_"))


#####################################
## package downloads


langchain.df <- read.csv('../current-output/package_download_langchain.csv')
huggingface.df <- read.csv('../current-output/package_download_huggingface.csv')
openai.df <- read.csv('../current-output/package_download_openai.csv')
llamaindex.df <- read.csv('../current-output/package_download_llamaindex.csv')




langchain.clean <- process_downloads(langchain.df, "langchain")

huggingface.clean <- process_downloads(huggingface.df, "huggingface")

openai.clean <- process_downloads(openai.df, "openai")

llama.clean <- process_downloads(llamaindex.df, "llamaindex")


comb.downloads <- purrr::reduce(list(langchain.clean,
                                     huggingface.clean,
                                     openai.clean,
                                     llama.clean), dplyr::left_join, by = 'date')

## add week_year serial to join on later


## pivot long to apply group and summarise aggregation

comb.downloads.long <- comb.downloads %>% gather("package", "downloads", -date)

comb.downloads.week <- comb.downloads.long %>% mutate(week_n = paste(year(date), isoweek(date), sep ="_"))

#comb.downloads.week <- comb.downloads.week %>% mutate(downloads = as.numeric(downloads))
comb.downloads.week <- comb.downloads.week %>% group_by(package, week_n) %>% summarise(downloads_w = sum(downloads), last_date = last(date))



## pivot wide to apply time series transformation

comb.downloads.week.wide <- comb.downloads.week %>% spread(package, downloads_w) %>% arrange(last_date)


comb.downloads.week.wide.transform <- comb.downloads.week.wide %>% mutate_at(vars(-c(week_n, last_date)),pcp)

## rejoin on original
comb.downloads <- comb.downloads %>% mutate(week_n = paste(year(date), isoweek(date), sep ="_"))

comb.downloads.rejoin <- comb.downloads %>% left_join(comb.downloads.week.wide.transform, by = c('week_n'))



##################
## arxiv papers

papers.df <- read.csv('../current-output/papers_list.csv')


papers.df.clean <- papers.df %>% mutate(Updated = ymd(Updated))


papers.df.clean.agg <- papers.df.clean %>% group_by(Updated) %>% summarise(papers_count = n())

papers.df.clean.agg <- papers.df.clean.agg %>% rename(date = Updated)

#papers.df.clean.agg.comb <- master.date %>% left_join(papers.df.clean.agg, by = c('date'))
papers.df.clean.agg.comb <- papers.df.clean.agg %>% mutate(week_n = paste(year(date), isoweek(date), sep ="_"))

papers.df.clean.agg.comb.week <- papers.df.clean.agg.comb %>% 
                                        group_by(week_n) %>% 
                                        summarise(total_papers = sum (papers_count), last_date = last(date)) %>% 
                                        arrange(last_date) %>% fill(total_papers, .direction = "down")

papers.df.clean.agg.comb.week.delta <- papers.df.clean.agg.comb.week %>% mutate_at(vars(-c(last_date,week_n)), pcp)





############################


## create master date sequence df and combine 

cut.off.date <- ymd("2022/11/27")



master.df <- purrr::reduce(list(
                                comb.trends.delta,
                                comb.downloads.week.wide.transform,
                                papers.df.clean.agg.comb.week.delta,
                                ai.stocks.df.sub.week.wide
                                ), dplyr::left_join, by = 'week_n')


master.df <- master.df %>% filter(last_date >= cut.off.date)
master.df <- master.df %>% select(c(last_date, week_n, gai.hits, gpt.hits, llm.hits, huggingface,
                                    langchain, llamaindex, openai, total_papers, AMZN, GOOGL, META, MSFT, NVDA))

master.df <- master.df %>% mutate(across(-last_date, ~ifelse(is.na(.), 0, .))) %>% arrange(last_date)

#write.csv(master.df, paste0("master_date_", Sys.Date(), ".csv"), row.names = F)


write.csv(master.df, paste0("../current-output/master_data_output", ".csv"), row.names = F)

write.csv(master.df, paste0("../historic-output/master_data_output_", Sys.Date(), ".csv"), row.names = F)

########################


master.df <- read.csv("../current-output/master_data_output.csv")



###############################

##weights matrix


## top level weights

## first 19 rows is not pypi download states from 11/22 - 04/23

stocks_factor1 <- 0.45

gtrends_factor1 <- 0.45

papers_factor1 <- 0.10


## after 04/23

stocks_factor2 <- 0.40

gtrends_factor2 <- 0.40

papers_factor2 <- 0.10

downloads_factor2 <- 0.1


## bottom level weights

##google trends

gai_trends_weight <- 1 / 3

gpt_trends_weight <- 1 / 3

llm_trends_weight <- 1 / 3

## stocks

msft_weight <- 1 / 5
meta_weight <- 1 / 5
goog_weight <- 1 / 5
amzn_weight <- 1 / 5 
nvda_weight <- 1 / 5 

## downloads


openai_weight <- 1 / 4

llamaindex_weight <- 1 / 4

langchain_weight <- 1 / 4

huggingface_weight <- 1 / 4




## before 04/23


# init_weights <- c( gtrends_factor1 * gai_trends_weight,
#                    
#                    gtrends_factor1 * gpt_trends_weight,
#                    
#                    gtrends_factor1 * llm_trends_weight,
#                    
#                    
#                    papers_factor1,
#                    
#                    stocks_factor1 * amzn_weight,
#                    
#                    stocks_factor1 * goog_weight,
#                    
#                    stocks_factor1 * meta_weight,
#                    
#                    stocks_factor1 * msft_weight,
#                    
#                    stocks_factor1 * nvda_weight
#   
#   )




init_weights <- tibble(gtrends_factor1 * gai_trends_weight,
                      
                      gtrends_factor1 * gpt_trends_weight,
                      
                      gtrends_factor1 * llm_trends_weight,
                      
                      downloads1 = 0,
                      
                      downloads2 =0,
                      
                      downloads3 = 0,
                      
                      downloads4 = 0,
                      
                      papers_factor1,
                      
                      stocks_factor1 * amzn_weight,
                      
                      stocks_factor1 * goog_weight,
                      
                      stocks_factor1 * meta_weight,
                      
                      stocks_factor1 * msft_weight,
                      
                      stocks_factor1 * nvda_weight
                      
                      )


init_weights_df <- init_weights %>% slice(rep(1:n(), each = 19))


colnames(init_weights_df) <- paste0("weights_", c(1:19))


new_weights <- tibble(gtrends_factor2 * gai_trends_weight,
              
                        gtrends_factor2 * gpt_trends_weight,
                        
                        gtrends_factor2 * llm_trends_weight,
                       
                       
                        downloads_factor2 * huggingface_weight,
                      
                        downloads_factor2 * langchain_weight,
                      
                        downloads_factor2 * llamaindex_weight,
                      
                        downloads_factor2 * openai_weight,
                        
                        papers_factor2,
                        
                        stocks_factor2 * amzn_weight,
                        
                        stocks_factor2 * goog_weight,
                        
                        stocks_factor2 * meta_weight,
                        
                        stocks_factor2 * msft_weight,
                        
                        stocks_factor2 * nvda_weight)

new_weights_df <- new_weights %>% slice(rep(1:n(), each = (nrow(master.df) - 19)))

colnames(new_weights_df) <- paste0("weights_", c(1:19))



comb_weights_df <- bind_rows(init_weights_df, new_weights_df)


master_df_vars <- master.df %>% select(c(3:15))


weighted_change_df <- comb_weights_df * master_df_vars


output_df <- cbind.data.frame(master.df$last_date, weighted_change_df)

output_df <- output_df %>% rename(date = `master.df$last_date`)

final_output_df <- output_df %>% mutate(total_delta = rowSums(across(-date)))

final_output_df <- final_output_df %>% mutate(ai_index = 100 * (cumprod(1 + total_delta)))


write.csv(final_output_df, 
              paste0("../historic-output/index_data_full_", Sys.Date(), ".csv"), row.names = F)

write.csv(final_output_df, 
            "../current-output/index_data_full.csv", row.names = F)


index_data_only <- final_output_df %>% select(c(date, total_delta, ai_index))

index_data_only <- index_data_only %>% rename(pct_change = total_delta, index = ai_index)


write.csv(index_data_only, 
            paste0("../historic-output/chart_data_", Sys.Date(), "_.csv"), row.names = F)

write.csv(index_data_only, 
          "../current-output/chart_data,csv", row.names = F)

#write.csv(index_data_only, "C:/Users/mike2/Desktop/whypred/Projects/AI Velocity Index/chart_data.csv", row.names = F)

index_data_only <- index_data_only %>% mutate(index = round(index, 2))

#saveRDS(index_data_only, file =  "C:/Users/mike2/Desktop/whypred/Projects/AI Velocity Index/shinyapp/ai-velocity-index/chart_data.rds")
write.csv(index_data_only, "C:/Users/mike2/Desktop/whypred/Projects/AI Velocity Index/shinyapp/ai-velocity-index/chart_data.csv", row.names = F)
