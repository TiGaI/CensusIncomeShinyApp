source("helpers.R")
source("load_csv_local&clean.R")


# Sanity Check
find_num_na_date <- function(df){
  tmp <- df %>% 
    transmute(
      missing_date = is.na(START.DATE) | is.na(END.DATE)
    )
  sum(tmp$missing_date)
}

sapply(lst_dfs, function(df){ df %>% mutate(
  START.DATE = remove_empty_date(START.DATE),
  END.DATE = remove_empty_date(END.DATE))
} %>% find_num_na_date()
)
sapply(lst_cleaned_dfs, find_num_na_date)

fname = "2018-Q3.csv"
View(
  cbind(
    lst_dfs[[fname]] %>% select(START.DATE, END.DATE),
    lst_cleaned_dfs[[fname]] %>%
      select(START.DATE1 = START.DATE, END.DATE1 = END.DATE)
  )  %>%
    filter(is.na(START.DATE1))
)

# Combine Data Frames
full_df = bind_rows(lst_cleaned_dfs)

# Question 1
ans_1 <- full_df %>% summarise(total=sum(AMOUNT))

# Question 2
library(lubridate)

ans_2 <- full_df %>% 
  filter(AMOUNT > 0, COVERAGE.PERIOD >=0) %>%
  summarise(std_coverage = sd(COVERAGE.PERIOD))

# Question 3
ans_3 <- full_df %>% 
  filter( 
    year(START.DATE) >= 2010,
    year(START.DATE) <= 2016,
    AMOUNT > 0
  ) %>% 
  summarise(annual_avg = sum(AMOUNT)/7)


# Question 4

exp_offices <- full_df %>%
  filter(year(START.DATE)==2016) %>%
  group_by(OFFICE) %>%
  summarise(ttl=sum(AMOUNT)) %>%
  arrange(desc(ttl))

highest = exp_offices$OFFICE[1]

ans_4 <- full_df %>% 
  filter(
    year(START.DATE)==2016, OFFICE==highest
  ) %>%
  arrange(desc(AMOUNT)) %>% 
  group_by(OFFICE, PURPOSE) %>%
  summarise(ttl=sum(AMOUNT)) %>% 
  mutate(prop = ttl/sum(ttl)) %>%
  arrange(desc(prop))











