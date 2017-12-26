setwd('N:/')

library('httr')
library('dplyr')
library('rtweet')
library('stringr')
library('pluralize')

source('keys.txt')
source('proc/functions.R')

# Max number of tweets retrieved for each user
max_tweets_per_user <- 3200

# The quantiles for selecting keywords and ngrams in TW descriptions
quantile_ngrams   <- 0.99
quantile_keywords <- 0.99

twitter_token <-
  oauth_app('twitter', key = oauth['key'], secret = oauth['secret']) %>%
  oauth1.0_token(oauth_endpoints('twitter'), .)


list_repec_twitter <-
  extract_tw(
    method = 'lists/members',
    parameters = list(slug='repec-twitter', owner_screen_name='chrMongeau')
  )

# We get both friends and tweets here so
# that we go through a single API limit
members_friends <- list()
members_tweets  <- list()

for (i in list_repec_twitter$id_str) {
  print(grep(i, list_repec_twitter$id_str))
  flush.console()

  members_friends[[i]] <-
    extract_tw(method = 'friends/ids', parameters = list(user_id = i))

  members_tweets[[i]] <- get_timeline(i, n = max_tweets_per_user)
}

common_keywords_on_list <-
  list_repec_twitter %>%
  select(description) %>%
  clean_tweets(variable = description) %>%
  # Also these in clean_tweets()?
  mutate(
    description = str_replace_all(description, '\\bopinions?|expressed|mine|rts?|retweets?|views?|endorsements?|tweet[^ ]*\\b', ''),
    description = str_replace_all(description, 'SOMENUMBER|HTTPLINK', ''),
    description = str_replace_all(description, '  *', ' ')
  ) %>%
  # At least three words (n spaces = n*1 words)
  filter(str_count(description, '  *') >= 3) %>%
  pull(description) %>%
  singularize()

twograms <-
  common_keywords_on_list %>%
  sapply(ngram_asweka, USE.NAMES = FALSE) %>%
  # for converting this to a vector...
  unlist() %>%
  table() %>%
  as.list() %>%
  unlist()

twograms_selection <-
  names(sort(twograms[twograms > quantile(twograms, quantile_ngrams)]))

# XXX removing PhD students
twograms_selection <-
  twograms_selection[!grepl('phd', twograms_selection)]

# XXX pasting everything, but regexps should have some char limits
twograms_regexp <-
  paste0('\\b(', paste(twograms_selection, collapse = '|'), ')\\b')

keywords <-
  common_keywords_on_list %>%
  sapply(strsplit, split = '  *', USE.NAMES = FALSE) %>%
  # for converting this to a vector...
  unlist() %>%
  table() %>%
  as.list() %>%
  unlist()

keywords_selection <-
  names(sort(keywords[keywords > quantile(keywords, quantile_keywords)]))

keywords_regexp <-
  paste0('\\b(', paste(keywords_selection, collapse = '|'), ')')

all_members_friends <-
  members_friends[sapply(members_friends, nrow) >0] %>%
  reshape2::melt() %>%
  tbl_df()

most_followed_not_on_list <-
  all_members_friends %>%
  count(value) %>%
  arrange(desc(n)) %>%
  filter(!(value %in% list_repec_twitter$id_str))

# Just for a subset: users followed by at least 3 users on list
# (help page says that a max of 90,000 can be retrieved each 15 mins)
info_most_followed_not_on_list <-
  most_followed_not_on_list %>%
  filter(n >= 3) %>%
  pull(value) %>%
  lookup_users()

# 'n_max_followers' will be used for filtering out the followed accounts
# with a number of followers more than its value. The first attempt was
# to use the most followed economist on Twitter (Krugman, as of 2017-12):
#
#n_max_followers <- list_repec_twitter %>% pull(followers_count) %>% max()
#
# However, it kept accounts like @TheTweetOfGod, @Snowden. As os 2017-12,
# a good start is 1,000,000: it will keep a lot of non-economists (e.g.,
# @ForeignPolicy, @ReutersScience), but allows to keep famous economists
# (one for all: @yanisvaroufakis). Probably, the number can even be set
# to 500,000, by manually picking those with a number of followers between
# 500,000 and 1,000,000 (no attempt was done in this direction)
n_max_followers <- 1000000

possible_economists <-
  info_most_followed_not_on_list %>%
  filter(followers_count < n_max_followers) %>%
  # 'descr_clean' (treatment similar to common_keywords_on_list)
  # will be used to search the most common keywords (ngrams)
  # 'name_clean' will be used instead of 'name'
  mutate(
    descr_clean = description,
    # Remove weird stuff from names
    name_clean  = str_replace_all(name, '[^ \\w]', '')
  ) %>%
  clean_tweets(variable = descr_clean) %>%
  mutate(
    descr_clean = str_replace_all(descr_clean, '\\bopinions?|expressed|mine|rts?|retweets?|views?|endorsements?|tweet[^ ]*\\b', ''),
    descr_clean = str_replace_all(descr_clean, 'SOMENUMBER|HTTPLINK', ''),
    descr_clean = str_replace_all(descr_clean, '  *', ' ')
  ) %>%
  filter(
    # remove names with only 3 or more uppercase letters
    !str_detect(name, '[A-Z]{3,}'),
    # PhD students
    !str_detect(descr_clean, 'phd (candidate|student)'),
    # 'The' something
    !str_detect(name_clean, '\\b[Tt]he '),
    # News websites (as 'Reuters Science News') and other 'New' something
    !str_detect(name_clean, '\\b[Nn]ews?\\b'),
    # As above, on screen_name (here, only 'News' and no word-boundary)
    !str_detect(screen_name, '[Nn]ews'),
    # Jornals / magazines
    !str_detect(name_clean, '[Jj]ournal|[Mm]agazine'),
    # UN agencies
    !str_detect(name_clean, '\\bUN\\b'),
    # European institutions, or Europe-related
    !str_detect(name_clean, '\\b[Ee]urope'),
    !str_detect(name_clean, 'E.?U.?|U.?K.?|U.?S.?'), # here also UK, US
    # 'Watch' something (e.g., 'Finance Watch')
    !str_detect(name_clean, '\\b[Ww]atch\\b'),
    # Banks
    !str_detect(name_clean, '\\b[Bb]ank\\b'), # XXX will keep Bundesbank
    # Institutes
    !str_detect(name_clean, '\\b[Ii]nstitut'),
    # Finance related accounts
    !str_detect(name_clean, '\\b[Ff]inance\\b'),
    # 'econ...' in username.
    # (It's very unlikely that a person puts that in the username)
    !str_detect(name_clean, '[EeÉé]con'),
    # Journalists
    !str_detect(descr_clean, '\\bjournalist'),
    # 'research' something (as 'NY Fed Research', 'Pew Research Center')
    !str_detect(descr_clean, '[Jj]ournal|[Mm]agazine'),
    # Feds
    !str_detect(name_clean, '(Fed|[Ff]ederal)\\b'),
    # Universities
    !str_detect(name_clean, '\\b[Uu]niversi'),
    # Foundations
    !str_detect(name_clean, '\\b[Ff]oundation'),
    # 'science' something
    !str_detect(name_clean, '\\b[Ss]cience'),
    # 'business' something
    !str_detect(name_clean, '\\b[Bb]usiness'),
    # 'data' something
    !str_detect(name_clean, '\\b[Dd]ata'),
    # Accounts with at least 5 characters in name
    str_count(name, ' ') >= 1,
  )

# Singularise descr_clean
p <- progress_estimated(nrow(possible_economists))
for (i in 1:nrow(possible_economists)) {

  possible_economists$descr_clean[i] <-
    possible_economists$descr_clean[i] %>%
    strsplit(split = '  *') %>%
    unlist() %>%
    singularize() %>%
    paste(collapse = ' ')

   p$tick()$print()
}

# Number of most common keywords, not necessarity ngrams
likely_economists <-
  possible_economists %>%
  mutate(
    case =
      case_when(
        # Very likely to be economists
        str_detect(descr_clean, twograms_regexp) ~ 1L,
        # Quite likely to be economists
        str_count(descr_clean, keywords_regexp) >= 2 ~ 2L,
        # Professors, hopefully of economics
        str_detect(description, '[Pp]rofessor') ~ 3L,
        # Self-defined Economists
        str_detect(description, '[Ee]conomist') ~ 4L,
        TRUE ~ 0L
      )
  ) %>%
  filter(case > 0)

# vi: set ts=2 sw=2 et
