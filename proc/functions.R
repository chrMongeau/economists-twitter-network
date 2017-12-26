epoch <- function(x) {
  return(as.numeric(format(x, format='%s')))
}

tw_sleep <- function(resource = NA) {

  rate_limit <-
    paste0('https://api.twitter.com/1.1/', 'application/rate_limit_status.json') %>%
    GET(config(token = twitter_token)) %>%
    content()

  x <- rate_limit$resources[[strsplit(resource, split = '/')[[1]][1]]][[paste0('/', resource)]]$reset
  to_sleep <- x - epoch(Sys.time()) + 10

  print(paste('Sleeping until', format(Sys.time()+to_sleep, '%H:%M:%S')))
  flush.console()
  Sys.sleep(to_sleep)
}

# dplyr 7+ and rlang
clean_tweets <- function(data = NA, variable = NA) {
  variable = enquo(variable)
  var_name = quo_name(variable)

  data %>%
  mutate(
    !!var_name := str_replace_all(!!variable, '\n|\r', ' '),
    # retweets like "xxx via @username"
    !!var_name := str_replace(!!variable, 'via @.*', ''),
    # Remove 1|2 non-capital characters (so keep "US", "NY", "EU", etc.)
    !!var_name := str_replace_all(!!variable, '\\b[a-z]{1,2}\\b +', ' '),
    # everythig lowercase now
    !!var_name := str_to_lower(!!variable),
    # replace all links. It could be nice to separate links from media. TODO
    !!var_name := str_replace_all(!!variable, 'http[^ ]+', ' HTTPLINK '),
    # Remove multiple -s as they are used as : (especially --)
    !!var_name := str_replace_all(!!variable, '-{2,}', ' '),
    # Remove @names
    !!var_name := str_replace_all(!!variable, '@([^ ]+)', ' '),
    # remove some HTML entities (XXX any left?)
    !!var_name := str_replace_all(!!variable, '&(gt|lt);', ' '),
    # &amp; = &
    !!var_name := str_replace_all(!!variable, '&amp;', ' & '),
    # Add space befor # (e.g., #consensus2017#cryptoparty)
    !!var_name := str_replace_all(!!variable, '([^ ])#', '\\1 #'),
    # remove # from hashtags (the #+ is because, e.g., ##hashtag)
    !!var_name := str_replace_all(!!variable, '#+([^ ]+)', '\\1'),
    # replace numbers with a placeholder.
    # XXX what about things like "Friday Jan. 5" or "10 years"??? 
    # XXX Note also that things like 1-on-1 will break (but are not very common)
    !!var_name := str_replace_all(!!variable, '[0-9]?[0-9,.]+ ?%?', ' SOMENUMBER '),
    # remove punctuation or other unnecessary stuff
    # (note that the first one is not a normal space (ASCII 32),
    # but a non-breaking space ASCII 160
    !!var_name := str_replace_all(!!variable, '[ `|°\u201e\u201d\u2026\u2022\u2014~\u00ab\u00bb^%\u201c\u201d£\u20ac$\\\\@/\\*+"¿?¡!.,;:=(){}\\[\\]_]', ' '),
    # characters require special treatment in regex
    !!var_name := str_replace_all(!!variable, '[\u2018\u2019]', "'"),
    # remove single-enquoted words
    !!var_name := str_replace_all(!!variable, "'([^ ]+)'?", '\\1'),
    ## remove left-enquoted words (e.g., 'absorptive)
    #!!var_name := str_replace_all(!!variable, "'([^ ]", ''),
    # remove one-letter words
    !!var_name := str_replace_all(!!variable, '\\b[a-z]\\b', ' '),
    # remove words of 30+ characters (hashtags, e.g.: #noshortageswhenpricesfreetoadjust)
    !!var_name := str_replace_all(!!variable, '[^ ]{30,}', ' '),
    # XXX At this point it SHOULD be safe to remove non-alpha chars
    !!var_name := str_replace_all(!!variable, '[^a-zA-Z0-9 ]', ' '),
    # fix random stuff
    !!var_name := str_replace_all(!!variable, 'yess*', 'yes'),
    # remove twitter-jargon
    !!var_name := str_replace_all(!!variable, '\\b(rt|ht|via)\\b', ' '),
    # remove numbers and number-related stuff
    !!var_name := str_replace_all(!!variable, '\\b(one|two|three|four|five|six|seven|eight|nine|ten|ii|iii|st|nd|rd|th)\\b', ' '),
    # remove some avvreviations, interjections
    !!var_name := str_replace_all(!!variable, '\\b(abt|etc|ah|oh|btw|hey)\\b', ' '),
    # remove abbreviations of internet slang
    !!var_name := str_replace_all(!!variable, '\\b(omg|lol|imo|imho|irl)\\b', ' '),
    # Keep American English
    !!var_name := str_replace_all(!!variable, 'lisation', 'lization'), # generalisation, visualisation
    !!var_name := str_replace_all(!!variable, '(lab|col|neigh|rum)our', '\\1or'),
    # replace abbreviations
    !!var_name := str_replace_all(!!variable, '(thn?ks|thx)', 'thanks'),
    !!var_name := str_replace_all(!!variable, '\\bmkt\\b', 'market'),
    # remove words coming from stopwords('english') in groups of max 10
    # Doing it here instead of corpus as tweets with less than n words will be removed
        !!var_name := str_replace_all(!!variable, '\\b(a|about|above|after|again|against|all|am|an|and)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(any|are|as|at|be|because|been|before|being|below)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(between|both|but|by|cannot|could|did|do|does|doing)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(down|during|each|few|for|from|further|had|has|have)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(having|he|her|here|hers|herself|him|himself|his|how)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(i|if|in|into|is|it|its|itself|me|more)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(most|my|myself|no|nor|not|of|off|on|once)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(only|or|other|ought|our|ours|ourselves|out|over|own)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(same|she|should|so|some|such|than|that|the|their)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(theirs|them|themselves|then|there|these|they|this|those|through)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(to|too|under|until|up|very|was|we|were|what)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(when|where|which|while|who|whom|why|with|would|you)\\b', ' '),
        !!var_name := str_replace_all(!!variable, '\\b(your|yours|yourself|yourselves)\\b', ' '),
    # things that appear not in previous list, but do their negation
        !!var_name := str_replace_all(!!variable, '\\b(can)\\b', ' '),
    # Negations: don = don't, won = won't (however, won can be past of win...)
        !!var_name := str_replace_all(!!variable, '\\b(don|won|isn|doesn)\\b', ' '),
    # auxiliary
        !!var_name := str_replace_all(!!variable, '\\b(will)\\b', ' '),
    # other stuff
        !!var_name := str_replace_all(!!variable, '\\b(without|whose|else|among|whithin|also|the|even|thus|whether|yet|maybe|sm)\\b', ' '),
    # trim whitespace at the beginning and end
    !!var_name := str_trim(!!variable, side = 'both'),
    # just one space, please
    !!var_name := str_replace_all(!!variable, '  *', ' ')
  )
}


extract_tw <- function(method = NA, parameters = NA) {

# slug='repec-twitter', owner_screen_name='chrMongeau'

# user_id='935588642747232256'

  if (method == 'lists/members') {
    url <- paste0('https://api.twitter.com/1.1/', 'lists/members.json',
      '?slug=', parameters$slug, '&owner_screen_name=', parameters$owner_screen_name,
      '&count=5000&skip_status=0')

    what <- 'users'
  }

  if (method == 'friends/ids') {
    url <- paste0('https://api.twitter.com/1.1/', 'friends/ids.json?', 'cursor=', -1,
       '&user_id=', parameters$user_id, '&count=5000&stringify_ids=true')

    what <- 'ids'
  }

  a <- GET(url, config(token = twitter_token))
  b <- content(a)

  while (!is.null(b$errors)) {
    if (b$errors[[1]]$code == 88)  {
      print('API ERROR: LIMIT') ; flush.console()

      tw_sleep(method)

      a <- GET(url, config(token = twitter_token))
      # check for a$status == 200?
      
      b <- content(a)

    } else {
      stop('Some error')
    }
  }

  res <-
    lapply(
      b[[what]],
      function(x) {
        list_pos <- sapply(x, function(x) is.list(x))

        null_pos <- sapply(x, function(x) is.null(x))

        z <- x
        z[list_pos | null_pos] <- NA

        z <- as_data_frame(z)

        #z[list_pos] <- x[list_pos]
        z[!list_pos]

        return(z)
      }
    )

  return(do.call(dplyr::bind_rows, res))
}


# vi: set ts=2 sw=2 et
