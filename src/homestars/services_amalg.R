# services_interim

services_df<-readRDS(here::here(
  "data",
  "homestars",
  "categories",
  "companies_details_original_source.RDS"
))
twitter_stop_terms <-
  c(
    ' ?(f|ht)tp(s?)://(.*)[.][a-z]+',
    'blogger.com(.*)[.][a-z]+',
    'aol.com(.*)[.][a-z]+',
    'aol.it(.*)[.][a-z]+',
    'kleencondition.com(.*)[.][a-z]+',
    'shareurl(.*)[.][a-z]+',
    '[/][a-z][0-9]+',
    '@',
    '=',
    'help',
    'wix',
    'status\\w+',
    'addtoany.\\w+',
    'search\\w+',
    '#\\w+',
    '&\\w+',
    'hash',
    'intent\\w+',
    'hey,\\w+',
    '[:digit:](.*)[.][a-z]+',
    '[^[:alnum:]_]',
    'ref_\\w+',
    'twsrc[^]\\w+',
    'langen',
    'h\\w+/',
    '/h\\w+',
    'armanch.c\\w+',
    '/as\\w+',
    '[[:blank:]]\\w+',
    '^\\d{1,45}$',
    '\\bhome\\b',
    '\\bdev\\b',
    'langen',
    '\\bshare\\b',
    '\\bhomes\\b',
    'shar\\w+',
    'iphonecomputers\\w+',
    'tag\\w+'
  )
# length(unique(services_regex_sample$domain))
print(paste0(twitter_stop_terms, collapse = "|"))
# names(services_df)
(services_df %>%
  dplyr::mutate(
    t_cnt = purrr::map_int(.x = twitter_user,  ~ {
      length(.x)
    }),
    twitter_user_regex = purrr::map(.x = twitter_user,  ~ {
      unique(trimws(gsub(
        paste0(twitter_stop_terms, collapse = "|"),
        "",
        tolower(URLdecode(.x))
      ))) %>% .[nchar(.) > 4]
    })
  ) %>%
  dplyr::filter(t_cnt > 1,!(domain %in% c(
    "https://twitter.com/", "https://goo.gl/"
  ))) %>%
  tidyr::unnest(twitter_user) %>%
  # dplyr::select(category, name, domain,twitter_user)%>%
  dplyr::select(category, name, domain, twitter_user,twitter_user_regex)->services_regex_sample) %>%
  # dplyr::mutate(twitter_user =) %>%
  print(n = 200)

readr::write_csv(x = services_regex_sample,file = here::here("twitter_user_problem.csv"))

services_df %>%
  dplyr::mutate(
    t_cnt = purrr::map_int(.x = twitter_user,  ~ {
      length(.x)
    }),
    i_cnt = purrr::map_int(.x = instagram_user,  ~ {
      length(.x)
    }))%>%
  dplyr::filter(t_cnt==1,i_cnt==1)%>%
  # dplyr::select(category, name, url, email, telephone) %>% print(n = 100)
  dplyr::select(category,
                name,
                url,
                email,
                telephone,
                instagram_user,
                twitter_user) %>%
  tidyr::unnest(c(instagram_user,twitter_user))%>%
  dplyr::mutate(email = trimws(email),
                instagram_user= trimws(instagram_user),
                twitter_user = trimws(twitter_user)) %>%
  dplyr::filter(!is.na(email), nchar(email) > 0 | nchar(instagram_user) > 0 | nchar(twitter_user) > 0)%>%
  readr::write_csv(., file = "sample_emails_with_social.csv")

services_df %>%
  # dplyr::select(category, name, url, email, telephone) %>% print(n = 100)
  dplyr::select(category,
                name,
                url,
                email,
                telephone,
                instagram_user,
                twitter_user) %>%
  dplyr::mutate(email = trimws(email)) %>%
  dplyr::filter(!is.na(email), nchar(email) > 0) %>%
  readr::write_csv(., file = "sample_emails_all.csv")
