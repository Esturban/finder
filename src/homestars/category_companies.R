# category_finder

require(tidyr)
require(rjson)

categories_unq <- readRDS(
  file = here::here(
    "data",
    "homestars",
    "categories",
    "all_cities_categories_unique.RDS"
  )
)

start<-Sys.time()
unique_cat_links<-categories_unq %>%
  dplyr::filter(status_code == 200) %>%
  dplyr::pull(Link)
# lapply(unique_cat_links[1:2],function(cat_link){
lapply(unique_cat_links,function(cat_link){
  fname<-paste0("cat_companies_", gsub("/","_",gsub("[[:punct:]]|[[:blank:]]|https://homestars.com/on/", "", cat_link)), ".csv")
  if (!file.exists(here::here(
    "data",
    "homestars",
    "categories",
    "details",
   fname
  ))){
    print(fname)
    # browser()
    categories_unq %>%
    dplyr::filter(Link == cat_link)%>%
    dplyr::mutate(
      page = map(.x = Link, ~ {
        page_dl(.x, skip = T)
      }),
      n_hits = map_dbl(.x = page,  ~ {
        # browser()
        if(.x[['check']]==0){
          .x[['page']] %>% html_nodes(
            "#search-page > div.search-page__head-container > div.search-page__result-links > div"
          ) %>% html_attr("data-react-props")->page_data_attr
          
          tryCatch(rjson::fromJSON(page_data_attr),error=function(e){return(list(totalHits = 9))}) %>% .[['totalHits']] ->
            hits
          
          if (length(hits) == 1)
            return(as.numeric(hits))
          else
            return (9)}else return(0)
      }),
      all_links = map2(.x = Link, .y = n_hits, ~ {
        if (.y > 10)
          paste0(.x, "?&page=", 2:ceiling(.y / 10), "&")
        else
          .x
      }),
      results = map(.x = page, ~ {
        if(.x[['check']]==0).x[['page']] %>% html_nodes("#results-sponsors > section > section > div") %>% html_attr("data-react-props") %>% lapply(., rjson::fromJSON) %>% unique(.)%>%return(.) 
        else return(NULL)
      }),
      all_results = map2(.x = all_links, .y = results, ~ {
        if (length(.x) > 1)
        {
          lapply(.x, page_dl,skip = T) -> pages
          
          results<- lapply(pages, function(x) {
            if (x$check == 0) {
              x[['page']] %>%
                html_nodes("#results-sponsors > section > section > div") %>%
                html_attr("data-react-props") %>%
                lapply(., rjson::fromJSON) %>%
                unique(.) -> page_results
              return(page_results)
            }
          })%>%unlist(.,recursive = F) %>% purrr::compact(.) %>% c(.y, .) -> results
          return(results)
        } else{
          return(.y)
        }
      }),
      companies_df = map(.x = all_results,  ~ {
        # browser()
        if(length(.x)>0) lapply(.x, function(x) {
          tibble(
            category = x[['categoryLinks']][[1]]$name,
            id = x[['company']][['id']],
            url = nullif(x[['company']][['url']]),
            hs_path = nullif(paste0(x[['serverPath']], x[['companyPath']])),
            profile = nullif(x[['company']][['profile']]),
            permalink = nullif(x[['company']][['permalink']]),
            created_at = nullif(x[['company']][['created_at']]),
            sic_code = nullif(x[['company']][['sic_code']]),
            service_area = nullif(x[['company']][['service_area']]),
            hours = nullif(x[['company']][['hours_of_operation']]),
            products = nullif(x[['company']][['products']]),
            services = nullif(x[['company']][['services']]),
            brands = nullif(x[['company']][['brands']]),
            specialities = nullif(x[['company']][['specialities']]),
            established = nullifN(x[['company']][['year_established']]),
            employee_cnt = nullifN(x[['company']][['number_of_employees']]),
            payment_method = nullif(x[['company']][['payment_method']]),
            diplomas = nullif(x[['company']][['diplomas']]),
            licenses = nullif(x[['company']][['licenses']]),
            memberships = nullif(x[['company']][['memberships']]),
            liability_insurance = nullif(x[['company']][['liability_insurance']]),
            bonded = nullif(x[['company']][['bonded']]),
            project_rate = nullif(x[['company']][['project_rate']]),
            project_minimum = nullif(x[['company']][['project_minimum']]),
            written_contract = nullif(x[['company']][['written_contract']]),
            written_warranty = nullif(x[['company']][['written_warranty']]),
            warranty_terms = nullif(x[['company']][['warranty_terms']]),
            parent_company_id = nullif(x[['company']][['parent_company_id']]),
            name = nullif(x[['company']][['name']]),
            total_number_of_reviews_cache = nullifN(x[['company']][['total_number_of_reviews_cache']]),
            total_number_of_approved_reviews_cache = nullifN(x[['company']][['total_number_of_approved_reviews_cache']]),
            number_of_photos_cache = nullifN(x[['company']][['number_of_photos_cache']]),
            avg_rating_cache = nullifN(x[['company']][['avg_rating_cache']]),
            categories_info_cache = x[['company']][['categories_info_cache']],
            savings_count = nullifN(x[['company']][['savings_count']]),
            claimed_at = x[['company']][['claimed_at']],
            subdomain = x[['company']][['subdomain']],
            fraudulent = x[['company']][['fraudulent']],
            fraudulent_count = nullifN(x[['company']][['fraudulent_count']]),
            total_number_of_pending_reviews_cache = nullifN(x[['company']][['total_number_of_pending_reviews_cache']]),
            images_count = nullifN(x[['company']][['images_count']]),
            completion_percent_cache = x[['company']][['completion_percent_cache']],
            bully = x[['company']][['bully']],
            secret = x[['company']][['secret']],
            replied_to_at_least_one_review_cache = x[['company']][['replied_to_at_least_one_review_cache']],
            latest_approved_review_created_at = x[['company']][['latest_approved_review_created_at']],
            has_set_tasks = x[['company']][['has_set_tasks']],
            lead_purchase_eligible = x[['company']][['lead_purchase_eligible']],
            service_area_id = x[['serviceAreaId']],
            star_score = nullif(as.character(x[['starScore']])),
            quote_button_category = x[['quoteButton']][['categoryId']],
            direct_phone = nullif(x[['company']][['direct_phone']]),
            direct_contact = nullif(x[['company']][['direct_contact']]),
          )
        }) %>% dplyr::bind_rows(.)%>%return(.) 
        
      }),all_links = paste0(all_links,collapse = "; ")
    )%>%dplyr::select(Link,n_hits,all_links,companies_df)->cc_list
    
    cc_list%>%tidyr::unnest(cols = c(companies_df))%>%unique(.)->ccc
    
    readr::write_csv(ccc,
                     here::here(
      "data",
      "homestars",
      "categories",
      "details",
      fname
    ))
    }
    
})

end<-Sys.time()
end-start
# for(i in 1:10){
#   hits_result %>%pull(results)%>%.[[1]]%>%.[[i]]%>%.[['quoteButton']]%>%.[['company']]%>%stack(.)->test_stack
# hits_result %>%pull(results)%>%.[[1]]%>%.[[i]]%>%.[['company']]%>%stack(.)->test_stack2
# print(identical(test_stack,test_stack2))
#
# }

