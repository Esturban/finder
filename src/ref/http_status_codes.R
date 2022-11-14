# http_status_codes
if(!file.exists(here::here("data","ref","http_codes.csv"))){"https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml"%>%
  read_html%>%
  html_nodes("#table-http-status-codes-1")%>%
  html_table->http_codes

readr::write_csv(http_codes[[1]],file = here::here("data","ref","http_codes.csv"))}