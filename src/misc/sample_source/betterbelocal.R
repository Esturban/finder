require(rvest)
'https://betterbelocal.com/toronto-restaurants'%>%read_html->local_res_page
local_res_page%>%html_nodes('.image-block-wrapper > div ')%>%html_children(.)
local_res_page%>%html_nodes('.image-block-wrapper > div ')%>%html_attrs(.)
local_res_page%>%html_nodes('.image-block-wrapper > div ')%>%html_nodes('img')
local_res_page%>%html_nodes('.image-block-wrapper > a')
local_res_page%>%html_nodes('strong')
local_res_page%>%html_nodes('a > div')%>%html_attrs()
