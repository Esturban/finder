#Load the SMBs data from the current date and time
# trailing<-unique(gsub("smbs_|tformed_","",list.files("data/",pattern = ".RDS",include.dirs = F)))
# for(i in trailing)
# {

# smbs %>%
#   #Remove etsy, IG and FB
#   #Approximately 14% removal
#   dplyr::filter(!on_etsy, !on_instagram, !on_fb)%>%
#   .[sample.int(nrow(.), 20), ]%>%select(1:5)%>%tibble()%>%tidyr::unnest(Photo)%>%select(Name,Category,url,thumbnails)->smb_int

smbs <-readRDS(paste0('data/smbs_', format(Sys.time(), "%Y%m%d"), '.RDS'))
# smbs <-readRDS(paste0('data/smbs_', i))
smbs_unique<-smbs%>%unique(.)%>%
  .[!duplicated(.$Name),]
table(smbs_unique$Category)%>%.[order(.,decreasing = T)]
smbs_unique %>%
  dplyr::mutate(
    #Binary flags of categories
    apparel_accessories = map_lgl(Category, grepl, pattern = 'APPAREL'),
    art = map_lgl(Category, grepl, pattern = 'ART'),
    beauty = map_lgl(Category, grepl, pattern = 'BEAUTY'),
    books = map_lgl(Category, grepl, pattern = 'BOOKS'),
    general = map_lgl(Category, grepl, pattern = 'GENERAL'),
    decor = map_lgl(Category, grepl, pattern = 'DECOR'),
    health = map_lgl(Category, grepl, pattern = 'HEALTH'),
    instruments = map_lgl(Category, grepl, pattern = 'INSTRUMENTS'),
    sustainable = map_lgl(Category, grepl, pattern = 'SUSTAINABLE'),
    goods = map_lgl(Category, grepl, pattern = 'GOODS'),
    music = map_lgl(Category, grepl, pattern = 'MUSIC'),
    printshops = map_lgl(Category, grepl, pattern = 'PRINTSHOPS'),
    stationery = map_lgl(Category, grepl, pattern = 'STATIONERY'),
    vintage = map_lgl(Category, grepl, pattern = 'VINTAGE'),
    floral = map_lgl(Category, grepl, pattern = 'FLORAL'),
    food = map_lgl(Category, grepl, pattern = 'FOOD'),
    handmade = map_lgl(Category, grepl, pattern = 'HANDMADE'),
    fun_games = map_lgl(Category, grepl, pattern = 'GAMES'),
    other_fun = map_lgl(Category, grepl, pattern = 'OTHER FUN STUFF'),
    children_baby = map_lgl(Category, grepl, pattern = 'CHILDREN'),
    jewelry = map_lgl(Category, grepl, pattern = 'JEWELRY'),
    weed = map_lgl(Category, grepl, pattern = 'WEED'),
    wine_beer = map_lgl(Category, grepl, pattern = 'WINE'),
    craft_hobby = map_lgl(Category, grepl, pattern = 'CRAFT'),
    coffee_tea = map_lgl(Category, grepl, pattern = 'COFFEE'),
    services = map_lgl(Category, grepl, pattern = 'SERVICES'),
    spirits = map_lgl(Category, grepl, pattern = 'SPIRITS'),
    restaurants = map_lgl(Category, grepl, pattern = 'RESTAURANTS'),
    pets = map_lgl(Category, grepl, pattern = 'PETS'),
    #Determining the domain names
    on_etsy = map_lgl(`Link to Website`, grepl, pattern = 'etsy'),
    on_instagram = map_lgl(`Link to Website`, grepl, pattern = 'instagram'),
    on_fb = map_lgl(`Link to Website`, grepl, pattern = 'facebook'),
    not_on_ig_etsy = map_lgl(`Link to Website`, grepl, pattern = '^instagram|^etsy|^facebook'),
    #Image source from the NAT grids
    img_src = map_chr(.x = Photo,  ~ regmatches(
      .x$url, gregexpr("(?<=\\().*?(?=\\))", .x$url, perl = T)
    )[[1]][1]),
    #Datatable image source and size for the DT
    dt_img = paste0('<img src="', img_src, '" height="52"></img>'),
    #Location of the local images for resources
    loc_img = map_chr(.x = img_src,  ~ {
      # browser()
      # print(.x)
      if (!dir.exists('data/imgs/'))
        dir.create('data/imgs/', recursive = T)
      local_file <- paste0('data/imgs/', basename(gsub("https://dl.airtable.com/.attachments","",.x)))
      # print(.x)
      if (!file.exists(local_file) & sum(grepl("jpeg|jpg|png",local_file))>0){
        
        download.file(url = .x,
                      mode = 'wb',
                      destfile = local_file)}
      return(local_file)
    })
    # on_etsy =
  ) -> smbs_test

length(unique(smbs_test$offset))
# ~7.9% on etsy
sum(smbs_test$on_etsy) / nrow(smbs_test)
# ~6% on instagram
sum(smbs_test$on_instagram) / nrow(smbs_test)
# ~0.6% on facebook
sum(smbs_test$on_fb) / nrow(smbs_test)

smbs_test%>%dplyr::filter(!on_etsy, !on_instagram, !on_fb)%>%tibble::tibble()

length(unique(smbs_test$Name))
length(is.null(smbs_test$loc_img))
table(smbs_test$loc_img)%>%.[order(.,decreasing = T)]%>%head(.)
table(smbs_test$Name)%>%.[order(.,decreasing = T)]%>%head(.)

table(basename(smbs_test$`Link to Website`))%>%.[order(.,decreasing = T)]%>%head()


saveRDS(object = smbs_test,file = paste0(here::here('data'),'/smbs_tformed_',format(Sys.time(),"%Y%m%d"),'.RDS'))
# saveRDS(object = smbs_test,file = paste0(here::here('data'),'/smbs_tformed_',i))
# }
smbs_test %>%
  dplyr::filter(on_etsy) %>%
  .[!duplicated(.$Name),]%>%
  dplyr::select(Name:URL, on_etsy, on_instagram, img_src, dt_img) %>%
  DT::datatable(., escape = F)
