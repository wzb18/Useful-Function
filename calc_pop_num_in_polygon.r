rm(list = ls()); gc()

library(data.table)
library(dplyr)
library(jsonlite)
library(stringr)
library(sf)

#### read json data, convert to polygon
xjxy <- read_json("dis_range_1.json")
xjxy_name <- xjxy[[1]]$list_name
xjxy_name_path <- as.matrix(bind_rows(xjxy[[1]]$path))
p1 <- xjxy_name_path[c(1:6, 1),]
xjxy_name_polygon <- st_as_text(st_polygon(list(p1)))
plot(st_polygon(list(p1)))

kxc <- read_json("dis_range_2.json")
kxc_name <- kxc[[1]]$list_name
kxc_name_path <- as.matrix(bind_rows(kxc[[1]]$path))
p1 <- kxc_name_path[c(1:nrow(kxc_name_path), 1),]
kxc_name_polygon <- st_as_text(st_polygon(list(p1)))
plot(st_polygon(list(p1)))


lbgy <- read_json("dis_range_3.json")
lbgy_name <- lbgy[[1]]$list_name
lbgy_name_path <- as.matrix(bind_rows(lbgy[[1]]$path))
p1 <- lbgy_name_path[c(1:nrow(lbgy_name_path), 1),]
lbgy_name_polygon <- st_as_text(st_polygon(list(p1)))
plot(st_polygon(list(p1)))

dist_data <- data.frame(dist = c(xjxy_name, kxc_name, lbgy_name),  geometry = c(xjxy_name_polygon, kxc_name_polygon, lbgy_name_polygon))
dist_data$geometry <- st_as_sfc(dist_data$geometry)

#### grid city 
xian_grid_use <- fread("grid_xian.csv")
xian_grid_use <- xian_grid_use[xian_grid_use$grid_level == 50]
xian_grid_use$box <- NULL
xian_grid_use$grid_level <- NULL
xian_grid_use$city <- NULL
xian_grid_use$grid_lat <- str_replace(xian_grid_use$grid_no, "-.*", "")
xian_grid_use$grid_lon <- str_replace(xian_grid_use$grid_no, ".*-", "")
xian_grid_use$lat <- as.numeric(str_replace_all(xian_grid_use$center, ".* |\\)", ""))
xian_grid_use$lon <- as.numeric(str_replace_all(xian_grid_use$center, "(POINT\\()|( .*\\))", ""))
xian_grid_use <- select(xian_grid_use, grid_no, grid_lat, grid_lon, lat, lon,  center)
xian_grid_use$center <- st_as_sfc(xian_grid_use$center)
xian_grid_use$center <- st_geometry(xian_grid_use$center)

##### 
xian_grid_use_test <- xian_grid_use[xian_grid_use$lat >= 34 &  xian_grid_use$lon >= 108.5]


##### dat_home_grid_popwnum
dat_home_out <- fread("dat_home.csv")

##### function input distname, return pop_wnum, pop_wnum around, dist area, dist around area, project pop_wnum
pop_num_cal_use_area <- function(dist_check){
  aa <- st_contains(dist_data$geometry[dist_data$dist == dist_check], xian_grid_use_test$center)[[1]]  
  # aa <- st_contains(dist_data$geometry[2], xian_grid_use_test$center)[[1]]  
  # aa <- st_contains(dist_data$geometry[3], xian_grid_use_test$center)[[1]]  
  
  
  xj_dist_grid <- xian_grid_use_test[aa,]
  pop_wnum_o <- sum(dat_home_out$pop_wnum[dat_home_out$homeid %in% xj_dist_grid$grid_no])
  # sum(dat_home_job$pop_wnum[dat_home_job$homeid %in% xj_dist_grid$grid_no])
  
  
  xj_dist_grid_around_tmp <- xj_dist_grid[, c("grid_no", "grid_lat", "grid_lon")]
  xj_dist_grid_around <- data.frame(grid_no = rep(xj_dist_grid_around_tmp$grid_no, each = 25),
                                    grid_lat = rep(xj_dist_grid_around_tmp$grid_lat, each = 25),
                                    grid_lon = rep(xj_dist_grid_around_tmp$grid_lon, each = 25),
                                    add_lat = rep(rep(c(-2:2), each = 5), nrow(xj_dist_grid_around_tmp)),
                                    add_lon = rep(rep(c(-2:2), times = 5), nrow(xj_dist_grid_around_tmp)))
  xj_dist_grid_around$grid_lat <- as.numeric(as.character(xj_dist_grid_around$grid_lat))
  xj_dist_grid_around$grid_lon <- as.numeric(as.character(xj_dist_grid_around$grid_lon))
  
  
  xj_dist_grid_around$grid_around <- paste0(xj_dist_grid_around$grid_lat + xj_dist_grid_around$add_lat, "-", xj_dist_grid_around$grid_lon + xj_dist_grid_around$add_lon)
  
  
  xj_dist_grid_around <- unique(select(xj_dist_grid_around, grid_around))
  # xj_dist_grid_around$pop_wnum <- dat_home_out$pop_wnum[match(xj_dist_grid_around$grid_around, dat_home_out$homeid)]
  # xj_dist_grid_around$center <- xian_grid_use_test$center[match(xj_dist_grid_around$grid_around, xian_grid_use_test$grid_no)]
  
  # xj_dist_grid_around <- st_as_sf(xj_dist_grid_around)
  
  
  
  pop_num_big <- sum(dat_home_out$pop_wnum[dat_home_out$homeid %in% xj_dist_grid_around$grid_around])
  
  pop_num_total <- round(pop_wnum_o + (pop_num_big - pop_wnum_o) * length(aa) / (nrow(xj_dist_grid_around) + length(aa)))
  return(data.frame(dist_check, pop_wnum_o, pop_num_big, area = length(aa), area_total = nrow(xj_dist_grid_around), pop_num_total))
}


dist_data$dist
result <- list()
result[[1]] <- pop_num_cal_use_area("dist_name1")
result[[2]] <- pop_num_cal_use_area("dist_name2")
result[[3]] <- pop_num_cal_use_area("dist_name3")
