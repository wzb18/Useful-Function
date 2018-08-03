### wgs_84转百度
x_pi = 3.14159265358979324 * 3000.0 / 180.0
pi = 3.1415926535897932384626  # π
a = 6378245.0  # 长半轴
ee = 0.00669342162296594323  # 偏心率平方 

transformlat <- function(lng, lat){
  ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret = ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret = ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret = ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
  return(ret)
}

transformlng <- function(lng, lat){
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  ret = ret + (20.0 * sin(6.0 * lng * pi) + 20.0 *sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret = ret + (20.0 * sin(lng * pi) + 40.0 * sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret = ret + (150.0 * sin(lng / 12.0 * pi) + 300.0 * sin(lng / 30.0 * pi)) * 2.0 / 3.0
  return(ret) 
}


out_of_china <- function(lng, lat){
  return(lng < 72.004 | lng > 137.8347 | lat < 0.8293 | lat > 55.8271)
}

wgs84_to_gcj02 <- function(lng, lat){
  ###WGS84到火星坐标
  if(out_of_china(lng, lat)){
    return(c(lng, lat))
  }
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return(c(mglng, mglat))
}




gcj02_to_bd09 <- function(lng, lat){
# 火星坐标系(GCJ-02)转百度坐标系(BD-09)| 谷歌、高德——>百度
  z = sqrt(lng * lng + lat * lat) + 0.00002 * sin(lat * x_pi)
  theta = atan2(lat, lng) + 0.000003 * cos(lng * x_pi)
  bd_lng = z * cos(theta) + 0.0065
  bd_lat = z * sin(theta) + 0.006
  return(c(bd_lng, bd_lat))
}


bd09_to_gcj02<- function(lng, lat){
  # 百度坐标系(BD-09)转火星坐标系(GCJ-02) |百度-->谷歌、高德
  x = lng - 0.0065
  y = lat - 0.006
  z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
  gg_lng = z * cos(theta)
  gg_lat = z * sin(theta)
  return(c(gg_lng, gg_lat))
}

gcj02_to_wgs84 <- function(lng, lat){
  ###火星坐标到wgs84
  if(out_of_china(lng, lat)){
    return(c(lng, lat))
  }
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return(c(lng * 2 - mglng, lat * 2 - mglat))
}



wgs84_to_bd09 <- function(lng, lat){
  ###wgs84到百度
  coordinates <-  wgs84_to_gcj02(lng, lat)
  return(gcj02_to_bd09(coordinates[1], coordinates[2]))
}

bd09_to_wgs84 <- function(lng, lat){
  ###百度到wgs84
  coordinates = bd09_to_gcj02(lng, lat)
  return(gcj02_to_wgs84(coordinates[1], coordinates[2]))
}
