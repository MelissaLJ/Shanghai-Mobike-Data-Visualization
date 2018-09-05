
# 加载包
library(stringr)
library(dplyr)
library(tidyr)
library(dplyr)
# 导入数据
md <- read.csv("mobike_shanghai_sample_updated.csv")
# 检查数据类型
str(md)
# 将orderid、bikeidhe userid从int改成factor
md$orderid <- as.factor(md$orderid)
md$bikeid <- as.factor(md$bikeid )
md$userid <- as.factor(md$userid )
# 将“start_time”和“end_time“数据类型改为datetime
md$start_time <- strptime(as.character(md$start_time),
                               format = "%Y-%m-%d %H:%M")
md$start_time <- as.POSIXct(md$start_time,format = "%Y-%m-%d %H:%M")

md$end_time <- strptime(as.character(md$end_time),
                             format = "%Y-%m-%d %H:%M")
md$end_time <- as.POSIXct(md$end_time,format = "%Y-%m-%d %H:%M")

# 增加新列计算每个bike被使用的次数
bikeid_count <- as.data.frame(table(md$bikeid))
names(bikeid_count) <- c("bikeid","bike_usage_count")
md.new <- left_join(md,bikeid_count,by = "bikeid")

# 将track列按照运动轨迹拆分为若干列
md.new0 <- separate(md.new,track,paste0("location",1:479),sep = "#")
# 长数据框变成短数据框
md.new1 <- gather(md.new0,"track_code","location","location1":"location479")
# 删除骑行轨迹为空值的点，并且按照订单编号排序
md.new2 <- subset(md.new1,!is.na(location)) %>% arrange(orderid)
# 将骑行轨迹各数据点按照经纬度分成两列
md.new3 <- separate(md.new2,location,c("location_x","location_y"),sep = ",") 
# 将"location_x"和"location_y"数据类型改为num
md.new3$location_x <- round(as.numeric(md.new3$location_x),3)
md.new3$location_y <- round(as.numeric(md.new3$location_y),3)
# 删除"track_code"列
drop <- c("track_code") 
md.new4 <- md.new3[,!(names(md.new3) %in% drop)]


# 创建新列计算骑行时长duration
md.new4$duration <- md.new4$end_time - md.new4$start_time
# 创建新列表示骑行时间是周几
md.new4$week <- weekdays(as.POSIXct(md.new4$start_time), abbreviate = F)

# 将骑行起始坐标转化为顺序值(1代表start，2代表end)
md.new4$start_location_x <- 
  round(as.numeric(as.character(md.new4$start_location_x)),3)
md.new4$start_location_y <-
  round(as.numeric(as.character(md.new4$start_location_y)),3)
md.new4$end_location_x <- 
  round(as.numeric(as.character(md.new4$end_location_x)),3)
md.new4$end_location_y <- 
  round(as.numeric(as.character(md.new4$end_location_y)),3)


md.new4$start_end <- ifelse(
  (md.new4$start_location_x == md.new4$location_x) &
  (md.new4$start_location_y == md.new4$location_y),
  1,ifelse(
    (md.new4$end_location_x == md.new4$location_x) &
    (md.new4$end_location_y == md.new4$location_y),
    2," ")) 

# 删除start_location_x、start_location_y、end_location_x和end_location_y列
drop1 <- c("start_location_x","start_location_y",
           "end_location_x","end_location_y")
md.new4 <- md.new4[,(!names(md.new4) %in% drop1)]

# 将数据框md.new4写入csv文件
write.csv(md.new4,file = "Mobike.csv")
