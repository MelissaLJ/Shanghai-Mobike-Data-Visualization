## 引言
该项目数据集来自2016年8月份上海共享单车骑行数据，数据集共包含102361条骑行记录，涉及10个变量，进行简单的数据清洗后，用Tableau可视化分析结果。
## 安装
#### 数据清洗部分
数据清洗部分需要安装 R。可以从[CRAN](https://cran.r-project.org/) 下载并安装 R。 安装 R 以后，你需要下载并安装 [R Studio](https://www.rstudio.com/products/rstudio/download/)。
#### 数据可视化部分
数据可视化部分需要安装 Tableau Public 或 Tableau Desktop。你需要获得使用 Tableau Desktop 的软件许可证。如果你没有，可以在此 下载 [Tableau Public](https://public.tableau.com/s/)。
## 数据清洗
#### 加载包
library(stringr)
library(dplyr)
library(tidyr)
library(dplyr)  
#### 数据类型的转换
将订单编号orderid，自行车编号bikeid和用户编号userid从int转化为factor;
将骑行开始时间start_time和结束时间end_time转化为datetime数据类型；
#### 创建新的变量
统计每个bike被使用的次数；
计算每个骑行订单的骑行时长；
计算每个骑行订单发生在周几；
#### 转换数据集格式
原数据集每个订单的骑行轨迹都集中在一个单元格里，需要转换为长格式显示，同时用1和2分别标记出每个骑行记录的起始点和终止点。
#### 删除多余的变量
#### 保存清洗后的数据集
## Tableau数据可视化
共包含5个故事点：
#### 引言
概述分析思路，串联故事点；
#### 骑行活跃度研究
通过热图研究了周一到周日一天内不同时间点的骑行活跃度情况（以订单数计），发现工作日和周末的区别后，又利用柱状图分别研究了周末和工作日的骑行订单数情况，
三张图做了高亮显示的交互；
#### 平均骑行时间v.s.骑行次数
通过散点图研究二者关系；
#### 用户骑行路线查询
通过筛选框可以输入用户id，进而在地图上显示出该用户本月骑行轨迹；
#### 单车使用情况
通过饼图显示了自行车的使用次数分布情况，同时通过散点图研究单车被骑行次数和被骑行时间的关系，从而可以帮助我们锁定潜在的磨损严重的单车；
#### 单车停放情况
在地图上显示单车停放情况，以颜色和形状表示停放密度。


