# replicate_paper_NHANES_coffee

复现《日常咖啡摄入与疼痛的关系：基于NHANES数据库的大样本横断面研究》论文，非常感谢作者闻蓓老师的指导和帮助。


# 代码说明

*按顺序执行代码*

* **00_install-pkgs.Rmd**: 安装需要的宏包
* **01_tidydata.R**: 整理数据（需要自己下载相应的数据集，并按年份周期存放）
* **02_tbl_summary.R**: 生成基线表(文中表1)
* **03_modeling.R**: 建模并输出结果(文中表2和表3）

# 帮助函数

* **helper/tidy_svyVGAM.R**: 用于模型结果的规整
