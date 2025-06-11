# 屏蔽所有包加载的消息和警告
suppressWarnings(
  suppressMessages({
    library(quantstrat)
    library(quantmod)
    library(PerformanceAnalytics)
    library(tidyverse)
    library(data.table)
    library(xts)
    library(TTR)
    library(ggthemes)
    library(purrr)
    library(ggplot2)
    library(lubridate)
    library(tidyr)
    library(DescTools)
    library(Tushare)
  # 处理showtext相关输出
  suppressMessages({
    library(showtext)
    font_add("SimHei", regular = "SimHei.ttf")
  })
    showtext_auto()  # 自动启用showtext
    options(stringsAsFactors = FALSE)  # 设置全局选项，避免字符串转换为因子
    options(digits = 4)  # 设置数字显示精度
    options(scipen = 999)  # 禁用科学计数法
    options(max.print = 1000)  # 设置最大打印行数
    options(tibble.print_max = 10, tibble.print_min = 10)  # 设置tibble打印行数
    options(dplyr.summarise.inform = FALSE)  # 禁用dplyr的summarise信息输出
    options(ggplot2.continuous.colour = "viridis")  # 设置ggplot2连续颜色主题为viridis
    options(ggplot2.continuous.fill = "viridis")  # 设置ggplot2连续填充颜色主题为viridis
    options(ggplot2.discrete.colour = "Set1")  # 设置ggplot2离散颜色主题为Set1
    options(ggplot2.discrete.fill = "Set1")  # 设置ggplot2离散填充颜色主题为Set1

    # 屏蔽showtext_auto()的输出
  invisible(showtext_auto())  # 屏蔽showtext_auto()的输出
  })
)

# 获取API Token（确保环境变量已设置）
my_token <- Sys.getenv("MY_API_TOKEN")
