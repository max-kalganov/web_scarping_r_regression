library(stringr)
library(XML)
library(htmltab)
library(RCurl)
library(httr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)

clean_table <- function(table){
  table <- iphones
  names <- colnames(iphones)
  colnames(table) <- NULL
  table <- t(table)
  table <- table[c(2:nrow(table)), c(5, 10, 11, 12, 13:ncol(table))]
  names <- names[c(3:length(names))]
  table <- as.data.frame(table)
  table <- table %>% mutate(across(where(is.factor), as.character))
  colnames(table) <- table[1,]
  table <- table[-1,]
  table <- table[c(1:30,32,34:nrow(table)), c(1:3,5,6,8,9,21,38:41,45)]
  names <- names[c(1:30,32,34:length(names))]
  table <- as.data.frame(table %>% lapply(function(x) type.convert(str_extract(x, "(\\d)+(.\\d){0,}"), as.is = TRUE)))
  colnames(table) <- c("date", "version","screen_size","ram","flash_mem","num_of_cameras","num_mart_dots","tech_proc","len","width","depth","weight","resolution")
  prices <- str_extract(str_extract(names, " (([4-9][\\d]{2})|[\\d]{4,})"), "\\d+")
  names <- str_extract(names, "iPhone .{1,2}( Pro Max| Pro| Plus| mini| Max)?")
  correct_rows <- is.na(prices) != TRUE
  
  table <- table[correct_rows,]
  names <- names[correct_rows]
  prices <- prices[correct_rows]
  table$price <- as.numeric(prices)
  table$name <- names
  
  return(table)
}


table_url <- "https://catalog.onliner.by/compare/iphone1164b+iphone12pro128gr+iphonese64b+iphone12+iphonexr64b+iphone12mini+iphone11p128sg+iphone732+iphone11pm256mg+iphonex+iphonexs64b+iphone8+iphone12prom128g+iphonexsmax64b+iphone7p128g+iphone8plus+iphonexcpo64s+iphone6scpo32gr+iphone6s16cpos+iphonexrd64b+iphone6s32gbsc+iphonese32sg+iphone7cpo32s+iphone6sp32gbsg+iphonese128cposg+iphone8p256g+iphone6s64cpog+iphonese32s+iphone7pr128+iphone632gbsg+iphone4_8gb+apple_iphone5_16+iphone4_16gb+apple_iphone5c16+iphone6s128s+iphone6s64gbs+iphone6s64gbrg+iphone6sp64gbs+iphone5s16gbg+iphone7p128s+iphone7p128rg+iphone7p128b+iphone7ppr128+iphone1128prpl+iphone11128b+iphone11128w+iphonese64r+iphonese128b+iphonese128w+iphone12pro128bl+iphone12pro245bl+iphone12pro256gr"
iphones <- htmltab(doc = table_url, which = "//th/ancestor::table")
iphones <- clean_table(iphones)
iphones <- unique(iphones)

set.seed(5)
training_samples <- iphones$price %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data <- iphones[training_samples,]
test.data <- iphones[-training_samples,]

model <- lm(price ~ date + version + screen_size + ram + flash_mem + num_of_cameras + num_mart_dots + tech_proc + len + width + depth + weight + resolution, data = train.data)
predictions <- model %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

model2 <- lm(price ~ log(date) + log(version) + screen_size + ram + flash_mem + num_of_cameras + I(num_of_cameras^2) + num_mart_dots + tech_proc + len +I(len^2) + width +I(width^2) + depth + I(depth^2) + weight + I(weight^2) + resolution, data = train.data)
predictions <- model2 %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

model3 <- lm(price ~ screen_size + I(screen_size^2) + num_of_cameras + I(num_of_cameras^2) + len +I(len^2) + width +I(width^2) + depth + I(depth^2) + weight + I(weight^2) + resolution, data = train.data)
predictions <- model3 %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

const <- rep(1, nrow(train.data))
test.data$const <- 1
model <- lm(price ~ const + version + ram + flash_mem + num_of_cameras
            + tech_proc + len + width + depth + weight + resolution + I(date^2)
            + I(version^2) + I(screen_size^2) + I(ram^2) + I(num_of_cameras^2)
            + I(tech_proc^2), data = train.data)
predictions <- model %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)


model <- lm(price ~ screen_size + flash_mem + num_of_cameras + tech_proc + len
            + width + depth + weight + resolution + I(date^2) + I(version^2)
            + I(screen_size^2), data = train.data)


model <- lm(price ~ const + version + screen_size + ram + flash_mem 
            + len + width + depth 
            + I(date^2) + I(version^2) + I(screen_size^2)
            + I(ram^2) + I(flash_mem^2) + I(num_of_cameras^2) 
            + I(num_mart_dots^2) + I(tech_proc^2) + I(len^2) + I(width^2)
            + I(depth^2) + I(weight^2) + I(resolution^2), data = train.data)
predictions <- model %>% predict(test.data)

data.frame(
  RMSE = RMSE(predictions, test.data$price),
  R2 = R2(predictions, test.data$price)
)

test.data$predictions <- predictions

ggplot(train.data, aes(x=name, y = price)) + 
  geom_point(color="black") +
  geom_jitter(data=test.data, aes(x=name, y = price), color="blue") +
  geom_point(data=test.data, aes(x=name, y = predictions), color="red", position="dodge") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
