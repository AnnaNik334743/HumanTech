install.packages(c("rvest", "dplyr", "stringr", "assertr", "openxlsx", "lsa", "pheatmap"))
library(pheatmap)
library(openxlsx)
library(rvest)
library(dplyr)
library(stringr)
library(assertr)
library(ggplot2)
library(lsa)

dir.create("data2020-2021", showWarnings=FALSE)
connection <- url("https://xn----7sbfbblhs1ckbe1bnb.xn--p1ai/olimp/results/mun/20202021/", "rb")
initial_web_page <- read_html(connection)

all_rows_from_table <- initial_web_page %>% html_element(".tbl") %>% html_nodes("tr") %>% html_nodes("a") 
some_rows_from_table <- all_rows_from_table[seq(1, length(all_rows_from_table), 3)]

filenames <- some_rows_from_table %>% html_attr("title")

download_links <- some_rows_from_table %>% html_attr("href")
download_links <- str_replace(download_links, "../../../../", "https://xn----7sbfbblhs1ckbe1bnb.xn--p1ai/")

for(i in c(1:length(download_links))){
  if (grepl("11", filenames[i], fixed=TRUE)){
    download.file(download_links[i], destfile=paste("./data2020-2021", paste(filenames[i], ".xlsx", sep=""), sep="/"), mode="wb")
  }
}

###

all_subjects <- c("русский язык", "литература", "английский язык", "искусство",
                  "обществознание", "история", "право", "экономика",
                  "математика", "информатика", "физика", "астрономия",
                  "химия", "биология", "экология", "география")

###

df <- data.frame()
for (file in list.files(path = "./data2020-2021/")){
  
  subject_name <- names(which(sapply(all_subjects, 
                                     function(x){grepl(x, tolower(file), fixed=TRUE)})))
  if (length(subject_name) == 0){
    next
  }
  
  curr_df <- data.frame()
  
  for (sheet in getSheetNames(paste("./data2020-2021/", file, sep=""))){
    
    curr_df_inner <- read.xlsx(paste("./data2020-2021/", file, sep=""), sheet, 9)
    curr_df_inner <- apply(curr_df_inner, 1, function(row){
      fio <- gsub("\\s+", " ", 
                  str_trim(paste(row["Фамилия"], row["Имя"], row["Отчество"], sep=" ")))
      status <- ifelse(((tolower(row["Тип.диплома"]) == "победитель") || 
                          (tolower(row["Тип.диплома"]) == "призер") ||
                          (tolower(row["Тип.диплома"]) == "призёр")), 1, 0)
      fio <- gsub("ё", "е", fio)
      return(c(fio, row["Номер.школы"], row["Номер.класса"], status))
    })
    
    curr_df_inner <- data.frame(t(curr_df_inner))
    colnames(curr_df_inner) <- c("ФИО", "Школа", "Класс", subject_name)
    
    if (length(curr_df) == 0){
      curr_df <- curr_df_inner
    }else{
      curr_df <- rbind(curr_df, curr_df_inner)
    }
  }
  
  if (length(df) == 0){
    df <- curr_df
  }else{
    df <- full_join(df, curr_df, by=c("ФИО", "Школа", "Класс"))
  }
}
df[is.na(df)] <- 0
df <- df[df$ФИО != "X X X",]

write.csv(df, "./csv2020-2021.csv", row.names=FALSE)

###

df <- read.csv("./csv2020-2021.csv")
df["Всего_призовых_мест"] <- rowSums(select(df, -c(ФИО, Школа, Класс)))
df_trunc <- df[(df["Всего_призовых_мест"] > 0) & (df["Класс"] > 6),]

ggplot(df_trunc, aes(x=Всего_призовых_мест))+
  geom_bar(fill="cornflowerblue")+
  geom_text(aes(label = ..count..), stat="count", vjust=-0.2, 
            size=3.5, colour="blue")+
  facet_grid(~Класс)+
  labs(x = "Количество призовых мест (без учета тех, кто ничего не занял)",
  y = "Количество",
  title = "Количество людей, занявших призовые места на олимпиадах, 
по классу и количеству призовых мест")

vectors <- df_trunc[,c("русский.язык", "литература", "английский.язык", "искусство",
                       "обществознание", "история", "право", "экономика",
                       "математика", "информатика", "физика", "астрономия",
                       "химия", "биология", "экология", "география")]
chart_df <- data.frame(value=colSums(vectors))
ggplot(chart_df, aes(x=colnames(vectors), y=value))+
  geom_bar(stat="identity", fill="cornflowerblue")+
  labs(title = "Количество призовых мест по различным предметам", 
       x = "Предметы", y = "Количество") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.3, hjust=1, size=10),
        plot.title = element_text(hjust = 0.5))

###

cos_dists <- data.frame(cosine(data.matrix(vectors)))

heatmap_full <- pheatmap(cos_dists, display_numbers=T, main="Близость предметов")

heatmap_repr <- pheatmap(cos_dists, display_numbers=T, main="Близость предметов", 
         cluster_cols=F, show_colnames=F, legend=F)

