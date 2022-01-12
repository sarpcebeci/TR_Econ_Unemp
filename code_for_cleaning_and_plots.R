library(tidyverse)
library(here)
library(readxl)
library(zoo)


# Data Cleaning -----------------------------------------------------------

before_2014 <- 
  here("data", "before_2014") %>% 
  list.files() %>% 
  tibble(
    file_name = .,
    paths = str_c(here("data", "before_2014", file_name))) %>% 
  mutate(lword = str_sub(paths, start = -1)) %>% 
  filter(lword == "x") %>% 
  separate(file_name, into = c("file_name", "trash"), sep = "\\.") %>% 
  mutate(datas = map(paths, read_xlsx)) %>% 
  select(file_name, datas)

after_2014 <- 
  here("data", "after_2014") %>% 
  list.files() %>% 
  tibble(
    file_name = .,
    paths = str_c(here("data", "after_2014", file_name))) %>% 
  separate(file_name, into = c("file_name", "trash"), sep = "\\.") %>% 
  mutate(datas = map(paths, read_delim, 
                     delim = "|", 
                     col_types = cols(.default = "c"))) %>% 
  select(file_name, datas)

requires_detailed_treatment <- 
  before_2014 %>% 
  slice(4, 9:11, 13, 14)

requires_basic_treatment <- 
  before_2014 %>% 
  slice(c(-4, -9, -10, -11, -13, -14))

basic_treated <- 
  requires_basic_treatment %>% 
  mutate(
    datas = 
      map(
        datas, 
        function(tbl){
          filter(tbl, !is.na(...2))
        }
      ),
    datas = 
      map2(
        datas,
        file_name,
        function(tbl, chr){
          if (chr == "tbl7") {
            tbl <- 
              mutate(tbl,
                     Cinsiyet = c("Cinsiyet",
                                  rep("Erkek", 109), 
                                  rep("Kadın", 110)))
          }
          return(tbl)
        }
      ),
    datas = 
      map(datas,
          function(tbl){
            colnames(tbl) <- 
              tbl %>% 
              slice(1) %>% 
              pivot_longer(cols = everything()) %>% 
              mutate(value = if_else(is.na(value), 
                                     "Date", value)) %>% 
              pull(value)
            
            return(slice(tbl, -1))
          }
      ),
    datas = 
      map(
        datas,
        function(tbl){
          filter(tbl, !is.na(Date))
        }
      )
  )

tmp <- requires_detailed_treatment$datas[[1]] 

detailed_treatment <- function(tmp){ 
  
  dates <- 
    tmp %>% 
    mutate(prs_num = parse_number(`TÜRKİYE İSTATİSTİK KURUMU`)) %>% 
    filter(prs_num > 2000, !is.na(prs_num)) %>% 
    pull(`TÜRKİYE İSTATİSTİK KURUMU`)
  
  ind_vec_start <- tmp$`TÜRKİYE İSTATİSTİK KURUMU` %in% dates %>% which()
  
  ind_vec_end <- c(ind_vec_start[2:length(ind_vec_start)]-1, nrow(tmp)-1)
  
  tmp1 <- 
    tibble(
      ind_vec_start,
      ind_vec_end,
      slice_vec = map2(
        ind_vec_start,
        ind_vec_end,
        function(s, e){
          c(s:e)
        }
      )
    )
  
  tmp2 <- 
    tmp %>% 
    mutate(prs_num = parse_number(`TÜRKİYE İSTATİSTİK KURUMU`)) %>% 
    filter(prs_num > 2000, !is.na(prs_num)) %>% 
    select(Dates = `TÜRKİYE İSTATİSTİK KURUMU`) %>% 
    bind_cols(
      select(tmp1, slice_vec)
    ) %>% 
    mutate(
      datas = 
        map(
          slice_vec,
          function(ind){
            tmp %>% 
              slice(ind)
          }
        )
    )
  
  tmp3 <- tmp2$datas[[1]] 
  
  tmp4 <- 
    tmp2 %>% 
    filter(!str_detect(Dates, "2009 yılından itibaren")) %>% 
    mutate(
      datas = 
        map(datas,
            function(tbl){
              colnames(tbl) <- 
                c("Var",
                  tbl %>% 
                    select(-1) %>% 
                    na.omit() %>% 
                    slice(1) %>% 
                    pivot_longer(cols = everything()) %>% 
                    mutate(value = if_else(is.na(value), 
                                           "Date", value)) %>% 
                    pull(value)
                )
              tbl %>% 
                filter(!is.na(Var)) %>% 
                slice(-1) %>% 
                mutate(
                  chk = 
                    case_when(
                      Var == "Erkek" ~ Var,
                      Var == "Kadın" ~ Var,
                      T ~ NA_character_
                    ),
                  chk = na.locf(chk, na.rm = F)
                ) %>% 
                filter(!Var %in% c("Erkek", "Kadın"))
            }
        )
    )
  
  tmp5 <- 
    tmp4 %>% 
    select(Dates, datas) %>% 
    unnest(datas)
  
  return(tmp5)
}

detailed_treated <- 
  requires_detailed_treatment %>% 
  mutate(
    datas = 
      map(
        datas, detailed_treatment
      )
  )

before_final_tidy <- 
  after_2014 %>% 
  mutate(
    datas = 
      map(
        datas,
        function(tmp){
          tmp1 <- 
            tmp %>% 
            select(-c(X1, X16)) %>% 
            slice(-1, -3) 
          
          tmp2 <- 
            tmp1 %>% 
            slice(1) %>% 
            pivot_longer(cols = everything()) %>% 
            pull(value)
          
          tmp2[1] <- "Var"
          tmp2[2] <- "Year"
          
          
          colnames(tmp1) <- tmp2
          
          tmp1 %>% 
            slice(-1) %>% 
            mutate(Var = na.locf(Var))
        }
      )
  )

before_final_tidy$datas[[1]] <- 
  before_final_tidy$datas[[1]] %>% 
  separate(Var, into = c("c1", "c2"), sep = "\\(") %>% 
  select(-c1) %>% 
  mutate(c2 = str_sub(c2, end = -2)) %>% 
  filter(!is.na(Year)) %>% 
  pivot_longer(cols = !c(Year, c2)) %>% 
  pivot_wider(names_from = c2,
              values_from = value)

before_final_tidy$datas[[2]] <- 
  before_final_tidy$datas[[2]] %>% 
  separate(Var, into = c("c1", "c2", "c3"), sep = "\\(") %>% 
  select(-c1) %>% 
  mutate(c3 = str_sub(c3, end = -2)) %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  pivot_longer(cols = !c(Year, d1, c3, d2)) 

before_final_tidy$datas[[3]] <- 
  before_final_tidy$datas[[3]] %>% 
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  mutate(d2 = str_sub(d2, start = 5, end = 9)) %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2)) 

before_final_tidy$datas[[4]] <- 
  before_final_tidy$datas[[4]] %>% 
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2))

before_final_tidy$datas[[5]] <- 
  before_final_tidy$datas[[5]] %>% 
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2))

before_final_tidy$datas[[6]] <- 
  before_final_tidy$datas[[6]] %>%
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2))

before_final_tidy$datas[[7]] <- 
  before_final_tidy$datas[[7]] %>%
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  mutate(d2 = str_sub(d2, start = 5, end = 9)) %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2))

before_final_tidy$datas[[8]] <- 
  before_final_tidy$datas[[8]] %>%
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2)) 

before_final_tidy$datas[[9]] <- 
  before_final_tidy$datas[[9]] %>%
  separate(Var, 
           into = c("c1", "c2", "c3", "c4"), 
           sep = "\\(") %>% 
  separate(c2, into = c("d1", "d2"), sep = "\\)") %>% 
  select(-c1) %>% 
  separate(c3, into = c("e1", "e2"), sep = "\\)") %>%
  select(-e2) %>% 
  mutate(c4 = str_sub(c4, end = -2)) %>% 
  pivot_longer(cols = !c(Year, d1, e1, c4, d2)) 

after_tidy <- 
  before_final_tidy %>% 
  mutate(
    datas =
      map(
        datas,
        function(tbl){
          tbl %>% 
            left_join(before_final_tidy$datas[[1]],
                      by = c("Year", "name"))
        }
      )) %>% 
  filter(file_name != "agg") %>% 
  mutate(
    datas = map(
      datas,
      function(tbl){
        tbl %>% 
          mutate(
            value = as.numeric(value),
            `15+` = as.numeric(`15+`),
            value = value / `15+`)
      }
    )
  )

before_treated <- 
  detailed_treated %>% 
  mutate(
    datas = map(datas, pivot_longer, cols = !c(Dates, Var, chk)),
    datas = map(datas, rename, Date = Dates)
  ) %>% 
  bind_rows(
    basic_treated %>% 
      mutate(
        datas = map(datas, pivot_longer, 
                    cols = !c(Date))
      ))

before_treated_final <- 
  before_treated %>% 
  filter(file_name != "agg") %>% 
  mutate(
    datas = map(
      datas, left_join, 
      before_treated$datas[[7]] %>% 
        filter(name == "Toplam") %>% 
        select(Date, value),
      by = "Date"
    ),
    datas = map(
      datas, mutate, 
      value.x = as.numeric(value.x),
      value.y = as.numeric(value.y),
      value = value.x / value.y
    )
  )

tr_months <- 
  after_tidy$datas[[3]] %>% 
  distinct(name) %>% 
  separate(name, into = c("mm", "mn"), sep = "-")

after_tidy_final <- 
  after_tidy %>% 
  mutate(
    datas = 
      map(datas, 
          function(tbl){
            tbl %>% 
              separate(name, into = c("mn", "mm"), sep = "-") %>% 
              mutate(Date = str_c(Year, "-", mn, "-01") %>% 
                       as.Date.character())
          }),
    datas = map(
      datas, 
      function(tbl){
        if (ncol(tbl) == 10) {
          tbl <- rename(tbl, e1 = c3) 
        }
        return(tbl)
      }
    )
  ) %>% 
  unnest(datas)

before_tidy_final <- 
  before_treated_final %>% 
  mutate(
    datas = 
      map(datas, 
          function(tbl){
            tbl %>% 
              separate(Date, into = c("Year", "Month"), sep = " ") %>% 
              left_join(tr_months, c("Month" = "mn")) %>% 
              mutate(
                Date = str_c(Year, "-", mm, "-01") %>% 
                  as.Date.character()
              )
          }),
    datas = 
      map(
        datas, 
        function(tbl){
          if (ncol(tbl) == 8) {
            tbl <- mutate(tbl, chk = NA, Var = NA)
          }
          return(tbl)
        }
      )
  ) %>% 
  unnest(datas)

final <- 
  after_tidy_final %>% 
  mutate(d2 = if_else(d2 %in% c("Erkek", "Kadın"),
                      d2, "Total")) %>% 
  select(Date, 
         Var1 = d1,
         Var2 = c3,
         Gender = d2,
         value, file_name) %>% 
  mutate(From = "After") %>% 
  bind_rows( 
    before_tidy_final %>% 
      select(Date, 
             Var1 = Var,
             Var2 = name,
             Gender = chk, 
             value, file_name) %>% 
      mutate(Gender = if_else(is.na(Gender), "Total", Gender),
             From = "Before")) %>% 
  mutate(
    Var1 = 
      if_else(Var1 == "İş bulmuş başlamak için bekleyen",
              "İş bulmuş başlamak için bekliyor", Var1),
    Var2 = tolower(Var2),
    Var2 = 
      if_else(Var2 == "iş bulmuş başlamak için bekleyen",
              "iş bulmuş başlamak için bekliyor", Var2),
    Var2 = 
      if_else(Var2 == "iş bulmuş, başlamak için bekleyen",
              "iş bulmuş başlamak için bekliyor", Var2),
    Var2 = 
      if_else(Var2 == "iş bulmuş, başlamak için bekliyor",
              "iş bulmuş başlamak için bekliyor", Var2),
    Var2 = 
      if_else(Var2 == "işinden ayrıldı,emekli oldu",
              "işinden ayrıldı-emekli oldu", Var2),
    Var2 = 
      if_else(Var2 == "işinden ayrıldı,kendi isteğiyle",
              "işinden ayrıldı-kendi isteğiyle", Var2),
    Var2 = 
      if_else(Var2 == "kendi işini kurmak istiyor",
              "kendi işini kurmak istiyor", Var2),
    Var2 = 
      if_else(Var2 == "lise dengi meslek okul",
              "lise dengi mesleki okul", Var2),
    Var2 = 
      if_else(Var2 == "okuma yazma bilmeyen",
              "okuma-yazma bilmeyen", Var2),
    Var2 = 
      if_else(Var2 == "tam zamanlı veya yarı zamanlı (farketmez)",
              "tam zamanlı veya yarı zamanlı farketmez", Var2))


# Plots -------------------------------------------------------------------



data <- read_csv("final12_18.csv",
                 col_types = cols(Var1 = col_character(),
                                  Var2 = col_character())) %>% 
  mutate(
    Gender = case_when(
      Gender == "Erkek" ~ "Male", 
      Gender == "Kadın" ~ "Female",
      T ~ Gender
    )
  )

tbl1 <- 
  data %>% 
  filter(
    file_name %in% c("tbl1_kent", "tbl1_kır")
  ) %>% 
  pivot_wider(names_from = file_name, 
              values_from = value) %>% 
  mutate(value = tbl1_kent + tbl1_kır,
         file_name = "tbl1") %>% 
  select(-c(tbl1_kent, tbl1_kır)) %>% 
  bind_rows(
    filter(data, file_name == "tbl1_3",
           Var1 == "+15"))

tbl1_vars <- 
  tbl1 %>% 
  distinct(Var2) %>% 
  pull()

tbl1 <- 
  tbl1 %>% 
  filter(
    Var2 %in% tbl1_vars,
    Var2 != "toplam"
  ) %>% 
  mutate(
    Order = 
      case_when(
        Var2 == "1-2 ay" ~ 1,
        Var2 == "3-5 ay" ~ 2,
        Var2 == "6-8 ay" ~ 3,
        Var2 == "9-11 ay" ~ 4,
        T ~ 5
      ),
    Var2 = 
      case_when(
        Var2 == "1-2 ay" ~ "1-2 months",
        Var2 == "3-5 ay" ~ "3-5 months",
        Var2 == "6-8 ay" ~ "6-8 months",
        Var2 == "9-11 ay" ~ "9-11 months",
        T ~ ">1 Year"
      ),
    Var2 = fct_reorder(Var2, Order)
  ) %>% 
  filter(Var2 != "Other") %>% 
  group_by(Var2, Date) %>% 
  summarise(value = sum(value)) 


plot1 <- 
  tbl1 %>% 
  ggplot(aes(Date, value), color = "Orange") +
  geom_smooth(linetype = 1) +
  geom_line(alpha = .7) +
  facet_wrap(~Var2, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "", 
       title = "Duration of Job Search")

tbl2 <- 
  data %>% 
  filter(
    is.na(Var1) | Var1 == "15+",
    file_name %in% c("tbl2_kent", "tbl2_kır", "tbl1_3")
  ) 

tbl2_vars <- 
  tbl2 %>% 
  filter(is.na(Var1)) %>% 
  distinct(Var2) %>% 
  pull()

tbl2_subsetted <- 
  tbl2 %>% 
  filter(
    Var2 %in% tbl2_vars,
    Var2 != "toplam"
  ) %>%
  mutate(
    Rural_Urban = 
      case_when(
        file_name == "tbl1_3" ~ "Urban", 
        file_name == "tbl2_kent" ~ "Urban",
        file_name == "tbl2_kır" ~ "Rural",
      ),
    Var1 = 
      case_when(
        str_detect(Var2, "kaybetti") ~ "Lost_the_Job",
        str_detect(Var2, "arayan") ~ "First_time_searchers",
        str_detect(Var2, "ayrıldı") ~ "Left_the_Job"
      ),
    Var2 =
      case_when(
        Var2 == "işini kaybetti-geçici bir işte çalışıyordu,iş bitti" ~ 
          "Finished_Temporary_Job",
        Var2 == "ilk kez iş arayan-ev işleri ile meşgul" ~ 
          "Housework",
        Var2 == "ilk kez iş arayan-diğer" ~ "Other",
        Var2 == "işini kaybetti-işten çıkartıldı" ~ "Fired",
        Var2 == "işini kaybetti-işyerini kapattı/iflas etti" ~ 
          "Bankrupted",
        Var2 == "işinden ayrıldı-kendi isteğiyle" ~ "With_Her_Want",
        Var2 == "işinden ayrıldı-ücretsiz aile işçisi olarak çalışıyordu" ~ 
          "Was_Family_Worker",
        Var2 == "işinden ayrıldı-emekli oldu" ~ "Retired",
        Var2 == "işinden ayrıldı-diğer" ~ "Other",
        Var2 == "ilk kez iş arayan-öğrenime devam ediyordu" ~ 
          "Was_in_education",
        Var2 == "ilk kez iş arayan-askerden yeni geldi" ~ 
          "Newcomer_from_military",
        Var2 == "ilk kez iş arayan-okuldan yeni mezun oldu" ~ 
          "New_grad"
      ))

tbl2_plot_fun <- 
  function(name_subset){ 
    tbl2_subsetted %>% 
      filter(Var1  == name_subset) %>% 
      ggplot(aes(Date, value, color = Rural_Urban)) +
      geom_smooth(color = "black") +
      geom_line(alpha = .7) +
      facet_wrap(~Var2, scales = "free_y") + 
      scale_y_continuous(labels = scales::label_percent()) +
      labs(x = "", y = "", title = str_c("Reasons of: ", name_subset))
  }

plot2 <- tbl2_plot_fun("Lost_the_Job")
plot3 <- tbl2_plot_fun("First_time_searchers")
plot4 <- tbl2_plot_fun("Left_the_Job")

tbl3 <- 
  data %>% 
  filter(
    is.na(Var1) | Var1 == "15+",
    file_name %in% c("tbl3_kent", "tbl3_kır", "tbl1_3")
  ) 

tbl3_vars <- 
  tbl3 %>% 
  filter(is.na(Var1)) %>% 
  distinct(Var2) %>% 
  pull()

plot5 <- 
  tbl3 %>% 
  filter(
    Var2 %in% tbl3_vars,
    Var2 != "toplam"
  ) %>% 
  mutate(
    Rural_Urban = 
      case_when(
        file_name == "tbl1_3" ~ "Urban", 
        file_name == "tbl3_kent" ~ "Urban",
        file_name == "tbl3_kır" ~ "Rural",
      ),
    Var2 =
      case_when(
        Var2 == "tam zamanlı" ~ "Full_Time",
        Var2 == "yarı zamanlı" ~ "Part_Time",
        Var2 == "tam zamanlı veya yarı zamanlı farketmez" ~ 
          "Full_or_part_time",
        Var2 == "iş bulmuş başlamak için bekliyor" ~ 
          "Waits_to_start",
        Var2 == "kendi işini kurmak istiyor" ~ 
          "Start_her_business"
      )) %>% 
  ggplot(aes(Date, value, color = Rural_Urban)) +
  geom_smooth(color = "black") +
  geom_line(alpha = .7) +
  facet_wrap(~Var2, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "")

plot6 <- 
  data %>% 
  filter(file_name == "tbl1_3", 
         Var2 %in% tbl1_vars,
         Var1 == "15-24") %>%
  mutate(
    Order = 
      case_when(
        Var2 == "1-2 ay" ~ 1,
        Var2 == "3-5 ay" ~ 2,
        Var2 == "6-8 ay" ~ 3,
        Var2 == "9-11 ay" ~ 4,
        T ~ 5
      ),
    Var2 = 
      case_when(
        Var2 == "1-2 ay" ~ "1-2 months",
        Var2 == "3-5 ay" ~ "3-5 months",
        Var2 == "6-8 ay" ~ "6-8 months",
        Var2 == "9-11 ay" ~ "9-11 months",
        T ~ ">1 Year"
      ),
    Var2 = fct_reorder(Var2, Order)
  ) %>% 
  filter(!is.na(Var2)) %>% 
  group_by(Var2, Date) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(Date, value)) +
  geom_smooth() +
  geom_line(alpha = .7) +
  facet_wrap(~Var2, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "", subtitle = "People between 15-24:")

tbl4 <- 
  data %>% 
  filter(file_name == "tbl4",
         Var1 != "toplam",
         Var2 != "toplam") %>% 
  mutate(Var1 = tolower(Var1),
         Order = 
           case_when(
             Var1 == "1-2 ay" ~ 1,
             Var1 == "3-5 ay" ~ 1,
             Var1 == "6-8 ay" ~ 2,
             Var1 == "9-11 ay" ~ 2,
             Var1 == "1 yıl ve daha fazla 2 yıldan az" ~ 3,
             Var1 == "2 yıl ve daha fazla 3 yıldan az" ~ 3,
             Var1 == "3 yıl ve daha fazla" ~ 3,
             Var1 == "iş bulmuş başlamak için bekliyor" ~ 8
           ),
         Var1 = 
           case_when(
             Var1 == "1-2 ay" ~ "0 - 6 months",
             Var1 == "3-5 ay" ~ "0 - 6 months",
             Var1 == "6-8 ay" ~ "6 - 12 months",
             Var1 == "9-11 ay" ~ "6 - 12 months",
             Var1 == "1 yıl ve daha fazla 2 yıldan az" ~ "Chronic",
             Var1 == "2 yıl ve daha fazla 3 yıldan az" ~ "Chronic",
             Var1 == "3 yıl ve daha fazla" ~ "Chronic",
             Var1 == "iş bulmuş başlamak için bekliyor" ~ "found job"
           ),
         Var1 = fct_reorder(Var1, Order)) 

plot7 <- 
  tbl4 %>%
  filter(!is.na(Var1),
         Var1 != "found job") %>% 
  group_by(Var1, Var2, Date) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(value > 0) %>% 
  ggplot(aes(Date, value)) +
  geom_line(alpha = .4, size = 2) + 
  geom_smooth(color = "blue", linetype = 2) +
  facet_grid(Var1 ~ Var2, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "")

tbl5 <- data %>% 
  filter(file_name == "tbl5",
         !is.na(Var2)) %>% 
  mutate(
    Var2 = 
      case_when(
        Var2 %in% c(
          "okuma-yazma bilmeyen", 
          "okuma yazma bilen fakat bir okul bitirmeyen",
          "ilkokul", "ilköğretim", "ortaokul veya dengi meslek ortaokul"
        ) ~ "Below High School",
        Var2 %in% c(
          "genel lise", "lise dengi mesleki okul",
          "ortaokul veya dengi meslek okul"
        ) ~ "High School",
        Var2 %in% c(
          "yüksekokul veya fakülte", "yüksek öğretim"
        ) ~ "Above High School",
        T ~ "other"
      ),
    Order = 
      case_when(
        Var2 == "Below High School" ~ 1,
        Var2 == "High School" ~ 2,
        T ~ 3
      ),
    Var2 = fct_reorder(Var2, Order)
  ) %>% 
  filter(Var2 != "other",
         Var2 != "toplam",
         !is.na(Var2)) %>% 
  mutate(
    Var1 = tolower(Var1),
    Var1 =
      case_when(
        Var1 == "tam zamanlı" ~ "Full_Time",
        Var1 == "yarı zamanlı" ~ "Part_Time",
        Var1 == "tam zamanlı veya yarı zamanlı (farketmez)" ~ "Both",
        T ~ "Other"
      )
  ) %>% 
  filter(Var1 != "Other")

plot8 <- 
  tbl5 %>% 
  filter(value > 0) %>% 
  group_by(Var1, Var2, Date) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(Date, value)) +
  geom_smooth() +
  geom_line(alpha = .7) +
  facet_wrap(Var2~Var1, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "")

tbl6 <- 
  data %>% 
  filter(file_name == "tbl6",
         !is.na(Var2),
         Var1 != "Toplam") %>% 
  mutate(Var1 = tolower(Var1),
         Order = 
           case_when(
             Var1 == "1-2 ay" ~ 1,
             Var1 == "3-5 ay" ~ 1,
             Var1 == "6-8 ay" ~ 2,
             Var1 == "9-11 ay" ~ 2,
             Var1 == "1 yıl ve daha fazla 2 yıldan az" ~ 3,
             Var1 == "2 yıl ve daha fazla 3 yıldan az" ~ 3,
             Var1 == "3 yıl ve daha fazla" ~ 3,
             Var1 == "iş bulmuş başlamak için bekliyor" ~ 8
           ),
         Var1 = 
           case_when(
             Var1 == "1-2 ay" ~ "0 - 6 months",
             Var1 == "3-5 ay" ~ "0 - 6 months",
             Var1 == "6-8 ay" ~ "6 - 12 months",
             Var1 == "9-11 ay" ~ "6 - 12 months",
             Var1 == "1 yıl ve daha fazla 2 yıldan az" ~ "Chronic",
             Var1 == "2 yıl ve daha fazla 3 yıldan az" ~ "Chronic",
             Var1 == "3 yıl ve daha fazla" ~ "Chronic",
             Var1 == "iş bulmuş başlamak için bekliyor" ~ "found job"
           ),
         Var1 = fct_reorder(Var1, Order)) %>% 
  filter(Var1 != "found job",
         Var1 != "toplam") %>% 
  mutate(
    Var2 = 
      case_when(
        Var2 %in% c(
          "okuma-yazma bilmeyen", 
          "okuma yazma bilen fakat bir okul bitirmeyen",
          "ilkokul", "ilköğretim", "ortaokul veya dengi meslek ortaokul"
        ) ~ "Below High School",
        Var2 %in% c(
          "genel lise", "lise dengi mesleki okul",
          "ortaokul veya dengi meslek okul"
        ) ~ "High School",
        Var2 %in% c(
          "yüksekokul veya fakülte", "yüksek öğretim"
        ) ~ "Above High School",
        T ~ Var2
      ),
    Order = 
      case_when(
        Var2 == "Below High School" ~ 1,
        Var2 == "High School" ~ 2,
        T ~ 3
      ),
    Var2 = fct_reorder(Var2, Order)
  ) %>% 
  filter(Var2 != "toplam")

plot9 <- 
  tbl6 %>% 
  group_by(Var1, Var2, Date) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  filter(value > 0) %>% 
  ggplot(aes(Date, value)) +
  geom_smooth(color = "black") +
  geom_line(alpha = .7) +
  facet_grid(Var1~Var2, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "")

tbl8 <- data %>% 
  filter(file_name == "tbl8")

tbl8_var1_keys <- 
  tbl8 %>% 
  mutate(Var1 = tolower(Var1)) %>% 
  filter(!is.na(Var1)) %>% 
  distinct(Var1) %>% 
  pull()

tbl8 <- 
  tbl8 %>% 
  filter(Var2 %in% tbl8_var1_keys) %>% 
  mutate(Var1 = 
           if_else(is.na(Var1), Var2, tolower(Var1))) %>% 
  select(Date, Var1, Gender, value)

tbl8 <- 
  tbl8 %>% 
  mutate(
    Var1 =
      case_when(
        Var1 == "işini kaybetti-geçici bir işte çalışıyordu,iş bitti" ~ 
          "Finished_Temp_Job",
        Var1 == "ilk kez iş arayan-ev işleri ile meşgul" ~ 
          "Housework",
        Var1 == "ilk kez iş arayan-diğer" ~ "Other",
        Var1 == "işini kaybetti-işten çıkartıldı" ~ "Fired",
        Var1 == "işini kaybetti-işyerini kapattı/iflas etti" ~ 
          "Bankrupted",
        Var1 == "işinden ayrıldı,kendi isteğiyle" ~ "With_Her_Want",
        Var1 == "işinden ayrıldı-ücretsiz aile işçisi olarak çalışıyordu" ~ 
          "Was_Family_Worker",
        Var1 == "işinden ayrıldı-emekli oldu" ~ "Retired",
        Var1 == "işinden ayrıldı-diğer" ~ "Other",
        Var1 == "ilk kez iş arayan-öğrenime devam ediyordu" ~ 
          "Was_in_education",
        Var1 == "ilk kez iş arayan-askerden yeni geldi" ~ 
          "Newcomer_from_military",
        Var1 == "ilk kez iş arayan-okuldan yeni mezun oldu" ~ 
          "New_grad",
        T ~ "Other"
      ),
    Type = 
      case_when(
        Var1 %in% c("Bankrupted", "Finished_Temp_Job", "Fired") ~ 
          "Plot1",
        Var1 %in% c("Was_Family_Worker", "Was_in_education") ~ 
          "Plot2",
        T ~ "Other"
        
      )
  ) %>% 
  filter(value > 0,
         Var1 != "Other") 

tbl8_plot_fun <- 
  function(pl_char){ 
    tbl8 %>% 
      filter(Type == pl_char) %>% 
      ggplot(aes(Date, value)) + 
      geom_smooth(color = "black") +
      geom_line(alpha = .7, aes(color = Gender)) +
      facet_wrap(Gender~Var1, scales = "free_y") + 
      scale_y_continuous(labels = scales::label_percent()) +
      labs(x = "", y = "")
  }

plot9 <- tbl8_plot_fun("Plot1")
plot10 <- tbl8_plot_fun("Plot2")

tbl10 <- 
  data %>% 
  filter(
    file_name == "tbl10"
  ) %>% 
  mutate(Var1 = tolower(Var1)) %>% 
  mutate(
    Var1 =
      case_when(
        Var1 == "tam zamanlı" ~ "Full_Time",
        Var1 == "yarı zamanlı" ~ "Part_Time",
        Var1 == "tam zamanlı veya yarı zamanlı farketmez" ~ 
          "Full_or_part_time",
        Var1 == "iş bulmuş başlamak için bekliyor" ~ 
          "Waits_to_start",
        Var1 == "kendi işini kurmak istiyor" ~ 
          "Start_her_business"
      ),
    Var2 = 
      case_when(
        Var2 %in% c(
          "okuma-yazma bilmeyen", 
          "okuma yazma bilen fakat bir okul bitirmeyen",
          "ilkokul", "ilköğretim", "ortaokul veya dengi meslek ortaokul"
        ) ~ "Below High School",
        Var2 %in% c(
          "genel lise", "lise dengi mesleki okul",
          "ortaokul veya dengi meslek okul"
        ) ~ "High School",
        Var2 %in% c(
          "yüksekokul veya fakülte", "yüksek öğretim"
        ) ~ "Above High School",
        T ~ Var2
      ),
    Order = 
      case_when(
        Var2 == "Below High School" ~ 1,
        Var2 == "High School" ~ 2,
        T ~ 3
      ),
    Var2 = fct_reorder(Var2, Order)
  )

plot11 <- 
  tbl10 %>% 
  filter(Var1 %in% c("Full_Time", "Part_Time"),
         Var2 != "toplam",
         value > 0) %>% 
  group_by(Var1, Var2, Gender, Date) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(Date, value)) + 
  geom_smooth(aes(color = Gender), se = F) +
  geom_line(alpha = .7, aes(color = Gender)) +
  facet_grid(Var1~Var2, scales = "free_y") + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "", y = "")

library(ggseas)

plot13 <- 
  plot1 + 
  stat_seas(start = c(2005, 1), 
            frequency = 12, 
            color = "pink", 
            size = 1.2) +
  labs(subtitle = "Seasonal Adjustment with SEATS, to comparison.",
       caption = "Source: Turkstat")


plot_tbl <- 
  tibble(
  name_plot = str_c("plot", c(1:11)),
  plots = list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, 
               plot8, plot9, plot10, plot11)
)

map2(
  plot_tbl$name_plot,
  plot_tbl$plots,
  function(pth, plt){
    ggsave(filename = str_c(pth, ".png"), 
           plot = plt,
           path = here("plots"))
  }
)

ggsave(filename = str_c("plot13", ".png"), 
       plot = plot13,
       path = here("plots"))


tbl11 <- 
  before_final_tidy$datas[[9]] %>% 
  select(-d2) %>% 
  mutate(
    Date = str_c(
      "01-", str_sub(name, 1, end = 3), Year) %>% 
      lubridate::dmy(),
    Var2 = tolower(e1),
    Var2 = 
      case_when(
        Var2 %in% c(
          "okuma-yazma bilmeyen", "okuma yazma bilmeyen",
          "okuma yazma bilen fakat bir okul bitirmeyen",
          "ilkokul", "ilköğretim", "ortaokul veya dengi meslek ortaokul"
        ) ~ "Below High School",
        Var2 %in% c(
          "genel lise", "lise dengi mesleki okul",
          "ortaokul veya dengi meslek okul"
        ) ~ "High School",
        Var2 %in% c(
          "yüksekokul veya fakülte", "yüksek öğretim"
        ) ~ "Above High School",
        T ~ Var2
      ),
    Order = 
      case_when(
        Var2 == "Below High School" ~ 1,
        Var2 == "High School" ~ 2,
        T ~ 3
      ),
    Var2 = fct_reorder(Var2, Order),
    e1 = Var2,
    Var1 = tolower(c4),
    Order = 
      case_when(
        Var1 == "1-2 ay" ~ 1,
        Var1 == "3-5 ay" ~ 1,
        Var1 == "6-8 ay" ~ 2,
        Var1 == "9-11 ay" ~ 2,
        Var1 == "1 yıl ve daha fazla 2 yıldan az" ~ 3,
        Var1 == "2 yıl ve daha fazla 3 yıldan az" ~ 3,
        Var1 == "3 yıl ve daha fazla" ~ 3,
        Var1 == "iş bulmuş başlamak için bekliyor" ~ 8
      ),
    Var1 = 
      case_when(
        Var1 == "1-2 ay" ~ "0 - 6 months",
        Var1 == "3-5 ay" ~ "0 - 6 months",
        Var1 == "6-8 ay" ~ "6 - 12 months",
        Var1 == "9-11 ay" ~ "6 - 12 months",
        Var1 == "1 yıl ve daha fazla 2 yıldan az" ~ "Chronic",
        Var1 == "2 yıl ve daha fazla 3 yıldan az" ~ "Chronic",
        Var1 == "3 yıl ve daha fazla" ~ "Chronic",
        Var1 == "iş bulmuş başlamak için bekliyor" ~ "found job"
      ),
    Var1 = fct_reorder(Var1, Order),
    c4 = Var1,
    value = as.numeric(value)
    ) %>% 
  select(Date, Var1, Var2, value) %>% 
  left_join(
    y = before_final_tidy$datas[[1]] %>% 
          mutate(Date = str_c(
            "01-", str_sub(name, 1, end = 3), Year) %>% 
              lubridate::dmy()) %>% 
          janitor::clean_names() %>% 
          select(lf = x15, Date = date),
    by = "Date"
  ) %>% 
  mutate(value = value / as.numeric(lf)) %>% 
  group_by(Date, Var1, Var2) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  filter(value > 0) 


plot14 <- 
  tbl11 %>% 
  ggplot(aes(Date, value)) +
  geom_line() +
  geom_smooth(method = "loess", formula = "y~x") +
  facet_grid(Var1 ~ Var2, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "", x = "", 
       title = "Duration of Job Search Per Eduation Group")
  
ggsave(plot = plot14, filename = here("plots", "plot14.png"))











