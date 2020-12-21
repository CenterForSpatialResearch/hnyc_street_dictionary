##### Prepare Streets Database for Dict
 library(tidyverse)
 
 streets_all_edit_bk <- read_csv("StreetsALL/Data/StreetsALL_edit_bkn.csv")
 streets_all_edit_mn <- read_csv("StreetsALL/Data/StreetsALL_edit_mtn.csv")
 
###glimpse(streets_all_edit_mn)

 # 1910
 ## MN
 streets_all_1910_mn <-  streets_all_edit_mn %>% 
     filter(`Valid 1910` == 1)
 
 mn_seg <- streets_all_1910_mn
    
 ###glimpse(streets_all_edit_bk)
 
 ## BK
 streets_all_1910_bk <-  streets_all_edit_bk %>% 
     filter(`Valid in 1910` == 1)
     
 bk_seg <- streets_all_1910_bk
 
 # Select Columns
 
 mn_seg <- mn_seg %>% 
     select( street_modern = StreetModern,
            hnyc_street_name =  `HNYC Street Name 1910`,
            street_alt =  `ST Name Alt 1910`, 
            Left_Low =  `Left Low 1910`, 
            Left_High = `Left High 1910`, 
            Right_Low = `Right Low 1910`, 
            Right_High = `Right High 1910`, 
            ED_Left = `ED Left 1910`, 
            ED_Right = `ED Right 1910`,
            OBJECTID, 
            JoinID)
 
 bk_seg <- bk_seg %>% 
     select( street_modern = FULL_STREE, 
            hnyc_street_name =  `HNYC Street Name 1910`,
            street_alt =  `ST Name Alternate 1910`, 
            Left_Low =  `Left Low 1910`, 
            Left_High = `Left High 1910`, 
            Right_Low = `Right Low 1910`, 
            Right_High = `Right High 1910`, 
            ED_Left = `ED Left 1910`, 
            ED_Right = `ED Right 1910`,
            OBJECTID, 
            JoinID)
 
     
# Clean up Street Name Columns (ISSUES WITH COLUMN NAMES)
 mn_seg$street_modern = clean(mn_seg$street_modern)
 mn_seg$street_alt = clean(mn_seg$street_alt)
 mn_seg$hnyc_street_name = clean(mn_seg$hnyc_street_name)
 
 bk_seg$street_modern = clean(bk_seg$street_modern)
 bk_seg$street_alt = clean(bk_seg$street_alt)
 bk_seg$hnyc_street_name = clean(bk_seg$hnyc_street_name)
 
 
 mn_seg <- as_tibble(sapply(mn_seg, toupper))
 bk_seg <- as_tibble(sapply(bk_seg, toupper))
 

 # COl number formatting
 
 mn_seg$Left_Low <- as.numeric(mn_seg$Left_Low)
 mn_seg$Left_High <- as.numeric(mn_seg$Left_High)
 mn_seg$Right_Low <- as.numeric(mn_seg$Right_Low)
 mn_seg$Right_High <- as.numeric(mn_seg$Right_High)
 mn_seg$ED_Left <- as.numeric(mn_seg$ED_Left)
 mn_seg$ED_Right <- as.numeric(mn_seg$ED_Right)
 
 bk_seg$Left_Low <- as.numeric(bk_seg$Left_Low)
 bk_seg$Left_High <- as.numeric(bk_seg$Left_High)
 bk_seg$Right_Low <- as.numeric(bk_seg$Right_Low)
 bk_seg$Right_High <- as.numeric(bk_seg$Right_High)
 bk_seg$ED_Left <- as.numeric(bk_seg$ED_Left)
 bk_seg$ED_Right <- as.numeric(bk_seg$ED_Right)
 
 # Create Priority Name Column
 mn_seg <- mn_seg %>% rowwise() %>% 
     mutate(Name = ifelse(hnyc_street_name != "NULL", hnyc_street_name, 
                          ifelse(street_modern != "NULL", street_modern, street_alt))) %>%
     ungroup()
 
 bk_seg <- bk_seg %>% rowwise() %>% 
     mutate(Name = ifelse(hnyc_street_name != "NULL", hnyc_street_name, 
                          ifelse(street_modern != "NULL", street_modern, street_alt))) %>%
     ungroup()
 
 write_csv(mn_seg, "StreetsALL/Data/segment_1910_mn.csv")
 write_csv(bk_seg, "StreetsALL/Data/segment_1910_bk.csv")
 
 #Create Geo Dict MN
 
 mn_seg_list1 <- mn_seg %>% filter(ED_Left == ED_Right) %>%
     mutate(ED = ED_Right) %>% select(ED, Name, OBJECTID, JoinID)
 

 mn_seg_list2 <- mn_seg %>% filter(ED_Left != ED_Right) %>% 
     select(Name, ED_Left, OBJECTID, JoinID) %>% rename(ED = ED_Left)
 mn_seg_list2 <- mn_seg_list2[c(2, 1, 3, 4)]

 mn_seg_list3 <- mn_seg %>% filter(ED_Left != ED_Right) %>% 
     select(Name, ED_Right, OBJECTID, JoinID) %>% rename(ED = ED_Right)
 mn_seg_list3 <- mn_seg_list3[c(2, 1, 3, 4)]

 mn_seg_list4 <- rbind(mn_seg_list1, mn_seg_list2, mn_seg_list3)

 mn_seg_list5 <- mn_seg_list4 %>% 
     spread(key = ED, value = Name, fill = NA) %>% select(-OBJECTID, -JoinID)
 colnames(mn_seg_list5) <- sub("", "ED", colnames(mn_seg_list5))
 mn_seg_list5 <- as.data.frame(mn_seg_list5)
 
 mn_seg_list6 <- apply(mn_seg_list5, 2, sort)

 mn_seg_list7 <- lapply(mn_seg_list6, unique)
 
 maxlen <- max(lengths(mn_seg_list7))
 B <- lapply(mn_seg_list7, function(lst) c(lst, rep(NA, maxlen - length(lst))))
 names(B) <- paste(colnames(mn_seg_list5))
 C <- lapply(B, unlist)
 my <- as.data.frame(C)

 geo_dict<- t(my)
 rownames(geo_dict) <- c(1:nrow(geo_dict))
 colnames(geo_dict) <- paste0("St", 1:ncol(geo_dict))

 ED <- colnames(my) 
 ED <- sub("ED", "", ED)
 ED <- as.numeric(ED)

 geo_dict_mn <- as_tibble(cbind(ED, geo_dict))
 geo_dict_mn$ED <- as.numeric(geo_dict_mn$ED)

 write_csv(geo_dict_mn, "StreetsALL/Data/geo_dict_1910_mn.csv")
 
 
 #Create Geo Dict BK
 
 bk_seg_list1 <- bk_seg %>% filter(ED_Left == ED_Right) %>%
     mutate(ED = ED_Right) %>% select(ED, Name, OBJECTID, JoinID)
 
 
 bk_seg_list2 <- bk_seg %>% filter(ED_Left != ED_Right) %>% 
     select(Name, ED_Left, OBJECTID, JoinID) %>% rename(ED = ED_Left)
 bk_seg_list2 <- bk_seg_list2[c(2, 1, 3, 4)]
 
 bk_seg_list3 <- bk_seg %>% filter(ED_Left != ED_Right) %>% 
     select(Name, ED_Right, OBJECTID, JoinID) %>% rename(ED = ED_Right)
 bk_seg_list3 <- bk_seg_list3[c(2, 1, 3, 4)]
 
 bk_seg_list4 <- rbind(bk_seg_list1, bk_seg_list2, bk_seg_list3)
 
 bk_seg_list5 <- bk_seg_list4 %>% 
     spread(key = ED, value = Name, fill = NA) %>% select(-OBJECTID, -JoinID)
 colnames(bk_seg_list5) <- sub("", "ED", colnames(bk_seg_list5))
 bk_seg_list5 <- as.data.frame(bk_seg_list5)
 
 bk_seg_list6 <- apply(bk_seg_list5, 2, sort)
 
 bk_seg_list7 <- lapply(bk_seg_list6, unique)
 
 maxlen <- max(lengths(bk_seg_list7))
 B <- lapply(bk_seg_list7, function(lst) c(lst, rep(NA, maxlen - length(lst))))
 names(B) <- paste(colnames(bk_seg_list5))
 C <- lapply(B, unlist)
 my <- as.data.frame(C)
 
 geo_dict<- t(my)
 rownames(geo_dict) <- c(1:nrow(geo_dict))
 colnames(geo_dict) <- paste0("St", 1:ncol(geo_dict))
 
 ED <- colnames(my) 
 ED <- sub("ED", "", ED)
 ED <- as.numeric(ED)
 
 geo_dict_bk <- as_tibble(cbind(ED, geo_dict))
 geo_dict_bk$ED <- as.numeric(geo_dict_bk$ED)
 
 write_csv(geo_dict_bk, "StreetsALL/Data/geo_dict_1910_bk.csv")
 
 # OTHER YEARS
 
 #streets_all_1880_mn <-  streets_all_edit_mn %>% 
     
 #streets_all_1880_bk <-  streets_all_edit_bk %>% 
     
 
 #streets_all_1850_mn <-  streets_all_edit_mn %>% 
     
 #streets_all_1850_bk <-  streets_all_edit_bk %>% 
     