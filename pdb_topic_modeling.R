
    # 1 - Scraping CIA Website for PDF Download Links and Downloading PDFs to Local Folder

        rm(list=ls())

        library(tidyverse)
        library(rvest)
        library(stringr)
        library(lubridate)
      
        setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/pdfs")) # Script R precisa estar dentro da pasta "PDBs" no Dropbox
        #load(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/backups/meta.df.61.77 data.rdata"))
        
        # 1.a - Downloading PDFS from [PDB 1961 - 1969] Collection
        url.61.69 <-"https://www.cia.gov/readingroom/collection/presidents-daily-brief-1961-1969?page="
        site.61.69 <- read_html(url.61.69) 
        links.61.69 <- html_attr(html_nodes(site.61.69,"a"),"href")
        totalpgs.61.69 <- gsub("page=","",str_extract(links.61.69[length(links.61.69)-2], "page=\\d*"))
        repeated <- "https://www.cia.gov/readingroom/docs/PDB%20CM%20Final%20Kennedy%20and%20Johnson_public%208%20Sep%202015.pdf"
      
            for (i in 0:totalpgs.61.69){
               cia.61.69 <- read_html(paste0(url.61.69, i))
               print(paste("Downloading links from page",i))
               raw_list <- cia.61.69 %>%
                  html_nodes("a") %>%  # find all links in the page
                  html_attr("href")%>% # get the url for these links
                  str_subset("\\.pdf") # find those that end in pdf only
               cleaned_list <- raw_list[!(raw_list %in% repeated)]%>% # remove repeated pdf
               walk2(., basename(.), download.file, mode = "wb") # use purrr to download the pdf associated with each url to the current working directory
            }
      
        # 1.b - Downloading PDFS from [PDB 1969 - 1977] Collection
        url.69.77 <- "https://www.cia.gov/readingroom/collection/presidents-daily-brief-1969-1977?page="
        site.69.77 <- read_html(url.69.77)
        links.69.77 <- html_attr(html_nodes(site.69.77,"a"),"href")
        totalpgs.69.77 <- gsub("page=","",str_extract(links.69.77[length(links.69.77)-2], "page=\\d*"))
        repeated <- "https://www.cia.gov/readingroom/docs/PDB%20Symposium%20Nixon%20and%20Ford%2024%20Aug%202016.pdf"
         
            for (i in 0:totalpgs.69.77){
                cia.69.77 <- read_html(paste0(url.69.77, i))
                print(paste("Downloading links from page",i))
                raw_list <- cia.69.77 %>%
                   html_nodes("a") %>%  # find all links in the page
                   html_attr("href")%>% # get the url for these links
                   str_subset("\\.pdf") # find those that end in pdf only
                cleaned_list <- raw_list[!(raw_list %in% repeated)]%>% # remove repeated pdf
                walk2(., basename(.), download.file, mode = "wb") # use purrr to download the pdf associated with each url to the current working directory
             }
      
        
        # 2 - Running OCR on Downloaded PDFs and Saving to text files
        
        
        library(pdftools)
        getwd()
        setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/pdfs/"))    
        files.in <- list.files(pattern = ".pdf")
        folder.out <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/txts/")
        
        for (i in 1:length(files.in)){
          pdf <- files.in[i]
          ocr <- pdf_ocr_text(pdf, dpi = 300, language = "eng")
          txt = paste0(folder.out,gsub(".pdf",".txt",pdf))
          cat(ocr,file=txt,sep="\n") 
          print(paste(">>>", i,"out of", length(files),"files converted to txt"))
        }
        
        
      # 3 - Extract metadata from PDF file names
      
        # 3.a - PDB 1961 - 1969 
        
            remove <- c("Document Number:", "Pages:", "AttachmentSize DOC_\\d*\\.pdf")
            meta.df.61.69 = data.frame()
       
             for (i in 0:totalpgs.61.69){
              cia.61.69 <- read_html(paste0(url.61.69, i))
              print(paste("Downloading metadata from page",i))
              print(paste0(url.61.69, i))
              raw.metadata.61.69 <-  html_text(html_nodes(cia.61.69,".view-content"),trim = TRUE)
              cleaned.meta <- as.data.frame(do.call(cbind, strsplit(str_remove_all(raw.metadata.61.69, paste(remove, collapse = "|"))," *\\\n *\\\n *\\\n *\\\n *\\\n *\\\n *")))
              dat <- separate(cleaned.meta, col = V1, sep = " *\\\n *",into = c("Title", "Doc", "Pages", "Size"))
              meta.df.61.69 <- rbind(meta.df.61.69,dat)
             }

            meta.df.61.69$date <- data.frame(date = str_extract(meta.df.61.69$Title,"\\d{1,2} *\\w* \\d{4}"))
            #meta.df.61.69$link <- data.frame(link.61.69 = paste0("https://www.cia.gov/readingroom/docs/DOC_",meta.df.61.69$Doc,".pdf"))
            
         
         # 3.b - PDB 1969 - 1977 
      
            remove <- c("Document Number:", "Pages:", "AttachmentSize DOC_\\d*\\.pdf")
            meta.df.69.77 = data.frame()
            for (i in 0:totalpgs.69.77){
              cia.69.77 <- read_html(paste0(url.69.77, i))
              print(paste("Downloading metadata from page",i))
              print(paste0(url.69.77, i))
              raw.metadata.69.77 <-  html_text(html_nodes(cia.69.77,".view-content"),trim = TRUE)
              cleaned.meta <- as.data.frame(do.call(cbind, strsplit(str_remove_all(raw.metadata.69.77, paste(remove, collapse = "|"))," *\\\n *\\\n *\\\n *\\\n *\\\n *\\\n *")))
              dat <- separate(cleaned.meta, col = V1, sep = " *\\\n *",into = c("Title", "Doc", "Pages", "Size"))
              meta.df.69.77 <- rbind(meta.df.69.77,dat)
            }
    
            meta.df.69.77$date <- data.frame(date = str_extract(meta.df.69.77$Title,"\\d{1,2} *\\w* \\d{4}")) 
            #meta.df.69.77$link <- data.frame(link.69.77 = paste0("https://www.cia.gov/readingroom/docs/DOC_",meta.df.69.77$Doc,".pdf"))
       
         # 3.c Merging both periods (1961-1977) into a single dataset
            
            meta.df.61.77 <- rbind(meta.df.61.69, meta.df.69.77)
            datas <- dput(meta.df.61.77$date)
            datas$date_ISO8601 = lubridate::dmy(datas$date) 
            
            # Formatando data para DD/MM/AAAA
            datas$date = format(datas$date_ISO8601 , "%d/%m/%Y")
            
            # Merging all the period (1961-1977) and structuring the dates into a single final dataset
            meta.df.61.77 <- data.frame(meta.df.61.77, datas)
            meta.df.61.77$date_ISO8601 <- NULL
            meta.df.61.77$date <- NULL
            names(meta.df.61.77)[5] <- "Date"
            
            # contando o número de NAs na coluna de data
            sum(is.na(meta.df.61.77$Date))
            
            # Corrigindo os NAs restantes manualmente (fonte: chk.61.69)
            
            meta.df.61.77[168,  5] <- "27/12/1961"
            meta.df.61.77[243,  5] <- "20/03/1962"
            meta.df.61.77[1140, 5] <- "23/10/1964"
            meta.df.61.77[1152, 5] <- "03/11/1964"
            
            # Checando novamente o número de NAs na coluna de data
            # Após rodar o código acima, espera-se que o número de NAs na coluna "Date" seja zero.
            
            sum(is.na(meta.df.61.77$Date))
      
            
       # 4 - Transforming text files into VCorpus, DTM, and Preparing for Topic Modeling
      
        library(tm)
            
        # 4.a -  Transforming text files into VCorpus
          
            folder.out <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/txts/")
            corp <- VCorpus(DirSource(directory = folder.out,pattern = "*.txt"))    
      
        # 4.b - Clean-up Process
        
        library(stopwords)
        pdb_stopwords = scan(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/pdb_stopwords.txt"), what = "character")
        corp_cleaned <- tm_map(corp,content_transformer(tolower))
        corp_cleaned <- tm_map(corp_cleaned, removeWords, c("data", stopwords("en"),pdb_stopwords))
        corp_cleaned <- tm_map(corp_cleaned, stemDocument, language = "en")  
        corp_cleaned <- tm_map(corp_cleaned,content_transformer(removePunctuation))
        corp_cleaned <- tm_map(corp_cleaned,content_transformer(removeNumbers))
        
        
        # 4.c - Binding meta.df.61.77 as to corp_cleaned as corpus metadata

        meta(corp_cleaned, c("Title", "Doc", "Pages","Size","Date")) <- meta.df.61.77
   
        
        # For later: join "South" "Africa" tokens into single "SouthAfrica" token André, veja como fazer isso com a função "tokens_compound" da biblioteca "quanteda", por favor
        
        # 5.c - Creating Document Term Matrix
        dtm <- DocumentTermMatrix(corp_cleaned)
        inspect(dtm)
      
        # 6 - Subsetting DTM with keywords
        
        library(dplyr)
        
        # 6.a -  Setting search terms and subsetting DTM
     
        
              # Treatment - Devolution - primus inter pares
              BRA <- "Brazil"
              IRN <- "Iran"
              IDN <- "Indonesia"
              ZAS <- "Pretoria" # Por enquanto, estamos usando uma das capitais da África do Sul como um proxy. 
              
              # Control - Neighborhood Countries - non primus inter pares
              BRA.region.ARG <- "Argentina"
              BRA.region.MEX <- "Mexico"
              IRN.region.ISR <- "Israel"
              IRN.region.KSA <- "Saudi"
              IDN.region.AUS <- "Australia"
              IDN.region.MAL <- "Malaysia"
              ZAS.region.ZAR <- "Zaire"
              ZAS.region.CON <- "Congo"
              ZAS.region.RHD <- "Rhodesia"
              

              search_results <- data.frame(as.matrix(dtm))%>%
                  select(tolower(BRA) | 
                           tolower(IRN) | 
                           tolower(IDN) | 
                           tolower(ZAS) | 
                           tolower(BRA.region.ARG) |
                           tolower(BRA.region.MEX) |
                           tolower(IRN.region.ISR) |          
                           tolower(IRN.region.KSA) |
                           tolower(IDN.region.AUS) |
                           tolower(IDN.region.MAL) |
                           tolower(ZAS.region.ZAR) |
                           tolower(ZAS.region.CON) |
                           tolower(ZAS.region.RHD))
              
              
          # Binding Metadata with DTM subset  
              
            pdbs.61.77 = cbind(meta.df.61.77,search_results)
            head(pdbs.61.77)  
            
            rm(list=setdiff(ls(),c("corp_cleaned", "dtm","search_results","pdbs.61.77")))
            
            save.image(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/dtm_output.RData"))
       
      
