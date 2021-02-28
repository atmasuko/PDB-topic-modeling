
    # 1 - Scraping CIA Website for PDF Download Links and Downloading PDFs to Local Folder

        rm(list=ls())

        library(tidyverse)
        library(rvest)
        library(stringr)
        library(lubridate)
      
        setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/pdfs")) # Script R precisa estar dentro da pasta "PDBs" no Dropbox
        #load(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/meta.df.61.77 data.rdata"))
        
        # 1.a - Downloading PDFS from [PDB 1961 - 1969] Collection
        url.61.69 <-"https://www.cia.gov/readingroom/collection/presidents-daily-brief-1961-1969?page="
        site.61.69 <- read_html(url.61.69) 
        links.61.69 <- html_attr(html_nodes(site.61.69,"a"),"href")
        totalpgs.61.69 <- gsub("page=","",str_extract(links.61.69[length(links.61.69)-2], "page=\\d*"))
        # repeated <- "https://www.cia.gov/readingroom/docs/PDB%20CM%20Final%20Kennedy%20and%20Johnson_public%208%20Sep%202015.pdf"
        
        # for (i in 0:totalpgs.61.69){
        #    cia.61.69 <- read_html(paste0(url.61.69, i))
        #    print(paste("Downloading links from page",i))
        #    raw_list <- cia.61.69 %>%
        #       html_nodes("a") %>%  # find all links in the page
        #       html_attr("href")%>% # get the url for these links
        #       str_subset("\\.pdf") # find those that end in pdf only
        #    cleaned_list <- raw_list[!(raw_list %in% repeated)]%>% # remove repeated pdf
        #      walk2(., basename(.), download.file, mode = "wb") # use purrr to download the pdf associated with each url to the current working directory
        # }
        
        # 1.b - Downloading PDFS from [PDB 1969 - 1977] Collection
        url.69.77 <- "https://www.cia.gov/readingroom/collection/presidents-daily-brief-1969-1977?page="
        site.69.77 <- read_html(url.69.77)
        links.69.77 <- html_attr(html_nodes(site.69.77,"a"),"href")
        totalpgs.69.77 <- gsub("page=","",str_extract(links.69.77[length(links.69.77)-2], "page=\\d*"))
        # repeated <- "https://www.cia.gov/readingroom/docs/PDB%20Symposium%20Nixon%20and%20Ford%2024%20Aug%202016.pdf"
        
        # for (i in 0:totalpgs.69.77){
        #    cia.69.77 <- read_html(paste0(url.69.77, i))
        #    print(paste("Downloading links from page",i))
        #    raw_list <- cia.69.77 %>%
        #       html_nodes("a") %>%  # find all links in the page
        #       html_attr("href")%>% # get the url for these links
        #       str_subset("\\.pdf") # find those that end in pdf only
        #    cleaned_list <- raw_list[!(raw_list %in% repeated)]%>% # remove repeated pdf
        #      walk2(., basename(.), download.file, mode = "wb") # use purrr to download the pdf associated with each url to the current working directory
        # }
        
      
      # 2 - Extract metadata from PDF file names
      
        # 2.a - PDB 1961 - 1969 
        
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

            meta.df.61.69$date <- data.frame(date = str_extract(meta.df.61.69$Title,"\\d{1,2} *\\w* \\d{4}")) #André, só para você entender, adicionei um asterisco antes de "\\w" para o código captar todos os espaços entre o mês e o dia
            meta.df.61.69$link <- data.frame(link.61.69 = paste0("https://www.cia.gov/readingroom/docs/DOC_",meta.df.61.69$Doc,".pdf"))

            chk.61.69 <- filter(meta.df.61.69, is.na(date)) # como vemos, não resolveu tudo. Estes casos remanescentes teremos de corrigir na mão, creio eu. Faça isso, por favor?
            # João, não sei por que motivo esse código acima não captou o NA da linha 1152 (ano: 1964)
            # São 4 NAs no total, e não 3. O único NA restante foi este da linha 1152 que, aliás, será devidamente corrigido.
            
           
         # 2.b - PDB 1969 - 1977 
      
            # Verifique se tá tudo certo com o código abaixo, plz. Acredito que esteja certo, mas não chequei com calma
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
            meta.df.69.77$link <- data.frame(link.69.77 = paste0("https://www.cia.gov/readingroom/docs/DOC_",meta.df.69.77$Doc,".pdf"))
                
            chk.69.77 <- filter(meta.df.69.77, is.na(date)) # Aqui arrumou tudo! Só vai precisar remendar os 3 lá de 61 - 69
            
            # Merging all the period (1961-1977) into a single dataset
            meta.df.61.77 <- rbind(meta.df.61.69, meta.df.69.77)
          
            
            datas <- dput(meta.df.61.77$date)
  
        datas$date_ISO8601 = lubridate::dmy(datas$date)
        
        # Formatando data para DD/MM/AAAA
        datas$date = format(datas$date_ISO8601 , "%d/%m/%Y")
        
        # Merging all the period (1961-1977) and structuring the dates into a single final dataset
        meta.df.61.77 <- data.frame(meta.df.61.77, datas)
        meta.df.61.77$date_ISO8601 <- NULL
        meta.df.61.77$date <- NULL
        names(meta.df.61.77)[6] <- "Date"
        
        # contando o número de NAs na coluna de data
        sum(is.na(meta.df.61.77$Date))
        
        # Corrigindo os NAs restantes manualmente (fonte: chk.61.69)
        
        meta.df.61.77[168, 6] <- "27/12/1961"
        meta.df.61.77[243, 6] <- "20/03/1962"
        meta.df.61.77[1140, 6] <- "23/10/1964"
        meta.df.61.77[1152, 6] <- "03/11/1964"
        
        # Checando novamente o número de NAs na coluna de data
        # Após rodar o código acima, espera-se que o número de NAs na coluna "Date" seja zero.
        
        sum(is.na(meta.df.61.77$Date))
        

           
            
       # 3 - Running OCR on Downloaded PDFs and Saving to text files
            
             
         library(pdftools)
         
         files.in <- list.files(pattern = ".pdf")
         folder.out <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/txts/")
         
         for (i in 1:length(files.in)){
           pdf <- files.in[i]
           ocr <- pdf_ocr_text(pdf, dpi = 300, language = "eng")
           txt = paste0(folder.out,gsub(".pdf",".txt",pdf))
           cat(ocr,file=txt,sep="\n") 
           print(paste(">>>", i,"out of", length(files),"files converted to txt"))
           }
            
       # 4 - Transforming text files into VCorpus, DTM, and Preparing for Topic Modeling
      
        library(tm)
            
        # 4.a -  Transforming text files into VCorpus
          
            folder.out <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/txts/")
            corp <- VCorpus(DirSource(directory = folder.out,pattern = "*.txt"))    
      
        # 4.b - Clean-up Process
        
        library(stopwords)
        
        #pdb_stopwords = scan("https://raw.githubusercontent.com/jdallapola/pdbs/stopwords_pdb.csv", what = "character")
        corp_cleaned <- tm_map(corp,content_transformer(tolower))
        corp_cleaned <- tm_map(corp_cleaned, removeWords, c("data", stopwords("en"))) #Reminder Add custom stopwords later
        corp_cleaned <- tm_map(corp_cleaned, stemDocument, language = "en")  
        corp_cleaned <- tm_map(corp_cleaned,content_transformer(removePunctuation))
        
        # 4.c - Creating Document Term Matrix
        
        dtm <- DocumentTermMatrix(corp_cleaned, control = list(bounds = list(global = c(3, Inf))))
        raw.sum=apply(dtm,1,FUN=sum) 
        dtm <- dtm[raw.sum!=0,] # This step is necessary to remove blanks from DTM so that LDA function will work
    
      
      # 5 - Separando DTM por palavras chave (Brazil, Iran, Indonesia, and South Africa)
        
        library(dplyr)
        
        # 5.a -  Creating search function
        
        go_search <- function(){
          search_results <<- data.frame(as.matrix(dtm))%>%
            select(tolower(search_key_1) | tolower(search_key_2) | tolower(search_key_3) | tolower(search_key_4))
            search_results
        }
        
        # >>>> User interface
              
           # User defined search keys
              search_key_1 = "Brazil"
              search_key_2 = "Iran"
              search_key_3 = "Indonesia"
              search_key_4 = "South Africa"
               
           # Search!
              go_search()
                
              search_results
                
           # write Results to .csv file
              write.csv2(search_results, paste0(dest,"/tm_output/","State_Department_Term_Map.csv"))
                

       # 6 - Topic Modeling  
        
        library(quanteda)
        library(topicmodels)
        
        # 6.a - Creating a preliminary Topic Model
        
        set.seed(1947)
        m = LDA(dtm, method = "Gibbs", k = 30, control = list(alpha = 0.1))
        
        # 6.b - Visualizing data in HTML widget
        
        library(LDAvis)
        
        dtm <- dtm[slam::row_sums(dtm)>0,]
        phi <- as.matrix(posterior(m)$terms)
        theta <- as.matrix(posterior(m)$topics)
        vocab <- colnames(phi)
        doc.length <- slam::row_sums(dtm)
        term.freq <- slam::col_sums(dtm)[match(vocab, colnames(dtm))]
        
        json = createJSON(phi = phi,theta = theta, vocab = vocab, doc.length = doc.length, term.frequency = term.freq)
        serVis(json)

        
        
        
      
