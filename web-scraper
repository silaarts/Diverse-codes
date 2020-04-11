
library("rvest")
  

# alle verpleeghuizen
verpleeghuizen <- read_html(paste0("https://www.zorgkaartnederland.nl/verpleeghuis-en-verzorgingshuis"))

# Hoeveel paginas met verpleeghuizen
paginas_huizen <- verpleeghuizen %>% html_nodes(".pagination_holder") %>% html_text(trim=T)
paginas_huizen <- as.numeric(unlist(regmatches(paginas_huizen, gregexpr("[[:digit:]]+", paginas_huizen))))
paginas_huizen <- paginas_huizen[length(paginas_huizen)]

# loop door alle paginas heen
all_data <- data.frame()
for (pagina_huizen in 1:paginas_huizen) {
    tryCatch({
    print(paste("Begonnen aan pagina:",pagina_huizen))
    verpleeghuizen <- read_html(paste0("https://www.zorgkaartnederland.nl/verpleeghuis-en-verzorgingshuis/pagina",pagina_huizen)) 
    
    # haal alle links naar praktijken eruit (20 per pagina)
    urls_praktijken <- verpleeghuizen %>% html_nodes("a") %>% html_attr("href")
    urls_praktijken <- urls_praktijken[grepl("/zorginstelling/verpleeghuis-en-verzorgingshuis-",urls_praktijken)]
    ## omdat er nog wat rotzooi links tussen zitten met het woord /waardering/ erin halen we die weg
    urls_praktijken <- urls_praktijken[!(grepl("/waardering/",urls_praktijken))]
    ## alle duplicaten eruit halen
    urls_praktijken <- urls_praktijken[duplicated(urls_praktijken)]
    
    # Nu kunnen we door alle urls van praktijken heen gaan loopen
    praktijk_data <- data.frame()
    for (url in 1:length(urls_praktijken)) {
      tryCatch({
      print(paste("Begonnen aan praktijk:",urls_praktijken[url]))
        praktijk <- read_html(paste0("https://www.zorgkaartnederland.nl/zorginstelling/verpleeghuis-en-verzorgingshuis-bartholomeus-gasthuis-utrecht-12798"))
      praktijk <- read_html(paste0("https://www.zorgkaartnederland.nl",urls_praktijken[url]))
      # haal adres gegevens op
      gegevens <- praktijk %>% html_nodes("address") %>% html_text(trim=T)
      postcode <- unlist(regmatches(gegevens, gregexpr("[[:digit:]]{4} [A-z]{2}", gegevens)))
      gegevens <- praktijk %>% html_nodes(".address_content") %>% html_text(trim=T)
      adres <- gegevens[1]
      website <- gegevens[2]
      gegevens <- praktijk %>% html_nodes(".col-md-7") %>% html_text(trim=T)
      telefoon <- unlist(regmatches(gegevens, gregexpr("[[:digit:]]{3}.{1}[[:digit:]]{6,14}\n", gegevens)))
      telefoon <- gsub("\n","",telefoon)  # deze extra stap niet in vorige stap zodat je 100% zeker weet dat je een foon nr hebt
      
      # nu de waarderingen:
      waardering <- read_html(paste0("https://www.zorgkaartnederland.nl",urls_praktijken[url],"/waardering"))
      
      
      # aantal paginas met waarderingen
      paginas_praktijk <- waardering %>% html_nodes(".pagination_holder") %>% html_text(trim=T)
      paginas_praktijk <- as.numeric(unlist(regmatches(paginas_praktijk, gregexpr("[[:digit:]]+", paginas_praktijk))))
      paginas_praktijk <- paginas_praktijk[length(paginas_praktijk)]
      
      reviews_praktijk <- data.frame()
      for (pagina_praktijk in 1:paginas_praktijk) {
          tryCatch({
          webcontent <- read_html(paste0("https://www.zorgkaartnederland.nl",urls_praktijken[url],"/waardering/pagina",pagina_praktijk) )
      
          # Helaas is de tekst per waardering 'afgekapt'... we moeten dus op de 'lees meer' links klikken voordat we de volledige tekst krijgen
          # haal alle 'lees meer' links eruit
          urls_waarderingen <- webcontent %>% html_nodes("a") %>% html_attr("href")
          urls_waarderingen <- urls_waarderingen[grepl(paste0(urls_praktijken[url],"/waardering/"),urls_waarderingen)]
          # alle duplicaten eruit halen (resultaat moet max 20 waarderingen zijn)
          urls_waarderingen <- urls_waarderingen[duplicated(urls_waarderingen)]
          
          # Loop door de waarderingen heen
          data_reviews <- data.frame()
          for (urlx in 1:length(urls_waarderingen)) {
              tryCatch({
              print(paste("Waardering nr:",urlx))
              waardering <- read_html(paste0("https://www.zorgkaartnederland.nl",urls_waarderingen[urlx]) )
              
              # Tekst
              reviews_tekst <- waardering %>% html_nodes(".with_right_margin") %>% html_text(trim=T)
              # alleen tekst pakken die een 'toelichting' is. De rest is troep
              reviews_tekst <- reviews_tekst[grepl("Toelichting\n",reviews_tekst)]
              # we willen alleen het laatste stukje tekst dus we splitsen op de lange spaties
              reviews_tekst <- unlist(strsplit(reviews_tekst,"       ")) 
              # nu willen we het laatste object van al die splitsingen
              reviews_tekst <- reviews_tekst[length(reviews_tekst)]
              
              # Score
              reviews_score <- waardering %>% html_nodes(".amount_value_holder") %>% html_text(trim=T)
              reviews_score <- as.numeric(unlist(regmatches(reviews_score,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",reviews_score))))
              
              # Datum
              reviews_datum <- waardering %>% html_nodes(".title-small") %>% html_text(trim=T)
              reviews_datum <- unlist(regmatches(reviews_datum, gregexpr("[0-9]{1}.*[0-9]{4}", reviews_datum)))
              
              # bind into dataframe
              data_review <- data.frame(score=reviews_score,
                                        datum=reviews_datum,
                                        tekst=reviews_tekst)
              data_reviews <- rbind(data_reviews,data_review)
              Sys.sleep(2)
              }, error=function(e){cat(paste("WAARDERING ERROR: "),conditionMessage(e), "\n")})
          }
      reviews_praktijk <- rbind(reviews_praktijk,data_reviews)
      Sys.sleep(2)
      }, error=function(e){cat(paste("PRAKTIJK_WAARDERING PAGINA ERROR: "),conditionMessage(e), "\n")})
      }
      # naam opschonen (zorginstelling uit naam halen + verwijderen van nummers op het einde)
      naam <- gsub("/zorginstelling/","", urls_praktijken[url])
      onnodig_getal <- unlist(regmatches(naam, gregexpr("[0-9]*$", naam)))
      naam <- gsub(paste0("-",onnodig_getal),"", naam)
      # voeg naam toe aan dataframe
      reviews_praktijk$naam <- naam
      # voeg adres gegevens toe aan dataframe
      reviews_praktijk$postcode <- ifelse(identical(postcode, character(0)),"",postcode)
      reviews_praktijk$adres <- ifelse(identical(adres, character(0)),"",adres)
      reviews_praktijk$telefoon <- ifelse(identical(telefoon, character(0)),"",telefoon)
      reviews_praktijk$website <- ifelse(identical(website, character(0)),"",website)
      
      # bind data per praktijk
      praktijk_data <- rbind(praktijk_data,reviews_praktijk)
      Sys.sleep(1)
      }, error=function(e){cat(paste("PRAKTIJK ERROR: "),conditionMessage(e), "\n")})
    } 
    
  # bind data per pagina van verpleeghuizen (20 verpleeghuizen per pagina, en pakweg 120 paginas)    
  all_data <- rbind(all_data,praktijk_data)
  Sys.sleep(1)
  }, error=function(e){cat(paste("PAGINA ERROR: "),conditionMessage(e), "\n")})
}
