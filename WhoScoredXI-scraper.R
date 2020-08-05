getWhoScoredXI = function(url){
  require(RSelenium)
  require(seleniumPipes) #for getElementAttribute
  require(dplyr)
  
  # For some unspecified reason we are starting and stopping the docker container initailly.
  # Similar to heating the bike's engine before shifting the gears.
  system("docker run -d -p 4445:4444 selenium/standalone-chrome")
  t = system("docker ps",intern=TRUE)
  if(is.na(as.character(strsplit(t[2],split = " ")[[1]][1]))==FALSE)
  {
    system(paste("docker stop ",as.character(strsplit(t[2],split = " ")[[1]][1]),sep=""))
  }
  
  # To avoid starting docker in Terminal
  
  system("docker run -d -p 4445:4444 selenium/standalone-chrome")
  Sys.sleep(3)
  remDr <- RSelenium::remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L,browserName = "chrome")
  # Automating the scraping initiation considering that Page navigation might crash sometimes in
  # R Selenuium and we have to start the process again. Good to see that this while() logic
  # works perfectly
  
  while (TRUE) {
    tryCatch({
      #Entering our URL gets the browser to navigate to the page
      remDr$open()
      remDr$navigate(as.character(url))
    }, error = function(e) {
      remDr$close()
      Sys.sleep(2)
      print("slept 2 seconds")
      next
    }, finally = {
      #remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer
      break
    })
  }
  
  # Scraping required stats
  
  # element2 = driver.find_element_by_xpath("//div[@title='div2']")
  # element2.find_element_by_xpath(".//p[@class='test']").text 
  
  remDr$navigate(url)
  webElem1 <- remDr$findElement(value = '/html/body/div[4]/div[8]/div[1]/div/div[2]/div/ul[2]')
  data <- webElem1 %>% getElementAttribute('title')
  remDr$close()
  remove(remDr)
  # Automating the following steps:
  # 1. run "docker ps" in Terminal and get the container ID from the output
  # 2. now run "docker stop container_id" e.g. docker stop f59930f56e38
  
  t = system("docker ps",intern=TRUE)
  system(paste("docker stop ",as.character(strsplit(t[2],split = " ")[[1]][1]),sep=""))
  
  return(data)
}
