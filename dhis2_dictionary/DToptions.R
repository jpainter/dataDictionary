

# DT table options... ####
buttonList = function( file_name = paste( 'downloaded' , Sys.Date() ) ){
  list( 'copy', 'print', 
        list(
          extend = 'collection', 
          buttons = list( 
            list( extend = 'csv'  , filename = file_name) , 
            list( extend = 'excel'  , filename = file_name) ,
            list( extend = 'pdf' , filename = file_name)  
          ) ,
          text = 'Download' 
        )
  )
}

DToptions_with_buttons = function(...){
  list( autoWidth = TRUE , 
        scrollX = TRUE  ,
        # lengthMenu = c(10, 25, -1) ,
        lengthMenu = list(c(10, 25, -1), list('10', '25', 'All') ) ,
        columnDefs = list( list( className = 'dt-right' , targets="_all" ) ) ,
        dom = 'Blfrtip' ,
        buttons = buttonList(...)
  )
}


DToptions_no_buttons = function(...){
  list( autoWidth = TRUE , 
        # scrollX = TRUE  ,
        lengthMenu = list(c(10, 25, -1), list('10', '25', 'All') ) ,
        columnDefs = list(list(className = 'dt-right', targets="_all" ) ) 
  )
}