

# DT table options... ####
buttonList = function( file_name = paste( 'downloaded_' , Sys.Date() ) ){
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
        lengthMenu = list( c( -1, 5, 10, 25, -1), list( 'All' , '5' , '10', '25') ) ,
        columnDefs = list( list( className = 'dt-right' , targets="_all" ) ) ,
        dom = 'l<"col-sm-6"B>fiprt' ,
        buttons = buttonList(...)
  )
}


DToptions_no_buttons = function(...){
  list( autoWidth = TRUE , 
        dom = 'l<"col-sm-6"i>fprt' ,
        # scrollX = TRUE  ,
        lengthMenu = list( c( -1, 5, 10, 25, -1), list( 'All' , '5' , '10', '25') ) ,
        columnDefs = list( list(className = 'dt-right', targets="_all" ) ) 
  )
}