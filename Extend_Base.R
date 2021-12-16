
Extend_Base <- function(Unit, Str_Year, End_Year, Var) {
        
        # Verifies the type of the arguments
        stopifnot(is.character(Unit))
        stopifnot(is.numeric(Str_Year))
        stopifnot(is.numeric(End_Year))
        
        require(dplyr) #for pipe and functions
        require(tidyr) #for fill function
        
        Uns <- unique(Unit) # Defines parameters
        StY <- min(Str_Year)
        EdY <- max(End_Year)
        
        # Creates a basic df, repeating units and years
        u <- list() 
        for(i in 1:length(Uns)){
                u[[i]] <-  sapply(Uns[i], rep, length(StY:EdY))
        } 
        
        u <- unlist(u)
        
        a <- data.frame()
        a <- cbind.data.frame(rep(StY:EdY, length(Uns)), u)
        names(a) <- c("Year", "Unit") # Rename columns
        
        # Joins the base dataset with one that includes the main variable
        b <- full_join(a, cbind.data.frame(Unit, Str_Year, Var), 
                       by=c("Unit" = "Unit", "Year"="Str_Year")) %>% 
                full_join(., cbind.data.frame(Unit, End_Year, Var),
                          by=c("Unit" = "Unit", "Year"="End_Year", "Var"="Var"))
        
        # Arranges and fills operating by groups
        b <- b %>% group_by(., Unit) %>% 
                arrange(., Year) %>% 
                fill(Var) # form tidyr, fills "down" by default
        
        # Converts to DF and order by Unit and Year 
        b <- as.data.frame(b) %>% arrange(., Unit, Year) %>% distinct(.)
        return(b)
}

