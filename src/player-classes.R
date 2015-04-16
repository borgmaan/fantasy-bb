
Player <- R6Class(
  "Player",
  public = list(
    
    ## our fields
    first_name = NA, 
    last_name = NA,
    team = NA,
    positions = NA,
    curr_owner = NA,
    stat_rankings = NA,
    projection_rankings = NA,
    global_fields = NA, # used to store things like ECS & global mWERTH -- full values for cross positional comparison....
    
    ## some functions
    set_pos = function(pos_vec) {
      self$positions <- na.omit(unique(c(self$positions, pos_vec)))
    },
    
    check_pos = function(to_check) {
      if (to_check %in% self$positions) return(TRUE)
      else return(FALSE)
    },
    
    set_field = function(field_name, val, name = NULL) {
      if (length(self[[field_name]]) == 1 & is.na(self[[field_name]])) {
        if (!is.null(name)) {
          self[[field_name]] <- setNames(val, name)
        } else {
          self[[field_name]] <- val
        }         
      } else {
        self[[field_name]] <- c(self[[field_name]], val)
        lt <- length(self[[field_name]])
        if (!is.null(name)) names(self[[field_name]])[lt] <- name
      }
    },
    
    # returns the current projections as a named data frame
    # if proj_names is a vector of names, it fills in missing
    # projections with NA
    get_projections = function(proj_names = NULL) {
      
      if (!is.null(proj_names)) {
        
        # create our data frame and name it
        out_df <- data.frame(matrix(NA, ncol = length(proj_names), nrow = 1))
        rownames(out_df) <- paste(self$first_name, self$last_name, 
                                  collapse = '')
        names(out_df) <- proj_names
        
        # pull in data
        out_df[1,] <- sapply(proj_names, function(x) {
          if (x %in% names(self$projection_rankings)) {
            return(self$projection_rankings[x])
          } else {
            return(NA)
          }
        })
        
        return(out_df)
        
      } else {        
        if (is.na(self$projection_rankings)) {
          return(NULL)
        } else {
          return(self$projection_rankings)
        }        
      }      
    },
    
    # function to acces stats
    get_stats = function(stat_name, agg_func = 'mean', ...) {
      if (is.null(agg_func)) {
        return(self[[stat_name]])
      } else {
        f <- match.fun(agg_func)
         return(f(self[[stat_name]], na.rm = T))
      }
    }
    
  )
)

Hitter <- R6Class(
  "Hitter",
  inherit = Player,
  public = list(
    
    # our fields
    obp = NA,
    slg = NA,
    hr = NA, 
    r = NA,
    rbi = NA,
    sb = NA
 
  )
)

Pitcher <- R6Class(
  "Pitcher",
  inherit = Player,
  public = list(
    
    # our fields
    whip = NA,
    era = NA,
    k = NA,
    k9 = NA,
    qs = NA, # quality start
    saves = NA
    
    # functions
    
    
  )
)

