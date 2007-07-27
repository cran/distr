# generating function for class 'DistrList'
DistrList <- function(...){ 
    new("DistrList", list(...)) 
}

# coerce to "DistrList"
setAs(from = "Distribution", to = "DistrList", 
    def = function(from){ new("DistrList", list(from)) })


# generating function for class 'UnivarDistrList'
UnivarDistrList <- function(...){ 
    new("UnivarDistrList", list(...)) 
}

# coerce to "UnivarDistrList"
setAs(from = "UnivariateDistribution", to = "UnivarDistrList", 
    def = function(from){ new("UnivarDistrList", list(from)) })
