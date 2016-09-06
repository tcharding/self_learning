-- /show
data FailableDouble = Failure 
                    | OK Double
  deriving Show
  
-- show
a = Failure
b = OK 3.4
