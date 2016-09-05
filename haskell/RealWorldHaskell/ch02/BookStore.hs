-- data BookInfo = Book Int String [String]
--                 deriving show

type ISBN = Int
type Title = String
type Authors = [String]

data BookInfo = Book ISBN Title Authors

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

dype BookRecord = (BookInfo, BookReview)
