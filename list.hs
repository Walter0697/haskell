--self implemented list data structure--

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

--- using +++ to combine two lists together
infixr 5 +++
(+++) :: List a -> List a -> List a
Empty +++ ys = ys
(x :-: xs) +++ ys = x :-: (xs +++ ys)

--- check if the list is empty
isEmpty :: (List a) -> Bool
isEmpty Empty = True
isEmpty _ = False

--- head, last, tail, init like the original list in haskell
listHead :: (List a) -> a
listHead Empty = error "Empty list"
listHead (x :-: _) = x

listLast :: (List a) -> (List a)
listLast Empty = error "Empty list"
listLast (_ :-: rest) = rest

listTail :: (List a) -> a
listTail Empty = error "Empty List"
listTail (x :-: Empty) = x
listTail (_ :-: rest) = listTail rest

listInit :: (List a) -> (List a)
listInit Empty = error "Empty List"
listInit (x :-: Empty) = Empty
listInit (x :-: rest) = x :-: (listInit rest)

--- count function
--- Input : List
--- Usage : get the length of the list
count :: (List a) -> Int
count Empty = 0
count (_ :-: rest) = 1 + (count rest)

--- insert function
--- Input : Integer position, List, element to be inserted
--- Usage : insert an element to a selected position
insert :: (Integral pos) => pos -> (List a) -> a -> (List a)
insert n Empty x
   | n == 0 = x :-: Empty
   | otherwise = Empty
insert n (element :-: rest) x 
   | n < 0 = x :-: (element :-: rest)
   | otherwise = element :-: insert (n - 1) rest x

--- remove function
--- Input : Integer position, List
--- Usage : remove an element in the selected position
remove :: (Integral pos) => pos -> (List a) -> (List a)
remove _ Empty = Empty
remove 0 (element :-: rest) = rest
remove n (element :-: rest) = element :-: (remove (n - 1) rest)

--- removeElement function
--- Input : List, element
--- Usage : remove the predicular element from the list
removeElement :: (Ord a) => (List a) -> a -> (List a)
removeElement Empty _ = Empty
removeElement (element :-: rest) x 
    | x == element = rest 
    | otherwise = element :-: (removeElement rest x)

--- removeElementAll function
removeElementAll :: (Ord a) => (List a) -> a -> (List a)
removeElementAll Empty _ = Empty
removeElementAll (element :-: rest) x 
    | x == element = removeElementAll rest x 
    | otherwise = element :-: (removeElement rest x)

--- get function
--- Input : Integer position, List
--- Usage : get the element in the selected position
get :: (Integral pos) => pos -> (List a) -> a
get _ Empty = error "element not in list"
get 0 (element :-: _) = element
get n (element :-: rest) = get (n - 1) rest

--- set function
--- Input : Integer position, List, element
--- Usage : replace the element in the selected position
set :: (Integral pos) => pos -> (List a) -> a -> (List a)
set _ Empty _ = Empty
set 0 (element :-: rest) x = x :-: rest
set n (element :-: rest) x = element :-: (set (n - 1) rest x)

--- contain funciton
--- Input : List, element
--- Usage : find out if the element exists in the list
contain :: (Ord a) => (List a) -> a -> Bool
contain Empty _ = False
contain (element :-: rest) x
    | x == element = True
    | otherwise = contain rest x

--- countElement function
--- Input : List, element
--- Usage : count the number of the element in the list
countElement :: (Ord a) => (List a) -> a -> Int
countElement Empty _ = 0
countElement (element :-: rest) x
    | x == element = 1 + (countElement rest x)
    | otherwise = countElement rest x

--- after function
--- Input : Integer position, List
--- Usage : get the list after the selected position
after :: (Integral pos) => pos -> (List a) -> (List a)
after _ Empty = Empty
after 0 list = list
after n (element :-: rest) = after (n - 1) rest

--- reduce function, which is fold in haskell
--- Input : function (from two variables to one), initial, List
--- Usage : using the self defined function to turn a list into a value
reduce :: (a -> a -> a) -> a -> (List a) -> a
reduce _ x Empty = x
reduce f x (element :-: Empty) = f element x
reduce f x (element :-: rest) = f element (reduce f x rest)

--- takeList function
--- Input : Integer number, List
--- Usage : take the first n numbers of elements in the list
takeList :: (Integral num) => num -> (List a) -> (List a)
takeList _ Empty = Empty
takeList 0 _ = Empty
takeList n (element :-: rest) = element :-: (takeList (n - 1) rest)

--- mapList function
--- Input : function (from one type to the other), List
--- Usage : map the list with the function
mapList :: (a -> b) -> (List a) -> (List b)
mapList f Empty = Empty
mapList f (element :-: rest) = (f element) :-: (mapList f rest)

--- filterList function
--- Input : function (that return boolean), List
--- Usage : filter the list with a certain condition
filterList :: (a -> Bool) -> (List a) -> (List a)
filterList f Empty = Empty
filterList f (element :-: Empty) = Empty
filterList f (element :-: rest) = if (f element) then element :-: (filterList f rest) else (filterList f rest)

--- reverseList function
--- Input : List
--- Usage : return the reverse version of the list
reverseList :: (List a) -> (List a)
reverseList Empty = Empty
reverseList (element :-: rest) = (reverseList rest) +++ (element :-: Empty)

--- generateSeries function
--- Input : function (Int to Int), starting number
--- Usage : generate an infinite list
--- Can use takeList as an advantage to produce finite list such as :
generateSeries :: (Integral num) => (num -> num) -> num -> (List num)
generateSeries f x = x :-: (generateSeries (f) (f x))

--- toHaskellList function
--- Input : List
--- Usage : turn this list datatype into haskell default list
toHaskellList :: (List a) -> [a]
toHaskellList Empty = []
toHaskellList (element :-: rest) = element : (toHaskellList rest)

--- fromHaskellList function
--- Input : list []
--- Usage : turn haskell default list into this list datatype
fromHaskellList :: [a] -> (List a)
fromHaskellList [] = Empty
fromHaskellList (x:xs) = x :-: (fromHaskellList xs)

------ function that generated by the above functions

--- spliceList function
--- Input : Integer number, List
--- Usage : separate a list into two lists by the selected position
spliceList :: (Integral pos) => pos -> (List a) -> ((List a), (List a))
spliceList n list = (takeList n list, after n list)

--- sumList function 
--- Input : List
--- Usage : return the sum of the list
sumList :: (Integral a) => (List a) -> a
sumList list = reduce (\x y -> x + y) 0 list

plus1 = generateSeries (\x -> x + 1) 0
time2 = generateSeries (\x -> x * 2) 1
all1 = generateSeries (\x -> x) 1
list1 = takeList 10 plus1
list2 = takeList 10 time2
list3 = takeList 10 all1