import DataFile
import System.Random
import System.IO.Unsafe

					 
createEmptyFreqList::[a] -> [(a,[b])]
createEmptyFreqList list = [ (x,[])  | x <- list ]


remove n [] =[] 
remove n (x:xs)|n==x = remove n xs 
			  |otherwise = x : remove n xs
-- getAll a list takes a list of lists and if item is in the list it removes it and append and do so for the remaining
getAll _ []= []
getAll item (x:xs)| elem item x =(remove item x) ++ (getAll item xs	)
				|otherwise = getAll item xs
					 
countOcc x []=0
countOcc x (y:ys) |y==x =1+ countOcc x ys
				 |otherwise = countOcc x ys
				 

getAllUsersStatsHelper listResultOfGetAll  =[(item,(countOcc item (listResultOfGetAll)))   |
												item <- items , (countOcc item (listResultOfGetAll)) /=0 ]
						
						
getListOfTuplesOfItems  listOfLists =[ (item,getAllUsersStatsHelper (getAll item listOfLists)) |
										 item <- items ]  
										 
getAllUsersStats list = [(user, ( getListOfTuplesOfItems listOfLists )) |(user,listOfLists)<- list ]


-- intersect :1st we make intersect that takes the frequency of item i and item i from another user and return their intersect by either increment the freq or append
intersect l [] = l
intersect l ((item,feq):ys)|not( checkItemExist item l) = intersect ((item,feq):l)  ys 
				  |otherwise    = intersect (incrementByN (item,feq) l) ys
	
--incrementByN takes (a,b) where a is item and b is the frequency of it and search for this item in the second input and increment its frequency	
incrementByN :: (Eq a, Num b) => (a,b) -> [(a,b)] -> [(a,b)]
incrementByN (item,value) ((userItem,freq):ys) |userItem== item =(userItem,freq+value):ys
											|otherwise =  (userItem,freq) : incrementByN (item,value) ys

--checkItemExits checks if item is in the list of pairs (item ,_)
checkItemExist _ [] = False
checkItemExist item ((item1,_):ys) |item==item1 = True
								| otherwise = checkItemExist item ys
								
--getSecOfPair takes item and list of pairs and return the second element in a pair where the first item in it is "item"								
getSecOfPair :: Eq a => a -> [(a,[b])] -> [b]
getSecOfPair _ [] = []
getSecOfPair item ((item1,freqList):ys) |item==item1 =freqList
								| otherwise = getSecOfPair item ys


purchasesIntersectionPerUser (_,listUser)  buyerStatList =[(item , intersect freqList ((getSecOfPair item buyerStatList))) |
														(item,freqList)<-listUser,(freqList/=[]&&(getSecOfPair item buyerStatList)/=[] )]
														
														
purchasesIntersection:: Eq a =>[(a,[(a,Int)])]->[(a,[(a,[(a,Int)])])]->[[(a,[(a,Int)])]]
purchasesIntersection buyerStatList usersStatList = [(purchasesIntersectionPerUser user buyerStatList)| user<- usersStatList]


sumItemFreq [] _=0
sumItemFreq ((s,v):xs) item= if(item==s) then v+sumItemFreq xs item else sumItemFreq xs item 

saya7List  l = foldr (++) [] [x|(_,x)<-l]	

--frqListItemsHelper we pass to it a metsya7 pair of items and their frequencies to return every item in a pair with the sum of its frquency 
			 
freqListItemsHelper [] =[]
freqListItemsHelper ((item,s):xs) = (item,sumFreq): freqListItemsHelper newList where {sumFreq = sumItemFreq ((item,s):xs) item ; newList = removeAllItem item ((item,s):xs) } 


removeAllItem n [] =[] 
removeAllItem item ((first,sec):xs)|first==item = removeAllItem item xs 
			  |otherwise = (first,sec) : removeAllItem item xs

			  
freqListItems user =  freqListItemsHelper(saya7List(getSecOfPair user(getAllUsersStats purchasesHistory)))



freqListUsers user = freqListItemsHelper (concatMap saya7List ( purchasesIntersection (buyerPrev) (users)))
	where { buyerPrev = getSecOfPair user (getAllUsersStats purchasesHistory);
	users = filter (/=(user,buyerPrev)) (getAllUsersStats purchasesHistory) }
	
	
-- check if the first element of the pair is member of the list				
containsFirstOfPair (item ,_) list = elem item list


freqListCart user listOfItems =freqListItemsHelper  ( saya7List ([x|x<- userStats,(containsFirstOfPair x listOfItems)]))   
				where {userStats=getSecOfPair user (getAllUsersStats purchasesHistory)}
				


freqListCartAndItems user cartList = intersect (freqListItems user) (freqListCart user cartList)

---Recommended based on user which takes freqList flattens it and makes a random choice
---recommendBasedOnUsers user 
recommendBasedOnUsers user	| x==[] = []
							|otherwise = x !! ((randomZeroToX ((length x)-1) )) where x=afrdlyList(freqListUsers user)
afrdlyList []=[]
afrdlyList ((item,c):xs)= krarlyItem (item,c)++afrdlyList xs
krarlyItem (item,0)=[]
krarlyItem (item,c)= item:krarlyItem(item,c-1)

recommendEmptyCart user | x==[] = []
						|otherwise = x !! ((randomZeroToX ((length x)-1) )) where x=afrdlyList(freqListItems user)
						
						
recommendBasedOnItemsInCart user cart | (getSecOfPair user purchasesHistory) ==[] = []
									  |otherwise= x !! ((randomZeroToX ((length x)-1) )) where x=afrdlyList(freqListCartAndItems user cart)
									  
recommend user cart | x==["",""]= items !! (randomZeroToX(length (items)-1))
					|otherwise = x !! (randomZeroToX 1)
					where {x=[recommendBasedOnItemsInCart user cart,recommendBasedOnUsers user]}
					
						



