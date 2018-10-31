module WordTree(WordTree(Word, Subword, Root), emptyTree, getAllPaths, addWords, deleteWords, getWordsBeginWith) where

data WordTree = Word String | Subword String [WordTree] | Root [WordTree]

emptyTree :: WordTree
emptyTree = Root []
-- DO NOT MODIFY ABOVE

getAllPaths :: WordTree -> [[String]]
addWords :: WordTree -> [String] -> WordTree
deleteWords :: WordTree -> [String] -> WordTree
getWordsBeginWith :: WordTree -> String -> [String]

instance Show WordTree where
    show (Root wordTree) = printWordTree "" wordTree 1 -- no prefix at this stage
    show (Word str) = str 
    show (Subword str _) = str 


isWord (Word _) = True
isWord _ = False

isRoot (Root _) = True
isRoot _ = False

isSubword (Subword _ _) = True
isSubword _ = False

spaceOfDepth 0 = ""
spaceOfDepth depth = " " ++ spaceOfDepth (depth-1)

printSubWord depth previousPrefix (Subword prefix wtList) = (spaceOfDepth (2*(depth-1))) ++ previousPrefix ++ prefix ++ ":" ++ "\n" 
                                                ++ ( printWordTree (previousPrefix++prefix) wtList (depth+1) ) 

printWord depth pre (Word str) = (spaceOfDepth (2*(depth-1))) ++ pre ++ str ++ "\n"
printWord _ _ _ = "something wrong!!"


printWordTree prefix wordTreeArray@(x:remain) d =       if (isSubword x) 
                                                        then (printSubWord d prefix x) ++ (printWordTree prefix remain d)
                                                        else if (isWord x) 
                                                        then (printWord d prefix x) ++ (printWordTree prefix remain d)
                                                        else "something wrong"
printWordTree _ _ _ = ""

getAllPathsInSubword prelist (Subword prefix wtList) = getAllPathsGeneral (prelist++[prefix]) wtList
getAllPathsInSubword _ _ = []

getAllPathsInWord prelist (Word str) = [prelist ++ [str]]
getAllPathsInWord _ _ = []

getAllPathsGeneral prelist wtList@(x:other) =   if (isSubword x)
                                                then (getAllPathsInSubword prelist x ) ++ (getAllPathsGeneral prelist other)
                                                else (getAllPathsInWord prelist x) ++ (getAllPathsGeneral prelist other)
getAllPathsGeneral _ _ = []

getAllPaths (Root x) = getAllPathsGeneral [] x
getAllPaths _ = []

-- if current element is a Subword
addOneWord former wtList@((Subword subwordStr subwordList):other) newstr = -- there is no similarity, continue
                                                                   if (similarStr == "")
                                                                   then (addOneWord (former++[Subword subwordStr subwordList]) other newstr)
                                                                   -- subword will divided --
                                                                   else if (subwordStr > similarStr)
                                                                   then (
                                                                        if (newstrLeft > substrLeft)
                                                                        then  Root ( former ++ [(Subword similarStr 
                                                                               [ Subword substrLeft subwordList , Word newstrLeft ])] ++ other )
                                                                        else  Root ( former ++ [(Subword similarStr 
                                                                               [ Word newstrLeft , 
                                                                               Subword substrLeft subwordList ])] ++ other )
                                                                   )
                                                                   -- newstr will be under the subword prefix --
                                                                   else if (subwordStr == newstr) 
                                                                   then Root ( former ++ [Subword subwordStr ([Word ""]++subwordList)]++other)
                                                                   else if (subwordStr == similarStr)                
                                                                   then Root ( former ++ 
                                                                        [Subword subwordStr (takeWt (addOneWord [] subwordList newstrLeft))] 
                                                                        ++ other )
                                                                   else Root [Word "upps"] -- it should never reach here 
                                                                   where similarStr = similarString subwordStr newstr 
                                                                         similarLen = similarLength subwordStr newstr
                                                                         newstrLeft = drop similarLen newstr
                                                                         substrLeft = drop similarLen subwordStr
-- if current element is Word
addOneWord former wtlist@((Word wordStr):other) newstr = if (similarStr /= "")
                                                  then (
                                                        if (wordStrLeft > newstrLeft)
                                                        then Root ( former ++ [Subword similarStr [Word newstrLeft , Word wordStrLeft]] ++ other)
                                                        else Root ( former ++ [Subword similarStr [Word wordStrLeft , Word newstrLeft]] ++ other)
                                                  )
                                                  else (addOneWord (former++[Word wordStr]) other newstr)
                                                  where similarStr = similarString wordStr newstr 
                                                        similarLen = similarLength wordStr newstr
                                                        newstrLeft = (drop similarLen newstr)
                                                        wordStrLeft = (drop similarLen wordStr)

addOneWord former [] newstr = Root (putRightPlace [] former newstr)

putRightPlace former wtList@((Subword substr subwt):other) newstr = if (newstr > substr) 
                                                                        then ( putRightPlace (former++[(Subword substr subwt)]) other newstr )
                                                                        else if (newstr < substr)
                                                                        then former ++ [Word newstr] ++ wtList
                                                                        else [Word "somethings wrong"]
 
putRightPlace former wtList@((Word wordstr):other) newstr = if (newstr > wordstr)
                                                                 then (putRightPlace (former++[(Word wordstr)]) other newstr)
                                                                 else if (newstr < wordstr)
                                                                 then former ++ [Word newstr] ++ wtList
                                                                 else [Word "somethings wrong"]             
putRightPlace former [] newstr = former ++ [Word newstr]


addWords root@(Root x) (newstr:other) =  if (elem newstr (getWordsGeneral "" x)) 
                                         then (addWords root other)
                                         else addWords (addOneWord [] x newstr) other
addWords root@(Root x) [] = root
addWords _ _ = Root []

control wtList@((Subword substr subwt@((Subword ssubstr ssubwt):sother)):other) = 
                                                if ((length subwt) == 1 )
                                                then [Subword (substr++ssubstr) ssubwt]
                                                else wtList
                                                
control wtList@((Subword substr subwt@((Word swordstr):sother)):other) =
                                                if ((length subwt)==1)
                                                then [Word (substr++swordstr)]
                                                else wtList                                               

deleteOneWord former wtList@(currentSubword@(Subword subStr subWt):other) str = if (similarStr /= "") -- there is similarity
                                                                 then Root (former ++ (control [Subword subStr (takeWt (deleteOneWord [] subWt strLeft) ) ]) 
                                                                 ++ other )
                                                                 else (deleteOneWord (former++[currentSubword]) other str)
                                                                 where similarStr = similarString subStr str
                                                                       similarLen = similarLength subStr str
                                                                       strLeft = drop similarLen str
                                
deleteOneWord former wtList@(currentWord@(Word wordStr):other) str = if (str==wordStr)
                                                         then (Root (former++other))
                                                         else (deleteOneWord (former++[currentWord]) other str)

deleteWords root@(Root x) (str:other) = if (elem str (getWordsGeneral "" x))
                                        then deleteWords (deleteOneWord [] x str) other
                                        else (deleteWords root other)

deleteWords root@(Root x) [] = root

deleteWords _ _ = Root []


getWordsBeginWithHelper root@((Subword subStr subWt):other) prefix ppre = 
                                                                       if (prefix == "") 
                                                                       then getWordsGeneral "" root
                                                                       else if (similarStr /= "")&&(prefix==similarStr)
                                                                       then map ((ppre++subStrDiff)++) (getWordsGeneral "" subWt)
                                                                       else if (similarStr /= "")&&(prefix/=similarStr)
                                                                       then getWordsBeginWithHelper (subWt) prefixLeft ppre
                                                                       else getWordsBeginWithHelper other prefix ppre
                                                                       where similarStr = similarString subStr prefix
                                                                             similarLen = similarLength subStr prefix
                                                                             prefixLeft = drop similarLen prefix
                                                                             subStrDiff = drop (length ppre) subStr
                                                                 

getWordsBeginWithHelper root@((Word str):other) prefix ppre = if (similarStr /= "")&&(prefix==similarStr)
                                                              then [(take ((length ppre) - (length prefix)) ppre)++str] -- arrange the necessary prefix 
                                                                     ++ getWordsBeginWithHelper other prefix ppre 
                                                              else getWordsBeginWithHelper other prefix ppre 
                                                              where similarStr = similarString str prefix 
                                                                 
getWordsBeginWithHelper _ _ _ = []                                                                

getWordsBeginWith (Root x) prefix = getWordsBeginWithHelper x prefix prefix                            
getWordsBeginWith _ _ = []


----------------------------------------------------------
-- USEFULL HELPERS

getWordsInSubword prestr (Subword str wtList) = getWordsGeneral (prestr++str) wtList
getWordsInSubword _ _ = []

getWordsInWord prestr (Word str) = [prestr++str]
getWordsInWord _ _ = []

getWordsGeneral prestr (x:other) =  if (isSubword x)
                                    then (getWordsInSubword prestr x) ++ (getWordsGeneral prestr other) 
                                    else (getWordsInWord prestr x) ++ (getWordsGeneral prestr other)
getWordsGeneral _ _ = []


takeString (Subword str wtList) = str
takeString (Word str) = str

-- take two string return its similarity as tuple
similarityState str1 str2 index (str,number) =  if (((length str1)>index)&&((length str2)>index)&&((str1!!index) == (str2!!index)))
                                                then similarityState str1 str2 (index+1) ((str++[(str1!!index)]),(number+1))
                                                else (str,number)

similarity str1 str2 = similarityState str1 str2 0 ("",0)

similarString str1 str2 = str 
        where (str,num) = similarity str1 str2 

similarLength str1 str2 = num
        where (str,num) = similarity str1 str2

takeWt (Root x) = x

---------------------------------------------------------

a = Root [ Subword "Da" [ Subword "m" [ Word "" , Subword "ag" [ Word "e", Word "ing"]] , Word "rk"]]
b = Root [Subword "Ca" [ Subword "n" [ Word "" , Word "teen"] , Word "ptain" , Subword "r" [ Word "" , Subword "r" [ Subword "ie" [ Word "d" , Word "s" ] , Word "y" ] ] ] , Subword "He" [ Word "ck" , Subword "l" [ Word "lo" , Subword "p" [ Word "" , Subword "e" [ Word "d" , Word "r" ] , Word "ing" ]]]]
c = Root [ Subword "F" [ Word "alse" , Subword "i" [ Word "asco" , Word "le" ] ] , Subword "Re" [ Word "aper" , Subword "po" [ Word "" , Word "sitory" ] ] , Subword "T" [ Subword "a" [ Subword "il" [ Word "" , Word "or" ] , Word "p" ] , Word "esla" ] ]

tree1 = addWords emptyTree []

tree2 = addWords emptyTree ["Hello"]

tree3 = addWords tree2 ["World", "Help"]

tree4 = addWords tree2 ["Hi"]

tree5 = addWords emptyTree ["Helper","Help","Helped","Helping","Hello","Heck"]

tree6 = addWords emptyTree ["Helper", "Help", "Helped", "Helping", "Hello", "Heck", "Car", "Carry", "Can" , "Carries", "Carried", "Captain", "Canteen"]

tree7 = addWords emptyTree ["File", "Repository","Repo","Reaper","False","Fiasco","Tail","Tailor","Tap","Tesla"]

tree8 = addWords tree2 ["Helper","Help","Helped","Helping","Hello","Heck"]

tree9 = addWords tree4 ["Helper","Help","Helped","Hello","Helping","Heck","Car","Carry","Can","Carries","Carried","Captain","Canteen"]

tree10 = addWords tree6 ["File","Repository","Repo", "Reaper","False","Fiasco","Tail","Tailor","Tap","Tesla"]

tree11 = addWords emptyTree ["Car","Carpenter"]

tree12 = addWords emptyTree ["Can","Car","Canopy"]

