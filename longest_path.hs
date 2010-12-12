module Main where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph


 
data ArchElem a =
    Function
        Id
        (Maybe Name)            -- ^ Optional name of the function
        [InPort]                -- ^ List of the inports
        OutPort                 -- ^ Outport of the function
        ([ArchElem a],[Wire a]) -- ^ Functions have deeper architecture. These are specified by a tuple of a list of architectures and a list of wires, which connect the different architectures.
        a |

    Operator
        Id
        OpType
        [In]
        Out
        a |

    Literal
        Id
        Value
        Out
        a |

    Mux Id
       [In] -- ^ List of inports from which the outport is selected
        Out -- ^ The outport
       [In]  -- ^ Inport signal to choise an from the list inports
        a |

    Register
        Id
        (Maybe In)
        Out
        a |

    PortReference
        Port

    deriving (Show,Eq,Ord)

data Wire a =
    Wire
        (Maybe Name)  -- ^ Optional name of the wire
        PortId        -- ^ Source port
        PortId        -- ^ Destination port
        a
    deriving (Show,Eq,Ord)

type Name = String
type Value = String
type OpType = String

type PortId = String
type Id = String

type In = PortId
type Out = Port

type InPort = Port
type OutPort = Port

data Port =
    SinglePort
        PortId |

    MultiPort
        PortId
        [Port]

    deriving (Show,Eq,Ord)

macc = Function 
  "accComponent_0" 
  Nothing 
  [(SinglePort "in")]
  (SinglePort "out")
  (
    [
      Operator "operatorId0" "+" ["newPortId0", "newPortId1"] (SinglePort "OpOut") ()
      ,Register "reg1" (Just "regIn") (SinglePort "opOut") ()
    ]
    ,[
       Wire (Just "wire1") "in" "newPortId0" ()
      --,Wire (Just "wire2") "regOut" "newPortId1" ()--there is no regOut
      ,Wire (Just "wire3") "opOut" "regIn" ()
      ,Wire (Just "wire4") "opOut" "out" ()
    ]
  ) ()


maccCycle = Function 
  "accComponent_0" 
  Nothing 
  [(SinglePort "in")]
  (SinglePort "out")
  (
    [
      Operator "operatorId0" "+" ["newPortId0", "newPortId1"] (SinglePort "OpOut") ()
      ,Register "reg1" (Just "regIn") (SinglePort "opOut") ()
	  ,Register "reg2" (Just "regIn2") (SinglePort "opOut2") ()
    ]
    ,[
       Wire (Just "wire1") "in" "newPortId0" ()
      --,Wire (Just "wire2") "regOut" "newPortId1" ()--there is no regOut
      ,Wire (Just "wire3") "OpOut" "regIn" ()
	  ,Wire (Just "wire3") "OpOut" "out" ()
	  ,Wire (Just "wire4") "opOut" "newPortId0" ()
      ,Wire (Just "wire5") "opOut" "regIn2" ()
	  ,Wire (Just "wire6") "regIn2" "newPortId1" ()
	  ,Wire (Just "wire6") "regIn2" "out" ()
    ]
  ) ()

{-
    o-->o------>o-->o
    ^			^
    |			|
o-->o<------	|    <---
	|			|		|
	v			|		|
	o			|		|
	|	--------|		|
	v	|		|		|
	o   o------>o		|
	|	^		^		|
	v	|		|		|
	o-->o-->o-->o-------|


-}  
  
test = Function 
  "test_0" 
  Nothing 
  [(SinglePort "in")]
  (SinglePort "out")
  (
    [
      Operator "operatorId0" "+" ["newPortId0"] (MultiPort "multi1" [SinglePort "multi1_1", SinglePort "multi1_2"] ) ()
      ,Operator "operatorId1" "+" ["newPortId1"] (SinglePort "o1Out") ()
	  ,Operator "operatorId2" "+" ["newPortId2"] (SinglePort "o2Out") ()
	  ,Operator "operatorId3" "+" ["newPortId3", "newPortId4", "newPortId5"] (SinglePort "o3Out") ()
	  ,Operator "operatorId4" "+" ["newPortId6"] (SinglePort "o6Out") ()
	  ,Operator "operatorId5" "+" ["newPortId7"] (SinglePort "o7Out") ()
	  ,Operator "operatorId6" "+" ["newPortId8"] (SinglePort "o8Out") ()
	  ,Operator "operatorId7" "+" ["newPortId9"] (MultiPort "multi2" [SinglePort "multi2_1", SinglePort "multi2_2"] ) ()
	  ,Operator "operatorId8" "+" ["newPortId10"] (SinglePort "o10Out") ()
	  ,Operator "operatorId9" "+" ["newPortId11"] (SinglePort "o11Out") ()
	  ,Operator "operatorId10" "+" ["newPortId12", "newPortId13"] (SinglePort "o12Out") ()
	  ,Operator "operatorId11" "+" ["newPortId14"] (MultiPort "multi3" [SinglePort "multi3_1", SinglePort "multi3_2"] )()
    ]
    ,[
		 Wire (Just "wire1") "in" "newPortId0" ()
		,Wire (Just "wire2") "o3Out" "out" ()
		,Wire (Just "wire3") "multi1_1" "newPortId1" ()
		,Wire (Just "wire4") "multi1_2" "newPortId6" ()
		,Wire (Just "wire5") "o1Out" "newPortId2" ()
		,Wire (Just "wire6") "o2Out" "newPortId3" ()
		,Wire (Just "wire7") "o6Out" "newPortId7" ()
		,Wire (Just "wire8") "o7Out" "newPortId8" ()
		,Wire (Just "wire9") "o8Out" "newPortId9" ()
		,Wire (Just "wire10") "multi2_1" "newPortId10" ()
		,Wire (Just "wire11") "o10Out" "newPortId11" ()
		,Wire (Just "wire12") "o11Out" "newPortId13" ()
		,Wire (Just "wire13") "multi2_2" "newPortId14" ()
		,Wire (Just "wire14") "multi3_1" "newPortId4" ()
		,Wire (Just "wire15") "multi3_2" "newPortId12" ()
		,Wire (Just "wire16") "o12Out" "newPortId5" ()
		,Wire (Just "wire17") "o11Out" "newPortId0" ()
    ]
  ) ()

  
{-



call the function lp with parameter aElem ID_begin ID_end     and get [IDs]

see example test:
lp test "operatorId0" "operatorId3"
returns:
["operatorId0","operatorId4","operatorId5","operatorId6","operatorId7","operator
Id8","operatorId9","operatorId10","operatorId3"]




UPDATE to check:
no longer sure if the out_id thing is needed... probably not
-} 
  
  
  
  
  
 



main :: IO ()
main = do
	--test of parts 
	print "test"
{-
	let li=toList maccCycle
	print (makeVertices li)
	let gV=makeVertices li

	let gC=(makeEdges gV [maccCycle] li)
	print gC


	print "4 3"
	print (pathList gC 4 3 )
	print "3 5"
	print (pathList gC 3 5 )

	print (remEmptyPaths (pathList gC 4 3))
	print (remEmptyPaths (pathList gC 3 5))
-}	
	
	
	
	--let comTest=makeEdges (makeVertices (toList test)) [test] (toList test) 
	print "TEST"
	print "   conversion list"
	print (toList test)
	--print (makeVertices (toList test))
	let comVerti=(makeVertices (toList test))
	print "   nodes"
	print comVerti
	let comComplete= makeEdges comVerti [test] (toList test) 
	print "GRAPH"
	print comComplete
	let pa=(remEmptyPaths (pathList comComplete 3 13	))
	print "All paths from operatorId0 to operatorId10"
	print pa

	let lpa=calcPathLengths comComplete(remEmptyPaths pa)
	print "tuples of (pathLength, path)"
	print lpa
	
	print "longest path (using nodeIds)"
	print (longest lpa)
	

	--test of all in one function
	print "ALL IN ONE    from operatorId0 to operatorId3"
	print (lp test "operatorId0" "operatorId3")--3 6

	
	return ()

----------------------------------------- MAIN FUNCTION ------------------------------------------
--call is not that efficient, couple of things are computed multiple times, change to do and everything that is computed multiple times in a let
lp aElem begin end= do
	let conversionList = (toList aElem)
	let searchGraph = (makeEdges (makeVertices conversionList) [aElem] conversionList)
	let lpath= longest (calcPathLengths searchGraph (remEmptyPaths (pathList searchGraph (idToNodeNr searchGraph begin) (idToNodeNr searchGraph end))))
	idsToRealIDs (pathNrsToIds searchGraph lpath) conversionList
--noch das oben in longestpath ding werfen, weil das nur alle paths returnt

----------------------------------------- GRAPH CREATION -----------------------------------------
makeEdges gV [macc] li = makeEdges1 gV [macc] li


--generate all edges in function id, by using the wires and add them to graph gr
makeEdges1 gr  ((Function id name ins out (fs, ws) a):es)  l = addWires gr ws l

--use wires to add an edge to the graph gr, by callind aEdge for all wires
addWires gr [] l = gr
addWires gr (w:ws) l = addWires (aEdge gr w l) ws l

--add an edge to a graph by usind a wire
aEdge gr (Wire o start end a) l = checkEdgesAndAdd gr (findElementOfPort start l) (findElementOfPort end l)


--takes a list and makes nodes from all the elements in that list
makeVertices :: [(String, String, [String])] -> Gr String Int
makeVertices li = makeVertices1 li empty
--helper for makeVertices
makeVertices1 :: [(String, String, [String])]-> Gr String Int -> Gr String Int
makeVertices1 [] gr = gr
makeVertices1 (l:ls) gr = makeVertices1 ls (run_ gr $ insMapNodeM eleID)
								where (eleID,realID, ports) = l


--converts a archelem to a list of the form (id_elem, real_id_elem, [ports])
--id_function is used to be able to split a function into an in- and outpart, to not have a cycle due to all functions starting and ending with the function itself
toList func = toList1 [func] []
toList1 ((Function id name ins out (fs, ws) a):es)  l = (id,id, map convertToPortId ins):(id++"_funcOut",id, fromOutToString out):toList1 fs l  {-++("_out", fromOutToString out)-}
toList1 ((Operator id name insf out a):fs)  				l	= (id,id, fromOutToString out++insf):toList1 fs l
toList1 ((Literal id val out a):fs)         				l	= (id,id, fromOutToString out):toList1 fs l
toList1 ((Mux id insf out inpf a):fs)      					l = (id,id, fromOutToString out++insf):toList1 fs l
toList1 ((Register id insf out a):fs)       				l = (id,id, fromOutToString out++(fMay insf)):toList1 fs l
toList1 []                                  				l = []


------------------------------------------ PATH FINDING ------------------------------------------

pathList gr current end = pathHelper gr current end []--[current] --direct call of remEmptyPaths here?????

forEvery [] 	gr current end visited = if(current==end) then [visited] else [[]]
forEvery (s:ss) gr current end visited = do
	let ret=pathHelper gr s end visited--current here useless
	if(ss==[])then ret else ret++forEvery ss gr current end visited

--pathHelper gr current end []
pathHelper gr current end visited = --do
	if (current == end) then [visited++[current]] else if ( current `elem` visited) then [[]] else 
		do
			let ss=suc gr current  --lsuc = Node + (linkLabe==weight)
			forEvery ss gr current end (visited++[current])


----------------------------------------- HELP FUNCTIONS -----------------------------------------
--removes possible empty paths out of a list of paths
remEmptyPaths []        = []
remEmptyPaths (l:lists) = if (length l > 0) then l:(remEmptyPaths lists) else remEmptyPaths lists



--Calculate the lengt of all paths in the list (2nd parameter) and returns [(path length, path)]
calcPathLengths gr []     = []
--calcPathLengths gr (p:ps) = [((length p),p)]:calcPathLengths gr ps{-[((calcLength gr p),p)]-}{-:calcPathLengths gr ps-}
calcPathLengths gr (p:ps) = ((calcLength gr p),p):calcPathLengths gr ps{-[((calcLength gr p),p)]-}{-:calcPathLengths gr ps-}


--Helper function for calcPathLengths
calcLength gr [] =0
calcLength gr (p:[]) = 0{-?-}
calcLength gr (p:ps) = (calcLengthHelper gr p (head ps))+(calcLength gr ps)

--Helper function for calcPathLengths
calcLengthHelper gr p ps = edgeWeigth(labEdges gr) p ps --labEdges gr --besser labEdges als param von top statt immer zu berechnen heir



--returns the weight of an edge, 0 if the list is empty of the edge doesn't exist
--parameter list of edges, begin of edge, end of edge
edgeWeigth:: [(Int, Int, Int)] -> Int -> Int -> Int
edgeWeigth [] s     e = 0
edgeWeigth (n:ns) s e = if ( no1== s && no2==e) then w else edgeWeigth ns s e
						where 
							(no1,no2,w) = n	
							

--returns last path with max length (if multiple with same length, longest otherwise)
longest l = longestPath l [] 0							
--longestPath (l:ls) = longestPathHelper l 0 
longestPath []     p plen= p--or something else if not existing node?
longestPath (l:ls) p plen= if (len>=plen) then longestPath ls pa len else longestPath ls p plen
			where
				(len, pa) = l

---------IDs to Node Nr and vice versa----------------

--returns the Nr of the node with Label id in the graph
idToNodeNr gr id = retNR (labNodes gr) id
--helper function for idToNodeNr
retNR []     id = -1--or something else if not existing node?
retNR (n:ns) id = if (id==name) then nr else retNR ns id
			where
				(nr, name) = n

--converts a list of Node numbers to their Labels				
pathNrsToIds gr []	   = []				
pathNrsToIds gr (n:ns) = (nodeNrToId gr n)++pathNrsToIds gr ns
nodeNrToId gr nr = fMay (lab gr nr)

--list of Labels to realsIDs (necessary for functions)
idsToRealIDs [] list  =[]
idsToRealIDs (i:ids) list = (idToRealID i list):idsToRealIDs ids list

--helper for idsToRealIDs (one label to one realid)
idToRealID id []     ="completely unreachable"
idToRealID id (l:ls) = if (id==idlist) then realid else idToRealID id ls
					where
						(idlist,realid, ports)=l



--returns the element corresponding to a specific port
--takes portId and list of (elementId, [ports])
findElementOfPort:: String-> [(String,String,[String])]-> String
findElementOfPort portId [] = []
findElementOfPort portId (l:ls) = if (portId `elem` elems) then (i++(findElementOfPort portId ls)) else findElementOfPort portId ls--if (contains portId elems) then (i++(findElementOfPort portId ls)) else findElementOfPort portId ls
										where (i,i2, elems)=l





fMay Nothing = []
fMay (Just a) = [a]

fromMaybe              :: a -> Maybe a -> a
fromMaybe d Nothing    =  d
fromMaybe d (Just a)   =  a


--taken from LayoutManager.hs
convertToPortId :: InPort -> PortId
convertToPortId p = case p of
    SinglePort id  -> id
    MultiPort id _ -> id

--outport to string
fromOutToString (MultiPort d [])     = [d]
fromOutToString (MultiPort d ((SinglePort o):os)) = o:fromOutToString (MultiPort d os)
fromOutToString (SinglePort d)       = [d]	



	
--true if a specifig node is present in the list of nodes (ns)
containsNode [] s     = False
containsNode (n:ns) s = if ( name==s ) then True else containsNode ns s
						where 
							(no, name) = n

--true if a specifig edge is present in the list of edges (ns)
containsEdge:: [(String, String, a)] -> String -> String -> Bool
containsEdge [] s     e = False
containsEdge (n:ns) s e = if ( no1== s && no2==e) then True else containsEdge ns s e
						where 
							(no1,no2,w) = n	




--tackes a list of edges and a graph an returns a list of the labels of the nodes (nodes are based on ints and this returns the labels corresponding with the ints)
edgeNumToLab gr []     = []
edgeNumToLab gr (e:es) = (head(fMay (lab gr e1)),head(fMay (lab gr e2)),v):edgeNumToLab gr es--the head, list thing due to fMay is somewhat ugly
						where 
							(e1,e2,v) = e





	
	--check if edge already present, so no double paths from a to b, might be overkill could be replaced with:
	{-
checkEdgesAndAdd1 :: Gr String Int -> String -> String -> Gr String Int
checkEdgesAndAdd1 gr b e = run_ gr $ insMapEdgeM (b, e, 1)
	-}
	--probably easier to convert the label of the nodes to the node int and get rid of the uglye edgenumtolables
checkEdgesAndAdd :: Gr String Int -> String -> String -> Gr String Int
checkEdgesAndAdd gr b e = do
		let edges=labEdges gr
		
		if (containsEdge (edgeNumToLab gr edges) b e )--probably easier to convert the label of the nodes to the node int and get rid of the uglye edgenumtolables
			then 
				gr 
			else 
				run_ gr $ insMapEdgeM (b, e, 1)--weight



--adds a node to the graph, if it isn't already present				
checkMakeAndAdd gr i = do
		let nodes=labNodes gr
		if (containsNode nodes i)
			then 
				gr 
			else 
				run_ gr $ insMapNodeM i