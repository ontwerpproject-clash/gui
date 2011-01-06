module LongestPath where

import Data.Graph.Inductive
import Data.Graph.Inductive.Graph
import ParseClash as AE

lp1 aElem = do
	let conversionList = (toList aElem)
	let start=head  conversionList
	let (a,b,c) = start
	lp aElem a (a++"_funcOut")
	--conversionList
--		where
--			(a,b,c) = start
		
----------------------------------------- MAIN FUNCTION ------------------------------------------
--call is not that efficient, couple of things are computed multiple times, change to do and everything that is computed multiple times in a let
lp aElem begin end= do
	let conversionList = (toList aElem)
	let searchGraph = (makeEdges (makeVertices conversionList) [aElem] conversionList)
	let lpath= longest (calcPathLengths searchGraph (remEmptyPaths (pathList searchGraph (idToNodeNr searchGraph begin) (idToNodeNr searchGraph end))))
	idsToRealIDs (pathNrsToIds searchGraph lpath) conversionList


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
toList1 ((Operator id name insf out a):fs)  				l	= (id,id, fromOutToString out++map convertToPortId insf):toList1 fs l  {-map convertToPortId insf war insf-}
toList1 ((Literal id val out a):fs)         				l	= (id,id, fromOutToString out):toList1 fs l
toList1 ((Mux id insf out inpf a):fs)      					l = (id,id, fromOutToString out++map convertToPortId insf):toList1 fs l {-map convertToPortId insf war insf-}
toList1 ((Register id insf out a):fs)       				l = (id,id, fromOutToString out++[(convertToPortId(head (fMay insf)))]):toList1 fs l  {-[(convertToPortId(head (fMay insf)))] war fMay insf    pretty dirty hack wegen fmay liste zurück etc.-}
toList1 []                                  				l = []




toList2 func = toList12 [func] []
toList12 ((Function id name ins out (fs, ws) a):es)   l = (id, map convertToPortId ins):(id, fromOutToString out):toList12 fs l  {-++("_out", fromOutToString out)-}
toList12 ((Operator id name insf out a):fs)  			  	l	= (id, fromOutToString out++map convertToPortId insf):toList12 fs l  {-map convertToPortId insf war insf-}
toList12 ((Literal id val out a):fs)         			  	l	= (id, fromOutToString out):toList12 fs l
toList12 ((Mux id insf out inpf a):fs)      					l = (id, fromOutToString out++map convertToPortId insf):toList12 fs l {-map convertToPortId insf war insf-}
toList12 ((Register id insf out a):fs)       			  	l = (id, fromOutToString out++[(convertToPortId(head (fMay insf)))]):toList12 fs l  {-[(convertToPortId(head (fMay insf)))] war fMay insf    pretty dirty hack wegen fmay liste zurück etc.-}
toList12 []                                  		  		l = []

------------------------------------------ PATH FINDING ------------------------------------------

pathList gr current end = pathHelper gr current end []

forEvery [] 	gr current end visited = if(current==end) then [visited] else [[]]
forEvery (s:ss) gr current end visited = do
	let ret=pathHelper gr s end visited
	if(ss==[])then ret else ret++forEvery ss gr current end visited


pathHelper gr current end visited =
	if (current == end) then [visited++[current]] else if ( current `elem` visited) then [[]] else 
		do
			let ss=suc gr current  
			forEvery ss gr current end (visited++[current])


----------------------------------------- HELP FUNCTIONS -----------------------------------------
--removes possible empty paths out of a list of paths
remEmptyPaths []        = []
remEmptyPaths (l:lists) = if (length l > 0) then l:(remEmptyPaths lists) else remEmptyPaths lists



--Calculate the lengt of all paths in the list (2nd parameter) and returns [(path length, path)]
calcPathLengths gr []     = []
calcPathLengths gr (p:ps) = ((calcLength gr p),p):calcPathLengths gr ps


--Helper function for calcPathLengths
calcLength gr [] =0
calcLength gr (p:[]) = 0{-?-}
calcLength gr (p:ps) = (calcLengthHelper gr p (head ps))+(calcLength gr ps)

--Helper function for calcPathLengths
calcLengthHelper gr p ps = edgeWeigth(labEdges gr) p ps



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
findElementOfPort portId (l:ls) = if (portId `elem` elems) then (i++(findElementOfPort portId ls)) else findElementOfPort portId ls
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
