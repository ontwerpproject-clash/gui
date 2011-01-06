module WireFuncs where
  import Graphics.Rendering.OpenGL
  import Graphics.UI.GLUT
  import ParseClash
  import LayoutManager
  import Debug.Trace

  type CoordD = (Double, Double)
  type Route  = [CoordD]
  --type FromTo = (Offset, Offset)
  --type Offset = (Int, Int)

  {- Main function to call when drawing all wires. When the Layoutmanager is finished adding important information in the Wires/Elements,
   - This function takes a list of annotated wires & Elements and draws all relevant wires. -}
  drawWires :: [Route] -> IO ()
  drawWires wires =
       if (wires /= [])
          then do drawWire (head wires)
                  drawWires (tail wires)
          else return ()

  {- Function drawing one wire instance. First adjusts the Offset (Int, Int) to proper (Double, Double) values to set start/end points, then
   - does, in order: 1. Draw the Wire Start
                     2. Reduce X distance between start & End
                     3. Reduce Y distance between start & End
                     4. Check whether exit is near, if not, evade obstacle and repeat from #2
                     5. draw the Wire Ending 
  -}
  drawWire :: Route -> IO ()
  drawWire route = 
        do  --color $ Color3 0 0 (1::GLfloat)
            displayPoints points LineStrip     
             where 
              points = [(toFloats r) | r <- route]

  drawHelper wires red = drawWires2 wires red 1
--same as drawWire except for the different color  
  drawWires2 :: [Route] -> [Int] -> Int -> IO ()
  drawWires2 wires red curr= do
       if ( containsInt red curr) then color $ Color3 1 0 (0::GLfloat) else color $ Color3 0 0 (1::GLfloat)
       if (wires /= [])
          then do drawWire2 (head wires) 
                  drawWires2 (tail wires) red (curr+1)
          else return ()


  drawWire2 :: Route -> IO ()
  drawWire2 route = 
        do  --color $ Color3 1 0 (0::GLfloat)
            displayPoints points LineStrip     
             where 
              points = [(toFloats r) | r <- route]





  {-Function converting a list of Wires into a list of Routes. -}
  calcRoutes :: [Wire FromTo] -> [Route] 
  calcRoutes wires 
      | wires /= [] = (calcRoute $ head wires) : (calcRoutes $ tail wires)
      | otherwise   = []

  calcRoute :: Wire FromTo -> Route
  calcRoute wire = 
       wireFinish end $ reduceYDist end $ reduceXDist end $ toCrossing end $ wireStart begin
          where 
              Wire _ _ _ (begin, end) = wire

  -- Function for simplifying Wires, merging points where necessary
  simplifyWires :: [Route] -> [Route]
  simplifyWires routes 
    | routes == []  = routes
    | otherwise     = (simplifyWire $ head routes) : (simplifyWires $ tail routes)

  simplifyWire :: Route -> Route
  simplifyWire route@(p1:p2:route')
    | route' == []              = route
    | (x1==x2 && x2 == x3) || 
      (y1==y2 && y2 == y3)      = p1 : (simplifyWire route')
    | otherwise                 = p1 : (simplifyWire (p2:route'))
      where
        (x1,y1) = p1
        (x2,y2) = p2
        (x3,y3) = head route'


  -- Functions for detecting and resolving of wire collisions
  --
  -- 1. Find all colliding points
  -- 2. (if they exist) make tuples of the form: (point, next point)
  -- 3. see where the wires go to and based on that decide the offset
  
  makeArrows :: [Route] -> [Route]
  makeArrows routes = map makeArrow routes
                      
  makeArrow :: Route -> Route
  makeArrow route = newRoute
                  where
                    (finishX,finishY) = head route
                    newRoute          = (finishX-0.05,finishY-0.05):(finishX,finishY):(finishX-0.05,finishY+0.05):route

  resolveCollisions :: [Route] -> [Route]
  resolveCollisions routes 
    | routes /= []  = (resolveWireSet (head routes) (tail routes)) : (resolveCollisions (tail routes))
    | otherwise     = []
  
  --Function changing the coordinates of one wire until no conflicts are detected, support function of resolveCollisions
  resolveWireSet :: Route -> [Route] -> Route
  resolveWireSet examined otherRoutes
    | otherRoutes == []   = examined
    | otherwise           = resolveWireSet (resolveSingleWires examined $ head otherRoutes) $ tail otherRoutes
  
  --Function Checking between single wires, support function of resolveWireSet. Recursively keeps calling resolveWirePart until the entire
  --Wire has been resolved, yielding a new wire, which does not conflict with the 2nd argument, the other wire, on any part of its path.
  resolveSingleWires :: Route -> Route -> Route
  resolveSingleWires (p1:p2:examined') other
    | examined' == []    = [resolvedp1, resolvedp2]
    | otherwise          = resolvedp1: (resolveSingleWires (resolvedp2:examined') other)
        where
          (resolvedp1, resolvedp2) = (resolveWirePart (p1,p2) other)
 
  -- Function checking a part of one wire against another wire, support function of resolveSingleWires.
  resolveWirePart :: (CoordD, CoordD) -> Route -> (CoordD, CoordD)
  resolveWirePart (p1, p2) (p3:other')
    | other' == []                    = (p1,p2)
    | (x == x2 && x2 == x3 && x3 == x4) && ((x <= x3 && x3 <= x2) ||
                                            (x >= x3 && x3 >= x2) ||
                                            (x <= x4 && x4 <= x2) ||
                                            (x >= x4 && x4 >= x2)   ) = trace ("x conflict detected, changing... " ++ show p1 ++ " and " ++ show p2) (((x+0.05),y),((x2+0.05),y2))  
    | (y == y2 && y2 == y3 && y3 == y4) && ((y <= y3 && y3 <= y2) ||
                                            (y >= y3 && y3 >= y2) ||
                                            (y <= y4 && y4 <= y2) ||
                                            (y >= y4 && y4 >= y2)   ) = trace ("y conflict detected, changing... " ++ show p1 ++ " and " ++ show p2) ((x,(y+0.05)),(x2,(y2+0.05)))
    | otherwise                       = resolveWirePart (p1,p2) other'
      where
        (x,y) = p1
        (x2,y2) = p2
        (x3,y3) = p3
        (x4,y4) = head other'  


  toFloats :: CoordD -> (GLfloat, GLfloat, GLfloat)
  toFloats (x, y) = ((realToFrac x)::GLfloat, (realToFrac y)::GLfloat, 0.0::GLfloat)
            
  wireStart :: CoordD -> Route
  wireStart start = [dest, start]
                       where
                        (xs,ys) = start
                        dest    = (fromIntegral (ceiling xs),ys)

  toCrossing :: CoordD -> Route -> Route
  toCrossing (finishX, finishY) route@((lastX,lastY):cs) = 
             if lastY < finishY
                then (lastX, fromIntegral (ceiling lastY)):route
                else (lastX, fromIntegral (floor lastY)):route

  reduceXDist :: CoordD -> Route -> [CoordD]
  reduceXDist (finishX, _) route@((lastP):cs) = let newP = (fromIntegral (floor finishX), snd lastP)
                                                in case (newP == lastP) of
                                                    True  -> route
                                                    False -> newP:route

  reduceYDist :: CoordD -> [CoordD] -> [CoordD]
  reduceYDist (_, finishY) route@((lastP):cs) = let newP = (fst lastP, fromIntegral (ceiling finishY))
                                                in case (newP == lastP) of
                                                    True  -> route
                                                    False -> newP:route

  wireFinish ::  CoordD -> [CoordD] -> [CoordD]
  wireFinish (finishX,finishY) route@((lastX,lastY):cs) =
             (finishX,finishY):(lastX,finishY):route

  {- Preparation and support funcs -}

  {- Checks the Start- and End- elements & changes the offsets to proper ones; stub implementation sets it to 0.5, 0.5 (middle of element block)
   - a and b are used in conjunction with the elemList to determine what elements we're dealing with as starting/ending point and which inner
   - offset should be used as a result to let a wire end at the boundary of an element. -}
{-
  setInnerOffset :: Wire FromTo -> Wire (CoordD, CoordD)
  setInnerOffset (Wire name src dest ((xs,ys),(xf,yf))) = Wire name src dest ((( xs)+0.7,
                                                                                        -(( ys)+0.5)),
                                                                                       (( xf)+0.3,
                                                                                        -(( yf)+0.5)))
  -}                                         
  displayPoints :: [(GLfloat,GLfloat,GLfloat)] -> PrimitiveMode -> IO ()
  displayPoints points primitiveShape = do
    renderAs primitiveShape points
    flush

  renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
  renderAs figure ps = renderPrimitive figure $ makeVertexes ps

  makeVertexes :: [(GLfloat, GLfloat,GLfloat)] -> IO ()
  makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)
  
  
  
  
  
  
  makeWires [] start           = []
  makeWires (element:es) start = (Wire (Just "") start element 1):(makeWires es element)

  makeWiresH e = makeWires (tail e) (head e)
	{-
	removeWires :: [Wire FromTo]-> [Wire FromTo] -> [(String, String, [String])]->[Wire FromTo]->[Wire FromTo ]
	removeWires []      path list wires = wires
	removeWires (a:all) path list wires = if(containsWire path a list ) then (removeWires all path list (a:wires)) else (removeWires all path list wires) --a--(containsWire path a list )--if(containsWire path a list ) then a:(removeWires all path list ) else removeWires all path list 
	-}
	--removeWires :: [Wire FromTo]-> [Wire FromTo] -> [(String, String, [String])]->[Wire FromTo]->[Wire FromTo ]
  removeWires []      path list  = []
  removeWires (a:all) path list  = if(containsWire path a list ) then a:(removeWires all path list) else (removeWires all path list ) --a--(containsWire path a list )--if(containsWire path a list ) then a:(removeWires all path list ) else removeWires all path list 

  containsInt []       value = False
  containsInt (l:list) value = if (l==value) then True else containsInt list value

  containsWire []        wire list = False
  containsWire (w:wires) wire list = if (equalsWire w wire list ) then True else containsWire wires wire list 

  equalsWire (Wire _ w1in w1out _) (Wire _ w2in w2out _) list = if ((portToElementId w2in list) == w1in && (portToElementId w2out list)==w1out) then True else False

  containsWire2 []        wire = False
  containsWire2 (w:wires) wire = if (equalsWire2 w wire ) then True else containsWire2 wires wire 

  equalsWire2 (Wire _ w1in w1out _) (Wire _ w2in w2out _) = if (w2in == w1in && w2out==w1out) then True else False

  portToElementId wirePort [] = []
  portToElementId wirePort ((id,ports):elementList) = if (contains wirePort ports) then id else  portToElementId wirePort elementList--((id,id2,ports):elementList)

  contains port []       = False
  contains port (l:list) = if ( port == l) then True else contains port list

  remPath []        path = []
  remPath (w:wires) path = if (containsWire2 path w) then remPath wires path else w:(remPath wires path)

  pathToPositions wires wiresToPaint = pathToPositionsHelper wires wiresToPaint 1 
  
  pathToPositionsHelper [] wiresToPaint currentElement = []
  pathToPositionsHelper (w:wires) wiresToPaint currentElement = if (containsWire2 wiresToPaint w) then currentElement:(pathToPositionsHelper wires wiresToPaint (currentElement+1)) else pathToPositionsHelper wires wiresToPaint (currentElement+1)
	
