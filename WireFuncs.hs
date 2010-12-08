module WireFuncs where
  import Graphics.Rendering.OpenGL
  import Graphics.UI.GLUT
  import ParseClash
  import LayoutManager

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
        do  color $ Color3 0 0 (1::GLfloat)
            displayPoints points LineStrip     
             where 
              points = [(toFloats r) | r <- route]

  calcRoutes :: [Wire FromTo] -> [Route] 
  calcRoutes wires 
      | wires /= [] = (calcRoute $ head wires) : (calcRoutes $ tail wires)
      | otherwise   = []

  calcRoute :: Wire FromTo -> Route
  calcRoute wire = 
       wireFinish end $ reduceYDist end $ reduceXDist end $ toCrossing end $ wireStart begin
          where 
              Wire _ _ _ (begin, end) = setInnerOffset wire


  -- Functions for detecting and resolving of wire collisions
  --
  -- 1. Find all colliding points
  -- 2. (if they exist) make tuples of the form: (point, next point)
  -- 3. see where the wires go to and based on that decide the offset
  resolveCollisions :: [Route] -> [Route]
  resolveCollisions 

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
  setInnerOffset :: Wire FromTo -> Wire (CoordD, CoordD)
  setInnerOffset (Wire name src dest ((xs,ys),(xf,yf))) = Wire name src dest (((fromIntegral xs)+0.5,
                                                                                        -((fromIntegral ys)+0.5)),
                                                                                       ((fromIntegral xf)+0.5,
                                                                                        -((fromIntegral yf)+0.5)))
                                           
  displayPoints :: [(GLfloat,GLfloat,GLfloat)] -> PrimitiveMode -> IO ()
  displayPoints points primitiveShape = do
    renderAs primitiveShape points
    flush

  renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
  renderAs figure ps = renderPrimitive figure $ makeVertexes ps

  makeVertexes :: [(GLfloat, GLfloat,GLfloat)] -> IO ()
  makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)