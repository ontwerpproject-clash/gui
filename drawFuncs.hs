--------------------------------------------------------------------------------------------------------------
--General module for drawing basic elements of the specified data structure. Note on defined variables:     --
--due to GL representation of variables, xyScale has to be set manually to the same value as xS/yS, since   --
--GLsizei is not automatically converted to GLfloat when needed.                                            --
--------------------------------------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
-- import ArchElements as AE
import LayoutManager as LM
import WireFuncs
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import LongestPath
import System

import ParseClash as AE

data ProgramState = ProgramState {sc :: Float,
                                  xPan,yPan :: Float,
                                  elementsSaved :: [ArchElem Offset], routesSaved :: [Route],
                                  xWS :: GLsizei,
                                  yWS :: GLsizei
                                  }

initPState = ProgramState {sc = (1/3),
                          xPan = (-1), yPan = 1,
                          elementsSaved = [], routesSaved = [],
                          xWS = 600, 
                          yWS = 600}
myparseClashFile :: FilePath -> IO (ArchElem ())
myparseClashFile "blaat" = myparseClashFile2 ""
myparseClashFile f = parseClashFile f

myparseClashFile2 _ = return $ 
    Function "cpuComponent_0" Nothing [MultiPort "addrszjQH2" [SinglePort "addrszjQH2.A",
                                                               SinglePort "addrszjQH2.B"]]
                                      (SinglePort "reszjR4zjR42")
    (
        [Operator "vlastoperatorId28" "vlast" [SinglePort "newPortId30"]
                                              (SinglePort "newPortId29") (()),
         Register "NSimple (Basic \"szjQJ2\")\"31\"" (Just (SinglePort "regIn\"32\"")) (SinglePort "regOut\"33\"") (()),
         Operator "singletonoperatorId1" "singleton" [SinglePort "newPortId3"]
                                                     (SinglePort "newPortId2") (()),
             Function "fuComponent_1" Nothing [SinglePort "paramzjS1zjS12",
                                               MultiPort "paramzjS3zjS32" [SinglePort "paramzjS3zjS32.A",
                                                                           SinglePort "paramzjS3zjS32.B"]]
                                              (SinglePort "casevalzjS9zjSr3")
             (
                 [Operator "resizeoperatorId4" "resize" [SinglePort "newPortId12",
                                                         SinglePort "newPortId13"]
                                                        (SinglePort "newPortId5") (()),
                  Operator "operatorId6" "*" [SinglePort "newPortId7",
                                              SinglePort "newPortId8"]
                                             (SinglePort "newPortId9") (()),
                  Literal "litoperatorId10" "16" (SinglePort "newPortId11") (()),
                  Operator "znoperatorId14" "zn" [SinglePort "newPortId16",
                                                  SinglePort "newPortId17"]
                                                 (SinglePort "newPortId15") (()),
                  Operator "znoperatorId18" "zn" [SinglePort "newPortId20",
                                                  SinglePort "newPortId21"]
                                                 (SinglePort "newPortId19") (())]
             ,
                 [Wire (Just "casevalzjS9zjSr3") "newPortId5" "casevalzjS9zjSr3" (()),
                  Wire (Just "a function call wire") "newPortId9" "newPortId12" (()),
                  Wire (Just "a function call wire") "newPortId11" "newPortId13" (()),
                  Wire (Just "argzjSJzjSJ2") "newPortId15" "newPortId7" (()),
                  Wire (Just "a function call wire") "paramzjS1zjS12" "newPortId16" (()),
                  Wire (Just "a1zjSt3") "paramzjS3zjS32.A" "newPortId17" (()),
                  Wire (Just "argzjSLzjSL2") "newPortId19" "newPortId8" (()),
                  Wire (Just "a function call wire") "paramzjS1zjS12" "newPortId20" (()),
                  Wire (Just "a2zjSs3") "paramzjS3zjS32.B" "newPortId21" (())]
             )
             (()),
         Operator "zpzgoperatorId22" "zpzg" [SinglePort "newPortId24",
                                             SinglePort "newPortId25"]
                                            (SinglePort "newPortId23") (()),
         Literal "litoperatorId26" "0" (SinglePort "newPortId27") (())]
    ,
        [Wire (Just "outzjQL2") "newPortId29" "reszjR4zjR42" (()),
         Wire (Just "szjQJ2") "regOut\"33\"" "newPortId30" (()),
         Wire (Just "szqzjQX2") "newPortId2" "regIn\"32\"" (()),
         Wire (Just "argzjRazjRa3") "casevalzjS9zjSr3" "newPortId3" (()),
         Wire (Just "inputszjQT2") "newPortId23" "paramzjS1zjS12" (()),
         Wire (Just "xzjQP2") "newPortId27" "newPortId24" (()),
         Wire (Just "szjQJ2") "regOut\"33\"" "newPortId25" (()),
         Wire (Just "argzjR8zjR82") "addrszjQH2" "paramzjS3zjS32" (())]
    )
    (())


main :: IO ()
main = do
    (f:_) <- getArgs 
    func <- myparseClashFile f
    let
      elem = offsetElements func
      routes = makeArrows collisionResolved
      collisionResolved = resolveCollisions $ simplifyWires $ calcRoutes wires
      wires = extractWires elem

      pState :: ProgramState
      pState = initPState{elementsSaved=[elem],routesSaved=routes}
      xyScale =  1
      convFromState val = realToFrac $ val pState

    {-
    let
    putStrLn "elems:"
    putStrLn $ show offsetElems
    putStrLn "wires:"
    putStrLn $ show wires
    putStrLn "routes:"
    putStrLn $ show routes
    putStrLn "simple:"
    putStrLn $ show simple
    -}
    getArgsAndInitialize
    createWindow "CLASH Visualisation Tool, pre-alpha"
    windowSize $= (Size (xWS pState) (yWS pState))
    scale (convFromState sc) (convFromState sc) (1::GLfloat)
    displayCallback $= (display pState elem)
--    keyboardMouseCallback $= (Just ( keyboard1 ))
    mainLoop
 
type Coord = (GLfloat, GLfloat)

----------------------------------------------------------------------------------------------------------------------------
-- Main Display function. Does the following: Clear the screen, set the color to white, calculate the grid, set the scale --
-- of the grid to proper levels, draw the elements and commit to the screen (flush).                                      --
----------------------------------------------------------------------------------------------------------------------------
display :: ProgramState -> ArchElem Offset -> IO ()
display pState func = do
    clear [ColorBuffer]
    color $ Color3 1 1 (1::GLfloat)
    drawElems elems pState
    color $ Color3 0 0 (1::GLfloat)
    drawStartCircles routes pState
    translatePannedDefault pState
    drawWires routes
    loadIdentity
    scale sc_ sc_ (1::GLfloat)
    color $ Color3 0 1 (0::GLfloat)
    renderPrimitive Points $ makeVertexes points
    keyboardMouseCallback $= (Just ( keyboard func pState))
    flush
        where
            points = [(x,y,0) | x <-[-10..10] , y<-[-10..10]]
            routes = routesSaved pState
            elems = elementsSaved pState
            sc_ = sc pState


keyboard func pState (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard func pState (Char '+') Down _ _   = do 
    print "zoom in"
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      sc_ = sc pState
      scNew = abs (1/((1/sc_)-1))
      newPState = pState{sc=scNew}    
    
keyboard func pState (Char '-') Down _ _   = do 
    print "zoom out"
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      sc_ = sc pState
      scNew = 1/((1/sc_)+1)
      newPState = pState{sc=scNew}
    
keyboard func pState (Char 'r') Down _ _   = do
    clear [ColorBuffer]
    color $ Color3 1 1 (1::GLfloat)
    --drawElems [offsetElements func] pState
    color $ Color3 0 0 (1::GLfloat)
    --drawWires $ makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ extractWires $ offsetElements func
    --color $ Color3 0 1 (0::GLfloat)
    renderPrimitive Points $ makeVertexes points
    --print (resolveCollisions $ simplifyWires $ calcRoutes $ extractWires $ offsetElements func)
    flush
        where
            points = [(x,y,0) | x <-[-10..10] , y<-[-10..10]]


keyboard func pState (Char 'l') Down _ _   = do 
	print "Printing Longest Path"
	--longestPath
	let path = (makeWiresH $ lp2 func){-lp1 here calculates longest path from begining of the function to end of the function, lp2 calculates longestPaht from somewhere in the function to its end-}
	--allWires
	let wires = extractWires $ offsetElements func
	--conversion step, because the result of longestpath, is a path based on the elementNames instead of the portIds (has to be changed)
	--result is the longesPath with portIds instead of ElementIds
	let wiresToPaint = removeWires wires path (toList2 func)
	--wires of the longestPath to their position in the wire list
	let wiresOfPath = (pathToPositions wires wiresToPaint)
	--calculateWires
	let completeWires = makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ wires
	--paint the wires (the wires on the positions in the list wiresOfPath are drawn in red)
	translatePannedDefault pState
	drawHelper completeWires wiresOfPath
	
keyboard func pState (Char 'p') Down _ _   = do 	
	--longestPathBackupVersion
	let path = (makeWiresH $ lp2 func){-lp1 here calculates longest path from begining of the function to end of the function, lp2 calculates longestPaht from somewhere in the function to its end-}
	--allWires
	let wires = extractWires $ offsetElements func

	let wiresToPaint = removeWires wires path (toList2 func)
	let wiresOfPath = (pathToPositions wires wiresToPaint)
	color $ Color3 1 0 (0::GLfloat)
	--draws all the wires of the longest Path in red (but just those, so it is probably not on top of all the others old wires)
	drawWires $ makeArrows $ resolveCollisions $ simplifyWires $ calcRoutes $ wiresToPaint

-- View panning with wasd
keyboard func pState (Char 'a') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      x = xPan pState
      x' = x-0.1
      newPState = pState{xPan=x'}
keyboard func pState (Char 'd') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      x = xPan pState
      x' = x+0.1
      newPState = pState{xPan=x'}
keyboard func pState (Char 'w') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      y = yPan pState
      y' = y+0.1
      newPState = pState{yPan=y'}
keyboard func pState (Char 's') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      y = yPan pState
      y' = y-0.1
      newPState = pState{yPan=y'}

-- Fast view panning with shift+wasd
keyboard func pState (Char 'A') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      x = xPan pState
      x' = x-0.3
      newPState = pState{xPan=x'}
keyboard func pState (Char 'D') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      x = xPan pState
      x' = x+0.3
      newPState = pState{xPan=x'}
keyboard func pState (Char 'W') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      y = yPan pState
      y' = y+0.3
      newPState = pState{yPan=y'}
keyboard func pState (Char 'S') Down _ _   = do 
    displayCallback $= display newPState func
    postRedisplay Nothing
    where
      y = yPan pState
      y' = y-0.3
      newPState = pState{yPan=y'}


keyboard func pState _ _ _ _               = return ()	
	



	
	
	
	
{-

keyboard1 (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard1 (Char '+') Down _ _   = do print "zoom in"
                                     let
                                        newPstate = initPState {sc=(1/9)}
                                     displayCallback $= display newPstate
                                     postRedisplay Nothing
    
keyboard1 (Char '-') Down _ _   = print "zoom out"
keyboard1 _ _ _ _               = return ()
-}
---------------------------------------------------------------------------------------------------
-- Functions directing the atomic draw operations given a list of elements with absolute offsets --
---------------------------------------------------------------------------------------------------

drawElems :: [ArchElem Offset] -> ProgramState -> IO ()
drawElems elems pState
    | elems == [] = return ()
    | otherwise   = do drawElem (head elems) pState
                       drawElems (tail elems) pState

drawElem :: ArchElem Offset -> ProgramState -> IO ()
drawElem (Function _ _ _ _ (innerElems , _ ) (x,y)) pState = do drawFunction innerElems (toOffset (x,y)) pState
                                                                drawElems innerElems pState
drawElem (Operator _ name _ _ (x,y))                pState = drawOperator name (toOffset (x,y)) pState
drawElem (Literal _ name _ (x,y))                   pState = drawLiteral name (toOffset (x,y)) pState
drawElem (Mux _ _ _ _ (x,y))                        pState = drawMux (toOffset (x,y)) pState
drawElem (Register _ _ _ (x,y))                     pState = drawRegister (toOffset (x,y)) pState

-----------------------------------------------------------------------------------------------------
-- Drawing Functions for all atomic elements (Function, Operator, Literal, Mux, Register and wires --
-----------------------------------------------------------------------------------------------------
--Needs: A unique identifier, possibly a label, a list of in ports, an out port, a list of elements (such as operators), a list of wires, a variable data entity (possibly empty) and Coordinates.
drawFunction :: [ArchElem Offset] -> Coord -> ProgramState -> IO ()
drawFunction innerElems (x,y) pState = do  loadIdentity
                                           scale sc_ sc_ (1::GLfloat)
                                           translatePannedDefault pState
                                           color $ Color3 1 1 (1::GLfloat)
                                           rect (Vertex2 (realToFrac x-0.05) ((realToFrac y+0.05)::GLfloat)) 
                                                (Vertex2 (x+x_+0.05) (y-y_-0.05))
                                           color $ Color3 0 1 (0::GLfloat)
                                           displayPoints [(x-0.05,y+0.05,(0::GLfloat)),
                                                          (x-0.05,y-y_-0.05,(0::GLfloat)),
                                                          (x+x_+0.05,y-y_-0.05,(0::GLfloat)),
                                                          (x+x_+0.05,y+0.05,(0::GLfloat))] LineLoop
                                           loadIdentity
                                           scale sc_ sc_ (1::GLfloat)
                                           where
                                             sc_     = realToFrac (sc pState)
                                             (xl,yl) = calcFuncSize innerElems (0,0)
                                             x_      = (realToFrac xl)::GLfloat
                                             y_      = (realToFrac yl)::GLfloat

drawOperator :: AE.Name -> Coord -> ProgramState -> IO ()
drawOperator name (x,y) pState = do  color $ Color3 1 0 (0::GLfloat)
                                     translatePanned pState $ Vector3 (x+0.5) (y-0.5) (0::GLfloat)
                                     fillCircle 0.3
                                     color $ Color3 0 0 (0::GLfloat)
                                     renderCircle 0.3
                                     loadIdentity
                                     let
                                        fact = realToFrac (length name)
                                        sc_ = realToFrac (sc pState)
                                        sca = realToFrac ((0.006*sc_)/fact)
                                     color $ Color3 0 0 (0::GLfloat)
                                     scale sca sca (1::GLfloat)
                                     translatePannedText pState sca (Vector3  ((x/sca*sc_)+(0.2/sca*sc_)+(0.03*fact/sca*sc_))
                                                         ((y/sca*sc_)-(0.85/sca*sc_)+(0.03*fact/sca*sc_)) 
                                                         (0::GLfloat))
                                     renderString Roman name
                                     loadIdentity
                                     scale sc_ sc_ (1::GLfloat)
                                            
drawLiteral :: AE.Name -> Coord -> ProgramState -> IO ()
drawLiteral name (x,y) pState = do  color $ Color3 0 1 (0::GLfloat)
                                    translatePannedDefault pState
                                    rect (Vertex2 (x+0.4) (y-0.2)) (Vertex2 (x+0.6) ((y-0.8)::GLfloat))
                                    loadIdentity
                                    let
                                        fact = realToFrac (length name)
                                        sc_ = realToFrac (sc pState)
                                        sca = realToFrac ((0.002*sc_)/fact)
                                    color $ Color3 0 0 (0::GLfloat)
                                    scale sca sca (1::GLfloat)
                                    translatePannedText pState sca (Vector3  ((x/sca*sc_)+(0.45/sca*sc_)+(0.01*fact/sca*sc_))
                                                        ((y/sca*sc_)-(0.75/sca*sc_)+(0.05*fact/sca*sc_))
                                                         (0::GLfloat))
                                    renderString Roman name
                                    loadIdentity
                                    scale sc_ sc_ (1::GLfloat)

drawMux :: Coord -> ProgramState -> IO ()
drawMux (x,y) pState = do color $ Color3 1 0 (0::GLfloat)
                          translatePannedDefault pState
                          rect (Vertex2 (x+0.2) (y-0.2)) (Vertex2 (x+0.8) ((y-0.8)::GLfloat))
                          color $ Color3 0 0 (0::GLfloat)
                          displayPoints [(x+0.1, y-0.5, 0), (x+0.5, y-0.5,0)] LineStrip

drawRegister :: Coord -> ProgramState -> IO ()
drawRegister (x,y) pState = do color $ Color3 0 1 (1::GLfloat)
                               translatePannedDefault pState
                               rect (Vertex2 (x+0.3) (y-0.2)) (Vertex2 (x+0.7) ((y-0.8)::GLfloat))
                               color $ Color3 0 0 (0::GLfloat)
                               displayPoints  [ (x+0.3, y-0.7, 0),
                                                (x+0.4, y-0.6, 0),
                                                (x+0.3, y-0.5, 0)
                                              ]
                                              LineStrip

drawStartCircles :: [Route] -> ProgramState -> IO ()
drawStartCircles routes pstate = if routes /= []
                                   then do drawStartCircle (head routes) pstate
                                           drawStartCircles (tail routes) pstate
                                   else return ()

drawStartCircle :: Route -> ProgramState -> IO ()
drawStartCircle route pstate = do  color $ Color3 0 0 (1::GLfloat)
                                   translatePanned pstate $ Vector3 (realToFrac x) (realToFrac y) (0::GLfloat)
                                   fillCircle 0.02
                                   loadIdentity
                                   scale sc_ sc_ (1::GLfloat)
                                   where
                                     (x,y) = last route
                                     sc_ = realToFrac (sc pstate)                                        

------------------------------------------------------------------------------------------------------
-- Support function to calculate the total number of elements to be rendered- this function is used --
-- to make sure all functions are neatly rendered.                                                  --
------------------------------------------------------------------------------------------------------
--calculateFunctionSize :: [ArchElem a] -> ((Int,Int) , (Int,Int))
--calculateFunctionSize innerElems
--    | innerElems == [] = ((0,0), (0,0))
--    | otherwise        = cFS innerElems ((0,0), (0,0))
--    
--cFS :: [ArchElem (Int,Int)] -> ((Int, Int),(Int, Int)) -> ((Int, Int),(Int,Int))
--
--cFS [] a = a
--
--cFS innerElems@((Function _ _ _ _ (innerElems2 , _) _):innerElems') ((xl, yt),(xr,yb)) = 
--   cFS innerElems' (((min xl2 xl),(max yt2 yt)),
--                     ((max xr2 xr),(min yb2 yb)))
--    where 
--      ((xl2,yt2),(xr2,yb2)) = (calculateFunctionSize innerElems2)
--    
--cFS ((ArchElem (x,y)):innerElems) ((xl,yt),(xr,yb)) = 
--      cFS innerElems (((min x xl),(max y yt)),
--                      ((max x xr),(min y yb)))
--
--cFS _ a = a

-----------------------------------------------------
-- Support functions for drawing a circle in GLUT. --
-----------------------------------------------------

--displayPoints :: [(GLfloat,GLfloat,GLfloat)] -> PrimitiveMode -> IO ()
--displayPoints points primitiveShape = do
--  renderAs primitiveShape points
--  flush

--renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
--renderAs figure ps = renderPrimitive figure $ makeVertexes ps

circlePoints :: Floating a => a -> Int -> [(a,a,a)]
circlePoints radius number =
  [let alpha = 2 * pi * fromIntegral i / fromIntegral number
   in (radius * sin alpha, radius * cos alpha, 0) 
  | i <- [1, 2 .. number]]

circle :: GLfloat -> [(GLfloat,GLfloat,GLfloat)]
circle radius = circlePoints radius 100

fillCircle :: GLfloat -> IO ()
fillCircle r = displayPoints (circle r) Polygon

renderCircle :: GLfloat -> IO ()
renderCircle r = displayPoints (circle r) LineLoop

--makeVertexes :: [(GLfloat, GLfloat,GLfloat)] -> IO ()
--makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)


----------------
-- Support function for the layout manager to convert to offset instead of coordinates. the first set of coordinates is the top left of
-- the function in which one looks, the second set (in ints) is the x and y offset.
---------------

toOffset :: (Int, Int) -> (GLfloat,GLfloat)
toOffset (xOffset, yOffset) = ((realToFrac xOffset), -(realToFrac yOffset))



-- Support functions for panning, used in the draw* functions, instead of the normal translate function
-- translate to vector + some offset from the state
translatePanned pState (Vector3 x y z)
  = translate $ Vector3 (x+xOff) (y+yOff) z
  where
    xOff = xPan pState
    yOff = yPan pState

translatePannedDefault pState = translatePanned pState $ Vector3 0 0 0

translatePannedText pState sca (Vector3 x y z)
  = translate $ Vector3 (x+xOff) (y+yOff) z
  where
    sc_ = realToFrac (sc pState)
    xOff = (xPan pState) / sca * sc_
    yOff = (yPan pState) / sca * sc_
