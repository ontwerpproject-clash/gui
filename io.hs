import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Data.IORef


a = MenuEntry "test" close
m = Menu [a]
lb = (MouseButton LeftButton)



keyboard mainWindow inpu (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard mainWindow inpu (Char '+') Down _ _ = print "zoom in"
keyboard mainWindow inpu (Char '-') Down _ _ = print "zoom out"
keyboard mainWindow inpu (MouseButton LeftButton) Down _ (Position x y) = mouseStuff x y mainWindow inpu --has to be (MouseButton LeftButton), when using lb (see line 8) this line starts catching keystrokes aswell, which seems a little weird
keyboard mainWindow inpu (MouseButton WheelUp) _ _ _ = exitWith ExitSuccess --mousewheel does not work
keyboard mainWindow inpu _ _ _ _ = return ()

--few test checks to do things according to the position of the click
--topleft of a window has the position 0/0 for mouseclicks
--pointerX == x pixel position of the mouseclick
--pointerY == y pixel position of the mouseclick
--windowXSize == the horizontal size of the window in pixels
--windowYSize == the vertical size of the window in pixels
--click in topleft closes the program, most other clicks produce squares and clicks on the right hand side of the window produce lines with origin close to the top right
mouseCheck pointerX pointerY windowXSize windowYSize mainWindow inpu
	| pointerX<50 && pointerY < 50 = exitWith ExitSuccess
	| test pointerX = paintSquare (fst (mouseXYPos pointerX pointerY windowXSize windowYSize)) (snd (mouseXYPos pointerX pointerY windowXSize windowYSize)) mainWindow inpu
	| otherwise = do
		checkExit inpu
		color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
		lineWidth $= 1.0
		let xyPos = (mouseXYPos pointerX pointerY windowXSize windowYSize)
		renderPrimitive LineStrip $ mapM_ vertex [ Vertex3 (0.9 :: GLfloat) (0.75) 0.0, Vertex3 (fst xyPos) (snd xyPos) 0.0]
		flush
	
	
test x = if ( x < 300 ) then True else False



checkExit inpu = do
	i <- get inpu
	if(i=="exit") then (exitWith ExitSuccess) else return()	
	
mouseStuff x y mainWindow inpu = do
	(position, size) <- get viewport
	mouseCheck x y (xSize size) (ySize size) mainWindow inpu
	

--might have to cut on precision or to account for some error margin if trying to match a position of an element and the click position
mouseXYPos xPixel yPixel xWindowSize yWindowSize = do
		--0/0
		let xCenter = intToFloat xWindowSize / 2 
		let yCenter = intToFloat yWindowSize / 2
		--difference in relation to 0/0
		let relX = intToFloat xPixel - xCenter
		let relY = (intToFloat yPixel - yCenter) * (-1.0) 
		let xPos = relX / xCenter
		let yPos = relY / yCenter

		(xPos, yPos)
		

intToFloat n = fromInteger (toInteger n)	

--returns the number of pixels in x direction
xSize si= xSize_ si
xSize_ p=x
	where 
		(Size x y)=p
--returns the number of pixels in y direction		
ySize si= ySize_ si
ySize_ p=y
	where 
		(Size x y)=p
		

makePoints :: GLfloat->GLfloat->[Vertex3 GLfloat]
makePoints x y = [
  Vertex3 (-x)   y  0.0,
  Vertex3   x    y  0.0,
  Vertex3   x  (-y) 0.0,
  Vertex3 (-x) (-y) 0.0]
 
--paints a square on screen and creates a subwindow 
paintSquare x y mainWindow inpu= do
	clear [ ColorBuffer ]
	color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
	lineWidth $= 1.0
	renderPrimitive Polygon $	mapM_ vertex (makePoints x y)
	sub<-createSubWindow mainWindow (Position 50 50) (Size 100 100)
	displayCallback $= (display1 inpu)
	keyboardMouseCallback $= (Just ( keyboard1 sub inpu))
	flush


keyboard1 sub inpu (Char '\27') Down _ _ = exitWith ExitSuccess  --press "esc" to quit
keyboard1 sub inpu (Char '+') Down _ _ = print "zoom in"
keyboard1 sub inpu (Char '-') Down _ _ = print "zoom out"
keyboard1 sub inpu (MouseButton LeftButton) Down _ (Position x y) = mouseStuff1 x y sub   --don't use lb Down, has to be (MouseButton LeftButton)
keyboard1 sub inpu (MouseButton WheelUp) Down _ _ = exitWith ExitSuccess --mousewheel does not work
keyboard1 sub inpu a Down _ _ = do
	print a
	i <- get inpu
	let tex=addChar a i
	if(enterInString tex) then do
		checkExit inpu
		destroyWindow sub 
		else inpu $= snd(checkEnter tex)
	color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
	i <- get inpu
	print i
	clear [ ColorBuffer ]
	translate $ Vector3 (-1.0::GLfloat) (0.0) (0.0)
	scale 0.003 0.003 (0.003::GLfloat)
	renderString Roman i
	loadIdentity
	flush
keyboard1 sub inpu _ _ _ _ = return ()

--adds chars to the ioref
addChar (MouseButton x) text = text
addChar (SpecialKey x) text  = text
addChar (Char x) text        = if(x == '\b') then (take (length text -1) text) else text++[x]

--checks if the string end in an enter, if so returns true and the string without the \r  otherwise false and the string
checkEnter text = if((length text) >0) then if((text!!(length text-1))=='\r') then (True, (take (length text -1) text) ) else (False, text) else (False, text)


enterInString text= fst (checkEnter text) 


mouseCheck1 pointerX pointerY windowXSize windowYSize sub 
	| pointerX<10 && pointerY < 10 =  destroyWindow sub
	| test pointerX = paintSquare1 (fst (mouseXYPos pointerX pointerY windowXSize windowYSize)) (snd (mouseXYPos pointerX pointerY windowXSize windowYSize))
	| otherwise = do
		color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
		lineWidth $= 1.0
		let xyPos = (mouseXYPos pointerX pointerY windowXSize windowYSize)
		renderPrimitive LineStrip $ mapM_ vertex [ Vertex3 (0.9 :: GLfloat) (0.75) 0.0, Vertex3 (fst xyPos) (snd xyPos) 0.0]
		flush
	
	

paintSquare1 x y= do
	clear [ ColorBuffer ]
	color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
	lineWidth $= 1.0
	renderPrimitive Polygon $	mapM_ vertex (makePoints x y)
	flush	
	

mouseStuff1 x y sub = do
	(position, size) <- get viewport
	mouseCheck1 x y (xSize size) (ySize size)  sub



  
display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   flush

--display function for the subwindow
--prints the content of the ioref when called
display1 inpu = do
	clearColor $= Color4 1 0 0 1
	clear [ ColorBuffer ]
	paintSq
	i <- get inpu
	print i
	clear [ ColorBuffer ]
	color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
	lineWidth $= 1.0
	translate $ Vector3 (-1.0::GLfloat) (0.0) (0.0)
	scale 0.003 0.003 (0.003::GLfloat)
	renderString Roman i
	flush

--paints a red rectangle to denote the subwindow
paintSq= do
	clear [ ColorBuffer ]
	color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
	lineWidth $= 1.0
	let p = (makePoints 0 0.5)
	print p
	renderPrimitive Polygon $	mapM_ vertex p
	flush

   
   
reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   matrixMode $= Modelview 0

--gives warning   
changeMen= do
	attachMenu RightButton m
close = exitWith ExitSuccess

openSub mainWindow inpu= do
	sub<-createSubWindow mainWindow (Position 50 50) (Size 100 100)
	displayCallback $= (display1 inpu)
	keyboardMouseCallback $= (Just ( keyboard1 sub inpu))

-- a function which takes both iorefs and prints their contents
calc start end = do
	print "func"
	s <- get start
	e <- get end
	print s
	print e
	
main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode ]
   initialWindowSize $= Size 400 150
   initialWindowPosition $= Position 100 100
   mainWindow <- createWindow progName
   inpu <- newIORef ""
   inpu2<- newIORef ""
   -- attach a menu to the right mousebutton
   attachMenu RightButton (Menu [MenuEntry "test!" changeMen,SubMenu "Sub" (Menu [MenuEntry "testSub" close, MenuEntry "SUBWINDOW" (openSub mainWindow inpu), MenuEntry "SUBWINDOW 2" (openSub mainWindow inpu2), MenuEntry "calc" (calc inpu inpu2)])])
   displayCallback $= display
   reshapeCallback $= Just reshape
   keyboardMouseCallback $= (Just ( keyboard mainWindow inpu))
   mainLoop

