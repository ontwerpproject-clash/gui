module LayoutManager where
    -- Imports
    import Graphics.Rendering.OpenGL
    import Graphics.UI.GLUT
    import Data.List
    import ParseClash as AE

    -- Type definitions
    type Offset = (Int, Int)
    type FromTo = (Offset, Offset)

    -- These are the relative x- and y-offsets for all three columns for the elements within a function
    type RelOffsets = ((Int, Int), (Int, Int), (Int, Int))
    initRelOffsets :: RelOffsets
    initRelOffsets = ((0, 0), (1, 0), (2, 0))

    -- Constants
    wireOffset :: Offset
    wireOffset = (-1, -1)

    -- Order elements of a function and calculate the offsets
    --
    -- Rough sketch of the possibilities and how to offset them. This is a first attempt to be able to get
    -- something drawn on the screen fast:
    -- 1. Wires go from Parent to Element/Function
    --      The Elements where the wires go to we need first, since they get drawn closest to the left edge of the parent
    --    These Elements get an offset in x-direction of 0
    -- 2. Wires go from Element/Function to Element/Function
    --    These elements get an offset of 1 in x-direction
    -- 3. Wires go from Element/Function to Parent
    --    The Elements where the wires come from we need last, they get drawn close to the right edge
    --    These Elements get an offset of 3 in x-direction
    --
    -- Offsets are all initial. They probably have to be fixed later to apply to situations where for example there
    -- are a lot of elements in between and only 2 on the left edge. The function in this case will be very stretched
    -- in the y-direction. But i guess that will be a relatively easy fix.
    --
    -- The offset for the wires is not relevant so there is a constant for that: (-1, -1)
    --
    -- Initial x-offset for the 1st, 2nd and 3rd column are 1, 2 and 3 respectively. If one of the elements in the list
    -- is a function, 2 gets added to the columns to the right of it during calculation of absolute positions.

    offsetElements :: Ord a => ArchElem a -> ArchElem Offset
    offsetElements (Function id name ins out (fs, ws) a) = Function id name ins out (newFs, newWs) (0, 0)
                                                         where
                                                            newWs = map offsetWire ws
                                                            newFs = offsetElementsH (sort fs, ws) ins out initRelOffsets

    offsetElementsH :: Ord a => ([ArchElem a], [Wire a]) -> [InPort] -> OutPort -> RelOffsets -> [ArchElem Offset]
    offsetElementsH ([], _) _ _ _                     = []
    offsetElementsH (f:fs, ws) inports out reloffsets = let 
                                                            ((c1x, c1y), (c2x, c2y), (c3x, c3y)) = reloffsets 
                                                         in case (isFunction f) of
        -- True:
        True  -> let 
                     (Function id name inports_ out_ (fs_, ws_) a) = f
                     newFs_                                           = offsetElementsH (sort fs_, ws_) inports_ out_ initRelOffsets
                     newWs_                                        = map offsetWire ws_
                     (x, y)                                           = calcFuncSize (reverse (sort newFs_)) (0, 0)
                 in case (getElementCase f ws inports out) of
                     1 -> (Function id name inports_ out_ (newFs_, newWs_) (c1x, c1y)) : offsetElementsH (fs, ws) inports out ((c1x, c1y_), (c2x_, c2y), (c3x_, c3y))
                          where
                              c1y_ = c1y + y
                              c2x_ = max c2x (c1x + x)
                              c3x_ = max c3x (c2x + x)
                     2 -> (Function id name inports_ out_ (newFs_, newWs_) (c2x, c2y)) : offsetElementsH (fs, ws) inports out ((c1x, c1y), (c2x, c2y_), (c3x_, c3y))
                         where
                             c2y_ = c2y + y
                             c3x_ = max c3x (c2x + x)
                     3 -> (Function id name inports_ out_ (newFs_, newWs_) (c3x, c3y)) : offsetElementsH (fs, ws) inports out ((c1x, c1y), (c2x, c2y), (c3x, c3y_))
                         where
                             c3y_ = c3y + y

        -- False: Check case of element and return element with x and y offset, then update just the y-offset
        False -> case (getElementCase f ws inports out) of
                1 -> (setOffset f (c1x, c1y)) : offsetElementsH (fs, ws) inports out ((c1x, c1y+1), (c2x, c2y), (c3x, c3y))
                2 -> (setOffset f (c2x, c2y)) : offsetElementsH (fs, ws) inports out ((c1x, c1y), (c2x, c2y+1), (c3x, c3y))
                3 -> (setOffset f (c3x, c3y)) : offsetElementsH (fs, ws) inports out ((c1x, c1y), (c2x, c2y), (c3x, c3y+1))

    getAttributes :: ArchElem a -> ([ArchElem a], [Wire a], [InPort], OutPort)
    getAttributes (Function id name ins out (fs, ws) a) = (fs, ws, ins, out)
    getAttributes _                                     = error "not a function"

    getElementCase :: ArchElem a -> [Wire a] -> [InPort] -> OutPort -> Int
    getElementCase element wires inports out
        | isCase1 element wires inports = 1
        | isCase3 element out wires     = 3
        | otherwise                     = 2

    -- Case 1:
    -- Check wether the element has (a) wire(s) coming in from one of the inports
    isCase1 :: ArchElem a -> [Wire a] -> [InPort] -> Bool
    isCase1 element ws []     = False
    isCase1 element ws (p:ps) 
        | (includes (getPortID p) (getElementID element) ws) = True
        | otherwise                                          = isCase1 element ws ps

    -- Case 3:
    -- Check wether the element has (a) wire(s) going out to the outport
    isCase3 :: ArchElem a -> OutPort -> [Wire a] -> Bool
    isCase3 element out ws = includes (getElementID element) (getPortID out) ws

    -- Check wether there is a wire from src to dest
    includes :: Id -> Id -> [Wire a] -> Bool
    includes src dest []                     = False
    includes src dest ((Wire name s d _):ws) = case (src==s && dest==d) of
        True  -> True
        False -> includes src dest ws

    -- Calculate the max x and y offsets
    -- In essence, this is the size of the circuit/function
    calcFuncSize :: [ArchElem Offset] -> (Int, Int) -> (Int, Int)
    calcFuncSize [] (x, y)     = (x+1, y+1)
    calcFuncSize (f:fs) (x, y) = case (isFunction f) of
        True  -> calcFuncSize fs (max x x_, max y y_)
                 where
                     (x_, y_) = calcFuncSize [f] (0, 0)
        False -> calcFuncSize fs (max x xf, max y yf)
                 where
                     (xf, yf) = getOffset f


    -- Transform Wire FSize to Wire Offset
    offsetWire :: Wire a -> Wire Offset
    offsetWire (Wire name src dest a) = Wire name src dest wireOffset

    -- Transform ArchElem a into ArchElem Offset
    setOffset :: ArchElem a -> Offset -> ArchElem Offset
    setOffset (Operator id name ins out a) offset          = Operator id name ins out offset
    setOffset (Literal id val out a) offset                = Literal id val out offset
    setOffset (Mux id ins out inp a) offset                = Mux id ins out inp offset
    setOffset (Register id inp out a) offset               = Register id inp out offset

    -- Returns the offset of an element
    getOffset :: ArchElem Offset -> Offset
    getOffset (Function _ _ _ _ _ offset) = offset
    getOffset (Operator _ _ _ _ offset)   = offset
    getOffset (Literal _ _ _ offset)      = offset
    getOffset (Mux _ _ _ _ offset)        = offset
    getOffset (Register _ _ _ offset)     = offset

    -- Check wether an object is a Function
    isFunction :: ArchElem a -> Bool
    isFunction (Function _ _ _ _ _ _) = True
    isFunction _                      = False

    -- Get the Id attribute of an ArchElem Offset
    -- TODO: make generic so that it works for ports and elements
    getElementID :: ArchElem a -> Id
    getElementID (Function id _ _ _ _ _) = id
    getElementID (Operator id _ _ _ _)   = id
    getElementID (Literal id _ _ _)      = id
    getElementID (Mux id _ _ _ _)        = id
    getElementID (Register id _ _ _)     = id

    -- Get the Id attribute of a Port
    getPortID :: Port -> Id
    getPortID (SinglePort id) = id
    getPortID (MultiPort id _)   = id

    -- Extract the wires from the circuit description and return all the wires
    -- with their start- and end-offsets
    extractWires :: ArchElem Offset -> [Wire FromTo]
    extractWires c = let (ios, wires) = extractElementIdAndOffset [c] ([], [])
    				 in map (setFromTo ios) wires

    -- Returns a list of all the function and element id's with the offset of the particular unit
    extractElementIdAndOffset :: [ArchElem Offset] -> ([(Id, Offset)], [Wire Offset]) -> ([(Id, Offset)], [Wire Offset])
    extractElementIdAndOffset [] results       = results
    extractElementIdAndOffset (f:fs) (ios, ws) = let 
                                                      (Function id n ins out (functions, wires) o) = f
                                                      list                                         = (id, o) : (toId out, o) : (map (combine o) ins)
                                                      tuple                                        = (getElementID f, getOffset f)
                                                      (a, b)                                       = extractElementIdAndOffset functions ([], [])
                                                  in case (isFunction f) of
        True  -> extractElementIdAndOffset fs (a ++ list ++ ios, b ++ wires ++ ws)
        False -> extractElementIdAndOffset fs ((tuple : ios), ws)

    -- Combines a list of inports with the offset of the function
    combine :: Offset -> Port -> (Id, Offset)
    combine o p = (toId p, o)

    -- Converts an OutPort to an Id
    toId :: Port -> Id
    toId (AE.SinglePort id)  = id
    toId (AE.MultiPort id _) = id

    -- Returns the list of wires in a function
    getFunctionsAndWires :: ArchElem Offset -> ([ArchElem Offset], [Wire Offset])
    getFunctionsAndWires (Function _ _ _ _ a _) = a
    getFunctionsAndWires _                      = error "Not a function"

    -- Finds the starting and ending offset for a wire given the wire and a list of element ids and corresponing offsets
    setFromTo :: [(Id, Offset)] -> Wire Offset -> Wire FromTo
    setFromTo ios (Wire name from to offset) = Wire name from to (findById from ios, findById to ios)

    -- Returns the offset of the elements with id
    findById :: Id -> [(Id, Offset)] -> Offset
    findById id []           = wireOffset
    findById id ((i, o):ios)
    	| (i == id) = o
    	| otherwise = findById id ios
