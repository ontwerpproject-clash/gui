module ArchElements where

    data ArchElem a = Function Id (Maybe Name) [InPort] OutPort ([ArchElem a],[Wire a]) a
                    | Operator Id Name [In] Out a
                    | Literal Id Value Out a
                    | Mux Id [In] Out In a
                    | Register Id (Maybe In) Out a
                    deriving (Show, Eq, Ord)

    data Wire a     = Wire (Maybe Name) PortId PortId a
    --                                  src    dst
                    deriving (Show, Eq, Ord)

    data Out        = Normal PortId
                    | Tuple PortId [Out]
                    deriving (Show, Eq, Ord)

    type Name    = String
    type PortId  = Id
    type In      = PortId
    type Port    = Out
    type InPort  = Port
    type OutPort = Port
    type Value   = String
    type Id      = String

    -- dat is en het macc voorbeeld uit een van de clash papers
    macc :: ArchElem ()
    macc = Function "macc" (Just "macc") 
           [(Normal "inX"), (Normal "inY")]                                                -- inputs
            (Normal "outS'")                                                               -- output
           ([Operator "*_0" "*" ["inX", "inY"] (Normal "*_res") (),          -- implementatie 
             Operator "+_0" "+" ["s", "*_res"] (Normal "s'") (),
             Register "Reg0" (Just "s'") (Normal "s") ()
            ],
            [Wire (Just "inX") "inX" "*_0" (),
             Wire (Just "inY") "inY" "+_0" (),
             Wire (Just "*_res") "*_0" "+_0" (),
             Wire (Just "+_res") "+_0" "Reg0" (),
             Wire (Just "s'") "Reg0" "+_0" (),
             Wire (Just "+_res") "+_0" "outS'" ()
            ])
           ()

    macct :: ArchElem ()
    macct = Function "macc" (Just "macc") 
           [(Normal "inX"), (Normal "inY")]                                                -- inputs
            (Normal "outS'")                                                               -- output
           ([Operator "*_0" "*" ["inX", "inY"] (Normal "*_res") (),          -- implementatie 
             Operator "+_0" "+" ["s", "*_res"] (Normal "s'") (),
             Register "Reg0" (Just "s'") (Normal "s") ()
            ],
            [--Wire (Just "inX") "inX" "*_0" (),
             --Wire (Just "inY") "inY" "+_0" (),
             --Wire (Just "*_res") "*_0" "+_0" (),
             Wire (Just "+_res") "+_0" "Reg0" ()
             --Wire (Just "s'") "Reg0" "+_0" (),
             --Wire (Just "+_res") "+_0" "outS'" ()
            ])
           ()


    macc2 :: ArchElem ()
    macc2 = Function "macc" (Just "macc") 
           [(Normal "inX"), (Normal "inY")]                                                -- inputs
            (Normal "outS'")                                                               -- output
           ([Operator "*_0" "*" ["inX", "inY"] (Normal "*_res") (),          -- implementatie 
             Operator "+_0" "+" ["s", "*_res"] (Normal "s'") (),
             Function "test" (Just "test") [(Normal "in1"), (Normal "in2")] (Normal "uit") ([], []) (),
            Register "Reg0" (Just "s'") (Normal "s") ()
            ],
            [Wire (Just "inX") "inX" "*_0" (),
             Wire (Just "inY") "inY" "+_0" (),
             Wire (Just "*_res") "*_0" "+_0" (),
             Wire (Just "+_res") "+_0" "Reg0" (),
             Wire (Just "s'") "Reg0" "+_0" (),
             Wire (Just "+_res") "+_0" "outS'" ()
           ])
           ()

    test :: ArchElem ()
    test = Function "testFunction" (Just "testFunction") 
           [(Normal "inX"), (Normal "inY")]                                                -- inputs
            (Normal "outS'")                                                               -- output
           ([Operator "*_0" "*" ["inX", "inY"] (Normal "*_res") (),          -- implementatie 
             Operator "+_0" "+" ["s", "*_res"] (Normal "s'") (),
             Function "inner1" (Just "inner1")
                [(Normal "inA"), (Normal "inB")]
                 (Normal "uitgang")
                ([Literal "litID" "metEenWaarde" (Normal "enEruit") (),
                  Function "nogMaarEenDan" (Just "nogEen") [(Normal "in8"), (Normal "in9")] (Normal "uit") ([Register "Reg2ofzo" Nothing (Normal "eruitWeer") ()], []) (),
                  Mux "dieHadIkNogNiet" ["in1", "in2"] (Normal "uitPoortje") "nogEenIn?" (),
                  Operator "laatste" "laatsteDus" ["in34", "in234"] (Normal "uit598") ()], [Wire (Just "okEenDraadjeDan") "id3458" "id34587" ()] ) (),
             Register "Reg0" (Just "s'") (Normal "s") ()
            ],
            [Wire (Just "inX") "macc" "*_0" (),
             Wire (Just "inY") "macc" "+_0" (),
             Wire (Just "*_res") "*_0" "+_0" (),
             Wire (Just "+_res") "+_0" "Reg0" (),
             Wire (Just "s'") "Reg0" "+_0" (),
             Wire (Just "+_res") "+_0" "macc" ()
            ])
           ()
