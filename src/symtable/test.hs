import Data.Tree

testTree = Node 1 [ Node 2 [ Node 4 [ Node 6 [], Node 8 [] ],
                             Node 5 [ Node 7 [], Node 9 [] ] ],
                    Node 3 [ Node 10 [], 
                             Node 11 [] ] 
                  ]

zipperTree = zipper testTree

