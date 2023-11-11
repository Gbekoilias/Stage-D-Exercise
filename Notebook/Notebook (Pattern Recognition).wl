( * import data * )
i1 = Import ["https://www.wolframcloud.com/obj/mar/Hamoye/Session%202/Data/Africa%20Food%20Production%20(2004%20-%202013).csv" , "Data" ]

( * create food production dataset * )
prodDataset = Dataset[AssociationThread[First[i1] ->#]&/@Rest[i1]]
