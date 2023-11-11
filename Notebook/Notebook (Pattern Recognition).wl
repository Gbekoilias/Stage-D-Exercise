( * import data * )
i1 = Import ["https://www.wolframcloud.com/obj/mar/Hamoye/Session%202/Data/Africa%20Food%20Production%20(2004%20-%202013).csv" , "Data" ]

( * create food production dataset * )
prodDataset = Dataset[AssociationThread[First[i1] ->#]&/@Rest[i1]]


Clear[tokiloton, toDateObj]

(* add kiloton unit to every value *)
tokiloton[v_]:=Quantity[v, "Metrickilotons"]

(* convert every year to a DateObject *)
toDateObj[y_] := DateObject[{y}]

(* And than apply these functions to every row in the dataset to form our final food production dataset *)
prodDataset = Once[prodDatasetl;;, {Year -> toDate0b), "Value" -> tokiloton}]]

(* group all entries by country, then by year, then by item *)
prodDatasetGrouped = 
(
   (
      (
         prodDataset GroupBy["Country"]]
      )[:: :: GroupBy["Year"]]
   )[:: :: GroupBy["Item"]]
)[;;, ;;, ;;, ;;, {4}]