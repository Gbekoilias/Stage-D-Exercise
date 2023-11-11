( * import data * )
i1 = Import ["https://www.wolframcloud.com/obj/mar/Hamoye/Session%202/Data/Africa%20Food%20Production%20(2004%20-%202013).csv" , "Data" ]

( * create food production dataset * )
prodDataset = Dataset[AssociationThread[First[i1] ->#]&/@Rest[i1]]
<<<<<<< HEAD


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

(* for each country, for each year, sum up all values of items *)
prodDatasetTotalled = prodDatasetGrouped[;;;;, Total[(Values@#)[[:;, 1, 1]]] &]
Total[(Values@#)[[:;, 1, 1]]] /@ prodDatasetGrouped[Country="Algeria", Year=2004]
Print[Country, Year, Total[(Values@#)[[:;, 1, 1]]] & /@ prodDatasetTotalled]

(* extract data from dataset *)
plotData = Normal@prodDatasetTotalled[1;;]; ( show a short excerpt of the data *)
Short[plotData, 41]

DateListLogPlot[plotData, PlotRange >> All, Plotlabels →→ Automatic, PlotLegends-> None, ImageSize-> 1000, PlotTheme "Detailed"]

(* import supply dataset *)
supplyDataset = Import["https://www.wolframcloud.com/obj/mar/Hamoye/Session%202/Data/Africa%20Food%20Supply%20(2804%20-%202013).csv", "Data"];

(* create food supply dataset *)
supplyDataset = Dataset[AssociationThread[First[supplyDataset] > #1 & /@ Rest[supplyDataset]]];

(* import supply dataset *)
supplyDataset = Import["https://www.wolframcloud.com/obj/mar/Hamoye/Session%202/Data/Africa%20Food%20Supply%20(2804%20-%202013).csv", "Data"];

(* create food supply dataset *)
supplyDataset = Dataset[AssociationThread[First[supplyDataset] > #1 & /@ Rest[supplyDataset]]];

(* Calculate the average value of the food supply for each country *)
averageSupplyByCountry = Dataset[
  Mean /@ GroupBy[supplyDataset, "Country"]
];

(* Print the average value of the food supply for each country *)
Print[averageSupplyByCountry]

(* Calculate the average food supply for each country between 2004 and 2011 *)
averageSupplyByCountry2004To2011 = Dataset[
  Mean /@ GroupBy[
    Filter[supplyDataset, #Year >= 2004 && #Year <= 2011 &],
    "Country"
  ]
];

(* Print the average food supply for each country between 2004 and 2011 *)
Print[averageSupplyByCountry2004To2011]
=======
>>>>>>> 8175a8363bccc1f4a6b9989c1e9dbe99fde6b08a
