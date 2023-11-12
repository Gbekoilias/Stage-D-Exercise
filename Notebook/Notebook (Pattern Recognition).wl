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
    Filter[supplyDataset, #Year >= 2004 && #Year <= 2011 &]
    "Country"
  ]
];

(* Print the average food supply for each country between 2004 and 2011 *)
Print[averageSupplyByCountry2004To2011]

(* GUnderstandin the distribution of 50 variables*)
data = RandomVariate[GammaDistribution[3,2], 50]

(*Reordering the dataset and preparing a suitable plot*)
ListPlot[ReverseSort@data, Filling -> Axis, PlotRange ->All]

BoxWhiskerChart[data, "Outliers"]

(* generates a boxplot of food consumption in 45 African countries between 2004 and 2013.*)
ListPlot[
  SortBy[
    GroupBy["Year", {1.43, 3.24, 5.24, 7.75}, "Value"][
      BoxWhiskerChart[#, ("Outliers", ("MeanMarker"))]},
    ChartLabels -> Range[2004, 2013],
    ChartElementFunction -> ChartElementData["Gradient ScaleBoxWhisker",
                                             "Color Scheme" -> "Light TemperatureMap"],
    PlotTheme -> "Business",
    FrameLabel -> {"Year", "Kcal per capita per day"},
    PlotLabel -> "Food consumption in 45 African countries between 2004 and 2013",
    ImageSize -> 500] &],
  Filling -> BottomFilling,
  PlotRange -> {{2004, 2013}, {0, 3500}},
  AspectRatio -> 1,
  XLabel -> "Year",
  YLabel -> "Kcal per capita per day",
  Title -> "Food consumption in 45 African countries between 2004 and 2013"

supplyDataset[GroupBy[ "Year" ] ,"Value" ] [
BoxWhiskerChart[#,{ "Outl ters " ,{"MeanMarker"}}, ChartLabels -> Range[2004,
2013], ChartElementFunctton -> ,
"ColorScheme" -> "Light TemperatureMap" ] , Plot Theme -> "Business" , FrameLabel ->
{"year", "kcal per capita per -> "F"ood consumption in 45 African countries between 2004 and 2013"
ImageSize -> 500] &] ,

foodSupply2012 = Values@Normal@Select[supplyDataset, #Year==DateObject[{2012}] &]
[;; , {"Country", "Value"} ] •
Short [%]


(* stats *)
   mean = QuantityMagnitude@Mean[foodSupply2012[[;; ,]]];
md = QuantityMagnitude@Median[foodSupply2012[[;; ,]]]

(* quartiles *)
   {Q1, Q3} = QuantityMagnitude@Quantile[foodSupply2012[[;;,2]]], {1/4, 3/4};
IQR = QuantityMagnitude@InterQuartileRange[foodSupply2012[[;;, 2]]]

(* fences *)  
(* values lower than this will be classified as an outlier*)
lowerfence = Q1 - 1.5 IQR;
(* values higher than this will be classified as an outlier*)
upperfence = Q3 + 1.5 IQR;

(* find which values are outliers *)
outliers = Cases[QuantityMagnitude@foodSupply2012[[;; ,2]],x_ /; (X< lowerFence || x > upperFence )];

(* and what countries they correspond to *)
Positon[foodSupply2012, #][[1, 1]]&/@outliers;
foodSupply2012[[%]]

(* create plot *)
ListPlot [Association[Rule@@@foodSupply2012] ,
Ticks -> {None, Automatic}, Epilog -> ({
         {#1, Thick, Line[{{l,#2}, {45,#2}}]},
         Inset [Style[#3, #1], {43, #2 + 40}]
         }&@@@ {{Blue, mean,"mean"}, {Pink, md,"median"},
            {Darker@Green, upperFence,"upper fence"}, {Orange,"lower fence"}}),lowerFence ,
         PlotRange {Full, {1000, 4000}}, ImageSize 700]




   