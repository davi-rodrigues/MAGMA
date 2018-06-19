(* ::Package:: *)

(* ::Input::Initialization:: *)
(************************************
ELPlot v0.2
Davi C. Rodrigues , November/2017

ELPlot implements error bars into ListPlot and includes a set of special bullets. 
It is an alternative package to the ErrorBarsPlot package. The central values are always print as graphics, no dependence on fonts. It accepts the ErrorBarsPlot sintaxe, and have nicer results with less options (at least for my purposes).

This version of ELPlot is distributed with MAGMA,  it is part of it and thus it is under the same license.
****************************)

BeginPackage["ELPlot`"];

ELPlot::usage="ELPlot[{{\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(y\), \(1\)]\)}, {\!\(\*SubscriptBox[\(x\), \(2\)]\), \!\(\*SubscriptBox[\(y\), \(2\)]\)},...}, {\!\(\*SubscriptBox[\(dy\), \(1\)]\),\!\(\*SubscriptBox[\(dy\), \(2\)]\),...}] plots bullets at coordinates {\!\(\*SubscriptBox[\(x\), \(n\)]\), \!\(\*SubscriptBox[\(y\), \(n\)]\)} and the corresponding error bars given by \!\(\*SubscriptBox[\(dy\), \(n\)]\), as in y=\!\(\*SubscriptBox[\(y\), \(n\)]\)\[PlusMinus]\!\(\*SubscriptBox[\(dy\), \(n\)]\).
ELPlot[{{\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(y\), \(1\)]\)}, {\!\(\*SubscriptBox[\(x\), \(2\)]\), \!\(\*SubscriptBox[\(y\), \(2\)]\)},...}, {{\!\(\*SubscriptBox[\(dy\), \(\(1\)\(+\)\)]\), \!\(\*SubscriptBox[\(dy\), \(\(1\)\(-\)\)]\)},{\!\(\*SubscriptBox[\(dy\), \(\(2\)\(+\)\)]\), \!\(\*SubscriptBox[\(dy\), \(\(2\)\(-\)\)]\)},...}] plots bullets at coordinates {\!\(\*SubscriptBox[\(x\), \(n\)]\), \!\(\*SubscriptBox[\(y\), \(n\)]\)} and the corresponding error bars given by \!\(\*SubscriptBox[\(dy\), \(\(n\)\(+\)\)]\) and \!\(\*SubscriptBox[\(dy\), \(\(n\)\(-\)\)]\), as in y=\!\(\*SubscriptBox[\(y\), \(n\)]\)+\!\(\*SubscriptBox[\(dy\), \(\(n\)\(+\)\)]\)-\!\(\*SubscriptBox[\(dy\), \(\(n\)\(-\)\)]\).
ELPlot[{{{\!\(\*SubscriptBox[\(x\), \(1\)]\), \!\(\*SubscriptBox[\(y\), \(1\)]\)}, Errorbar[\!\(\*SubscriptBox[\(err\), \(1\)]\)]}, {{\!\(\*SubscriptBox[\(x\), \(2\)]\), \!\(\*SubscriptBox[\(y\), \(2\)]\)}, Errorbar[\!\(\*SubscriptBox[\(err\), \(2\)]\)]},...}, {}] constitutes an alternative entry form, the output is the same of the previous cases."

{MarkersColor,LinesColor,  MarkersSize, MarkersThickness, LinesThickness, Markers, LinesHorizontalLength, circle, circlecross, disk, utriangle, dtriangle, square, square90, hexagon, squarefull, utrianglefull, dtrianglefull, hexagonfull};

Begin["`Private`"];

circle[color_,size_:10, thickness_:1] := Graphics[{color, AbsoluteThickness[thickness],Circle[]}, ImageSize -> size];
circlecross[color_,size_:10,thickness_:1] := Graphics[{color, AbsoluteThickness[thickness],Circle[], Line[{{0,-0.95},{0,0.95}}],Line[{{-0.95,0},{0.95,0}}]}, ImageSize -> size];
disk[color_,size_:10, thickness_:1] := Graphics[{color, AbsoluteThickness[thickness],Disk[]}, ImageSize -> size];
utriangle[color_,size_:10, thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[],Polygon[{{1,0},{0,Sqrt[3]},{-1,0}}]}, ImageSize -> size];
utrianglefull[color_,size_:10, thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[color],Polygon[{{1,0},{0,Sqrt[3]},{-1,0}}]}, ImageSize -> size];
dtriangle[color_,size_:10, thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[],Polygon[{{1,0},{0,-Sqrt[3]},{-1,0}}]}, ImageSize -> size];
dtrianglefull[color_,size_:10, thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[color],Polygon[{{1,0},{0,-Sqrt[3]},{-1,0}}]}, ImageSize -> size]
square[color_,size_:10,thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[],Polygon[{{-1,-1},{-1,1},{1,1}, {1,-1}}]}, ImageSize -> size];
square90[color_,size_:10,thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[],Polygon[{{-1,0},{0,1},{1,0}, {0,-1}}]}, ImageSize -> size];
squarefull[color_,size_:10,thickness_:1] := Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[color],Polygon[{{-1,-1},{-1,1},{1,1}, {1,-1}}]}, ImageSize -> size];
hexagon[color_, size_:10, thickness_:1] :=Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[],Polygon[Table[{Cos[2\[Pi] k/6],Sin[2\[Pi] k/6]},{k,0,5}]]}, ImageSize -> size];
hexagonfull[color_, size_:10, thickness_:1] :=Graphics[{EdgeForm[{AbsoluteThickness[thickness],color}],FaceForm[color],Polygon[Table[{Cos[2\[Pi] k/6],Sin[2\[Pi] k/6]},{k,0,5}]]}, ImageSize -> size];


Options[ELPlot] = {
MarkersColor -> Black,
LinesColor-> Black,
MarkersSize -> 10,
MarkersThickness -> 1,
LinesThickness -> 1.2,
Markers-> disk,
LinesHorizontalLength -> 0.01
};

ELPlot[data_?ListQ, error_?ListQ,opts:OptionsPattern[{ELPlot,ListPlot}]]:= 
Block[{dt = data, er = error,listErrorBar, dataerrorpos, horizontallength, verticallines,horizontallines, mcolor = OptionValue[MarkersColor], lcolor = OptionValue[LinesColor], linesthickness = OptionValue[LinesThickness], markersthickness = OptionValue[MarkersThickness], markersize = OptionValue[MarkersSize], markers = OptionValue[Markers], k = OptionValue[LinesHorizontalLength],SavedOptionsLinTicks, SavedOptionsListPlot, LPopts},

(*START: Convertion from a ErrorListPlot entry to an ELPlot entry.*)
If[StringPosition[ToString[dt], "ErrorBar"]=== {}, Null,   
listErrorBar = List@@#&/@ dt[[All,2]]; (*A list of all the data inside all the "ErrorBar".*)
If[Dimensions[#] == {1} || Dimensions[#] == {1,2}, Null, Print["This use of ErrorBar is not yet supported. The data inside of ErrorBar must be in either of the forms ErrorBar[0.5] or ErrorBar[{-0.2,2}]"]; Print[listErrorBar]; Abort[]  ] & /@ listErrorBar;
er = Partition[Flatten[listErrorBar /. {x_?NumberQ} -> {-x,x}],2]; (*Replaces the list {0.2, {0.1,2}, 0.1} to {{-0.2,0.2}, {0.1,2}, {-0.1,0.1}}*)
dt= dt[[All,1]]; (*The data, without errorbars come from the first entry of dt.*)
];
(*END: Convertion from a ErrorListPlot entry to an ELPlot entry.*)

If[\[Not]Length[dt]== Length[er], Print["The two entries of ELPlot (data and error) shoud have the same Length."]; Abort[]];
If[Length[Dimensions[dt]]!=2 || Last[Dimensions[dt]] !=2, Print["The first entry of ELPlot, data, should have dimensions {x,2}, where x is arbitrary. Alternatively, data can be given in the ErrorListPlot format."]; Abort[]];
If[Length[Dimensions[er]]>2 ||( Length[Dimensions[er]]==2 && Last[Dimensions[er]] !=2), Print["The second entry of ELPlot, error, should have dimensions {x} or {x,2}, where x is arbitrary."]; Abort[]];
dataerrorpos = Flatten[{{dt[[#,1]], dt[[#,2]] + er[[#,1]]}, {dt[[#,1]], dt[[#,2]] + er[[#,2]]}} & /@ Range[Length[dt]],1];
horizontallength = (Last[#] - First[#])&[Sort[dt[[All,1]]]];
horizontallines= Sequence@@(Line[{{dataerrorpos[[#,1]]- k horizontallength, dataerrorpos[[#,2]]}, {dataerrorpos[[#,1]]+k horizontallength, dataerrorpos[[#,2]]}}]& /@ Range[Length[dataerrorpos]]);
verticallines = Sequence@@(Line[{dataerrorpos[[#]], dataerrorpos[[#+1]]}]& /@ Range[1,Length[dataerrorpos], 2]);
LPopts=FilterRules[{opts},Options[ListPlot]];
Show[ListPlot[dt , LPopts,PlotMarkers-> {markers[mcolor, markersize, markersthickness] }],Graphics[{AbsoluteThickness[linesthickness], lcolor,verticallines}],ListPlot[dt ,LPopts, PlotMarkers-> {markers[mcolor, markersize, markersthickness] }],
Graphics[{AbsoluteThickness[linesthickness], lcolor, horizontallines}]]
];
End[];
EndPackage[]
