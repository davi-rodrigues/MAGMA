(* ::Package:: *)

(* ::Input::Initialization:: *)
(*MAGMA - Mathematica Automatized Galaxy Mass Analysis*)
(*MAGMAplots v0.4*)
(*Davi C. Rodrigues (2018)*)

(*Description: generates galaxy plots from the output of MAGMAfits.*)


version = "0.4("<> DateString[FileDate[$InputFileName],"ISOOrdinalDate"]<>")";
Print["Starting MAGMAplots v"<> version];
Needs["CustomTicks`"];
Needs["ELPlot`"];


(* ::Input::Initialization:: *)
Begin["`PrivatePlots`"];

 
frameticksoptions = FrameTicks->{{LinTicks,StripTickLabels[LinTicks]}, {LinTicks,StripTickLabels[LinTicks]}};
(*Options for the ticks, depends on CustomTicks.m*)

generaloptions = Delete[{Frame-> True, Axes -> False, GridLines -> Automatic, FrameStyle -> 20, GridLinesStyle -> Directive[Dashed, LightGray, AbsoluteThickness[1]], AspectRatio -> 1, LabelStyle->{FontFamily->"Times"}},0];

plotstyleoptions[thickness_] = PlotStyle->{Directive[GrayLevel[0],AbsoluteThickness[thickness]],Directive[RGBColor[0.34398, 0.49112, 0.89936],AbsoluteThickness[thickness]],Directive[RGBColor[0.97, 0.606, 0.081],AbsoluteThickness[thickness]],Directive[RGBColor[0.448, 0.69232, 0.1538],AbsoluteThickness[thickness]],Directive[RGBColor[0.62168, 0.2798, 0.6914],AbsoluteThickness[thickness]],Directive[RGBColor[0.91, 0.318, 0.243],AbsoluteThickness[thickness]],Directive[RGBColor[0.09096, 0.6296, 0.85532],AbsoluteThickness[thickness]],Directive[RGBColor[0.46056, 0.40064, 0.81392],AbsoluteThickness[thickness]],Directive[RGBColor[0.94, 0.462, 0.162],AbsoluteThickness[thickness]],Directive[RGBColor[0., 0.7, 0.7],AbsoluteThickness[thickness]],Directive[RGBColor[0.827051, 0.418034, 0.0243459],AbsoluteThickness[thickness]],Directive[RGBColor[0.5511749434976025, 0.32014794962639853`, 0.8720626412559938],AbsoluteThickness[thickness]],Directive[RGBColor[0.72694101250947, 0.7196601125010522, 0.],AbsoluteThickness[thickness]],Directive[RGBColor[0.8680706456216862, 0.2563858708756628, 0.30321559063052295`],AbsoluteThickness[thickness]],Directive[RGBColor[0.2418693812442152, 0.5065044950046278, 0.9902432574930582],AbsoluteThickness[thickness]],Directive[RGBColor[0.9573908706237908, 0.5369543531189542, 0.11504464931576472`],AbsoluteThickness[thickness]],Directive[GrayLevel[0],AbsoluteThickness[thickness]]};

colorplot[i_] :=  plotstyleoptions[1.8][[2,i,1]]; (*Calls the i-th color used in plotstyleoptions*)

nELPlot[Args___] := ELPlot[Args, Frame-> True, Axes -> False, GridLines -> Automatic, FrameStyle -> 20, GridLinesStyle -> Directive[Dashed, LightGray, AbsoluteThickness[1]], AspectRatio -> 1, LabelStyle->{FontFamily->"Times"}, FrameTicks->{{LinTicks,StripTickLabels[LinTicks]}, {LinTicks,StripTickLabels[LinTicks]}}, Evaluate[plotstyleoptions[1]]];



(* ::Input::Initialization:: *)
SetAttributes[ImposePrecision, Listable];
ImposePrecision[number_, precision_?NumberQ] := 
If[NumberQ[number] && number ==0, 
number, 
If[NumberQ[number], 
 N[Round[number, 10^(Floor[Log[10,Abs[number]]] - precision + 1)], precision], 
number]];

TSI[x_] := ToString[x, FormatType -> InputForm ]; (*Defines TSI function, ToString with InputForm*)

CheckDirectories[directorynames_?ListQ] := If[DirectoryQ[FileNameJoin[{Directory[], #}]], Null, Print["Directory "<>#<>" does not exist. Aborting."]; Abort[]] &/@ directorynames;

filein[directoryname_,file_] := FileNameJoin[{Directory[], directoryname, file}]; 


startname[i_] := StringPosition[ResultFiles[[i]],FileNameJoin[{Directory[], "results-chi2"}]][[1,2]] + 2;
endname[i_] := StringPosition[ResultFiles[[i]], "-Min-"][[1,1]] - 1; (*the position of the last galaxy name character*)
endmodel[i_] := StringPosition[ResultFiles[[i]], "-var-"][[1,1]] - 1;
endvar[i_] := StringPosition[ResultFiles[[i]], ".m"][[1,1]] - 1;
galaxy[i_] := StringTake[ResultFiles[[i]], {startname[i], endname[i]}]; 

s2[x_] = x Abs[x]; (*special square*)
sroot[x_] = Sign[x]Sqrt[Abs[x]]; (*special root*)



(* ::Input::Initialization:: *)
Options[MAGMAplots] = {
Galaxies-> All,
FitDistance -> False,
FitDisk-> True,
FitBulge-> True,
YDFixed -> 0.5,
YBFixed -> 0.7,
PrintFittedParameters -> True,
ParametersSymbols -> {} (*Example: {a0 \[Rule] "Subscript[a, 0]", YD \[Rule] "Subscript[\[CapitalUpsilon], D]" , YB\[Rule] "Subscript[\[CapitalUpsilon], B]"}*),
FLabel-> {}
};

MAGMAplots[ModelName_,Variation_, OptionsPattern[]] := 
Block[{modelname = ToString[ModelName], variation=ToString[Variation], galaxies2plot = OptionValue[Galaxies], galnamePrint,ResultFiles, galname,Vmodel,VobsE, posYD, posYB, posdf2, Rmax, dataResidues, SaveLinTicksOptions}, 

SaveLinTicksOptions = Options[LinTicks];
SetOptions[LinTicks, MajorTickLength->{0.025,0},MinorTickLength->{0.010,0}, MajorTickStyle-> AbsoluteThickness[1],MinorTickStyle-> AbsoluteThickness[1] ];

(******* BEGIN: File handling ********)
CheckDirectories[{"results-chi2", "results-plots"}];

ResultFiles = Sort[FileNames[filein["results-chi2","*Min-"<>modelname<>"-var-"<>variation<>".m"]]]; 

If[galaxies2plot=== All, galaxies2plot = Length[ResultFiles]];
(******* END: File handling ********)

(******************************)
(******* THE MAIN LOOP ********)
(******************************)
Do[
Clear[YD, YB, df2, best ];
Get[ResultFiles[[galnumber]]] ; 
galname = galaxy[galnumber]; (*a nickname for the current galaxy*)

Echo[Row[{"Galaxy: ", galname, " (", galnumber, ")"}]];

(******* BEGIN: Values of special variables and the curves to be ploted********)
posYD = Position[Last[best], YD];
If[Length[posYD]==0, 
YD=OptionValue[YDFixed], 
YD = YD /. Last[best]];

posYB = Position[Last[best], YB];
If[Length[posYB]==0, 
YB=OptionValue[YBFixed], 
YB = YB /. Last[best]];

posdf2 = Position[Last[best], df2];
If[Length[posdf2]==0, 
df2=1, 
df2 = df2 /. Last[best]];

VobsE = VobsExp /. EB -> ErrorBar;
Vmodel = { VobsE[[#,1,1]], VmodelVec[[#]]}&  /@ Range[Length[VobsE]];
dataResidues = {{VobsE[[#,1,1]], VobsE[[#,1,2]] - VmodelVec[[#]]}, VobsE[[#,2]]}& /@ Range[Length[VobsE]];

VgasI[R_] =  Interpolation[Prepend[Vgasplot, {0,0}], Method-> Spline, InterpolationOrder->2][R]; 
VdiskI[R_] = Interpolation[Prepend[Vdiskplot,{0,0}], Method-> Spline, InterpolationOrder->2][R]; 
VbulgeI[R_] = Interpolation[Prepend[Vbulgeplot, {0,0}], Method-> Spline, InterpolationOrder->2][R] ;
VmodelI[R_] = Interpolation[Prepend[Vmodel, {0,0}], Method-> Spline, InterpolationOrder->2][R];

VnonBaryonicVector =  sroot[s2[VmodelVec] - s2[Vgasplot[[All,2]]] - s2[Vdiskplot[[All,2]]] - s2[Vbulgeplot[[All,2]]]];
VnonBaryonic = Transpose@Join[{Vgasplot[[All,1]]}, {VnonBaryonicVector}];VnonBaryonicI[R_] = Interpolation[Prepend[VnonBaryonic, {0,0}], Method-> Spline, InterpolationOrder->2][R];

Rmax = Last[VobsE[[All,1,1]]]; 

(******* END: Values of special variables and the curves to be ploted********)

galnamePrint = 
If[StringTake[galname,3] == "ESO" || StringTake[galname,3] =="NGC" || StringTake[galname,3] =="DDO" || StringTake[galname,3] =="UGC" || StringTake[galname,3] =="PGC",   
StringTake[galname,3]<> " "<>StringDrop[galname,3],
galname];(*Prepares galname to be printed. Inserts a space in the galaxy name, ESO1234 \[Rule] ESO 1234.*)

Clear[YD,YB,df2];
parametersPrint =best[[2,All,1]] /. OptionValue[ParametersSymbols];
parametersSolPrint = best[[2,All,2]];
If[ Length[posdf2] ===0,Null,posdf2 = posdf2[[1,1]]];

If[OptionValue[PrintFittedParameters],
parplotlabel = Table[
Row[{parametersPrint[[k]], Style[" = ",14] , Style[ScientificForm[parametersSolPrint[[k]],2, ExponentFunction->(If[-3<#<3,Null,#]&)],14]}], 
{k,Length[parametersPrint]}];
plotlabel = Column[{Style[galnamePrint , Large], Row[parplotlabel, ", "]}, Center],
plotlabel = Style[galnamePrint , Large]];

plotstyle = {{Black,Thickness[0.002] },{colorplot[16],Thickness[0.006],Dashing[{0.025}]}, {colorplot[16],Thickness[0.006],DotDashed}, {colorplot[2],Thickness[0.006],Dotted}, {Darker[Cyan,0.4],Thickness[0.005],Dashing[0.075]}, {Black, Thickness[0.007]}};

plotfunctions = {0,VdiskI[R],VbulgeI[R], VgasI[R],VnonBaryonicI[R], VmodelI[R] };

If[\[Not]bulge,
plotfunctions =  Drop[plotfunctions, {3}]; plotstyle= Drop[plotstyle, {3}]];

plot=Labeled[Show[ 
nELPlot[VobsE, {}, Markers-> circlecross,MarkersColor -> colorplot[14], MarkersSize -> 7, LinesColor-> colorplot[14], ImagePadding->{{40,5}, {20,5}}],nELPlot[dataResidues, {}, Markers-> square90,MarkersSize -> 7, MarkersColor-> Gray, LinesColor-> Gray, ImagePadding->{{40,5}, {20,5}}],  Plot[ plotfunctions, {R,0, Rmax }, PlotRange->All, PlotStyle->  plotstyle], PlotRange->  {{0,(Rmax  + Rmax/50)}, All}, PlotLabel-> plotlabel, ImageSize-> Medium],OptionValue[FLabel],{Bottom,Left}, RotateLabel->True];

Export[filein["results-plots",ToString[galname<>"-Plot-" <> modelname <> "-var-"<>variation<>".pdf"]], plot, "PDF"],
{galnumber,galaxies2plot}
];

SetOptions[LinTicks, SaveLinTicksOptions];
];



(* ::Input::Initialization:: *)
End[]; 
