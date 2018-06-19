(* ::Package:: *)

(* ::Input::Initialization:: *)
(*MAGMA - Mathematica Automatized Galaxy Mass Analysis*)
(*MAGMAtable v0.5*)
(*Davi C. Rodrigues (2018)*)

(*Description: Generates a table that summarizes the results from all galaxies. The table is exported to both an .m file and a .csv one.*)

versionMAGMAtable = "0.5("<> DateString[FileDate[$InputFileName],"ISOOrdinalDate"]<>")";
Print["Starting MAGMAtable v"<> versionMAGMAtable];


(* ::Input::Initialization:: *)
Begin["`PrivateTable`"];


(* ::Input::Initialization:: *)
ClearList[x_] := Clear[x];
SetAttributes[ClearList, {HoldAll, Listable}]; (*Applies to both variables or list of variables.*)

HeadList[x_] := Head[x];
SetAttributes[HeadList, Listable];(*Applies to both variables or list of variables.*)

SetAttributes[ImposePrecision, Listable];
ImposePrecision[number_, precision_?NumberQ] := 
If[NumberQ[number] && number ==0, 
number, 
If[NumberQ[number], 
 N[Round[number, 10^(Floor[Log[10,Abs[number]]] - precision + 1)], precision], 
number]];

SetAttributes[ImposePrecisionRule, Listable];
ImposePrecisionRule[number_, precision_?NumberQ] := 
If[NumberQ[number] , 
 ImposePrecision[number,precision],
If[Head[number]== Rule && NumberQ[number[[2]]] && Length[number] == 2,
Rule[number[[1]],ImposePrecision[number[[2]],precision]],
Print["ImposePrecisionRule error: neither a number nor a compatible rule."]
]
]; (*Rounds a number to a given amount of precision. Any information beyond the imposed precision is lost. By exporting such number the exported version is identical to the one printed.*)

TSI[x_] := ToString[x, FormatType -> InputForm ]; (*Defines TSI function, ToString with InputForm*)

filein[directoryname_,file_] := FileNameJoin[{Directory[], directoryname, file}]; 

CheckDirectories[directorynames_?ListQ] := If[DirectoryQ[FileNameJoin[{Directory[], #}]], Null, Print["Directory "<>#<>" does not exist. Aborting."]; Abort[]] &/@ directorynames;

CheckDependences[functionname_?StringQ, dependences_?ListQ] := If[Or@@((HeadList[dependences][[#]] == Symbol ) & /@ Range[Length[dependences]]) , Echo["Some dependence of "<>functionname<>" is missing."];Print[TableForm[Join[{dependences},{HeadList[dependences]}]]];Echo["Aborting."];Abort[]];



(* ::Input::Initialization:: *)
Options[MAGMAtable] = {
Galaxies-> All,
FitDistance -> False,
FitDisk-> True,
FitBulge-> True,
YDFixed -> 0.5,
YBFixed -> 0.7,
OutputFileName-> Automatic,
LinesToDropFromGlobalDataFile -> 1, (*Assumes the first line is the header, and eliminates it.*)
AuxiliaryDataFiles-> None, (*Especify a list with names of auxiliary files. Each filemust be formated with the first line being the header, and the first column the galaxy names.*)
HeaderLineGlobalDataFile -> 1, (*The line *)
HeaderGlobalDataFile -> None, (*If different from None, it will be used in place of HeaderLineGlobalDataFile.*)
FittedParametersFunctionTransformList -> None,
HItoGasFactor -> 1.33,
mBayesFiles -> None, (*Specify a list of files if results from mBayes should appear in the final table, example: {"*Min-MOND-var-1-run1-sigmatab*.txt"}. These files must be in a subfolder called "mBayes-results".*)
mBayesParameter -> None, (*Specify a single parameter that was analysed with mBayes. Example: {a0}.*)
mBayesNumberofSigmas -> {1,2,3}(*A list that specifies which sigma results should be exported*)
};

MAGMAtable[ModelName_,Variation_, FittedParameters_?ListQ , GlobalDataFileName_?StringQ, OptionsPattern[]] := Block[{modelname = ToString[ModelName], variation=ToString[Variation], nsigmas = OptionValue[mBayesNumberofSigmas], colReff, colh, colLD, colLB, colL, colD, colYD, colYB,  colMHI, colMgas, coldf2, colMB, colMD, colMbar, colChi2, colChi2red, colNdata},

Clear@@col;

(******* BEGIN: File handling ********)
CheckDirectories[{"results-chi2", "results-tables"}];

GlobalData = Drop[Import[GlobalDataFileName, "Table"],OptionValue[LinesToDropFromGlobalDataFile]] ;
ResultFiles = Sort[FileNames[filein["results-chi2","*Min-"<>modelname<>"-var-"<>variation<>".m"]]]; 

If[OptionValue[mBayesFiles] === None, 
mBayesfilesV={}, 
(*else*)
CheckDirectories[{"mBayes-results"}];
If[Head[OptionValue[mBayesFiles]]===List,
mBayesfilesV = 
Flatten@Sort@
(FileNames[filein["mBayes-results",#]]& /@ OptionValue[mBayesFiles]),
(*else*)
Print["The option mBayesFiles is neither None nor a List. Aborting."];Abort[]
]
];

If[Head[OptionValue[AuxiliaryDataFiles]]===List,
AuxFiles = Sort[FileNames[OptionValue[AuxiliaryDataFiles]]],
(*else*)
If[Head[OptionValue[AuxiliaryDataFiles]] === None,
Null,
(*else*)
Print["The option AuxiliaryDataFiles is neither None nor a List. Aborting."];Abort[]
]
];

startname[i_] := StringPosition[ResultFiles[[i]],FileNameJoin[{Directory[], "results-chi2"}]][[1,2]] + 2;
endname[i_] := StringPosition[ResultFiles[[i]], "-Min-"][[1,1]] - 1; (*the position of the last galaxy name character*)
endmodel[i_] := StringPosition[ResultFiles[[i]], "-var-"][[1,1]] - 1;
endvar[i_] := StringPosition[ResultFiles[[i]], ".m"][[1,1]] - 1;
galaxy[i_] := StringTake[ResultFiles[[i]], {startname[i], endname[i]}]; 

(******* END: File handling ********)

(****** BEGIN: Header *********)
If[OptionValue[HeaderGlobalDataFile] === None,
headerObs = GlobalData[[OptionValue[HeaderLineGlobalDataFile]]],
(*else*)
headerObs = OptionValue[HeaderGlobalDataFile]
];

headerbasicChi =  {"N.data points","Chi2", "Chi2red"};

headerMass = {"MD", "MB", "Mbar"};

headermBayes = 
If[OptionValue[mBayesFiles]===None,
{},
(*else*)
Flatten[
Join[{ToString[First[OptionValue[mBayesParameter]]]<>"_mBayes"},
Table[{ToString[First[OptionValue[mBayesParameter]]] <> " S"<>ToString[j]<>"-"  , ToString[First[OptionValue[mBayesParameter]]] <> " S"<>ToString[j]<>"+"}, 
{j,OptionValue[mBayesNumberofSigmas]}]
]
]
];

headerAux=
If[Head[AuxFiles] ===List,
(auxfile[#] = Import[AuxFiles[[#]], "Data"]) & /@ Range[Length[AuxFiles]];
Join[
Table[
Drop[Flatten[Take[auxfile[i],1]],1],
{i,Length[AuxFiles]}]
] // Flatten,
(*else*)
{}
];

header = Join[Sequence@@{headerObs, headerAux, ToString /@ FittedParameters, headermBayes, headerMass, headerbasicChi}];
(******** END: header *********)

(******** BEGIN: Column identification *********)
colGalaxyName=1; (*the columun with the galaxy name is always assumed to be the first one.*)

specialheadernames = {"Reff", "h", "LD", "LB", "L", "D", "YD", "YB", "MHI", "Mgas", "df2", "MB","MD", "Mbar", "Chi2", "Chi2red","N.data points"};
correspondingcolnumbers = {colReff, colh, colLD, colLB, colL, colD, colYD, colYB,  colMHI, colMgas, coldf2, colMB, colMD, colMbar, colChi2, colChi2red, colNdata} ;

MapThread[If[Position[header, #1] == {},Null,  #2 = Position[header, #1][[1,1]]]&,{specialheadernames, correspondingcolnumbers}];

posaux = Position[correspondingcolnumbers, _Integer];
identifiedcolumns = (Take[specialheadernames, #] & /@ posaux);

Echo[Flatten[identifiedcolumns ], "Identified columns: "];

(******** END: Column identification *********) 

(******** BEGIN: Header of the CSV file Stream *******)
exportfilename= FileNameJoin[{"results-tables",modelname <>"-"<> variation<>"-MAGMAtableResults"}];
stream = OpenWrite[exportfilename<>".csv", PageWidth-> Infinity];
WriteLine[stream, "# MAGMA - Mathematica Automatized Galaxy Mass Analysis (2017)"];
WriteLine[stream, "# Model: "<>modelname<>" - var: "<>variation];
WriteLine[stream, "# Number of galaxies: "<>ToString[Length[ResultFiles]]];
WriteLine[stream, "# "<>DateString[] <> " - current user: " <> TSI[$UserName]<>" - Mathematica version: "<>TSI[$Version]];
WriteLine[stream,"# "];
WriteString[stream, ToString[FortranForm[header[[#]]]]<> ","] & /@ Range[Length[header]-1];
WriteString[stream,ToString[FortranForm[header[[Length[header]]]]]<> "\n"];
(******** END: Header of the CSV file Stream *******)

(***********************)
(*    THE MAIN LOOP    *)
(***********************)

TableResults = {header}; (*Further lines will be appended*)

Do[
Get[ResultFiles[[galnumber]]] ; (*loads the variables: bulge, best, Chi2red, VobsExp, Chi2, Chi2TrackDistance...*)
galname = galaxy[galnumber]; (*a nickname for the current galaxy*)

(*Data from the main global data file:*)
lineObs = Select[GlobalData, Quiet[#[[colGalaxyName]] ==galname] &] // First; 

(*Data from the auxiliary files: *)
If[Head[AuxFiles] ===List,
Do[
lineAux[i] = Drop[Select[auxfile[i], #[[colGalaxyName]] ==galname &] // First,1], (*Drop removes the galaxy name part.*)
{i,Length[AuxFiles]}
];
lineAuxAll = Flatten[Table[lineAux[j],{j,Length[AuxFiles]}]],
(*else*)
lineAuxAll = {}
];

(*The fitted parameters values:*)
FPsol =FittedParameters /. Last[MAGMA`best] ; (*FPsol has the same order of FittedParameters*)
FPsolResults = If[NumericQ[#] == False, "--", #] & /@ FPsol;(*If some parameters were not fitted for this galaxy FPsolResults receives "--" in their place. *)

(*Computation of the masses: *)
df2sol=If[NumberQ[coldf2par], MAGMA`df2/.Last[MAGMA`best], 1]; 
YDsol = If[OptionValue[FitDisk], MAGMA`YD /. Last[MAGMA`best], OptionValue[YDFixed]];
YBsol = If[OptionValue[FitBulge], MAGMA`YB /. Last[MAGMA`best], OptionValue[YBFixed]];

If[NumberQ[colLB], Null, Print["No column for LB was found. Aborting."]; Abort[]];
If[NumberQ[colLD] || NumberQ[colL], Null, Print["No column for LD or L was found. Aborting."]; Abort[]];
LBsol= Flatten[Join[lineObs,lineAuxAll]][[colLB]];
LDsol = 
If[NumberQ[colLD],
Flatten[Join[lineObs,lineAuxAll]][[colLD]],
(*else*)
Flatten[Join[lineObs,lineAuxAll]][[colL]] - LBsol
];

MD := df2sol^2 LDsol YDsol; 
MB := df2sol^2 LBsol YBsol;
Mgas := df2sol^2 If[NumberQ[colMgas], lineObs[[colMgas]], OptionValue[HItoGasFactor] lineObs[[colMHI]]];
Mbar := MB + MD + Mgas;

massesResults={MD, MB, Mbar}; (*Computes the masses considering the distance correction (df2sol)*)
ClearAll[LBsol,LDsol, YBsol,YDsol,def2sol];

(*Computation of N. of data points, Chi2 and related quantities: *)
basicChiResults = {Length[MAGMA`VobsExp] (*N. of data points*),First[MAGMA`best] (*Chi2*), MAGMA`Chi2red};

(*Preparing and exporting mBayes results: *)

filemBayesGalaxy = Select[mBayesfilesV, StringMatchQ["*"<>galname<>"*"]]; (*Selects the name of the file that contains the mBayes data of this galaxy*)
If[Length[filemBayesGalaxy] >0, 
sigmadata = Flatten[Take[Drop[Import[filemBayesGalaxy[[1]], "Data"],1], {#}] & /@ nsigmas,1];
sigmaR = Flatten[sigmadata[[All, {2,3}]]];
sigmaResults=  If[NumericQ[#],# , "--"] & /@ sigmaR;
mBayesParResult = {sigmadata[[1, 1]]},
(*else*)
sigmaResults={};
mBayesParResult={};
]; (*A table with all the sigma data*)


(*The newline to be inserted in TableResults: *)
newline =Flatten[{lineObs, lineAuxAll, FPsolResults,mBayesParResult,  sigmaResults, massesResults, basicChiResults}];

(*Exporting the newline: *)
TableResults = Join[TableResults, {newline}];
WriteString[stream, ToString[FortranForm[ImposePrecision[newline[[#]],5]]]<> ","] & /@ Range[Length[newline]-1];
WriteString[stream,ToString[FortranForm[ImposePrecision[newline[[-1]],5]]]<> "\n"];,{galnumber, Length[ResultFiles]}
];

If[Dimensions[Dimensions[TableResults]] != {2},
Echo["Inconsistent dimensions. TableResults is not a matrix", "Error: "]];

Quiet[DeleteFile[exportfilename<>".m"]];
Save[exportfilename<>".m", TableResults];
Close[stream];
Echo[exportfilename,"Files saved to: "];
Echo["TableResults","Table saved in variable "];
TableResults
];




(* ::Input::Initialization:: *)
End[]; 
