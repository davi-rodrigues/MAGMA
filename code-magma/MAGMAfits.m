(* ::Package:: *)

(* ::Input::Initialization:: *)
(*MAGMA - Mathematica Automatized Galaxy Mass Analysis*)
(*MAGMAfits v0.6*)

(*Davi C. Rodrigues (2018)*)

(*Description: perform galaxy model fits based on rotation curve data and a given model for dark matter or non-Newtonian dark matter-like effects.*)

versionMAGMAfits = "0.6("<> DateString[FileDate[$InputFileName],"ISOOrdinalDate"]<>")";
Print["Starting MAGMAfits v"<> versionMAGMAfits];


(* ::Input::Initialization:: *)
(******** START MODEL SPECIFIC ENTRIES. **********)

ModelDefine[x_]  := 
Block[{VVmodel, aNewt, constraintsaux},
aNewt[R_] =  1/(R kpc ) (VVgasI[R]+  VVdiskI[R] +  VVbulgeI[R]);  (*the Newtonian acceleration (in km/s^2)*) 
Which[
x =="MOND",  
	VVmodel[R_] =  R kpc  aNewt[R]/Sqrt[2] Sqrt[1+Sqrt[4 a0^2/aNewt[R]^2+1]]  , (*km^2/s^2*)
x == "MONDRAR",
	VVmodel[R_] = R  kpc   aNewt[R]/(1 - E^-Sqrt[Abs[aNewt[R]]/a0]),
x == "MONDsimple",
	VVmodel[R_] = R  kpc  aNewt[R]/2 (1+ Sqrt[4 a0/Abs[aNewt[R]]+1]),
(*else*)
True, Print["No valid model specified. Aborting."]; Abort[]
];
If[VModelSign, (Sign[VVmodel[#]]Sqrt[Abs[VVmodel[#]]] )& /@ Vobs[[All,1]], (Sqrt[VVmodel[#]] )& /@ Vobs[[All,1]]]
]; 
  (****** END MODEL SPECIFIC ENTRIES ****) 


(* ::Input::Initialization:: *)
Begin["`PrivateFits`"];


(* ::Input::Initialization:: *)
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

Constraints2Range[constraints_?ListQ, parameter_] :=
Block[{pos, item},
Quiet[Check[pos = Position[constraints, parameter][[1,1]], 
Return[{}]]];
Quiet[item = constraints[[pos]]];
If[Length[List@@item] == 3, 
Which[
Head[item] === Less,
Return[(List@@item)[[{2,1,3}]]],
Head[item] === Greater,
Return[(List@@item)[[{2,3,1}]]],
True,
Return[{}]],
{}]
]; (*Constraint2Range[{a,c, 1<YD<2, h, k}, YD] \[Rule] {YD,1,2}*)

PSroutine[NMstart_, NMend_] := Module[{aux, starttime, timewarning, passed, run,temp},
timewarning := (Print["Time limit passed. Duration of this run in seconds: ", passed]; WriteLine[streamlog,"Time limit passed. Duration of this run in seconds: " <> TSI[N[passed]]];);
If[Head[NMsol[nI]] === List, run=2, run=1]; (*detects whether the context is in the first or second run.*)
temp=PrintTemporary[aux = Table[ParallelSubmit[{i}, NM[i]], {i,NMstart,NMend}]]; (*Submit to parallel kernels and prints dynamical output*)
starttime = AbsoluteTime[];
 aux = WaitNext[aux]; (*The first answer*)
Write[streamlog, N[First[aux]]];
Do[
aux = WaitNext[Last[aux]]; (*The remaining answers*)
Write[streamlog, N[First[aux]]];
passed = AbsoluteTime[] -starttime;
If[run == 1&& passed  > timelimit1stRun && i<nI-1, timewarning;Quiet[CloseKernels[]];Break[]];
If[run == 2&& passed  > timelimit2ndRun && i<nI-1, timewarning;Quiet[CloseKernels[]];Break[]], {i,1,nI -1}]; 
NotebookDelete[temp]; (*deletes the temporary print*)
Select[NMsol/@ Range[NMstart,NMend], Head[#]==List &]]; (*Selects only those that have answers, which will be the output of PSroutine*)



(* ::Input::Initialization:: *)
If[DirectoryQ[FileNameJoin[{Directory[], #}]], Null, Print["Directory "<>#<>" does not exist."]; Abort[]]; &/@ {"data-global", "data-galaxies", "logs", "results-chi2"}; (*Tests whether the necessary directories exist.*)

filein[directoryname_,file_] := FileNameJoin[{Directory[], directoryname, file}]; 

files = Sort[FileNames[filein["data-galaxies","*.dat"]]]; (*All the .dat files in the data folder are considered as individual galaxies.*)
data = Join[Import[files[[#]], "Data"] & /@ Range[Length[files]]] ;(*Joins all the data from all the files in the vairable data*)

Join[Take[data[[1]],{2,3}] , {  Range[0, 8]}]// TF ;(*Prints the header with column numbers if the semicolon is removed*)
colRad = 1; (*set variables for each column number*)
colVobs = 2;
col\[Delta]Vobs = 3;
colVgas = 4;
colVdisk=5;
colVbulge = 6;
colSBdisk = 7;
colSBbul = 8;

galdata[i_] := Select[data[[i]], \[Not]#[[1]] == "#" & ] ; (*the complete data of the ith galaxy without the header, i.e. excluding the lines that begin with "# ".*)

startname[i_] := StringPosition[files[[i]],FileNameJoin[{Directory[], "data-galaxies"}]][[1,2]] + 2;
endname[i_] := StringPosition[files[[i]], "_rotmod"][[1,1]] - 1; (*the position of the last galaxy name character*)
galaxy[i_] := StringTake[files[[i]], {startname[i], endname[i]}]; 

distance[i_] := data[[i,1,4]]; (*distance of the ith galaxy, in Mpc*)



(* ::Input::Initialization:: *)
(*****THE MAIN LOOP STARTS HERE*****)
Options[MAGMAfit] = {
Galaxies-> All,
FitDistance -> False,
FitDisk-> True,
FitBulge-> True,
Constraints-> {YD>= 0, YB>= 0},
YDFixed -> 0.5,
YBFixed -> 0.7,
DistanceTolerance -> 0.20, 
VModelTestSign -> True, (*False is slightly faster and easier to simplify, but False is less robust.  *)
ImaginaryConstraint -> False,
ParallelMinimizations -> $ProcessorCount,(*The number of parallel computations to be launched. It can be larger than the number of available cores, but it should not be larger than the number of available kernels.*)
MinimizationInstances-> $ProcessorCount, (*The number of minimizations to perform in each of the two steps. It can be larger or smaller than the number of ParallelMinimizations. Commonly, it should be either equal to the latter or larger.*)
SimplifyChi2 -> False, (*Commonly, the improvements on the simplification are not worth the time spent, hence False is the standard option*)
TimeLimitRun1 ->  120 , (*seconds to wait for the first run paralell solutions. After this time passes, the computations will continue up to one more result, the remaning computations will be cancelled. There is a minimum of 2 sols/run.*)
TimeLimitRun2 ->  240 , (*the same as above, but for the second run*)
MaxIterationsRun1 ->  700, 
MaxIterationsRun2 ->  1400, 
PrecisionGoalRun1 ->  4,
PrecisionGoalRun2 ->  5,
SearchPointsRun1 ->  Automatic, (*the population number of DifferentialEvolution, Automaitc is the standard value.*)
SearchPointsRun2 ->  Automatic, 
ToleranceRun1 -> 10^-4, (* DifferentialEvolution tolerance for violation of constraints. 0.001 is the standard value of Differential evolution.*)
ToleranceRun2 -> 10^-5 
};
MAGMAfit[modelname_,variation_, modelPar2Fit_?ListQ, modelExpectedRange_?ListQ, OptionsPattern[]] := 
Block[{model = ToString[modelname],varnumber=ToString[variation], modelexpectedrange = modelExpectedRange, modelpar2fit=modelPar2Fit, VModelSign = OptionValue[VModelTestSign], nE = OptionValue[ParallelMinimizations], nI = OptionValue[MinimizationInstances], simplifyChi2=OptionValue[SimplifyChi2], timelimit1stRun= OptionValue[TimeLimitRun1], timelimit2ndRun= OptionValue[TimeLimitRun2], maxiterations1stRun = OptionValue[MaxIterationsRun1], maxiterations2ndRun = OptionValue[MaxIterationsRun2],precisiongoal1stRun=OptionValue[PrecisionGoalRun1], precisiongoal2ndRun=OptionValue[PrecisionGoalRun2], searchpoints1stRun = OptionValue[SearchPointsRun1], searchpoints2ndRun = OptionValue[SearchPointsRun2], tolerance1stRun = OptionValue[ToleranceRun1], tolerance2ndRun = OptionValue[ToleranceRun2], fitCaux = Simplify[And@@OptionValue[Constraints]] (*in fitCaux the constraints are simplified, but it is no more a list. fitConstraints is a list.*)}, 
Clear[Chi2];

fitConstraints = If[Head[fitCaux]===And ,  List@@(fitCaux), List@(fitCaux)];

If[OptionValue[Galaxies] === All, galaxies = Range[Length[files]], galaxies=OptionValue[Galaxies]];

If[NumberQ[OptionValue[ImaginaryConstraint]],
AppendTo[fitConstraints, Abs[Im[Chi2]]< OptionValue[ImaginaryConstraint] ]];

If[OptionValue[FitDistance],
AppendTo[fitConstraints,1+ OptionValue[DistanceTolerance]  > df2 > 1-OptionValue[DistanceTolerance] ]]; (*df2 is the factor that corrects the baryonic V^2 due to distance change.*)

Echo[varnumber, "Variation: "];
Do[
Clear[YD, YB, par2fitsol, bulge, df2, chiTD, galaxyfitConstraints, Chi2];
par2fit=modelpar2fit;
par2fitExpected = modelexpectedrange;
galname = galaxy[galnumber]; (*a nickname for the current galaxy name*)

Echo[Row[{galname, " (", galnumber, ")"}], "Galaxy: "];

gd = galdata[galnumber]; (*a nickname for the current galaxy data*)
gDist = distance[galnumber]; (*a ninckname for the current galaxy distance*)
Rmax = gd[[-1,colRad]];
If[OptionValue[FitDistance], Clear[df2], df2 = 1]; (*df2 is the factor that corrects the baryonic V^2 due to distance change, i.e. df2=1 means standard distance.*)

(*Preparing the variables to be fit, their expected range and constraints.*)
If[Total[(gd[[All, colVbulge]])^2] > 1, 
bulge = True, 
bulge = False]; (*verifies if there is a bulge from Vbulge data, and saves the result in "bulge" variable*)

galaxyfitConstraints = If[Length[fitConstraints]>1,List@@Simplify[And@@fitConstraints],
List@Simplify[And@@fitConstraints]];

Echo[Row[{"Constraints: ", galaxyfitConstraints}]];

YDexpectedRange = Constraints2Range[galaxyfitConstraints, YD];
YBexpectedRange = Constraints2Range[galaxyfitConstraints, YB];

If[Length[par2fit]!= Length[par2fitExpected], Print["modelpar2fit and modelexpectedrange are inconsistent. Stopping here."]; Print[par2fit];Print[par2fitExpected]; Abort[]];

If[OptionValue[FitDisk], 
AppendTo[par2fit,  YD];
AppendTo[par2fitExpected, YDexpectedRange],
YD = OptionValue[YDFixed]];
If[bulge && OptionValue[FitBulge], 
AppendTo[par2fit, YB];
AppendTo[par2fitExpected, YBexpectedRange]]; 
If[bulge && \[Not] OptionValue[FitBulge], 
YB = OptionValue[YBFixed]];
If[\[Not]bulge, YB = 0 ];
If[OptionValue[FitDistance], 
AppendTo[par2fit, df2];
AppendTo[par2fitExpected, {df2, 1- OptionValue[DistanceTolerance] , 1+ OptionValue[DistanceTolerance]}], 
df2 = 1 ];

(*Definig velocity related varibles, vectors, and functions. It considers all the df2 corrections. *)

Vobsaux  = gd[[All, {colRad, colVobs}]];
Vobs = {Vobsaux[[#,1]] df2, Vobsaux[[#,2]]} & /@ Range[Length[Vobsaux]]; 
VobsVec = Vobs[[All,2]]; (*A vector with the observed RC values*)
errorvector = gd[[All, col\[Delta]Vobs]];
errorvectorTrackDistance  = errorvector/(chiTD[#] & /@ Range[Length[errorvector]])  ;
VobsE = {{Vobs[[#,1]], Vobs[[#,2]]}, ErrorBar[errorvector[[#]]]} & /@ Range[Length[Vobs]];

Vdisk =  gd[[All,{colRad,colVdisk}]]; (*these matrixes do not include df2 or YD, YB corrections.*)
Vbulge =  gd[[All,{colRad,colVbulge}]];
Vgas =  gd[[All,{colRad,colVgas}]];

VgasI[R_] = Sqrt[df2] Interpolation[Prepend[Vgas, {0,0}], Method-> Spline, InterpolationOrder->2][R/df2];  (*These functions have full df2 corrections*) 
VdiskI[R_] =Sqrt[df2 YD ] Interpolation[Prepend[Vdisk,{0,0}], Method-> Spline, InterpolationOrder->2][R/df2]; 
VbulgeI[R_] =Sqrt[df2 YB ] Interpolation[Prepend[Vbulge, {0,0}], Method-> Spline, InterpolationOrder->2][R/df2] ;

VVdiskI[R_] = VdiskI[R] Abs[VdiskI[R]];
VVbulgeI[R_] = VbulgeI[R] Abs[VbulgeI[R]];
VVgasI[R_] = VgasI[R] Abs[VgasI[R]];

VmodelVec = ModelDefine[model]; (*Runs the model definition, and then defines a vector for the total model RC.*)



(*******  MINIMIZATION START **********)
(**Prologue**)

galtimestart = AbsoluteTime[];
Clear[par2fitsol, NM, NMsol];

filenameaux = filein["results-chi2",ToString[galname<>"-Min-" <> model <> "-var-"<>varnumber<>".m"]];
filenamelog = filein["logs", ToString[galname<>"-Min-" <> model <> "-var-"<>varnumber<>".log"]];

streamlog = OpenWrite[filenamelog, PageWidth-> 300];
WriteLine[streamlog, "Starting log file of "<> galname <> " (n."<>ToString[galnumber] <> "), "<>model<>", var: "<>varnumber];
WriteLine[streamlog, DateString[] <> ", current user: " <> TSI[$UserName]<>", N. of cores: "<>TSI[$ProcessorCount]<> ", MAGMAfits version: "<>TSI[versionMAGMAfits]<>", Mathematica version: "<>TSI[$Version]];

WriteLine[streamlog, "fitConstraints: " <>TSI[galaxyfitConstraints]];
WriteLine[streamlog, "par2fitExpected: " <>TSI[par2fitExpected]];
WriteLine[streamlog, ""];
WriteLine[streamlog, "Minimization parameters: nE=" <> TSI[nE] <> ", nI="<> TSI[nI]<>", timelimit1stRun="<> TSI[timelimit1stRun] <> ", timelimit2ndRun=" <> TSI[timelimit2ndRun] <> ", precisiongoal1stRun="<>TSI[precisiongoal1stRun]<>", precisiongoal2ndRun="<>TSI[precisiongoal2ndRun]<>", searchpoints1stRun="<>TSI[searchpoints1stRun]<>", searchpoints2ndRun="<>TSI[searchpoints2ndRun]<>", tolerance1stRun="<> TSI[tolerance1stRun]<>", tolerance2ndRun="<>TSI[tolerance2ndRun]];
WriteLine[streamlog, ""];

If[simplifyChi2,  
Chi2 = Simplify[Total[((VmodelVec - VobsVec)/errorvector)^2],Assumptions ->galaxyfitConstraints];WriteLine[streamlog, "Chi2 simplification time: " <> TSI[N[AbsoluteTime[]-galtimestart]]];
WriteLine[streamlog, ""], 
(*else*)
Chi2 =Total[((VmodelVec - VobsVec)/errorvector)^2]
]; 


Chi2TrackDistance = Total[((VmodelVec - VobsVec)/errorvectorTrackDistance)^2]; (*useful for commputing the quantities Chi2h, since it keeps track of each term in Chi2*)



(**first run**)
CloseKernels[];
LaunchKernels[nE];
ParallelEvaluate[Off[General::precw]; Off[RandomReal::precw]];

(NM[#] :=   NMsol[#] =  NMinimize[{Chi2, Sequence@@galaxyfitConstraints}, par2fitExpected, WorkingPrecision -> 15, PrecisionGoal -> precisiongoal1stRun, AccuracyGoal -> \[Infinity],MaxIterations -> maxiterations1stRun, Method -> {"DifferentialEvolution", "RandomSeed"-> #, "Tolerance" -> tolerance1stRun, "SearchPoints"-> searchpoints1stRun}] )& /@ Range[1,nI];

DistributeDefinitions[NM, Chi2, par2fitExpected, galaxyfitConstraints, nE, nI, tolerance1stRun, searchpoints1stRun, maxiterations1stRun, precisiongoal1stRun];
SetSharedFunction[NMsol];

results1stRun = PSroutine[1, nI];
best1stRun = First[Sort[results1stRun]];
par2fitsol = par2fit /.Last[best1stRun]; (*The results of the best fit are saved in the par2fitsol vector, they may be improved in the second run*)
WriteLine[streamlog, "---------End of the first run---------"];

text="The best Chi2 value for this galaxy is not numeric. Proceeding to the next galaxy.";
If[NumberQ[best1stRun[[1]]]== False, 
Print[text];
WriteLine[streamlog, text]; 
Continue[]];

text= "The constraints are not satisfied in the first run. If relevant, try decreasing ToleranceRun1.";
If[And@@Simplify[best1stRun[[2]] /. Rule -> Equal , Assumptions ->  galaxyfitConstraints/. {Greater -> GreaterEqual, Less -> LessEqual}] === False,
Print[text];
WriteLine[streamlog, text]]; 


(**second run**)
par2fitExpected = 
{par2fitExpected[[#,1]], 
If[par2fitsol[[#]] ==0.,Sequence@@Sort[{par2fitExpected[[#,2]]/5, par2fitExpected[[#,3]]/5}], 
(*else*)
Sequence@@Sort[{ 0.9 par2fitsol[[#]], 1.1 par2fitsol[[#]]}]]
} & /@ Range[Length[par2fit]]; (*Set the expected range to be about 10% the 1st run best fit, or divide the original expected range by 5 if the derived value was zero.*)

WriteLine[streamlog, "par2fitExpected: " <>TSI[N[par2fitExpected]]];
WriteLine[streamlog, " "];

CloseKernels[];
LaunchKernels[nE];

 ParallelEvaluate[Off[General::precw]; Off[RandomReal::precw]];

(NM[#] :=   NMsol[#] =  NMinimize[{Chi2, Sequence@@galaxyfitConstraints}, par2fitExpected, WorkingPrecision -> 18, PrecisionGoal -> precisiongoal2ndRun, AccuracyGoal -> \[Infinity],MaxIterations -> maxiterations2ndRun, Method -> {"DifferentialEvolution", "RandomSeed"-> #, "Tolerance" -> tolerance2ndRun, "SearchPoints"-> searchpoints2ndRun}] )& /@ Range[nI+1,2 nI ];

DistributeDefinitions[NM, Chi2, par2fitExpected, galaxyfitConstraints, nE, nI, tolerance2ndRun, searchpoints2ndRun, maxiterations2ndRun, precisiongoal2ndRun];
SetSharedFunction[NMsol];

results2ndRun = PSroutine[nI+1, 2nI];
best2ndRun = First[Sort[results2ndRun]];
WriteLine[streamlog, "---------End of the second run---------"];

text= "The constraints are not satisfied in the second run. If relevant, try decreasing ToleranceRun2.";
If[And@@Simplify[best2ndRun[[2]] /. Rule -> Equal , Assumptions ->  galaxyfitConstraints/. {Greater -> GreaterEqual, Less -> LessEqual}] === False,
Print[text];
WriteLine[streamlog, text]]; 


(**minimization epilogue**)

best = First[Sort[{best1stRun, best2ndRun}]];
Chi2red =First[best]/(Length[Vobs] - Length[par2fit]);

Echo[Row[{"Best fit: ", best}]];
Echo[Row[{"\!\(\*SubscriptBox[SuperscriptBox[\(\[Chi]\), \(2\)], \(red\)]\)=", Chi2red}]];

WriteLine[streamlog, "" ];
WriteLine[streamlog, "best=" <> TSI[N[best]] ];
WriteLine[streamlog,"Chi2red=" <> TSI[N[Chi2red]]];

If[best  == best1stRun && Abs[1 - First[best1stRun]/First[best2ndRun]] > 0.01, Print["The best run was the first one."]; WriteLine[streamlog, "The best run was the first one."]];

diffOfBest = Abs[(1 - #[[2]]/#[[1]])] & @  Sort[Join[best1stRun, best2ndRun]]; (* Relative difference between the two best solutions*)
If[ diffOfBest > 0.05, Print["Attention: Second run best fit isolated by ", diffOfBest, "%."]; WriteLine[streamlog, "Attention: Second run best fit is isolated by "<> TSI[N[diffOfBest]] <> "%."]];

WriteLine[streamlog,"Total time: " <> TSI[N[AbsoluteTime[] - galtimestart ]]];
Close[streamlog];


par2fitsol = par2fit /.Last[best]; 
(Evaluate[par2fit[[#]]] = par2fitsol[[#]]) & /@ Range[Length[par2fit]]; (*set the values of each of the parameters that were fitted to the best fit.*)

VobsExp = VobsE /. ErrorBar -> EB; (*A simple workaround to avoid an issue of using "Save" with "Errorbar", thus ErrorBar \[Rule] EB *)
Vdiskplot = {#,VdiskI[#]} &/@ Vobs[[All,1]];
Vbulgeplot ={#,VbulgeI[#]}&/@ Vobs[[All,1]];
Vgasplot = {#,VgasI[#]} &/@ Vobs[[All,1]];
VmodelVec = Simplify[VmodelVec];

ClearList[x_] := Clear[x];
SetAttributes[ClearList, {HoldAll, Listable}];
Last[MapAt[ClearList,First[OwnValues[par2fit]] ,2]]; (*Clear the values of the parameters, without changing the definition of par2fit*)
Clear[par2fit, par2fitsol];

best = ImposePrecisionRule[best,5];
Chi2red = ImposePrecision[Chi2red,5];
Vgasplot = ImposePrecision[Vgasplot,5];
Vdiskplot = ImposePrecision[Vdiskplot,5];
Vbulgeplot = ImposePrecision[Vbulgeplot,5];
VmodelVec = ImposePrecision[VmodelVec,5];
VobsExp = ImposePrecision[VobsExp,5];


Quiet[DeleteFile[filenameaux]];
Save[filenameaux, {bulge, best, Chi2red, VobsExp,Vgasplot, Vdiskplot, Vbulgeplot , VmodelVec, Chi2,Chi2TrackDistance} ];
Clear[bulge]
,{galnumber, galaxies}] (*Closes the largest Do*)
Quiet[Close[streamlog]];
];
(*******  MINIMIZATION ENDS **********)


(* ::Input::Initialization:: *)
End[]; 
