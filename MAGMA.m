(* ::Package:: *)

(* ::Text:: *)
(*MAGMA - Mathematica Automatized Galaxy Mass Analysis*)
(*Version 0.3 *)
(*Licensed under GPL-3.0*)
(**)
(*By Davi C. Rodrigues 2018*)
(*davi.cosmo-ufes.org*)
(**)
(*MAGMA@Github:  https://github.com/davi-rodrigues/MAGMA*)
(**)
(*MAGMA.m = Main file package*)
(*-------------------------------*)
(**)


(* ::Input::Initialization:: *)
SetDirectory[NotebookDirectory[]];
AppendTo[$Path,FileNameJoin[{DirectoryName[$InputFileName], "code-magma"}]];
AppendTo[$Path,FileNameJoin[{DirectoryName[$InputFileName], "aux-magma"}]];




BeginPackage["MAGMA`"];

kpc = 3.08568025*10^16;(*Converts kpc to km*)
G0 = 4.518236657457777`*^-39; (*Gravitational constant in kpc^3/(Msun*s^2)*)
ckpc = 9.715603488080142`*^-12; (*speed of light in kpc/s*)

Print[Style["MAGMA - Mathematica Automatized Galaxy Mass Analysis (2018)", "Title", 15]];
Print["MAGMA master file v 0.3(", DateString[FileDate[$InputFileName],"ISOOrdinalDate"], ")."];
Print["Type '? MAGMA' for general help."];


MAGMA::usage = "...";
MAGMAfit::usage=
"MAGMAfit[model name, variation name, {model parameters to be fitted}, {expected ranges on the parameters}]. 
There are the following options:
...";
MAGMAtable::usage=".....";
ModelDefine::usage = "...";
MAGMAplots::usage=".....";

{YD, YB, df2, bulge, a0, RhoS, rS, Galaxies,FitDistance ,FitDisk,FitBulge,Constraints,YDFixed ,YBFixed ,DistanceTolerance ,VModelTestSign ,ParallelMinimizations, MinimizationInstances,SimplifyChi2,TimeLimitRun1, TimeLimitRun2 , MaxIterationsRun1, MaxIterationsRun2, PrecisionGoalRun1, PrecisionGoalRun2 , SearchPointsRun1, SearchPointsRun2,ToleranceRun1,ToleranceRun2, ImaginaryConstraint}; (*Options of MAGMAfit*)
{best, Chi2red, VobsExp,Vgasplot, Vdiskplot, Vbulgeplot , VmodelVec, Chi2,Chi2TrackDistance, EB, chiTD}; 
{OutputFileName, LinesToDropFromGlobalDataFile, AuxiliaryDataFiles,HeaderLineGlobalDataFile , HeaderGlobalDataFile ,FittedParametersFunctionTransformList ,ChihValuesList , ChiRValuesList, HItoGasFactor,
mBayesFiles ,mBayesParameter, mBayesNumberofSigmas, header, TableResults, FLabel};
{PrintFittedParameters, ParametersSymbols};


Get["MAGMAfits.m"];
Get["MAGMAtable.m"];
Get["MAGMAplots.m"];


EndPackage[]

