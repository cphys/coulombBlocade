ClearAll["Global`*"]

numberOfEnValsToPlot = 
  9; (*for now numberOfEnVals must be Odd, To reproduce fig in paper set to 9*)

(*Plot Parameters*)
ymax = 2;
yvals = {0, 2.1};
ytix = Prepend[SetAccuracy[Table[i, {i, .5, ymax, .5}], 2], 0];
ps = Table[
   If[
    OddQ[i],
    {Black, Thickness[0.005]},
    {Red, Thickness[0.005]}],
   {i, 1, numberOfEnValsToPlot}];

(*ytix=Table[NumberForm[i,{3,1}],{i,0,2,.5}]*)
spacingBetweenPlots = -210;
iPadFirstPlot = {{80, 10}, {100, 36}};

(*Energy of the System*)
Egs[Num_, ng_, \[CapitalDelta]_] := If[OddQ[Num], (Num - ng)^2 + \[CapitalDelta], (Num - ng)^2]

(*Gives the x coordiante of the point which two neighboring parabalas cross*)
x1[Num_, \[CapitalDelta]_] := Solve[Egs[Num, ng, \[CapitalDelta]] == Egs[Num + 1, ng, \[CapitalDelta]], ng][[All, 1, 2]][[1]]
x2[Num_, \[CapitalDelta]_] := Solve[Egs[Num, ng, \[CapitalDelta]] == Egs[Num - 1, ng, \[CapitalDelta]], ng][[All, 1, 2]][[1]]
x3[Num_, \[CapitalDelta]_] := Solve[Egs[Num, ng, \[CapitalDelta]] == Egs[Num + 2, ng, \[CapitalDelta]], ng][[All, 1, 2]][[1]]

(*Gives the y coordiante of the point which two neighboring parabalas cross*)
y1[Num_, \[CapitalDelta]_] := Egs[Num, x1[Num, \[CapitalDelta]], \[CapitalDelta]]
y2[Num_, \[CapitalDelta]_] := Egs[Num, x2[Num, \[CapitalDelta]], \[CapitalDelta]]
y3[Num_, \[CapitalDelta]_] := Egs[Num, x3[Num, \[CapitalDelta]], \[CapitalDelta]]

(*Gives the conductance point, the lowest energy point in which two of the parabalas cross*)
CondPoint1[Num_, \[CapitalDelta]_] :=
 If[
  If[y1[-2, \[CapitalDelta]] < y3[-2, \[CapitalDelta]],
     {x1[Num, \[CapitalDelta]], y1[Num, \[CapitalDelta]]},
     {x3[Num, \[CapitalDelta]], y3[Num, \[CapitalDelta]]}][[2]] >= 2,
  If[y1[-2, \[CapitalDelta]] < y3[-2, \[CapitalDelta]],
   {x1[Num - 1, \[CapitalDelta]], y1[Num - 1, \[CapitalDelta]]},
   {x3[Num - 1, \[CapitalDelta]], y3[Num - 1, \[CapitalDelta]]}],
  If[y1[-2, \[CapitalDelta]] < y3[-2, \[CapitalDelta]],
   {x1[Num, \[CapitalDelta]], y1[Num, \[CapitalDelta]]},
   {x3[Num, \[CapitalDelta]], y3[Num, \[CapitalDelta]]}]]

(*Table of the Energy parabalas to be plotted*)
energyTable[totalNumb_, ng_, \[CapitalDelta]_] := 
 Table[Egs[i - (totalNumb + 1)/2, ng, \[CapitalDelta]], {i, 1, totalNumb}]

(*Function which plots parabalas. Blue Points representing conductance crossings are given in Epilog*)

plotFuncs[\[CapitalDelta]_] := 
 Plot[Evaluate[energyTable[numberOfEnValsToPlot, ng, \[CapitalDelta]]],
  {ng, -(numberOfEnValsToPlot/2), numberOfEnValsToPlot/2},
  PlotRange -> {{-((numberOfEnValsToPlot - 2)/2), (
     numberOfEnValsToPlot - 2)/2}, yvals},
  PlotTheme -> "Scientific",
  PlotStyle -> ps,
  AspectRatio -> .5,
  FrameLabel -> {{None, "\!\(\*FractionBox[\(E\), SubscriptBox[\(E\), \(c\)]]\)"}, {"\!\(\*SubscriptBox[\(Q\), \(o\)]\)/e", None}},
  LabelStyle -> {
    FontFamily -> "Latex",
    FontSize -> 24,
    FontColor -> Black},
  RotateLabel -> False,
  ImageSize -> 800,
  Epilog -> {
    PointSize[0.02],
    Blue,
    Point[Table[CondPoint1[ Num, \[CapitalDelta]], {Num, -Quotient[numberOfEnValsToPlot, 2], Quotient[numberOfEnValsToPlot, 2]}]]},
  FrameTicks -> {{None, ytix}, {Automatic, None}}]

(*This Plot gives Subscript[n, g ]as a function of B and \[CapitalDelta](B) showing an energy gap of e for small \[CapitalDelta] and 2e for large*)

fig1plt = 
  ListPlot[ParallelTable[{\[CapitalDelta], CondPoint1[Num, \[CapitalDelta]][[1]]}, {\[CapitalDelta], 0, 1.25, .001}, {Num, -Quotient[numberOfEnValsToPlot, 2], Quotient[numberOfEnValsToPlot, 2]}],
   PlotTheme -> "Scientific",
   PlotStyle -> Black,
   PlotRange -> {Full, {-Quotient[numberOfEnValsToPlot, 2]*1.1, 
      Quotient[numberOfEnValsToPlot, 2]*1.1}}, 
   FrameTicks -> {{Automatic, None}, {{{0, "\!\(\*SubscriptBox[\(B\), \(c\)]\)"}, {0.5, Style["\[LeftArrow]B", 28]}, {1, "\!\(\*SuperscriptBox[\(B\), \(*\)]\)"}, {1.25, "0"}}, {{0, "0"}, {0.5, Style["\[CapitalDelta](B)\[Rule]", 28]}, {1, "\!\(\*SubscriptBox[\(E\), \(c\)]\)"}, {1.25, "\!\(\*SubscriptBox[\(\[CapitalDelta]\), \(o\)]\)"}}}},
   LabelStyle -> {
     FontFamily -> "Latex",
     FontSize -> 24,
     FontColor -> Black},
   RotateLabel -> False,
   AspectRatio -> 1,
   ImageSize -> 460,
   FrameLabel -> {{Style["\!\(\*SubscriptBox[\(n\), \(g\)]\)", 28], None}, {None, None}},
   ImagePadding -> iPadFirstPlot];

line[\[CapitalDelta]_] := Show[fig1plt, Graphics[{Blue, Dashed, Line[{{\[CapitalDelta], -Quotient[numberOfEnValsToPlot, 2]}, {\[CapitalDelta], Quotient[numberOfEnValsToPlot, 2]}}]}]]

(*Table combining plots for animated gif*)
tableOfPlots = ParallelTable[ GraphicsRow[{line[\[CapitalDelta]], plotFuncs[\[CapitalDelta]]}, Spacings -> spacingBetweenPlots], {\[CapitalDelta], 0, 2, .1}];

exportFileName = NotebookDirectory[] <> "coulombBlocadeAnimation.gif"
Export[exportFileName, tableOfPlots, "DisplayDurations" -> 0.5, AnimationRepetitions -> \[Infinity]];
