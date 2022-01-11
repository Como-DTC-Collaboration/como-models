## code to prepare `seirdage_structure` dataset goes here
library(DiagrammeR)
library(DiagrammeRsvg)

# seird -------
g <- DiagrammeR::grViz("
  digraph PrimC{
  graph [rankdir = 'LR']
  node [shape = circle, style=filled]
  S [fillcolor=deepskyblue]
  E [fillcolor=orange]
  I [fillcolor=coral]
  R [fillcolor=turquoise]
  D [fillcolor=tomato]
  S -> E [label = '&beta; S I']
  E -> I [label = 'k E']
  I -> R [label = '&gamma; I']
  I -> D [label = '&mu; I']
  }")
seird_structure <- DiagrammeRsvg::export_svg(g)

# seirdage --------
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle, style=filled]
    S_1[label = 'S&#x2081;', fillcolor=deepskyblue]
    E_1[label = 'E&#x2081;', fillcolor=orange]
    I_1[label = 'I&#x2081;', fillcolor=coral]
    R_1[label = 'R&#x2081;', fillcolor=turquoise]
    D_1[label = 'D&#x2081;', fillcolor=tomato]
    S_2[label = 'S&#x2082;', fillcolor=deepskyblue]
    E_2[label = 'E&#x2082;', fillcolor=orange]
    I_2[label = 'I&#x2082;', fillcolor=coral]
    R_2[label = 'R&#x2082;', fillcolor=turquoise]
    D_2[label = 'D&#x2082;', fillcolor=tomato]
    S_1 -> E_1 [label = '&beta; S&#x2081; (C&#x2081;&#x2081; I&#x2081; + C&#x2081;&#x2082; I&#x2082;)']
    E_1 -> I_1 [label = 'k E&#x2081;']
    I_1 -> R_1 [label = '&gamma;&#x2081; I&#x2081;']
    I_1 -> D_1 [label = '&mu;&#x2081; I&#x2081;']
    S_2 -> E_2 [label = '&beta; S&#x2082; (C&#x2082;&#x2081; I&#x2081; + C&#x2082;&#x2082; I&#x2082;)']
    E_2 -> I_2 [label = 'k E&#x2082;']
    I_2 -> R_2 [label = '&gamma;&#x2082; I&#x2082;']
    I_2 -> D_2 [label = '&mu;&#x2082; I&#x2082;']
    }")
seirdage_structure <- DiagrammeRsvg::export_svg(g)

# seird_bd ----------
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle, style=filled]
    S [fillcolor=deepskyblue]
    E [fillcolor=orange]
    I [fillcolor=coral]
    R [fillcolor=turquoise]
    D [fillcolor=tomato]
    nowhere [style=invis,shape=point]
    nowhere -> S [label = '&lambda;']
    S -> E [label = '&beta; S I']
    S -> D [label = '&nu; S']
    E -> I [label = 'k E']
    E -> D [label = '&nu; E']
    I -> R [label = '&gamma; I']
    I -> D [label = '(&nu; + &mu;) I']
    R -> S [label = '&delta; R']
    R -> D [label = '&nu; R']
}")
seird_bd_structure <- DiagrammeRsvg::export_svg(g)

# seird_ru ---------
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle, style=filled]
    S_U[label = 'S&#x1d64;', fillcolor=deepskyblue]
    E_U[label = 'E&#x1d64;', fillcolor=orange]
    I_U[label = 'I&#x1d64;', fillcolor=coral]
    R_U[label = 'R&#x1d64;', fillcolor=turquoise]
    D_U[label = 'D&#x1d64;', fillcolor=tomato]
    S_Y[label = 'S&#x1d67;', fillcolor=deepskyblue]
    E_Y[label = 'E&#x1d67;', fillcolor=orange]
    I_Y[label = 'I&#x1d67;', fillcolor=coral]
    R_Y[label = 'R&#x1d67;', fillcolor=turquoise]
    D_Y[label = 'D&#x1d67;', fillcolor=tomato]
    S_U -> E_U [label = '&beta; S&#x1d64; ((I&#x1d64; + I&#x1d67;)(φ&#x1d64; N&#x1d64; + φ&#x1d67; N&#x1d67;)C + I&#x1d64;∕φ&#x1d64;N&#x1d64;(1-C))']
    E_U -> I_U [label = 'k E&#x1d64;']
    I_U -> R_U [label = '&gamma; I&#x1d64;']
    I_U -> D_U [label = '&mu; I&#x1d64;']
    S_Y -> E_Y [label = '&beta; S&#x1d67;((I&#x1d64; + I&#x1d67;)(φ&#x1d64; N&#x1d64; + φ&#x1d67; N&#x1d67;)C + I&#x1d67;∕φ&#x1d67;N&#x1d67;(1-C))']
    E_Y -> I_Y [label = 'k E&#x1d67;']
    I_Y -> R_Y [label = '&gamma; I&#x1d67;']
    I_Y -> D_Y [label = '&mu; I&#x1d67;']
}")
seird_ru_structure <- DiagrammeRsvg::export_svg(g)

# seirdnpiage --------
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle]
    node [shape = circle, style=filled]
    S_1[label = 'S&#x2081;', fillcolor=deepskyblue]
    E_1[label = 'E&#x2081;', fillcolor=orange]
    I_1[label = 'I&#x2081;', fillcolor=coral]
    R_1[label = 'R&#x2081;', fillcolor=turquoise]
    D_1[label = 'D&#x2081;', fillcolor=tomato]
    S_2[label = 'S&#x2082;', fillcolor=deepskyblue]
    E_2[label = 'E&#x2082;', fillcolor=orange]
    I_2[label = 'I&#x2082;', fillcolor=coral]
    R_2[label = 'R&#x2082;', fillcolor=turquoise]
    D_2[label = 'D&#x2082;', fillcolor=tomato]
    S_1 -> E_1 [label = '&beta;(t) S&#x2081; (C&#x2081;&#x2081;(t) I&#x2081; + C&#x2081;&#x2082;(t) I&#x2082;)']
    E_1 -> I_1 [label = 'k E&#x2081;']
    I_1 -> R_1 [label = '&gamma;&#x2081; I&#x2081;']
    I_1 -> D_1 [label = '&mu;&#x2081; I&#x2081;']
    S_2 -> E_2 [label = '&beta;(t) S&#x2082; (C&#x2082;&#x2081;(t) I&#x2081; + C&#x2082;&#x2082;(t) I&#x2082;)']
    E_2 -> I_2 [label = 'k E&#x2082;']
    I_2 -> R_2 [label = '&gamma;&#x2082; I&#x2082;']
    I_2 -> D_2 [label = '&mu;&#x2082; I&#x2082;']
    }")
seirdnpiage_structure <- DiagrammeRsvg::export_svg(g)

# seiaimisrd --------
g <- DiagrammeR::grViz("
  digraph PrimC{
  graph [rankdir = 'LR']
  node [shape = circle, style=filled]
  S [fillcolor=deepskyblue]
  E [fillcolor=orange]
  R [fillcolor=turquoise]
  D [fillcolor=tomato]
  Ia [label = 'Ia', fillcolor=coral]
  Im [label = 'Im' fillcolor=coral]
  Is [label = 'Is' fillcolor=coral]
  S -> E [label = 'S (&beta;a Ia + &beta;m Im + &beta;s Is) ']
  E -> Ia [label = 'ηa k E']
  E -> Im [label = 'ηm k E']
  E -> Is [label = 'ηs k E']
  Ia -> R [label = '&gamma;a Ia']
  Ia -> D [label = '&mu;a Ia']
  Im -> R [label = '&gamma;m Im']
  Im -> D [label = '&mu;m Im']
  Is -> R [label = '&gamma;s Is']
  Is -> D [label = '&mu;s Is']
  }")
seiaimisrd_structure <- DiagrammeRsvg::export_svg(g)

# seimrd --------
g <- DiagrammeR::grViz("
  digraph PrimC{
  graph [rankdir = 'LR']
  node [shape = circle, style=filled]
  S [fillcolor=deepskyblue]
  I [fillcolor=coral]
  R [fillcolor=turquoise]
  D [fillcolor=tomato]
  E_1[label = 'E&#x2081;', fillcolor=orange]
  E_2[label = 'E&#x2082;', fillcolor=orange]
  E_3[label = 'E&#x2083;', fillcolor=orange]
  S -> E_1 [label = '&beta; S I']
  E_1 -> E_2 [label = 'k E&#x2081;']
  E_2 -> E_3 [label = 'k E&#x2082;']
  E_3 -> I [label = 'k E&#x2083;']
  I -> R [label = '&gamma; I']
  I -> D [label = '&mu; I']
  }")
semird_structure <- DiagrammeRsvg::export_svg(g)

# seird_ct
g <- DiagrammeR::grViz("
  digraph PrimC{
  graph [rankdir = 'LR']
  node [shape = circle, style=filled]
  R [label = '   R   ', fillcolor=turquoise]
  D [label = '   D   ', fillcolor=tomato]
  S [label = '   S   ', fillcolor=deepskyblue]
  E [label = '   E   ', fillcolor=orange]
  P [label = '   P   ', fillcolor=chocolate]
  A [label = '   A   ', fillcolor=gold]
  I [label = '   I   ', fillcolor=coral]
  Et [label = 'E&#x1d40;', fillcolor=orange]
  Pt [label = 'P&#x1d40;', fillcolor=chocolate]
  At [label = 'A&#x1d40;', fillcolor=gold]
  It [label = 'I&#x1d40;', fillcolor=coral]
  S -> E [label = '(1 - χ)&beta;(P + I)S + &beta;&#x2090; AS']
  E -> P [label = '(1 - η&#x2090;)ωE']
  E -> A [label = 'η&#x2090;ωE']
  P -> I [label = '(1 - φ)ψ P']
  P -> It [label = 'φψ P']
  A -> R [label = '&gamma; A']
  I -> R [label = '&gamma; I']
  I -> D [label = '&mu; I']
  S -> Et [label = 'χ&beta;(P + I)S']
  Et -> Pt [label = '(1 - η&#x2090;)ωE&#x1d40;']
  Et -> At [label = 'η&#x2090;ωE&#x1d40;']
  Pt -> It [label = 'ψ P&#x1d40;']
  At -> R [label = '&gamma; A&#x1d40;']
  It -> R [label = '&gamma; I&#x1d40;']
  It -> D [label = '&mu; I&#x1d40;']
  }")
seird_ct_structure <- DiagrammeRsvg::export_svg(g)

# make all available --------
usethis::use_data(seird_structure,
                  seirdage_structure,
                  seird_bd_structure,
                  seird_ru_structure,
                  seirdnpiage_structure,
                  seiaimisrd_structure,
                  semird_structure,
                  seird_ct_structure,
                  overwrite = TRUE, internal = TRUE)

