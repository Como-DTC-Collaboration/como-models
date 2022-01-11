## code to prepare `seirdage_structure` dataset goes here
library(DiagrammeR)
library(DiagrammeRsvg)

# seird
g <- DiagrammeR::grViz("
  digraph PrimC{
  graph [rankdir = 'LR']
  node [shape = circle]
  S E I R D
  S -> E [label = '&beta; S I']
  E -> I [label = '&kappa; E']
  I -> R [label = '&gamma; I']
  I -> D [label = '&mu; I']
  }")
seird_structure <- DiagrammeRsvg::export_svg(g)

# seirdage
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle]
    S_1[label = 'S&#x2081;']
    E_1[label = 'E&#x2081;']
    I_1[label = 'I&#x2081;']
    R_1[label = 'R&#x2081;']
    D_1[label = 'D&#x2081;']
    S_2[label = 'S&#x2082;']
    E_2[label = 'E&#x2082;']
    I_2[label = 'I&#x2082;']
    R_2[label = 'R&#x2082;']
    D_2[label = 'D&#x2082;']
    S_1 -> E_1 [label = '&beta; S&#x2081; (C&#x2081;&#x2081; I&#x2081; + C&#x2081;&#x2082; I&#x2082;)']
    E_1 -> I_1 [label = '&kappa; E&#x2081;']
    I_1 -> R_1 [label = '&gamma;&#x2081; I&#x2081;']
    I_1 -> D_1 [label = '&mu;&#x2081; I&#x2081;']
    S_2 -> E_2 [label = '&beta; S&#x2082; (C&#x2082;&#x2081; I&#x2081; + C&#x2082;&#x2082; I&#x2082;)']
    E_2 -> I_2 [label = '&kappa; E&#x2082;']
    I_2 -> R_2 [label = '&gamma;&#x2082; I&#x2082;']
    I_2 -> D_2 [label = '&mu;&#x2082; I&#x2082;']
    }")
g
seirdage_structure <- DiagrammeRsvg::export_svg(g)

# seird_bd
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle]
    S E I R D
    nowhere [style=invis,shape=point]
    nowhere -> S [label = '&lambda;']
    S -> E [label = '&beta; S I']
    S -> D [label = '&nu; S']
    E -> I [label = '&kappa; E']
    E -> D [label = '&nu; E']
    I -> R [label = '&gamma; I']
    I -> D [label = '(&nu; + &mu;) I']
    R -> S [label = '&delta; R']
    R -> D [label = '&nu; R']
}")
seird_bd_structure <- DiagrammeRsvg::export_svg(g)

# seird_ru
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle]
    S_U[label = 'S&#x1d64;']
    E_U[label = 'E&#x1d64;']
    I_U[label = 'I&#x1d64;']
    R_U[label = 'R&#x1d64;']
    D_U[label = 'D&#x1d64;']
    S_Y[label = 'S&#x1d67;']
    E_Y[label = 'E&#x1d67;']
    I_Y[label = 'I&#x1d67;']
    R_Y[label = 'R&#x1d67;']
    D_Y[label = 'D&#x1d67;']
    S_U -> E_U [label = '&beta; S&#x1d64; ((I&#x1d64; + I&#x1d67;)(φ&#x1d64; N&#x1d64; + φ&#x1d67; N&#x1d67;)C + I&#x1d64;∕φ&#x1d64;N&#x1d64;(1-C))']
    E_U -> I_U [label = '&kappa; E&#x1d64;']
    I_U -> R_U [label = '&gamma; I&#x1d64;']
    I_U -> D_U [label = '&mu; I&#x1d64;']
    S_Y -> E_Y [label = '&beta; S&#x1d67;((I&#x1d64; + I&#x1d67;)(φ&#x1d64; N&#x1d64; + φ&#x1d67; N&#x1d67;)C + I&#x1d67;∕φ&#x1d67;N&#x1d67;(1-C))']
    E_Y -> I_Y [label = '&kappa; E&#x1d67;']
    I_Y -> R_Y [label = '&gamma; I&#x1d67;']
    I_Y -> D_Y [label = '&mu; I&#x1d67;']
}")
seird_ru_structure <- DiagrammeRsvg::export_svg(g)

# seirdnpiage
g <- DiagrammeR::grViz("
    digraph PrimC{
    graph [rankdir = 'LR']
    node [shape = circle]
    S_1[label = 'S&#x2081;']
    E_1[label = 'E&#x2081;']
    I_1[label = 'I&#x2081;']
    R_1[label = 'R&#x2081;']
    D_1[label = 'D&#x2081;']
    S_2[label = 'S&#x2082;']
    E_2[label = 'E&#x2082;']
    I_2[label = 'I&#x2082;']
    R_2[label = 'R&#x2082;']
    D_2[label = 'D&#x2082;']
    S_1 -> E_1 [label = '&beta;(t) S&#x2081; (C&#x2081;&#x2081;(t) I&#x2081; + C&#x2081;&#x2082;(t) I&#x2082;)']
    E_1 -> I_1 [label = '&kappa; E&#x2081;']
    I_1 -> R_1 [label = '&gamma;&#x2081; I&#x2081;']
    I_1 -> D_1 [label = '&mu;&#x2081; I&#x2081;']
    S_2 -> E_2 [label = '&beta;(t) S&#x2082; (C&#x2082;&#x2081;(t) I&#x2081; + C&#x2082;&#x2082;(t) I&#x2082;)']
    E_2 -> I_2 [label = '&kappa; E&#x2082;']
    I_2 -> R_2 [label = '&gamma;&#x2082; I&#x2082;']
    I_2 -> D_2 [label = '&mu;&#x2082; I&#x2082;']
    }")
seirdnpiage_structure <- DiagrammeRsvg::export_svg(g)

# make all available
usethis::use_data(seird_structure,
                  seirdage_structure,
                  seird_bd_structure,
                  seird_ru_structure,
                  seirdnpiage_structure,
                  overwrite = TRUE, internal = TRUE)

