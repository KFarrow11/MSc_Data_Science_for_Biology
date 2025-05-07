# Load required packages
library(DiagrammeR)

godwit_presentation_flowchart <- grViz("
  digraph godwit_methodology {
    graph [rankdir = TB, 
           fontname = 'Gill Sans MT', fontstyle = 'bold', 
           fontsize = 80, 
           layout = dot,
           bgcolor = 'white',
           nodesep = 1.5,
           ranksep = 3]
    
    node [shape = rectangle, 
          style = 'filled,rounded', 
          fontname = 'Gill Sans MT',
          fontstyle = 'bold',
          fontsize = 80,
          height = 2,
          width = 12,
          margin = '0.5,0.5',  # Increased margin around text
          color = 'black',
          penwidth = 5]
          
    edge [fontname = 'Gill Sans MT',
          fontstyle = 'bold',
          fontsize = 80,
          penwidth = 8,
          arrowsize = 4]

    # Nodes with updated fontsize
    A [label = 'Godwits.csv', fillcolor = '#306326', fontcolor = 'white', fontsize = 100]
    B [label = 'Data Preparation\\n(Standardise variable names)', fillcolor = '#D35400', fontcolor = 'white', fontsize = 100]
    C1 [label = 'Aim 1:\\nGodwit Presence/Absence', fillcolor = '#1E6091', fontcolor = 'white', fontsize = 100]
    C2 [label = 'Aim 2:\\nGodwit Population Density', fillcolor = '#6A0DAD', fontcolor = 'white', fontsize = 100]
    
    # Other nodes
    DCA1 [label = 'Detrended Correspondence Analysis\\n(DCA)', fillcolor = '#E3E8E9', fontcolor = 'black']
    PCA1 [label = 'Principal Component Analysis\\n(PCA)', fillcolor = '#E3E8E9', fontcolor = 'black']
    R1 [label = 'Results', fillcolor = '#C41E3A', fontcolor = 'white', width = 5]
    
    D1a [label = 'Convert categorical variables to factors\\n(swardht, ditches, sedgepools)', fillcolor = '#FFD53D', fontcolor = 'black']
    F1a [label = 'Full Binomial GLM', fillcolor = '#C3E0E5']
    F2a [label = 'Reduced Binomial GLM', fillcolor = '#C3E0E5']
    F2ab [label = 'Reduced Binomial\\nGLM + elevation', fillcolor = '#C3E0E5']
    F3a [label = 'Final Binomial GLM', fillcolor = '#C3E0E5']
    R2 [label = 'Results', fillcolor = '#C41E3A', fontcolor = 'white', width = 5]

    E1a [label = 'Exploratory Analysis', fillcolor = '#50C878', fontcolor = 'black']
    E2a [label = 'Descriptive Statistics', fillcolor = '#50C878', fontcolor = 'black']
    E3a [label = 'Results', fillcolor = '#C41E3A', fontcolor = 'white', width = 5]

    E1b [label = 'Exploratory Analysis', fillcolor = '#50C878', fontcolor = 'black']
    E2b [label = 'Descriptive Statistics', fillcolor = '#50C878', fontcolor = 'black']
    E3b [label = 'Results', fillcolor = '#C41E3A', fontcolor = 'white', width = 5]

    D2 [label = 'Filtered dataset for density analysis', fillcolor = '#EFDCF9']
    D1b [label = 'Convert categorical variables to factors\\n(swardht, ditches, sedgepools)', fillcolor = '#FFD53D', fontcolor = 'black']
    F1b [label = 'Scatterplot Matrix', fillcolor = '#EFDCF9']
    F2b [label = 'Statistical Modeling', fillcolor = '#EFDCF9']
    F3b [label = 'Multiple Linear Regression', fillcolor = '#EFDCF9']
    F3c [label = 'Poisson GLM', fillcolor = '#EFDCF9']
    F4 [label = 'Final Density Model', fillcolor = '#EFDCF9']
    R4 [label = 'Results', fillcolor = '#C41E3A', fontcolor = 'white', width = 5]

    DCA2 [label = 'Detrended Correspondence Analysis\\n(DCA)', fillcolor = '#E3E8E9']
    PCA2 [label = 'Principal Component Analysis\\n(PCA)', fillcolor = '#E3E8E9']
    R5 [label = 'Results', fillcolor = '#C41E3A', fontcolor = 'white', width = 5]

    # Edges for the flow
    A -> B
    B -> C1
    B -> C2
    C1 -> DCA1
    DCA1 -> PCA1
    PCA1 -> R1

    C1 -> D1a
    D1a -> F1a
    F1a -> F2a
    F2a -> F3a
    F1a -> F2ab
    F2ab -> F3a
    F3a -> R2

    D1a -> E1a
    E1a -> E2a
    E2a -> E3a

    C2 -> D2
    D2 -> DCA2
    DCA2 -> PCA2
    PCA2 -> R5

    D2 -> D1b
    D1b -> E1b
    E1b -> E2b
    E2b -> E3b

    D1b -> F1b
    F1b -> F2b
    F2b -> F3b
    F2b -> F3c
    F3b -> F4
    F3c -> F4
    F4 -> R4
  }
")

# Display updated flowchart
godwit_presentation_flowchart
