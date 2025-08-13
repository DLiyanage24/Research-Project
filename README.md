## Create a framework for comprehensive testing across multiple levels of user engagement

Current methods of assessing graphics using measures like accuracy or speed, misses how users actually engage with charts. There are different ways to evaluate visualizations based on user tasks, but do not test multiple methods on the same charts. This proposed work aims to fill that gap by testing the same charts using multiple methods with the same participants, data and testing conditions. This helps us get a clearer, more complete understanding of how chart design affects user experience.

By combining multiple methods of user testing within the same experiment, we can gather information that spans multiple levels of engagement with acceptably small impact on participant cognitive load.

**Augmentation of statistical lineup:**

eg. - combining lineups with direct annotation, lineups with talk-aloud protocol

**Augmenting single-plot studies:**

eg. - combine numerical estimation, direct annotation , think-aloud or add forced-choice questions

### Last week( 6 Aug - 12 Aug)

-   Tested adding new functionalities to  “You Draw It” lineup setup, making adjustments to the D3.js code, JavaScript logic, and R Shiny server logic to enable and refine these features.
    -   Added functionality for highlighting interesting features in the selected plot.
    -   Implemented circle-based annotation tools: Users can enable a “Highlight Interesting” mode to draw a circular region over key features.
    -   Added a free-text annotation box below the selected plot to record user explanations for their selection.
    -   Integrated annotation tools with Shiny so that both the drawn region details (center and radii) and the user’s explanatory text are recorded and stored in CSV files.

### To do list for the next week

-   Test and implement additional feature refinements.
