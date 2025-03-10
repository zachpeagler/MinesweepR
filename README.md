# MinesweepR
 A minesweeper clone made entirely in R and published with Shinylive via GitHub Pages.

 ## Screenshots

 ### Known Issues
 - Flags sometimes do not work. I think it's an issue with the recursive reveal_cells function. Check revealed assignments, and consider adding a new can_be_flagged value to each cell.

### Ideas
- It might be a good idea to change the architecture from a series of matrixes (matrix for revealed, matrix for # of adjacent mines, matrix for flags, etc) to a matrix where each element in the matrix is a cell object. And those cell objects each have values for flagged, revealed, mine, adjacent mine #, etc. I did it this way when I rewrote it in Python [Pynesweeper](https://github.com/zachpeagler/Pynesweeper) and it worked well.
