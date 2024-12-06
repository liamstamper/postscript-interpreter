# Postscript Interpreter
This project is a Haskell-based implementation of an interpreter for a subset of the PostScript language, a programming language commonly used for document and graphics rendering. The interpreter processes PostScript programs using a stack-based execution model, implementing key features such as arithmetic operations, conditional logic, and basic drawing commands like moveto, lineto, and stroke. Built on top of the Cairo graphics library, the project renders graphical output from interpreted PostScript commands. The main components include a defined interpreter state, a parsing system for PostScript files, and a rendering pipeline to generate visual output. The project is structured to extend PostScript's functionality progressively while focusing on a clean and modular design.

### Image examples
<table>
  <tr>
    <td><img src="/week1/ref/lines.png" alt="Lines" width="420"/></td>
    <td><img src="/week1/ref/rays.png" alt="Rays" width="420"/></td>
  </tr>
  <tr>
    <td><img src="/week1/ref/twoshapes.png" alt="Two Shapes" width="420"/></td>
    <td><img src="/week1/ref/simpleshape.png" alt="Simple Shape" width="420"/></td>
  </tr>
</table>

### Testing Week 1 Code:
Make sure you have [Cairo](https://www.cairographics.org/) and [Haskell](https://www.haskell.org/) installed on your local machine. Then run the following command inside of the /week1 directory:

```cabal run postscript -- testfile.ps out.png```
