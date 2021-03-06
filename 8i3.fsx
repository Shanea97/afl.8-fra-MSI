open ImgUtil
type point = int * int // A point (x,y) in the plane
type colour = int * int * int //(red, green, blue), 0...255
type figure =
  | Circle of point * int * colour // Defined by center, radius and colour
  | Rectangle of point * point * colour // Defined by corners bottom-left, top-right and colour
  | Mix of figure * figure // Combine figures with mixed colours at overlap

/// <param name (r, g, b)>
/// The parameter is a tripple consisting of the colors red green and blue
/// </param name>
/// <summary>
/// The function checks for each of the colors to be between 0 and 255
/// </summary>
/// <returns>
/// The function returns a boolean
/// </returns>
let CheckColour (r, g, b) : bool =
  r>=0 && r<=255 && g>=0 && g<=255 && b>=0 && b<=255

/// <param name "f">
/// The parameter f takes form of a figure
/// </param name>
/// <summary>
/// the function matches the parameter f with one of the three figures. and depending on which type it matches, it checks for different things, regarding of the figure turns out to be true. Here it also uses the function CheckColour
/// </summary>
/// <returns>
/// The function returns a boolean
/// </returns>
let rec CheckFigure (f:figure): bool =
  match f with
  | Circle ((cx,cy),r,col) ->
      r > 0 && CheckColour col
  | Rectangle ((x0,y0), (x1,y1), col) ->
      x0 > x1 && y0 > y1 && CheckColour col
  | Mix (f1,f2) ->
      CheckFigure(f1) && CheckFigure(f2)


let circle = Circle((45,45), (41), (0,0,255))
let rrectangle = Rectangle((81,99), (36,36), (255,0,0))
let mix = Mix (circle, rrectangle)
printfn "Circle: %b" (CheckFigure (Circle((125,125), (125), (0,0,255))))
printfn "Rectangle: %b" (CheckFigure (Rectangle((81,99), (36,36), (255,0,0))))
printfn "Mix: %b" (CheckFigure (mix))
