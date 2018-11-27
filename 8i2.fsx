open ImgUtil

let circle = Circle((50,50), (45), (0,0,255))
let rrectangle = Rectangle((40,40), (90,110), (255,0,0))
let mix = Mix (circle, rrectangle)
/// <param name "filnavn" "figur" "b" and "h">
/// filnavn: string in form of the name of the given file you want to produce 
/// figur: Figure
/// b: integer in form of width  
/// h: integer in form of height 
/// </param name>
/// <summary>
/// the functions makes a BitMap (bmp) that takes the form of (b x h), afterwards, by 2 for-loops, it runs through the whole BitMap and checks "colourAt" if one of the given figures is presented, and if this is true, it "prints" a colour onto the BitMap
/// </summary>
/// <returns>
/// The function returns a file, given the name of the String, which is a .png file, and you´re able to open your picture
/// </returns>
let MakePicture (filnavn:string) (figur:figure) (b:int) (h:int) =
  let bmp = ImgUtil.mk b h
  for x = 0 to (b-1) do
    for y = 0 to (h-1) do
      match (colourAt (x,y) figur) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) mix).Value)) (x,y) bmp
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) bmp
  ImgUtil.toPngFile filnavn bmp

MakePicture "figTest.png" mix 100 150
