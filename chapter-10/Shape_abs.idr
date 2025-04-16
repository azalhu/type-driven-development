module Shape_abs

export
total
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

export
total
triangle : Double -> Double -> Shape
triangle = Triangle

export
total
rectangle : Double -> Double -> Shape
rectangle = Rectangle

export
total
circle : Double -> Shape
circle = Circle

total public export
data ShapeView : Shape -> Type where
  STriangle : {base, height : _} -> ShapeView (triangle base height)
  SRectangle : {width, height : _} -> ShapeView (rectangle width height)
  SCircle : {radius : _} -> ShapeView (circle radius)

total export
shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle
shapeView (Rectangle width height) = SRectangle
shapeView (Circle radius) = SCircle

