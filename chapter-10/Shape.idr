module Shape

public export
total
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

private
total
rectangle_area : Double -> Double -> Double
rectangle_area width height = width * height

export
total
area : Shape -> Double
area (Triangle base height) = 0.5 * rectangle_area base height
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius

