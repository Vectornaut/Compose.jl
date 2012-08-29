
load("compose.jl")

const phi = (1 + sqrt(5)) / 2

function golden_rect(n::Int)
    if n == 0; return nothing; end
    {Canvas(),
      {Rectangle(), Fill(LCHab(90, 80, 70 - 15n))},
      {Canvas(0, -1/phi, 1h, 1/phi, Rotation(pi/2, 0, 1)),
         golden_rect(n - 1)}}
end

@upon SVG("golden_rect.svg", phi * 3inch, 3inch) begin
    draw(pad!({golden_rect(10), Fill(nothing),
               Stroke("white"), LineWidth(0.2mm)}, 1mm))
end

