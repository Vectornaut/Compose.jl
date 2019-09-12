
# Path Primitives


# From svg.jl:

function svg_print_path_op(io::IO, op::MoveAbsPathOp)
    print(io, 'M')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::MoveRelPathOp)
    print(io, 'm')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

svg_print_path_op(io::IO, op::ClosePathOp) = print(io, 'z')

function svg_print_path_op(io::IO, op::LineAbsPathOp)
    print(io, 'L')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::LineRelPathOp)
    print(io, 'l')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::HorLineAbsPathOp)
    print(io, 'H')
    svg_print_float(io, op.x.value)
end

function svg_print_path_op(io::IO, op::HorLineRelPathOp)
    print(io, 'h')
    svg_print_float(io, op.Δx.value)
end

function svg_print_path_op(io::IO, op::VertLineAbsPathOp)
    print(io, 'V')
    svg_print_float(io, op.y.value)
end

function svg_print_path_op(io::IO, op::VertLineRelPathOp)
    print(io, 'v')
    svg_print_float(io, op.Δy.value)
end

function svg_print_path_op(io::IO, op::CubicCurveAbsPathOp)
    print(io, 'C')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveRelPathOp)
    print(io, 'c')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveShortAbsPathOp)
    print(io, 'S')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::CubicCurveShortRelPathOp)
    print(io, 's')
    svg_print_float(io, op.ctrl2[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl2[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveAbsPathOp)
    print(io, 'Q')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveRelPathOp)
    print(io, 'q')
    svg_print_float(io, op.ctrl1[1].value)
    print(io, ' ')
    svg_print_float(io, op.ctrl1[2].value)
    print(io, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveShortAbsPathOp)
    print(io, 'T')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::QuadCurveShortRelPathOp)
    print(io, 't')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::ArcAbsPathOp)
    print(io, 'A')
    svg_print_float(io, op.rx.value)
    print(io, ' ')
    svg_print_float(io, op.ry.value)
    print(io, ' ')
    svg_print_float(io, op.rotation)
    print(io, ' ',
          op.largearc ? 1 : 0, ' ',
          op.sweep ? 1 : 0, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function svg_print_path_op(io::IO, op::ArcRelPathOp)
    print(io, 'a')
    svg_print_float(io, op.rx.value)
    print(io, ' ')
    svg_print_float(io, op.ry.value)
    print(io, ' ')
    svg_print_float(io, op.rotation)
    print(io, ' ',
          op.largearc ? 1 : 0, ' ',
          op.sweep ? 1 : 0, ' ')
    svg_print_float(io, op.to[1].value)
    print(io, ' ')
    svg_print_float(io, op.to[2].value)
end

function draw(img::SVG, prim::PathPrimitive, idx::Int)
    indent(img)
    print(img.out, "<path d=\"")
    for op in prim.ops
        svg_print_path_op(img.out, op)
    end
    print(img.out, '"')
    print_vector_properties(img, idx)
    print(img.out, "/>\n")
end
