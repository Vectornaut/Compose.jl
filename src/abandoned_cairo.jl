
# Path Primitives


# From cairo_backends.jl:

function draw(img::Image, prim::PathPrimitive)
    for op in prim.ops
        draw_path_op(img, op)
    end
    fillstroke(img)
end

draw_path_op(img::Image, op::MoveAbsPathOp) = move_to(img, op.to)
draw_path_op(img::Image, op::MoveRelPathOp) = rel_move_to(img, op.to)
draw_path_op(img::Image, op::ClosePathOp)   = close_path(img)
draw_path_op(img::Image, op::LineAbsPathOp) = line_to(img, op.to)
draw_path_op(img::Image, op::LineRelPathOp) = rel_line_to(img, op.to)

function draw_path_op(img::Image, op::HorLineAbsPathOp)
    pos = current_point(img)
    line_to(img, (op.x, pos.y))
end

draw_path_op(img::Image, op::HorLineRelPathOp) = rel_line_to(img, (op.Δx, 0.0mm))

function draw_path_op(img::Image, op::VertLineAbsPathOp)
    pos = current_point(img)
    line_to(img, (pos.x, op.y))
end

draw_path_op(img::Image, op::VertLineRelPathOp) = rel_line_to(img, (0.0mm, op.Δy))

function draw_path_op(img::Image, op::CubicCurveAbsPathOp)
    curve_to(img, op.ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = op.ctrl2
end

function draw_path_op(img::Image, op::CubicCurveRelPathOp)
    xy = current_point(img)
    rel_curve_to(img, op.ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = (op.ctrl2[1] + xy[1], op.ctrl2[2] + xy[2])
end

function draw_path_op(img::Image, op::CubicCurveShortAbsPathOp)
    xy = current_point(img)
    ctrl1 = img.last_ctrl2_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = (2*xy[1] - ctrl1[1], 2*xy[2] - ctrl1[2])
    end
    curve_to(img, ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point = op.ctrl2
end

function draw_path_op(img::Image, op::CubicCurveShortRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value

    ctrl1 = img.last_ctrl2_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = (Measure(abs=(2*x1 - ctrl1[1].value) - x1),
                 Measure(abs=(2*y1 - ctrl1[2].value) - y1))
    end
    cx, cy = ctrl1[1].value, ctrl1[2].value

    rel_curve_to(img, ctrl1, op.ctrl2, op.to)
    img.last_ctrl2_point =
        (Measure(abs=op.ctrl2[1].value + xy.x.abs),
         Measure(abs=op.ctrl2[2].value + xy.y.abs))
end

function draw_path_op(img::Image, op::QuadCurveAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value
    cx, cy = op.ctrl1[1].value, op.ctrl1[2].value
    curve_to(img,
             (Measure(abs=(x1 + 2*cx)/3),
              Measure(abs=(y1 + 2*cy)/3)),
             (Measure(abs=(x2 + 2*cx)/3),
              Measure(abs=(y2 + 2*cy)/3)),
             op.to)
    img.last_ctrl1_point = op.ctrl1
end

function draw_path_op(img::Image, op::QuadCurveRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value
    cx, cy = op.ctrl1[1].value, op.ctrl1[2].value
    rel_curve_to(img,
                 (Measure(abs=(x1 + 2*cx)/3),
                  Measure(abs=(y1 + 2*cy)/3)),
                 (Measure(abs=(x2 + 2*cx)/3),
                  Measure(abs=(y2 + 2*cy)/3)),
             op.to)
    img.last_ctrl1_point =
        (Measure(abs=op.ctrl1[1].value + xy.x.abs),
         Measure(abs=op.ctrl1[2].value + xy.y.abs))
end

function draw_path_op(img::Image, op::QuadCurveShortAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value

    ctrl1 = img.last_ctrl1_point
    if img.last_ctrl1_point === nothing
        ctrl1 = xy
    else
        ctrl1 = (Measure(abs=2*x1 - ctrl1[1].value),
                 Measure(abs=2*y1 - ctrl1[2].value))
    end
    cx, cy = ctrl1[1].value, ctrl1[2].value

    curve_to(img,
             (Measure(abs=(x1 + 2*cx)/3),
              Measure(abs=(y1 + 2*cy)/3)),
             (Measure(abs=(x2 + 2*cx)/3),
              Measure(abs=(y2 + 2*cy)/3)),
             (Measure(abs=x2), Measure(abs=y2)))
    img.last_ctrl1_point = ctrl1
end

function draw_path_op(img::Image, op::QuadCurveShortRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = x1 + op.to[1].value, y1 + op.to[2].value

    ctrl1 = img.last_ctrl1_point
    if ctrl1 === nothing
        ctrl1 = xy
    else
        ctrl1 = (Measure(abs=(2*x1 - ctrl1[1].value) - x1),
                 Measure(abs=(2*y1 - ctrl1[2].value) - y1))
    end
    cx, cy = ctrl1[1].value, ctrl1[2].value

    rel_curve_to(img,
                 (Measure(abs=(x1 + 2*cx)/3),
                  Measure(abs=(y1 + 2*cy)/3)),
                 (Measure(abs=(x2 + 2*cx)/3),
                  Measure(abs=(y2 + 2*cy)/3)),
                 (Measure(abs=x2), Measure(abs=y2)))
    img.last_ctrl1_point =
        (Measure(abs=op.ctrl1[1].value + x1),
         Measure(abs=op.ctrl1[2].value + y1))
end

function draw_path_op(img::Image, op::ArcAbsPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = op.to[1].value, op.to[2].value
    rx, ry = op.rx.abs, op.ry.abs
    φ = deg2rad(op.rotation)
    draw_endpoint_arc(img, rx, ry, φ, op.largearc, op.sweep, x1, y1, x2, y2)
end

function draw_path_op(img::Image, op::ArcRelPathOp)
    xy = current_point(img)
    x1, y1 = xy[1].value, xy[2].value
    x2, y2 = x1 + op.to[1].value, y1 + op.to[2].value
    rx, ry = op.rx.abs, op.ry.abs
    φ = deg2rad(op.rotation)
    draw_endpoint_arc(img, rx, ry, φ, op.largearc, op.sweep, x1, y1, x2, y2)
end

# Draw an SVG style elliptical arc
function draw_endpoint_arc(img::Image, rx::Float64, ry::Float64, φ::Float64,
                           largearc::Bool, sweep::Bool,
                           x1::Float64, y1::Float64,
                           x2::Float64, y2::Float64)
    function uvangle(ux, uy, vx, vy)
        t = (ux * vx + uy * vy) / (sqrt(ux^2 + uy^2) * sqrt(vx^2 + vy^2))
        t = max(min(t, 1.0), -1.0)
        return (ux * vy - uy * vx < 0.0 ? -1 : 1.0) * acos(t)
    end

    # From: http://www.w3.org/TR/SVG/implnote.html#ArcConversionEndpointToCenter
    xm, ym = (x1 - x2)/2, (y1 - y2)/2
    x1p =  cos(φ) * xm + sin(φ) * ym
    y1p = -sin(φ) * xm + cos(φ) * ym

    u = (rx^2 * ry^2 - rx^2 * y1p^2 - ry^2 * x1p^2) / (rx^2 * y1p^2 + ry^2 * x1p^2)
    u = u >= 0.0 ? sqrt(u) : 0.0

    cxp =  u * (rx * y1p) / ry
    cyp = -u * (ry * x1p) / rx
    if sweep == largearc
        cxp = -cxp
        cyp = -cyp
    end
    cx = (x1 + x2)/2 + cos(φ) * cxp - sin(φ) * cyp
    cy = (y1 + y2)/2 + sin(φ) * cxp + cos(φ) * cyp

    θ1 = uvangle(1.0, 0.0, (x1p - cxp) / rx, (y1p - cyp) / ry)
    Δθ = uvangle((x1p - cxp) / rx, (y1p - cyp) / ry,
                 (-x1p - cxp) / rx, (-y1p - cyp) / ry) % (2.0*π)
    if Δθ > 0.0 && !sweep
        Δθ -= 2*π
    elseif Δθ < 0.0 && sweep
        Δθ += 2*π
    end

    Cairo.save(img.ctx)
    Cairo.translate(img.ctx,
                    absolute_native_units(img, cx),
                    absolute_native_units(img, cy))
    Cairo.rotate(img.ctx, φ)
    Cairo.scale(img.ctx, rx, ry)
    if sweep
        arc(img, 0.0, 0.0, 1.0, θ1, θ1 + Δθ)
    else
        arc_negative(img, 0.0, 0.0, 1.0, θ1, θ1 + Δθ)
    end
    Cairo.restore(img.ctx)
end


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
