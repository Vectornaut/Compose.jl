
# Path Primitives


# From form.jl:
# Path
# ----

# An implementation of the SVG path mini-language.

abstract type PathOp end

struct MoveAbsPathOp <: PathOp
    to::Vec
end

function assert_pathop_tokens_len(op_type, tokens, i, needed)
    provided = length(tokens) - i + 1
    provided < needed &&
            error("In path $(op_type) requires $(needed) argumens but only $(provided) provided.")
end

function parsepathop(::Type{MoveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveAbsPathOp, tokens, i, 2)
    op = MoveAbsPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::MoveAbsPathOp) =
        MoveAbsPathOp(resolve(box, units, t, p.to))

struct MoveRelPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{MoveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(MoveRelPathOp, tokens, i, 2)
    op = MoveRelPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

function resolve_offset(box::AbsoluteBox, units::UnitBox, t::Transform, p::Vec)
    absp = resolve(box, units, t, p)
    zer0 = resolve(box, units, t, (0w, 0h))
    return (absp[1] - zer0[1], absp[2] - zer0[2])
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::MoveRelPathOp) =
        MoveRelPathOp(resolve_offset(box, units, t, p.to))

struct ClosePathOp <: PathOp
end

parsepathop(::Type{ClosePathOp}, tokens::AbstractArray, i) = (ClosePathOp(), i)

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ClosePathOp) = p

struct LineAbsPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{LineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineAbsPathOp, tokens, i, 2)
    op = LineAbsPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::LineAbsPathOp) =
        LineAbsPathOp(resolve(box, units, t, p.to))

struct LineRelPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{LineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(LineRelPathOp, tokens, i, 2)
    op = LineRelPathOp((x_measure(tokens[i]), y_measure(tokens[i + 1])))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::LineRelPathOp) =
        LineRelPathOp(resolve(box, units, t, p.to))

struct HorLineAbsPathOp <: PathOp
    x::Measure
end

function parsepathop(::Type{HorLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineAbsPathOp, tokens, i, 1)
    op = HorLineAbsPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::HorLineAbsPathOp) =
        HorLineAbsPathOp(resolve(box, units, t, (p.x, 0mm))[1])

struct HorLineRelPathOp <: PathOp
    Δx::Measure
end

function parsepathop(::Type{HorLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(HorLineRelPathOp, tokens, i, 1)
    op = HorLineRelPathOp(x_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::HorLineRelPathOp) =
        HorLineRelPathOp(resolve(box, units, t, p.Δx))

struct VertLineAbsPathOp <: PathOp
    y::Measure
end

function parsepathop(::Type{VertLineAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineAbsPathOp, tokens, i, 1)
    op = VertLineAbsPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::VertLineAbsPathOp) =
        VertLineAbsPathOp(resolve(box, units, t, (0mm, p.y))[2])

struct VertLineRelPathOp <: PathOp
    Δy::Measure
end

function parsepathop(::Type{VertLineRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(VertLineRelPathOp, tokens, i, 1)
    op = VertLineRelPathOp(y_measure(tokens[i]))
    return (op, i + 1)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::VertLineRelPathOp) =
        VertLineAbsPathOp(resolve(box, units, t, (0mmm, p.Δy))[2])

struct CubicCurveAbsPathOp <: PathOp
    ctrl1::Vec
    ctrl2::Vec
    to::Vec
end

function parsepathop(::Type{CubicCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveAbsPathOp, tokens, i, 6)
    op = CubicCurveAbsPathOp((tokens[i],     tokens[i + 1]),
                             (tokens[i + 2], tokens[i + 3]),
                             (tokens[i + 4], tokens[i + 5]))
    return (op, i + 6)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveAbsPathOp) =
        CubicCurveAbsPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))

struct CubicCurveRelPathOp <: PathOp
    ctrl1::Vec
    ctrl2::Vec
    to::Vec
end

function parsepathop(::Type{CubicCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveRelPathOp, tokens, i, 6)
    op = CubicCurveRelPathOp((tokens[i],     tokens[i + 1]),
                             (tokens[i + 2], tokens[i + 3]),
                             (tokens[i + 4], tokens[i + 5]))
    return (op, i + 6)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveRelPathOp) =
        CubicCurveRelPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))

struct CubicCurveShortAbsPathOp <: PathOp
    ctrl2::Vec
    to::Vec
end

function parsepathop(::Type{CubicCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveShortAbsPathOp, tokens, i, 4)
    op = CubicCurveShortAbsPathOp((x_measure(tokens[i]),     y_measure(tokens[i + 1])),
                                  (x_measure(tokens[i + 2]), y_measure(tokens[i + 3])))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveShortAbsPathOp) =
        CubicCurveShortAbsPathOp(
            resolve_offset(box, units, t, p.ctrl2),
            resolve_offset(box, units, t, p.to))

struct CubicCurveShortRelPathOp <: PathOp
    ctrl2::Vec
    to::Vec
end

function parsepathop(::Type{CubicCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(CubicCurveShortRelPathOp, tokens, i, 4)
    op = CubicCurveShortRelPathOp((x_measure(tokens[i]),     y_measure(tokens[i + 1])),
                                  (x_measure(tokens[i + 2]), y_measure(tokens[i + 3])))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::CubicCurveShortRelPathOp) =
        CubicCurveShortRelPathOp(
            resolve(box, units, t, p.ctrl2),
            resolve(box, units, t, p.to))

struct QuadCurveAbsPathOp <: PathOp
    ctrl1::Vec
    to::Vec
end

function parsepathop(::Type{QuadCurveAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveAbsPathOp, tokens, i, 4)
    op = QuadCurveAbsPathOp((tokens[i],     tokens[i + 1]),
                            (tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveAbsPathOp) =
        QuadCurveAbsPathOp(
            resolve(box, units, t, p.ctrl1),
            resolve(box, units, t, p.to))

struct QuadCurveRelPathOp <: PathOp
    ctrl1::Vec
    to::Vec
end

function parsepathop(::Type{QuadCurveRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveRelPathOp, tokens, i, 4)
    op = QuadCurveRelPathOp((tokens[i],     tokens[i + 1]),
                            (tokens[i + 2], tokens[i + 3]))
    return (op, i + 4)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveRelPathOp) =
        QuadCurveRelPathOp(
            (resolve(box, units, t, p.ctrl1[1]),
             resolve(box, units, t, p.ctrl1[2])),
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))

struct QuadCurveShortAbsPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{QuadCurveShortAbsPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortAbsPathOp, tokens, i, 2)
    op = QuadCurveShortAbsPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveShortAbsPathOp) =
        QuadCurveShortAbsPathOp(resolve(box, units, t, p.to))

struct QuadCurveShortRelPathOp <: PathOp
    to::Vec
end

function parsepathop(::Type{QuadCurveShortRelPathOp}, tokens::AbstractArray, i)
    assert_pathop_tokens_len(QuadCurveShortRelPathOp, tokens, i, 2)
    op = QuadCurveShortRelPathOp((tokens[i], tokens[i + 1]))
    return (op, i + 2)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::QuadCurveShortRelPathOp) =
        QuadCurveShortRelPathOp(
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))

struct ArcAbsPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Vec
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ArcAbsPathOp) =
        ArcAbsPathOp(
            resolve(box, units, t, p.rx),
            resolve(box, units, t, p.ry),
            p.rotation,
            p.largearc,
            p.sweep,
            resolve(box, units, t, p.to))

struct ArcRelPathOp <: PathOp
    rx::Measure
    ry::Measure
    rotation::Float64
    largearc::Bool
    sweep::Bool
    to::Vec
end

function parsepathop(::Type{T}, tokens::AbstractArray, i) where T <: Union{ArcAbsPathOp, ArcRelPathOp}
    assert_pathop_tokens_len(T, tokens, i, 7)

    if isa(tokens[i + 3], Bool)
        largearc = tokens[i + 3]
    elseif tokens[i + 3] == 0
        largearc = false
    elseif tokens[i + 3] == 1
        largearc = true
    else
        error("largearc argument to the arc path operation must be boolean")
    end

    if isa(tokens[i + 4], Bool)
        sweep = tokens[i + 4]
    elseif tokens[i + 4] == 0
        sweep = false
    elseif tokens[i + 4] == 1
        sweep = true
    else
        error("sweep argument to the arc path operation must be boolean")
    end

    isa(tokens[i + 2], Number) || error("path arc operation requires a numerical rotation")

    op = T(x_measure(tokens[i]),
           y_measure(tokens[i + 1]),
           convert(Float64, tokens[i + 2]),
           largearc, sweep,
           (x_measure(tokens[i + 5]), y_measure(tokens[i + 6])))

    return (op, i + 7)
end

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::ArcRelPathOp) =
        ArcRelPathOp(
            resolve(box, units, t, p.rx),
            resolve(box, units, t, p.ry),
            p.rotation,
            p.largearc,
            p.sweep,
            (resolve(box, units, t, p.to[1]),
             resolve(box, units, t, p.to[2])))

const path_ops = Dict(
     :M => MoveAbsPathOp,
     :m => MoveRelPathOp,
     :Z => ClosePathOp,
     :z => ClosePathOp,
     :L => LineAbsPathOp,
     :l => LineRelPathOp,
     :H => HorLineAbsPathOp,
     :h => HorLineRelPathOp,
     :V => VertLineAbsPathOp,
     :v => VertLineRelPathOp,
     :C => CubicCurveAbsPathOp,
     :c => CubicCurveRelPathOp,
     :S => CubicCurveShortAbsPathOp,
     :s => CubicCurveShortRelPathOp,
     :Q => QuadCurveAbsPathOp,
     :q => QuadCurveRelPathOp,
     :T => QuadCurveShortAbsPathOp,
     :t => QuadCurveShortRelPathOp,
     :A => ArcAbsPathOp,
     :a => ArcRelPathOp
)

# A path is an array of symbols, numbers, and measures following SVGs path
# mini-language.
function parsepath(tokens::AbstractArray)
    ops = PathOp[]
    last_op_type = nothing
    i = 1
    while i <= length(tokens)
        tok = tokens[i]
        strt = i
        if isa(tok, Symbol)
            if !haskey(path_ops, tok)
                error("$(tok) is not a valid path operation")
            else
                op_type = path_ops[tok]
                i += 1
                op, i = parsepathop(op_type, tokens, i)
                push!(ops, op)
                last_op_type = op_type
            end
        else
            op, i = parsepathop(last_op_type, tokens, i)
            push!(ops, op)
        end
    end

    return ops
end

struct PathPrimitive <: FormPrimitive
    ops::Vector{PathOp}
end

const Path = Form{PathPrimitive}

path(tokens::AbstractArray, tag=empty_tag) = Path([PathPrimitive(parsepath(tokens))], tag)

path(tokens::AbstractArray{T}, tag=empty_tag) where T <: AbstractArray =
        Path([PathPrimitive(parsepath(ts)) for ts in tokens], tag)

resolve(box::AbsoluteBox, units::UnitBox, t::Transform, p::PathPrimitive) =
        PathPrimitive([resolve(box, units, t, op) for op in p.ops])

# TODO: boundingbox
