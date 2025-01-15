import gleam/io
import gleam/list
import gleam/option
import gleam/string
import gleam/yielder.{type Yielder}

import cel/interpreter/type_.{type Type}
import cel/parser.{type ExpressionData}

// fn member_path(expr: ExpressionData) -> List(String) {
//   case parser.expr(expr) {
//     parser.Member(parent, parser.Attribute(name)) -> {
//       case parser.expr(parent) {
//         parser.Ident(parent) -> [name, parent]
//         _ -> [name, ..member_path(parent)]
//       }
//     }
//     _ -> []
//   }
// }

pub type Term {
  // Expr(expr: Context)
  // Ident(String)
  Known(Type)
  Var(String)
  Num
  List(Term)
  // Key(Term, Term)
  Arrow(domain: Term, range: Term)
}

fn term_from_atom(atom: parser.Atom) -> Term {
  case atom {
    parser.Int(_) -> Num
    parser.UInt(_) -> Num
    parser.Float(_) -> Num
    parser.Bool(_) -> Known(type_.BoolT)
    parser.String(_) -> Known(type_.StringT)
    parser.Bytes(_) -> Known(type_.BytesT)
    parser.Null -> Known(type_.NullT)
  }
}

pub type Substitution {
  Substitution(id: Int, var: Term, is: Term)
}

type Constraint {
  Constraint(origin: Int, lhs: Term, rhs: Term)
}

type Context {
  Context(vars: Yielder(String), cons: List(Constraint))
}

// fn ref(ctx: Context, id: Int) -> Result(Reference, Nil) {
//   dict.get(ctx.refs, id)
// }

fn var_yielder() {
  yielder.unfold(#(0, ""), fn(acc) {
    let #(counter, prefix) = acc
    let assert Ok(codepoint) = { counter % 25 } + 97 |> string.utf_codepoint

    case [codepoint] |> string.from_utf_codepoints {
      "z" -> yielder.Next(prefix <> "z", #(counter + 1, prefix <> "_"))
      letter -> yielder.Next(prefix <> letter, #(counter + 1, prefix))
    }
  })
}

pub fn infer_types(expr: ExpressionData) -> List(Substitution) {
  let ctx = Context(vars: var_yielder(), cons: [])

  let ctx = generate_constraints(ctx, expr)
  unify(ctx.cons |> list.reverse, [])
}

fn unify(
  consts: List(Constraint),
  subs: List(Substitution),
) -> List(Substitution) {
  case consts {
    [] -> subs
    [first, ..rest] -> {
      let Constraint(origin: id, lhs: left, rhs: right) = first

      case left, right {
        l, r if l == r -> unify(rest, subs)
        Var(_), _ -> {
          let #(rest, subs) =
            replace_all(rest, subs, occurance: left, with: right)
          unify(rest, [Substitution(id, left, right), ..subs])
        }
        _, Var(_) -> {
          let #(rest, subs) =
            replace_all(rest, subs, occurance: right, with: left)

          unify(rest, [Substitution(id, right, left), ..subs])
        }
        Arrow(l_domain, l_range), Arrow(r_domain, r_range) -> {
          let rest = [
            Constraint(id, l_domain, r_domain),
            Constraint(id, l_range, r_range),
            ..rest
          ]

          unify(rest, subs)
        }
        _, _ -> {
          let found_subs = subs |> string.inspect
          io.println("[err] found: " <> found_subs)

          let left_str = string.inspect(left)
          let right_str = string.inspect(right)
          let err_msg =
            "[err]: <" <> left_str <> "> and <" <> right_str <> "> do not unify"

          panic as err_msg
        }
      }
    }
  }
}

fn replace(left: Term, term: Term, right: Term) -> Term {
  case term {
    Arrow(domain, range) ->
      Arrow(
        domain: replace(left, domain, right),
        range: replace(left, range, right),
      )

    _ if left == term -> right
    _ -> term
  }
}

fn replace_all(
  consts: List(Constraint),
  subs: List(Substitution),
  occurance left: Term,
  with right: Term,
) -> #(List(Constraint), List(Substitution)) {
  let consts =
    list.map(consts, fn(c) {
      let Constraint(origin:, lhs:, rhs:) = c
      let lhs = replace(left, lhs, right)
      let rhs = replace(left, rhs, right)
      Constraint(origin:, lhs:, rhs:)
    })

  let subs =
    list.map(subs, fn(s) {
      let Substitution(id:, var:, is:) = s
      let var = replace(left, var, right)
      let is = replace(left, is, right)
      Substitution(id:, var:, is:)
    })

  #(consts, subs)
}

fn get_var(ctx: Context) -> #(Term, Context) {
  let assert yielder.Next(var, vars) = ctx.vars |> yielder.step
  #(Var(var), Context(..ctx, vars:))
}

fn add(ctx, con) {
  Context(..ctx, cons: [con, ..ctx.cons])
}

fn generate_constraints(ctx: Context, expr: ExpressionData) -> Context {
  let origin = parser.id(expr)
  case parser.expr(expr) {
    parser.Atom(atom) -> {
      let #(var, ctx) = get_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: term_from_atom(atom))

      add(ctx, con)
    }
    parser.Ident(_) -> {
      let #(var, ctx) = get_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: var)

      add(ctx, con)
    }
    parser.List(inner_exprs) -> {
      let #(outer, ctx) = get_var(ctx)
      let #(inner, ctx) = get_var(ctx)

      let ctx = list.fold(inner_exprs, ctx, generate_constraints)
      let cons =
        list.fold(inner_exprs, ctx.cons, fn(cons, inner_expr) {
          let inner_id = parser.id(inner_expr)
          [Constraint(origin: inner_id, lhs: inner, rhs: inner), ..cons]
        })

      let con = Constraint(origin:, lhs: outer, rhs: List(inner))
      Context(..ctx, cons: [con, ..cons])
    }
    parser.Map(_fields) -> todo
    parser.Member(_, _) -> todo
    parser.Ternary(cond, then, otherwise) -> {
      let #(outer, ctx) = get_var(ctx)
      let #(cond_var, ctx) = get_var(ctx)
      let #(inner, ctx) = get_var(ctx)

      let ctx = generate_constraints(ctx, cond)
      let ctx = generate_constraints(ctx, then)
      let ctx = generate_constraints(ctx, otherwise)

      let cond_con =
        Constraint(
          origin: parser.id(cond),
          lhs: cond_var,
          rhs: Known(type_.BoolT),
        )

      let then_con = Constraint(origin: parser.id(then), lhs: inner, rhs: inner)
      let otherwise_con =
        Constraint(origin: parser.id(otherwise), lhs: inner, rhs: inner)

      let tern_con = Constraint(origin:, lhs: outer, rhs: inner)

      let cons = [tern_con, otherwise_con, then_con, cond_con, ..ctx.cons]
      Context(..ctx, cons:)
    }
    parser.Unary(op, inner_expr) -> {
      let #(outer, ctx) = get_var(ctx)
      let #(inner, ctx) = get_var(ctx)

      let ctx = generate_constraints(ctx, inner_expr)

      let inner_id = parser.id(inner_expr)
      let inner_con = case op {
        parser.Not ->
          Constraint(origin: inner_id, lhs: inner, rhs: Known(type_.BoolT))
        parser.UnarySub -> Constraint(origin: inner_id, lhs: inner, rhs: Num)
      }

      let outer_con = case op {
        parser.Not -> Constraint(origin:, lhs: outer, rhs: Known(type_.BoolT))
        parser.UnarySub -> Constraint(origin:, lhs: outer, rhs: Num)
      }

      Context(..ctx, cons: [outer_con, inner_con, ..ctx.cons])
    }
    parser.BinaryOperation(lhs, op, rhs) -> {
      let #(outer, ctx) = get_var(ctx)
      let #(lhs_var, ctx) = get_var(ctx)
      let #(rhs_var, ctx) = get_var(ctx)

      let lhs_id = parser.id(lhs)
      let rhs_id = parser.id(rhs)

      let ctx = generate_constraints(ctx, lhs)
      let ctx = generate_constraints(ctx, rhs)

      let cons = case op {
        parser.Arithmetic(_) -> {
          let lhs_con = Constraint(origin: lhs_id, lhs: lhs_var, rhs: Num)
          let rhs_con = Constraint(origin: rhs_id, lhs: rhs_var, rhs: Num)

          let con = Constraint(origin:, lhs: outer, rhs: Num)
          [con, rhs_con, lhs_con, ..ctx.cons]
        }

        parser.Logical(_) -> {
          let lhs_con =
            Constraint(origin: lhs_id, lhs: lhs_var, rhs: Known(type_.BoolT))
          let rhs_con =
            Constraint(origin: rhs_id, lhs: rhs_var, rhs: Known(type_.BoolT))

          let con = Constraint(origin:, lhs: outer, rhs: Known(type_.BoolT))
          [con, rhs_con, lhs_con, ..ctx.cons]
        }

        parser.Relation(parser.In) -> {
          let lhs_con = Constraint(origin: lhs_id, lhs: lhs_var, rhs: lhs_var)
          let rhs_con =
            Constraint(origin: rhs_id, lhs: rhs_var, rhs: List(lhs_var))

          let con = Constraint(origin:, lhs: outer, rhs: Known(type_.BoolT))
          [con, rhs_con, lhs_con, ..ctx.cons]
        }
        parser.Relation(_) -> {
          let lhs_con = Constraint(origin: lhs_id, lhs: lhs_var, rhs: lhs_var)
          let rhs_con = Constraint(origin: rhs_id, lhs: rhs_var, rhs: lhs_var)

          let con = Constraint(origin:, lhs: outer, rhs: Known(type_.BoolT))
          [con, rhs_con, lhs_con, ..ctx.cons]
        }
      }

      Context(..ctx, cons:)
    }
    parser.FunctionCall(_name, this, args) -> {
      let #(outer, ctx) = get_var(ctx)
      let #(return, ctx) = get_var(ctx)

      let #(this_term, ctx) = case this {
        option.Some(this_expr) -> {
          let ctx = generate_constraints(ctx, this_expr)
          let #(term, ctx) = get_var(ctx)
          #([term], ctx)
        }
        option.None -> #([], ctx)
      }

      let ctx = list.fold(args, ctx, generate_constraints)
      let #(arrow_parts, ctx) =
        list.fold(args, #(this_term, ctx), fn(acc, arg) {
          let #(arg_terms, ctx) = acc
          let ctx = generate_constraints(ctx, arg)
          let #(arg_var, ctx) = get_var(ctx)

          #([arg_var, ..arg_terms], ctx)
        })

      let assert Ok(arrow) =
        [return, ..arrow_parts]
        |> list.reverse
        |> list.reduce(fn(a, b) { Arrow(a, b) })

      let con = Constraint(origin:, lhs: outer, rhs: arrow)
      add(ctx, con)
    }
  }
}
