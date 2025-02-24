import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option
import gleam/result
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
  Known(Type)
  Var(String)
  Num
  List(Term)
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

pub opaque type Context {
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

pub fn infer_types(expr: ExpressionData) -> #(Context, Dict(String, Term)) {
  let ctx = Context(vars: var_yielder(), cons: [])

  let ctx = generate_constraints(ctx, expr)
  let assert Ok(subs) = unify(ctx.cons |> list.reverse, dict.new())
  #(ctx, subs)
}

pub type TypeEnv =
  Dict(String, Term)

fn unify(constraints: List(Constraint), env: TypeEnv) -> Result(TypeEnv, String) {
  case constraints {
    [] -> Ok(env)
    [Constraint(origin: id, lhs: left, rhs: right), ..rest] -> {
      let left = substitute_term(env, left)
      let right = substitute_term(env, right)

      case left, right {
        l, r if l == r -> unify(rest, env)

        Var(name), term -> {
          case occurs_check(name, term) {
            True ->
              Error(
                "Infinite type: "
                <> name
                <> " occurs in "
                <> string.inspect(term),
              )
            False -> {
              let new_env = dict.insert(env, name, term)
              let rest = substitute_constraints(new_env, rest)
              unify(rest, new_env)
            }
          }
        }

        term, Var(name) -> {
          case occurs_check(name, term) {
            True ->
              Error(
                "Infinite type: "
                <> name
                <> " occurs in "
                <> string.inspect(term),
              )
            False -> {
              let new_env = dict.insert(env, name, term)
              let rest = substitute_constraints(new_env, rest)
              unify(rest, new_env)
            }
          }
        }

        Arrow(l_domain, l_range), Arrow(r_domain, r_range) -> {
          let new_constraints = [
            Constraint(id, l_domain, r_domain),
            Constraint(id, l_range, r_range),
            ..rest
          ]
          unify(new_constraints, env)
        }

        List(l_inner), List(r_inner) -> {
          let new_constraints = [Constraint(id, l_inner, r_inner), ..rest]
          unify(new_constraints, env)
        }

        _, _ ->
          Error(
            "Type mismatch at "
            <> string.inspect(id)
            <> ": cannot unify "
            <> string.inspect(left)
            <> " with "
            <> string.inspect(right),
          )
      }
    }
  }
}

fn substitute_term(env: TypeEnv, term: Term) -> Term {
  case term {
    Var(name) -> {
      case dict.get(env, name) {
        Ok(t) -> substitute_term(env, t)
        Error(_) -> term
      }
    }
    Arrow(domain, range) ->
      Arrow(substitute_term(env, domain), substitute_term(env, range))
    List(inner) -> List(substitute_term(env, inner))
    _ -> term
  }
}

fn substitute_constraints(
  env: TypeEnv,
  constraints: List(Constraint),
) -> List(Constraint) {
  list.map(constraints, fn(c) {
    Constraint(
      origin: c.origin,
      lhs: substitute_term(env, c.lhs),
      rhs: substitute_term(env, c.rhs),
    )
  })
}

fn occurs_check(name: String, term: Term) -> Bool {
  case term {
    Var(var_name) -> name == var_name
    Arrow(domain, range) ->
      occurs_check(name, domain) || occurs_check(name, range)
    List(inner) -> occurs_check(name, inner)
    _ -> False
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

// fn replace_all(
//   consts: List(Constraint),
//   subs: List(Substitution),
//   occurance left: Term,
//   with right: Term,
// ) -> #(List(Constraint), List(Substitution)) {
//   let consts =
//     list.map(consts, fn(c) {
//       let Constraint(origin:, lhs:, rhs:) = c
//       let lhs = replace(left, lhs, right)
//       let rhs = replace(left, rhs, right)
//       Constraint(origin:, lhs:, rhs:)
//     })

//   let subs =
//     list.map(subs, fn(s) {
//       let Substitution(id:, var:, is:) = s
//       let var = replace(left, var, right)
//       let is = replace(left, is, right)
//       Substitution(id:, var:, is:)
//     })

//   #(consts, subs)
// }

fn gen_var(ctx: Context) -> #(Term, Context) {
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
      let #(var, ctx) = gen_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: term_from_atom(atom))

      add(ctx, con)
    }
    parser.Ident(_) -> {
      let #(var, ctx) = gen_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: var)

      add(ctx, con)
    }
    parser.List(inner_exprs) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

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
      let #(outer, ctx) = gen_var(ctx)
      let #(cond_var, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

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
      let #(outer, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

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
      let #(outer, ctx) = gen_var(ctx)
      let #(lhs_var, ctx) = gen_var(ctx)
      let #(rhs_var, ctx) = gen_var(ctx)

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
      let #(outer, ctx) = gen_var(ctx)
      let #(return, ctx) = gen_var(ctx)

      let #(this_term, ctx) = case this {
        option.Some(this_expr) -> {
          let ctx = generate_constraints(ctx, this_expr)
          let #(term, ctx) = gen_var(ctx)
          #([term], ctx)
        }
        option.None -> #([], ctx)
      }

      let ctx = list.fold(args, ctx, generate_constraints)
      let #(arrow_parts, ctx) =
        list.fold(args, #(this_term, ctx), fn(acc, arg) {
          let #(arg_terms, ctx) = acc
          let ctx = generate_constraints(ctx, arg)
          let #(arg_var, ctx) = gen_var(ctx)

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

pub fn mapped_types(ctx: Context, env: Dict(String, Term)) -> Dict(Int, Term) {
  ctx.cons
  |> list.filter_map(fn(con) {
    case con.lhs {
      Var(name) -> {
        case dict.get(env, name) {
          Ok(found) -> Ok(#(con.origin, found))
          Error(_) -> {
            io.debug(env)
            io.println(
              "didn't find '"
              <> name
              <> "' in term refs, lost: "
              <> string.inspect(con),
            )
            Error(Nil)
          }
        }
      }
      other -> {
        io.println("got unexpected term: " <> string.inspect(other))

        Error(Nil)
      }
    }
  })
  |> dict.from_list
}
