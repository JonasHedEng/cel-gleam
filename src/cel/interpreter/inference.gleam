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

pub type InferenceError {
  InfiniteType(term: Term, occurs_in: Term)
  TypeMismatch(origin: Int, left: Term, right: Term)
}

pub type Term {
  Known(Type)
  Var(String)
  Iter(Term)
  Num
  Arrow(domain: Term, range: Term)
}

fn term_from_atom(atom: parser.Atom) -> Term {
  case atom {
    parser.Int(_) -> Known(type_.IntT)
    parser.UInt(_) -> Known(type_.UIntT)
    parser.Float(_) -> Known(type_.FloatT)
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
  Context(
    vars: Yielder(String),
    cons: List(Constraint),
    fn_sigs: Dict(String, #(List(Term), Term)),
  )
}

/// Create a `context.Context` with the default functions.
/// - **expr**: CEL Expression to infer types for.
/// - **function_signatures**: An optional `Dict` of function names along with their type signatures.
///
/// `infer_types` will do its best without any function type signatures provided but will not be able to figure out certain expression type relations without the provided type signatures.
pub fn infer_types(
  for expr: ExpressionData,
  with function_signatures: Dict(String, #(List(Term), Term)),
) -> Dict(Int, Term) {
  let vars =
    yielder.unfold(#(0, ""), fn(acc) {
      let #(counter, prefix) = acc
      let assert Ok(codepoint) = { counter % 25 } + 97 |> string.utf_codepoint

      case [codepoint] |> string.from_utf_codepoints {
        "z" -> yielder.Next(prefix <> "z", #(counter + 1, prefix <> "_"))
        letter -> yielder.Next(prefix <> letter, #(counter + 1, prefix))
      }
    })

  let ctx =
    Context(vars:, cons: [], fn_sigs: function_signatures)
    |> rewrite_fn_sig_vars

  let #(ctx, _) = generate_constraints(ctx, expr)
  let assert Ok(env) = unify(ctx.cons |> list.reverse, dict.new())
  ref_map_terms(ctx, env)
}

fn rewrite_term_rec(
  ctx: Context,
  map: Dict(String, Term),
  term: Term,
) -> #(Context, Dict(String, Term), Term) {
  case term {
    Num | Known(_) -> #(ctx, map, term)
    Arrow(domain: d, range: r) -> {
      let #(ctx, map, domain) = rewrite_term_rec(ctx, map, d)
      let #(ctx, map, range) = rewrite_term_rec(ctx, map, r)
      #(ctx, map, Arrow(domain:, range:))
    }
    Iter(inner) -> {
      let #(ctx, map, new_inner) = rewrite_term_rec(ctx, map, inner)
      #(ctx, map, Iter(new_inner))
    }
    Var(old_name) -> {
      case dict.get(map, old_name) {
        Ok(new) -> #(ctx, map, new)
        Error(_) -> {
          let #(new, ctx) = gen_var(ctx)
          let map = dict.insert(map, old_name, new)

          #(ctx, map, new)
        }
      }
    }
  }
}

fn rewrite_fn_sig_vars(ctx: Context) -> Context {
  let #(ctx, new_fn_sigs) =
    ctx.fn_sigs
    |> dict.to_list
    |> list.fold(#(ctx, []), fn(sigs_acc, sig) {
      let map = dict.new()
      let #(ctx, sigs) = sigs_acc
      let #(name, #(args, return)) = sig

      let assert #(ctx, _, [new_return, ..new_args]) =
        list.flatten([args, [return]])
        |> list.fold(#(ctx, map, []), fn(terms_acc, term) {
          let #(ctx, map, terms) = terms_acc
          let #(ctx, map, new_term) = rewrite_term_rec(ctx, map, term)
          #(ctx, map, [new_term, ..terms])
        })

      #(ctx, [#(name, #(new_args |> list.reverse, new_return)), ..sigs])
    })

  Context(..ctx, fn_sigs: new_fn_sigs |> dict.from_list)
}

fn unify(
  constraints: List(Constraint),
  env: Dict(String, Term),
) -> Result(Dict(String, Term), InferenceError) {
  case constraints {
    [] -> Ok(env)
    [Constraint(origin: id, lhs: left, rhs: right), ..rest] -> {
      let #(left, env) = substitute_term(env, left)
      let #(right, env) = substitute_term(env, right)

      case left, right {
        l, r if l == r -> unify(rest, env)

        Known(type_.IntT), Num
        | Known(type_.UIntT), Num
        | Known(type_.FloatT), Num
        | Num, Known(type_.IntT)
        | Num, Known(type_.UIntT)
        | Num, Known(type_.FloatT)
        -> unify(rest, env)

        Var(name), term | term, Var(name) -> {
          use _ <- result.try(occurs_check(Var(name), term))

          let env = dict.insert(env, name, term)
          let #(rest, env) = substitute_constraints(env, rest)

          unify(rest, env)
        }

        Arrow(l_domain, l_range), Arrow(r_domain, r_range) -> {
          let new_constraints = [
            Constraint(id, l_domain, r_domain),
            Constraint(id, l_range, r_range),
            ..rest
          ]
          unify(new_constraints, env)
        }

        Iter(l_inner), Iter(r_inner) -> {
          let new_constraints = [Constraint(id, l_inner, r_inner), ..rest]
          unify(new_constraints, env)
        }

        _, _ -> Error(TypeMismatch(origin: id, left:, right:))
      }
    }
  }
}

fn substitute_term(
  env: Dict(String, Term),
  term: Term,
) -> #(Term, Dict(String, Term)) {
  case term {
    Var(name) -> {
      dict.get(env, name)
      |> result.map(substitute_term(env, _))
      |> result.unwrap(#(term, env))
    }
    Arrow(domain, range) -> {
      let #(d, env) = substitute_term(env, domain)
      let #(r, env) = substitute_term(env, range)
      #(Arrow(d, r), env)
    }
    Iter(inner) -> {
      let #(i, env) = substitute_term(env, inner)
      #(Iter(i), env)
    }
    _ -> #(term, env)
  }
}

fn substitute_constraints(
  env: Dict(String, Term),
  constraints: List(Constraint),
) -> #(List(Constraint), Dict(String, Term)) {
  let #(cons, env) =
    list.fold(constraints, #([], env), fn(acc, con) {
      let #(cons, env) = acc
      let #(lhs, env) = substitute_term(env, con.lhs)
      let #(rhs, env) = substitute_term(env, con.rhs)

      #([Constraint(origin: con.origin, lhs:, rhs:), ..cons], env)
    })

  #(cons |> list.reverse, env)
}

fn occurs_check(left: Term, right: Term) -> Result(Nil, InferenceError) {
  case left {
    Arrow(domain, range) -> {
      use _ <- result.try(occurs_check(left, domain))
      occurs_check(left, range)
    }
    Iter(inner) -> occurs_check(left, inner)
    _ if left == right -> Error(InfiniteType(term: left, occurs_in: right))
    _ -> Ok(Nil)
  }
}

fn gen_var(ctx: Context) -> #(Term, Context) {
  let assert yielder.Next(var, vars) = ctx.vars |> yielder.step
  #(Var(var), Context(..ctx, vars:))
}

fn add(ctx, con) {
  Context(..ctx, cons: [con, ..ctx.cons])
}

fn generate_constraints(ctx: Context, expr: ExpressionData) -> #(Context, Term) {
  let origin = parser.id(expr)
  case parser.expr(expr) {
    parser.Atom(atom) -> {
      let #(var, ctx) = gen_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: term_from_atom(atom))

      #(add(ctx, con), var)
    }
    parser.Ident(_) -> {
      let #(var, ctx) = gen_var(ctx)
      let con = Constraint(origin:, lhs: var, rhs: var)

      #(add(ctx, con), var)
    }
    parser.List(inner_exprs) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

      let ctx =
        list.fold(inner_exprs, ctx, fn(ctx, inner_expr) {
          let inner_id = parser.id(inner_expr)
          let #(ctx, inner_var) = generate_constraints(ctx, inner_expr)

          add(ctx, Constraint(origin: inner_id, lhs: inner, rhs: inner_var))
        })

      let con = Constraint(origin:, lhs: outer, rhs: Iter(inner))
      #(add(ctx, con), outer)
    }
    parser.Map(_fields) -> todo
    parser.Member(_, _) -> todo
    parser.Ternary(cond, then, otherwise) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(cond_var, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

      let #(ctx, inner_cond_var) = generate_constraints(ctx, cond)

      let ctx =
        [inner_cond_var, Known(type_.BoolT)]
        |> list.map(Constraint(origin: parser.id(cond), lhs: cond_var, rhs: _))
        |> list.fold(ctx, add)

      let ctx =
        [then, otherwise]
        |> list.fold(ctx, fn(ctx, branch) {
          let origin = parser.id(branch)
          let #(ctx, inner_var) = generate_constraints(ctx, branch)

          add(ctx, Constraint(origin:, lhs: inner, rhs: inner_var))
        })

      #(add(ctx, Constraint(origin:, lhs: outer, rhs: inner)), outer)
    }
    parser.Unary(op, inner_expr) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(inner, ctx) = gen_var(ctx)

      let inner_id = parser.id(inner_expr)
      let #(ctx, inner_var) = generate_constraints(ctx, inner_expr)
      let inner_mapping =
        Constraint(origin: inner_id, lhs: inner, rhs: inner_var)

      let inner_con = case op {
        parser.Not ->
          Constraint(origin: inner_id, lhs: inner, rhs: Known(type_.BoolT))
        parser.UnarySub -> Constraint(origin: inner_id, lhs: inner, rhs: Num)
      }

      let outer_con = case op {
        parser.Not -> Constraint(origin:, lhs: outer, rhs: Known(type_.BoolT))
        parser.UnarySub -> Constraint(origin:, lhs: outer, rhs: Num)
      }

      let cons = [outer_con, inner_mapping, inner_con, ..ctx.cons]
      #(Context(..ctx, cons:), outer)
    }
    parser.BinaryOperation(lhs, op, rhs) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(lhs_var, ctx) = gen_var(ctx)
      let #(rhs_var, ctx) = gen_var(ctx)

      let lhs_id = parser.id(lhs)
      let rhs_id = parser.id(rhs)

      let #(ctx, inner_lhs_var) = generate_constraints(ctx, lhs)
      let #(ctx, inner_rhs_var) = generate_constraints(ctx, rhs)

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
            Constraint(origin: rhs_id, lhs: rhs_var, rhs: Iter(lhs_var))

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

      let ctx = Context(..ctx, cons:)
      let ctx =
        add(ctx, Constraint(origin: lhs_id, lhs: lhs_var, rhs: inner_lhs_var))
      let ctx =
        add(ctx, Constraint(origin: rhs_id, lhs: rhs_var, rhs: inner_rhs_var))

      #(Context(..ctx, cons:), outer)
    }
    parser.FunctionCall(name, this, args) -> {
      let #(outer, ctx) = gen_var(ctx)
      let #(return, ctx) = gen_var(ctx)

      let args = case this {
        option.Some(t) -> [t, ..args]
        option.None -> args
      }

      let #(sig_args, ctx) = case dict.get(ctx.fn_sigs, name) {
        Ok(#(sig_args, sig_return)) -> {
          let a =
            sig_args
            |> list.index_map(fn(arg, i) { #(i, arg) })
            |> dict.from_list
          let ctx = add(ctx, Constraint(origin:, lhs: return, rhs: sig_return))

          #(a, ctx)
        }
        Error(_) -> #(dict.new(), ctx)
      }

      let #(arrow_parts, ctx) =
        list.index_fold(args, #([], ctx), fn(acc, arg, i) {
          let #(arg_terms, ctx) = acc

          let origin = parser.id(arg)
          let #(ctx, inner_var) = generate_constraints(ctx, arg)
          let #(arg_var, ctx) = gen_var(ctx)

          let ctx = add(ctx, Constraint(origin:, lhs: arg_var, rhs: inner_var))

          let ctx = case dict.get(sig_args, i) {
            Ok(sig_arg) ->
              add(ctx, Constraint(origin:, lhs: arg_var, rhs: sig_arg))
            Error(_) -> ctx
          }

          #([arg_var, ..arg_terms], ctx)
        })

      let arrow =
        [return, ..arrow_parts]
        |> list.reverse
        |> list.reduce(Arrow)
        |> result.unwrap(return)

      let con = Constraint(origin:, lhs: outer, rhs: arrow)
      #(add(ctx, con), outer)
    }
  }
}

fn unify_constraint_origin(cons: List(Constraint)) -> Dict(Int, #(String, Term)) {
  use acc, con <- list.fold(cons, dict.new())
  use entry <- dict.upsert(acc, con.origin)

  case entry, con.rhs {
    option.None, _
    | option.Some(#(_, Var(_))), Known(_)
    | option.Some(#(_, Var(_))), Iter(_)
    -> {
      let assert Var(label) = con.lhs
      #(label, con.rhs)
    }
    option.Some(e), _ -> e
  }
}

fn ref_map_terms(ctx: Context, env: Dict(String, Term)) -> Dict(Int, Term) {
  ctx.cons
  |> unify_constraint_origin
  |> dict.to_list
  |> list.map(fn(con) {
    let #(origin, #(name, term)) = con
    case dict.get(env, name) {
      Ok(found) -> #(origin, found)
      Error(_) -> {
        io.println(
          "didn't find '"
          <> name
          <> "' in term refs, lost: "
          <> string.inspect(term),
        )

        #(origin, term)
      }
    }
  })
  |> dict.from_list
}
