/* categories bidirectionally 

http://davidchristiansen.dk/tutorials/bidirectional.pdf

*/



module StringMap = Map.Make({
  type t = string;
  let compare = compare
});


type ty
= TyBool | TyInt
| TyFn(ty, ty);

type context = StringMap.t(ty);
let emptyContext : context = StringMap.empty;

type term
= TVar(string)
| TAp(term, term)
| TAbs(string, term)
| TTrue | TFalse
| TVal(int)
| TIf(term, term, term)
| TTy(term, ty);

let lookup = (ctx:context, x:string) : option(ty) =>
try(Some(StringMap.find(x, ctx))) {
  | Not_found => None
};

let extend = (ctx:context, l:string, t:ty) : context => StringMap.add(l, t, ctx);

let rec checkType = (ctx:context, x:term, t:ty) => switch(x) {
| TIf(t1,t2,t3) => {
    let i1 = checkType(ctx, t1, TyBool);
    let i2 = checkType(ctx, t2, t);
    let i3 = checkType(ctx, t3, t);
    switch (i1, i2, i3) {
      | (Some(TyBool), Some(_), Some(_)) => Some(t)
      | _ => None
    }
}
/* \l. ex : a -> b ? */
| TAbs(lam, expr) => switch(t) {
    | TyFn(a,b) => switch(checkType(extend(ctx,lam,a),expr,b)){
      | Some(_) => Some(t)
      | _ => None 
    }
    | _ => None
}
| _ => switch(inferTy(ctx,x)) {
    | Some(t2) => if (t == t2) { Some(t) } else None
    | _ => None
  }
}

and inferTy = (ctx : context, t : term) : option('ty) => switch(t) {
| TTrue => Some(TyBool)
| TFalse => Some(TyBool)
| TVar(x) => lookup(ctx, x)
| TTy(x,t) => checkType(ctx,x,t)
| TAp(f,x) => {
  let fi = inferTy(ctx, f);
  switch(fi) {
    | Some(TyFn(a,b)) => switch(checkType(ctx, x, a)) {
        | Some(_) => Some(b)
        | _ => None
    }
    | _ => None
  }
}
| TIf(t1, t2, t3) => {
    let i1 = inferTy(ctx, t1);
    let i2 = inferTy(ctx, t2);
    let i3 = inferTy(ctx, t3);
    switch (i1, i2, i3) {
    | (Some(TyBool), Some(ty2), Some(ty3)) =>
      if (ty2 == ty3) {
          Some(ty2)
      } else None
    | _ => None
    }
}
| _ => None
};


let rec showTyOption = (x) => switch (x) {
| Some(t) => switch(t) {
  | TyBool => "bool"
  | TyInt => "int"
  | TyFn(a,b) => {
      let sa = showTyOption(Some(a));
      let sb = showTyOption(Some(b));
      "(" ++ sa ++ ")->(" ++ sb ++ ")"
      }
  }
| _ => "couldn't infer type"
};

let ctx = StringMap.add("y", TyBool, emptyContext)
  /* |> StringMap.add("y", TyInt)
  |> StringMap.add("f", TyFn(TyInt,TyInt))
  |> StringMap.add("g", TyFn(TyInt,TyBool)) */
  |> StringMap.add("z", TyInt);

/* let gy = TAp(TVar("g"),TVar("y"));
let fy = TAp(TVar("f"),TVar("y"));
let t = TIf(gy,fy,TVar("z"));
let t2 = TIf(gy,TVar("g"),TVar("g")); */

let foo = TTy(TAbs("x", TVar("x")),TyFn(TyBool,TyBool));
let fooZ = TAp(foo,TVar("y"));

Js.log(showTyOption(inferTy(ctx, fooZ)));
/* Js.log(showTyOption(inferTy(ctx, t2))); */