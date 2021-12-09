module CSC7B  // at the very top
open CSC7B;

//Returns the first value in an environment using pattern matching
let getfirst (env:environ) = 
    match env with
        | [] -> raise(System.Exception("Environment is empty"))
        | ((y, rv)::cdr) -> !rv;

//Returns the name of a value or lambda-term/closure
let rec lookupname x (env:environ) = 
    match env with
        | [] -> raise(System.Exception("not found in environment/table"))
        | ((y,rv)::cdr) when x = !rv -> y
        | ((y,rv)::cdr) -> lookupname x cdr 

let cbv = 1; //0 if implementing cbn

let base_eval = eval
eval <- fun env exp ->  // eval takes a environ structure 'env' and and expr 'exp'
  //printfn "%A" exp
  match exp with   //if you match against (env,exp), your pattern must be a pair
    | Binop("&&", a, b) -> if (eval env a)=0 || (eval env b)=0 then 0 else 1
    | Binop("||", a, b) -> if (eval env a)=0 && (eval env b)=0 then 0 else 1 
    | Binop("==", a, b) -> if (eval env a)=(eval env b) then 1 else 0
    | Binop("<", a, b) -> if (eval env a)<(eval env b) then 1 else 0
    | Binop("<=", a, b) -> if (eval env a)<=(eval env b) then 1 else 0
    | Binop("while", clause, body) ->
        let mutable retVal = 0
        while (eval env clause) <> 0 do
            retVal <- (eval env body)
        retVal;
    | Uniop("!", a) -> if (eval env a)=0 then 1 else 0
    | Ifelse(case, t, f) -> 
        if (eval env case)=0 then (eval env f) else (eval env t)
    | Assign(var, value) -> 
        let n = (eval env value)
        changeval var n env; 
        n;
    | Seq(elist) -> 
        let mutable n = 0
        let mutable retVal = 0
        while n < elist.Length do
            retVal <- (eval env (elist.[n]))
            n <- n+1
        retVal;
      //Let for handling closure - create a closure for the Lambda,
      //add closure to a new_env and evaluate the body of the let
    | Letexp(var, Lambda(lvar, lbody), body) ->
        let mutable cl = Closure(env, Lambda(lvar, lbody))
        let new_env = appended var cl env 
        (eval new_env body) //App(str-func-name, Expr)
      //Let for regular val - implement call by value by evaluating value
      //first, then add it to a new_env and eval the body
    | Letexp(var, value, body) ->
        let valx = eval env value
        let new_env = appended var (Val(valx)) env
        (eval new_env body)
      //Apply a value to a lambda term - evaluate the value,
      //locate the closure, and add the value to a temporary variable
      //in a new_env which we will evaluate the closure in
    | App(var, expres) when cbv=1 ->
        let f = lookup var env //f is a closure
        let x = Val(eval env expres)
        let mutable new_env = appended "temp" x env 
        eval new_env f //Closure(env, Lambda(lvar, lbody))
      //Closure for a lambda term - find the value we will apply to
      //the lambda term, append it to the new_env, find the name of
      //the closure so we can implement recursion by adding it to the
      //new_env, and evaluate the lambda body.
    | Closure(environment, Lambda(lvar, lbody)) as cl ->  
        let apply = getfirst env //value to apply to lambda term
        let mutable new_env = appended lvar apply environment
        //add the func to the closure environment
        let test = lookupname cl env
        new_env <- appended test cl new_env
        eval new_env lbody 
    | Var(str) ->
        let expres = lookup str env
        eval env expres
    | _ -> base_eval env exp;; // this links your extentions with the original
    
runit(); // at the very bottom
