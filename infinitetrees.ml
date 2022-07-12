type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);;

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec layer_tree r = LNode(r, (fun() -> layer_tree(r+1)), (fun() -> layer_tree (r+1)) );; 

let rec interval_tree l r = LNode ( (l,r), ( fun() -> interval_tree l ((l+.r)/.2.) ), ( fun() -> interval_tree  ((l+.r)/.2.)  r) );;

let rec rational_tree n d = LNode ( (n,d), ( fun() -> rational_tree n (d+1) ), ( fun() -> rational_tree (n+1) d) );; 

let rec top n tree=
  match n with 
  | 0 -> Empty
  | _ -> (
      match tree with
      | LNode(value,fun_l,fun_r) -> Node (value, top (n-1) (fun_l()) , top (n-1) (fun_r()) )
    )
;; 

let rec map f tree=
  match tree with 
  | LNode(value,fun_l,fun_r) -> LNode(f value, (fun() -> map f (fun_l())),(fun() -> map f (fun_r())))
;;

let rec find_help p queue=
  match queue with
  | LNode (value, fun_l, fun_r)::rest -> 
      if p value then LNode (value, fun_l, fun_r)
      else find_help p (rest @ [fun_l();fun_r()] )
;;
  
let rec find p tree= find_help p [tree] ;;









