

let fact x = 
    if x < 0 then failwith "parametr shoud not be negative"
    else 
        let rec _innerfact x prev =
            if prev = 0 then 1  else x * _innerfact prev (prev-1)        
        _innerfact x (x-1)

let fib x =
   if x < 0 then failwith "enter a positive number"
   else 
       let rec _innerfib prev1 prev2 i =
           if i = 0 then prev2
           else _innerfib (prev1 + prev2) prev1 (i-1)
       _innerfib 1 0 x

let listReverse (l: List<int>) =
    let rec _innerrev l = 
        match l with 
        | [] ->[]
        | [x]->[x]
        | h::t -> _innerrev (t) @ [h]
    _innerrev l
       
let pows n m =
    let rec powsList (l: List<int>) m = 
        if m = 0 then l
        else 
            match l with 
            | []-> powsList [pown 2 n] (m-1)
            | [x]->powsList ((2 * x)::l) (m-1)
            | h::t -> powsList ((2 * h)::l) (m-1)
    listReverse (powsList [] m)

let findEl (l: List<int>) el =
    let rec _innerFind l el pos = 
        match l with 
        |[] -> -1
        |[x] -> 
            if x = el then pos else -1
        | h:: t->
            if h = el then pos else _innerFind t el pos+1
    _innerFind l el 0

let rec printList (l: List<int>) =
    match l with 
    |[] -> "the list is empty"
    |[x]-> printfn "%d " x
    |h::t ->
        printf "%d " h
        printList(t)