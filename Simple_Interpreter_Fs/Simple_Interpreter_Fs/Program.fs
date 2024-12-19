open System
// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report
 
type BindingType =
    | Variable
    | Function
type terminal = 
    | Func
    | For
    | While
    | Rem
    | Pow
    | Exp
    | Log of log
    | Lpar
    | Rpar
    | Lbrace
    | Rbrace
    | Neg
    | Arith of arithmetic
    | Num of number
    | Var of string
    | Assign
    | Trig of trig
    | Null
    | Pi
    | Abs
    | End
    | DataType of dataType
    | Relational of relational
    | Logical of logical
    
    member this.toNumber() =
        match this with
        | Num n -> n
        | _ -> raise (Exception("Cannot cast this terminal type to a number")) 
and number =
    Int of int | Flt of float | Bool of bool | Frac of int * int
    override this.ToString() =
        match this with
        | Int i -> i.ToString()
        | Flt f -> f.ToString()
        | Frac (num,denom) -> num.ToString() + "/" + denom.ToString()
        | Bool b -> b.ToString().ToLower()
    member this.TypeToString() =
        match this with
        | Int _ -> "int"
        | Flt _ -> "float"
        | Frac _ -> "frac"
        | Bool _ -> "bool"
    static member fltVal n = match n with
                             | Flt f -> f
                             | Int i -> float i
                             | Frac (upper,lower) -> float upper / float lower
                             | Bool b -> if b then 1.0 else 0.0
    static member intVal n = match n with
                             | Flt f -> int f
                             | Int i -> i
                             | Frac (num,denom) -> int (float num / float denom)
                             | Bool b -> if b then 1 else 0
    member this.ToInt() =
        match this with
        | Int i -> Int i
        | Flt f -> Int (int f)
        | Frac (n,d) -> Int (int (number.fltVal(Frac(n,d))))
        | Bool b -> if b then Int 1 else Int 0
    
    member this.ToBool() =
        match this with
        | Int i -> if i = 0 then Bool false else Bool true
        | Flt f -> if f = 0.0 then Bool false else Bool true
        | Frac (num,denom) -> if num = 0 then Bool false else Bool true
        | Bool b -> Bool b
    static member toFraction(inputIn) =
     let num, denom = number.toFractionHelp(inputIn, Int 1)
     Frac (number.intVal(num),number.intVal(denom))
     
    static member toFractionHelp(num, denom) =
        if num % Int 10 <> Flt 0.0 then
            number.toFractionHelp(num * Flt 10.0, denom * Int 10)
        else
            (num / Int 10, denom / Int 10)

    static member simplflyFract(input:number) =
        let inputs = match input with
                     | Frac (num, denom) -> (num, denom)
                     | _ -> (0,0)
        let gcdNum (n:int, d:int) =    
            match (n % d) with
                | 0 -> d
                | _ -> n % d
        let gcdDenom (n:int, d:int) =
            match (d % n) with
                | 0 -> n
                | _ -> d % n
        let rec gcd (upper, lower) = 
            if upper >= lower then    
                let divisor = gcdNum (upper, lower)
                if divisor < lower = true then gcd(divisor, lower) else divisor
            else 
                let divisor = gcdDenom (upper, lower)
                if divisor < upper = true then gcd(upper, divisor) else divisor
        let num, denom = inputs
        let result = gcd(num, denom)
        (num/result, denom/ result) 
    static member (+) (x: number, y: number) = match (x, y) with
                                               | Int x, Int y -> Int (x + y)
                                               | Frac (upper, lower), Int y ->
                                                   Frac (number.simplflyFract(number.toFraction(Int y) + Frac (upper, lower)))
                                               | Int y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Int y) + Frac (upper, lower)))   
                                               | Frac (upper, lower), Flt y ->
                                                   Frac (number.simplflyFract(number.toFraction(Flt y) + Frac (upper, lower)))
                                               | Flt y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Flt y) + Frac (upper, lower)))   
                                               | Frac (upper, lower), Frac (upperTwo, lowerTwo) ->
                                                   Frac (number.simplflyFract(Frac (upper*lowerTwo + lower * upperTwo, lower*lowerTwo)))    
                                               | _ -> Flt (number.fltVal x + number.fltVal y)
    static member (-) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x - y)
                                             | Frac (upper, lower), Int y ->
                                                   Frac (number.simplflyFract(Frac (upper, lower) - number.toFraction(Int y))) 
                                             | Int y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Int y) - Frac (upper, lower)))      
                                             | Frac (upper, lower), Flt y ->
                                                   Frac (number.simplflyFract(Frac (upper, lower) - number.toFraction(Flt y))) 
                                             | Flt y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Flt y) - Frac (upper, lower)))
                                             | Frac (upper, lower), Frac (upperTwo, lowerTwo) ->
                                                   Frac (number.simplflyFract(Frac (upper*lowerTwo - lower * upperTwo, lower*lowerTwo))) 
                                             | _ -> Flt (number.fltVal x - number.fltVal y)
    static member (*) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x * y)
                                             | Frac (upper, lower), Int y ->
                                                   Frac (number.simplflyFract(number.toFraction(Int y) * Frac (upper, lower)))
                                             | Int y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Int y) * Frac (upper, lower)))     
                                             | Frac (upper, lower), Flt y ->
                                                   Frac (number.simplflyFract(number.toFraction(Flt y) * Frac (upper, lower)))
                                             | Flt y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Flt y) * Frac (upper, lower)))      
                                             | Frac (upper, lower), Frac (upperTwo, lowerTwo) ->
                                                   Frac (number.simplflyFract(Frac (upper * upperTwo, lower * lowerTwo))) 
                                             | _ -> Flt (number.fltVal x * number.fltVal y)
    static member (/) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x / y)
                                             | Frac (upper, lower), Int y ->
                                                   Frac (number.simplflyFract(Frac (upper, lower) / number.toFraction(Int y))) 
                                             | Int y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Int y) / Frac (upper, lower)))    
                                             | Frac (upper, lower), Flt y ->
                                                   Frac (number.simplflyFract(Frac (upper, lower) / number.toFraction(Flt y))) 
                                             | Flt y, Frac (upper, lower) ->
                                                   Frac (number.simplflyFract(number.toFraction(Flt y) / Frac (upper, lower)))      
                                             | Frac (upper, lower), Frac (upperTwo, lowerTwo) ->
                                                   Frac (number.simplflyFract(Frac (upper * lowerTwo, lower * upperTwo))) 
                                             | _ -> Flt (number.fltVal x / number.fltVal y)
    static member (%) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x % y)
                                             //| Frac (upper, lower), Int y ->
                                             //      Frac (upper, lower) % (number.toFraction(Int y)) 
                                             //| Int y, Frac (upper, lower) ->
                                             //      (number.toFraction(Int y)) % Frac (upper, lower)
                                             //| Frac (upper, lower), Flt y ->
                                             //       Frac (upper, lower) % (number.toFraction(Flt y)) 
                                             //| Flt y, Frac (upper, lower) ->
                                             //       (number.toFraction(Flt y)) % Frac (upper, lower)     
                                             //| Frac (upper, lower), Frac (upperTwo, lowerTwo) ->
                                             //       Flt (((upper * lowerTwo) / lower) % upperTwo)       
                                             | _ -> Flt (number.fltVal x % number.fltVal y)
    static member Pow (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (pown x y)
                                             | Frac (upper, lower), Int y ->
                                                   Frac (number.simplflyFract(number.Pow(Frac (upper, lower), number.toFraction(Int y)))) 
                                             | Int y, Frac (upper, lower) ->                                      
                                                   let power = float upper / float lower
                                                   let result = (Flt (number.fltVal(Int y) ** number.fltVal (Flt power)))
                                                   result.ToInt()
                                             | Frac (upper, lower), Flt y -> 
                                                   Frac (number.simplflyFract(number.Pow(Frac (upper, lower), number.toFraction(Flt y))))
                                             | Flt y, Frac (upper, lower) ->
                                                   Flt (number.fltVal (Flt y) ** (float upper/float lower))      
                                             | Frac (upper, lower), Frac (upperTwo, lowerTwo) ->
                                                   let upperFloat = ((float upper) ** (float upperTwo/float lowerTwo))
                                                   let lowerFloat = ((float lower) ** (float upperTwo/float lowerTwo)) 
                                                   Frac (number.simplflyFract(number.toFraction(Flt upperFloat/Flt lowerFloat))) 
                                             | _ -> Flt (number.fltVal x ** number.fltVal y)
    static member (~-) (n:number) = match n with
                                    | Int n -> Int -n
                                    | Frac (upper, lower) -> Frac (-upper, lower)
                                    | Flt n -> Flt -n
    static member (~+) (n:number) = match n with
                                    | Int n -> Int +n
                                    | Frac (upper, lower) -> Frac (+upper, lower)
                                    | Flt n -> Flt +n
    static member Floor (n:number) = match n with
                                     | Flt n -> Flt (Math.Floor(n))
                                     | Frac (upper, lower) -> Frac (number.simplflyFract(number.toFraction(Flt (Math.Floor(float upper/ float lower)))))
                                     | _ -> n
    static member Abs (n:number) = match n with
                                   | Int n -> Int (Math.Abs(n))
                                   | Flt n -> Flt (Math.Abs(n))
                                   | Frac (upper, lower) -> Frac (number.simplflyFract(number.toFraction(Flt (float (Math.Abs(upper/lower))))))
and trig =
     Sin | Cos | Tan | ASin | ACos | ATan
and arithmetic =
    Add | Sub | Mul | Div | FlrDiv
and log =
    LogN | LogOther
and relational =
    Equal | Greater | Less | GreaterEqu | LessEqu
and logical =
    And | Or | Not
and dataType =
    Integer | Float | Fraction | Boolean | Auto

//Errors
let lexError = Exception("invalid symbol in expression")
let varError = Exception("incorrect var assignment")
let intVal (c:char) = int(int c - int '0')
let floatVal (c:char) = float(int c - int '0')
let strVal (c:char) = string c
let parseError = Exception("Parser error")
let divisionByZeroError = Exception("Division by zero is undefined")
let DenominatorisZeroError = Exception("Zero Denominator is undefined")
let tanUndefinedError = Exception("Tan call will result in undefined behavior.")
let sinUndefinedError = Exception("Sin call will result in undefined behavior.")
let cosUndefinedError = Exception("Cos call will result in undefined behavior.")
let logInputError = Exception("Input Error By User for function Log and Ln")
let unclosedParensError = Exception("Syntax error Unmatched Parentheses")
let unclosedBracketsError = Exception("Syntax error Unmatched Bracket")
let unclosedBracesError = Exception("Syntax error Unmatched Brace")
let undefinedVarError = Exception("Syntax error Variable is not defined")
let typeError = Exception("Type mismatch")
let unmatchedParamError = Exception("Syntax error Function body contains parameters not specified in function signature")
let NaNError = Exception("Syntax error SymbolTable entry of BindingType Variable contains non-Num value in token list")
let bindingTypeError = Exception("Syntax error BindingType in table is undefined")
let funcAsParamError = Exception("Syntax error Functions not yet supported as functions")
let countLessThanOneException = Exception("Syntax error Cannot iterate expression for less than 1 iteration")

//Functions
let str2lst s = [for c in s -> c]
let isblank c = Char.IsWhiteSpace c
let isdigit c = Char.IsDigit c
let isletter c = Char.IsLetter c
let isletterordigit c = Char.IsLetterOrDigit c
let tanUndefinedList = [for i in -1000.0 .. 1000.0 do yield ((Math.PI/2.0) + Math.PI*i)]
let indexOf (e, array) = Array.IndexOf(array, e)
let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail               
    | [] -> Console.Write("EOL\n")
            []
let checkAgainstTanList(x:float) =
    tanUndefinedList |> List.contains x
let checkPositive (x:float) =
    if x > 0.0 then true else false
let checkLogEdgeCase (newBase:float) =
    if (newBase = 1.0) then true else false  
let checkBetweenATrigValues(x:float) =
    let out = fun input -> (input >= -(1.0) && input <= 1.0)
    out x
let getFloatRadian value =
     number.fltVal(value) * (Math.PI / 180.0)
let getFloatDegrees value =
    value * (180.0/Math.PI)        
let rec scInt(iStr, iVal) =
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)
and scFloat(iStr, iVal, weight) =
    match iStr with
    c :: tail when isdigit c ->
        scFloat(tail, iVal + weight * floatVal c, weight / 10.0)
    | _ -> (iStr, iVal)
    
/////MORE HERE // MORE HERE // MORE HERE // MORE HERE // MORE HERE // MORE HERE // MORE HERE // MORE HERE // MORE HERE // MORE HERE //                      
    
let trigHelperFunction (value, mathsFunction, listInput) =
    let result = mathsFunction(getFloatRadian value)
    if Math.Round(float result, 10) = 0.0 then
         (listInput, Flt 0.0) 
    else (listInput, Flt (mathsFunction(getFloatRadian value)))

let isReservedWord inString =
    match inString with
    | "ln"  -> Log LogN
    | "log" -> Log LogOther
    | "asin" -> Trig ASin
    | "acos" -> Trig ACos
    | "atan" -> Trig ATan
    | "sin" -> Trig Sin
    | "cos" -> Trig Cos
    | "tan" -> Trig Tan
    | "pi" -> Pi
    | "abs" -> Abs
    | "int" -> DataType Integer
    | "float" -> DataType Float
    | "frac" -> DataType Fraction
    | "bool" -> DataType Boolean
    | "var" -> DataType Auto
    | "fn" -> Func
    | "for" -> For
    | "while" -> While
    | "true" -> Num (Bool true)
    | "false" -> Num (Bool false)
    | _ -> Null

let rec scName(remain : list<char>, word : string) =
    match remain with
    c :: tail when isletterordigit c -> let cStr =  string c
                                        scName(tail, word + cStr)
    | _ -> (remain, word)
 
let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Arith Add :: scan tail
        | '-'::tail -> Arith Sub :: scan tail
        | '*'::tail -> Arith Mul :: scan tail
        | '/'::'/'::tail -> Arith FlrDiv :: scan tail
        | '/'::tail -> Arith Div :: scan tail
        | '%'::tail -> Rem :: scan tail
        | 'e'::'^'::tail -> Exp :: scan tail
        | '^'::tail -> Pow :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | '!'::tail -> Logical Not :: scan tail
        | '&'::'&'::tail -> Logical And :: scan tail
        | '|'::'|'::tail -> Logical Or :: scan tail
        | '<'::'='::tail -> Relational LessEqu :: scan tail
        | '>'::'='::tail -> Relational GreaterEqu :: scan tail
        | '<'::tail -> Relational Less :: scan tail
        | '>'::tail -> Relational Greater :: scan tail
        | '='::'='::tail -> Relational Equal :: scan tail
        | '{'::tail -> Lbrace:: scan tail
        | '}'::tail -> Rbrace:: scan tail
        | '='::tail -> Assign:: scan tail
        | ';'::tail -> End:: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let iStr, iVal = scInt(tail, intVal c)
                                      match iStr with
                                      | '.' :: c :: tail when isdigit c -> let iStr, iVal = scFloat(c :: tail, float iVal, 0.1)
                                                                           Num (Flt iVal) :: scan iStr
                                      | _ -> Num (Int iVal) :: scan iStr
        | c :: tail when isletter c -> let iStr, oStr = scName(tail, string c)
                                       let result = isReservedWord oStr
                                       if result <> Null then result :: scan iStr else Var oStr :: scan iStr                             
        | _ -> Console.WriteLine("Unexpected symbol '" + input.[0].ToString() + "'")
               raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:

//<trig> ::= "Sin" | "Cos" | "Tan" | "Asin" | "Acos" | "Atan"
//<number> ::= "Flt" | "Int" | "Bool" | "Frac" 
//<log> ::= "logN" | "logOther"

// version 1.0
// <L>        ::= <R> <Lopt>
// <Lopt>     ::= "!" <R> <Lopt> | "&&" <R> <Lopt> | "||" <R> <Lopt>
// <R>        ::= <E> <Ropt>
// <Ropt>     ::= "==" <E> <Ropt> | "<" <E> <Ropt> | ">" <E> <Ropt>
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> |  "%" <F> <Topt> | <empty>
// <F>        ::= <NR> <Fopt>
// <Fopt>     ::= "^" <NR> <Fopt> | "-" <NR> | <trig> <F> | <log> <F> | "Exp" <F> |<empty> 
// <NR>       ::= "Var" <value> "Assign" <NR> | "Var" <value> | <number> <value> | "(" <E> ")"

// version 2.0
// <L>        ::= <R> <Lopt>
// <Lopt>     ::= "!" <R> <Lopt> | "&&" <R> <Lopt> | "||" <R> <Lopt>
// <R>        ::= <E> <Ropt>
// <Ropt>     ::= "==" <E> <Ropt> | "<" <E> <Ropt> | ">" <E> <Ropt>
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> |  "%" <F> <Topt> | <empty>
// <F>        ::= <NR> <Fopt>
// <Fopt>     ::= "^" <NR> <Fopt> | "-" <NR> | <trig> <F> | <log> <F> | "Exp" <F> |<empty>
// <NR>       ::= "Var" <value> "Assign" <NR> | "Var" <value> | <number> <value> | "(" <E> ")"

let parser tList =
    let rec L tList = (R >> Lopt) tList
    and Lopt tList =
        match tList with
        | Logical Not :: tail -> (R >> Lopt) tail
        | Logical Or :: tail -> (R >> Lopt) tail
        | Logical And :: tail -> (R >> Lopt) tail
        | _ -> tList
    and R tList = (E >> Ropt) tList
    and Ropt tList =
        match tList with
        | Relational Equal :: tail -> (E >> Ropt) tail
        | Relational Less :: tail -> (E >> Ropt) tail
        | Relational Greater :: tail -> (E >> Ropt) tail
        | _ -> tList
    and E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Arith Add :: tail -> (T >> Eopt) tail
        | Arith Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (F >> Topt) tList
    and Topt tList =
        match tList with
        | Arith Mul :: tail -> (F >> Topt) tail
        | Arith Div :: tail -> (F >> Topt) tail
        | Arith FlrDiv :: tail -> (F >> Topt) tail
        | Rem :: tail -> (F >> Topt) tail
        | _ -> tList
    and F tList = (NR >> Fopt) tList
    and Fopt tList = 
        match tList with
        | Pow :: tail -> (NR >> Fopt) tail
        | _ -> tList
    and NR tList =
        match tList with
        | Arith Sub :: tail -> tail
        | Num number :: tail -> tail
        | Exp :: tail -> (NR >> Fopt) tail
        | Log terminal :: tail -> (NR >> Fopt) tail 
        | Trig terminal :: tail -> (F >> Topt) tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise unclosedParensError
        | _ -> raise parseError
    L tList

//let mutable variables = Map.empty   //acts as the symbol table currently (may want revision, very rudimentary)
let mutable symbolTable = Map.empty

let parseNeval tList =                         //Because L=R, then R=E and so on, tList executes from bottom to top (NR -> Fopt -> Topt -> Eopt -> Ropt -> Lopt)
    let rec L tList = (R >> Lopt) tList         
    and Lopt (tList, value: number) =
        match tList with
        | End :: tail -> match tail with
                         | [] -> (tList, value)
                         | _ -> L tail
        | Logical And :: tail -> let tLst, tval = R tail
                                 let andval = 
                                     match value.ToBool(), tval.ToBool() with
                                     | Bool false, _ -> Bool false
                                     | _ , Bool false -> Bool false
                                     | _ -> Bool true
                                 Lopt (tLst, andval)
        | Logical Or :: tail -> let tLst, tval = R tail
                                let orval =
                                    match value.ToBool(), tval.ToBool() with
                                    | Bool true, _ -> Bool true
                                    | _, Bool true -> Bool true
                                    | _ -> Bool false
                                Lopt (tLst, orval)
        | _ -> (tList, value)       //<Lopt>     ::= "&&" <R> <Lopt> | "||" <R> <Lopt> | <empty>       //("!" <R> <Lopt>) moved to NR -> not symbol must execute first
    and R tList = (E >> Ropt) tList
    and Ropt (tList, value) =
        match tList with
        | Relational Equal :: tail -> let tLst, tval = E tail
                                      let isEqual = if number.fltVal(value) = number.fltVal(tval) then Bool true else Bool false
                                      Ropt (tLst, isEqual)
        | Relational Less :: tail -> let tLst, tval = E tail
                                     let isLess = if number.fltVal(value) < number.fltVal(tval) then Bool true else Bool false
                                     Ropt (tLst, isLess)
        | Relational Greater :: tail -> let tLst, tval = E tail
                                        let isGreater = if number.fltVal(value) > number.fltVal(tval) then Bool true else Bool false
                                        Ropt (tLst, isGreater)
        | Relational LessEqu :: tail -> let tLst, tval = E tail
                                        let isLessEqu = if number.fltVal(value) <= number.fltVal(tval) then Bool true else Bool false
                                        Ropt (tLst, isLessEqu)
        | Relational GreaterEqu :: tail ->  let tLst, tval = E tail
                                            let isGreaterEqu = if number.fltVal(value) >= number.fltVal(tval) then Bool true else Bool false
                                            Ropt (tLst, isGreaterEqu)
        | _ -> (tList, value)               // <Ropt>     ::= "==" <E> <Ropt> | "<" <E> <Ropt> | ">" <E> <Ropt> | <empty>          
    and E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Arith Add :: tail -> let tLst, tval = T tail 
                               Eopt (tLst, value + tval)
        | Arith Sub :: tail -> let tLst, tval = T tail
                               Eopt (tLst, value - tval)
        | _ -> (tList, value)              // <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    and T tList = (F >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Arith Mul :: tail -> let tLst, tval = F tail
                               Topt (tLst, value * tval)
        | Arith Div :: tail -> let tLst, tval = F tail
                               if tval = Int 0 || tval = Flt 0.0 then raise divisionByZeroError else Topt (tLst, value / tval)
        | Arith FlrDiv :: tail -> let tLst, tval = F tail
                                  if tval = Int 0 || tval = Flt 0.0 then raise divisionByZeroError else Topt (tLst, number.Floor(value / tval))
        | Rem :: tail -> let tLst, tval = F tail
                         Topt (tLst, value % tval)                  
        | _ -> (tList, value)               // <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> | "//" <F> <Topt> | "%" <F> <Topt> | <empty>
    and F tList = (NR >> Fopt) tList           
    and Fopt (tList, value) =
        match tList with
        | Pow :: tail -> let tLst, tval = NR tail
                         Fopt (tLst, number.Pow(value, tval))
        | _ -> (tList, value)               // <Fopt>     ::= "^" <NR> <Fopt> | <empty>         //("-" <NR> | <trig> <F> | <log> <F> | "Exp" <F>) moved to NR as must be evaluated first 
    and NR tList =
        // <NR> :=  "!" <NR> |
        //          "-" <NR> | "+" <NR> |
        //          <int> <value> | <flt> <value> | <bool> <value> |
        //          HOW DO YOU DO FRACTIONS !!!! |
        //          "(" <int> <value> "/" <int> <value> ")"

        match tList with           
        | Logical Not :: tail -> let tLst, tval = NR tail
                                 let notval = match tval.ToBool() with
                                              | Bool b -> Bool (not b)
                                              | _ -> Console.WriteLine("NOT operator expected a boolean but got " + tval.TypeToString())
                                                     raise parseError
                                 (tLst, notval)
        | Arith Sub :: tail -> let tLst, tval = NR tail
                               (tLst, -tval)
        | Arith Add :: tail -> let tLst, tval = NR tail
                               (tLst, +tval)
        | Num (Int value) :: tail -> (tail, Int value)
        | Num (Flt value) :: tail -> (tail, Flt value)
        | Num (Bool value) :: tail -> (tail, Bool value)
        | DataType Fraction :: Num (Int num) :: Arith Div :: Num (Int denom) :: tail ->  if denom <> 0 then (tail, Frac (num, denom))
                                                                                         else raise DenominatorisZeroError                                                                                 
        | DataType Fraction :: Lpar :: Num (Int num) :: Arith Div :: Num (Int denom) :: Rpar :: tail -> if denom <> 0 then (tail, Frac (num, denom))
                                                                                                        else raise DenominatorisZeroError 
        | Abs :: tail -> let tLst, tval = NR tail
                         (tLst, number.Abs(tval))
        | Log LogN :: tail -> let tLst, tval = NR tail
                              if checkPositive (number.fltVal(tval)) then (tLst, Flt (Math.Log(number.fltVal(tval))))
                              else raise logInputError                  
        | Log LogOther :: tail -> let tLst, tval = NR tail
                                  if checkPositive (number.fltVal(tval)) && (checkPositive (number.fltVal(snd (NR tLst)))) && not(checkLogEdgeCase (number.fltVal(tval)))
                                  then (tLst.Tail, Flt (Math.Log(number.fltVal(snd (NR tLst)), number.fltVal(tval)))) 
                                  else raise logInputError
        | Exp :: tail -> let tLst, tval = NR tail
                         (tLst, Flt (Math.Exp(number.fltVal(tval))))                  
        | Trig Sin :: tail -> let tLst, tval = NR tail
                              trigHelperFunction(tval, Math.Sin, tLst)
        | Trig Cos :: tail -> let tLst, tval = NR tail
                              trigHelperFunction(tval, Math.Cos, tLst)                             
        | Trig Tan :: tail -> let tLst, tval = NR tail
                              if checkAgainstTanList (number.fltVal(tval) * (Math.PI / 180.0)) = false then
                                trigHelperFunction(tval, Math.Tan, tLst)
                              else raise tanUndefinedError
        | Trig ASin :: tail -> let tLst, tval = NR tail
                               if checkBetweenATrigValues (number.fltVal(tval)) then
                                trigHelperFunction(tval, Math.Asin, tLst)
                               else raise sinUndefinedError
        | Trig ACos :: tail -> let tLst, tval = NR tail
                               if checkBetweenATrigValues (number.fltVal(tval)) then
                                trigHelperFunction(tval, Math.Acos, tLst)
                               else raise cosUndefinedError 
        | Trig ATan :: tail -> let tLst, tval = NR tail
                               trigHelperFunction(tval, Math.Atan, tLst)
        | Pi :: tail -> (tail, Flt Math.PI)
        //---------------------------------------------------------------------------------------------------------------------------------------------------------

        | Var name :: Assign :: tail when symbolTable.ContainsKey(name) ->  let tLst, tval = E tail
                                                                            let (b_type, pList, tList: terminal list) = symbolTable.[name]
                                                                            // Console.WriteLine(tList.Head.toNumber().GetType())
                                                                            // Console.WriteLine(tval.GetType())
                                                                            if (tList.Head.toNumber().GetType() = tval.GetType()) && (b_type = Variable) then
                                                                                symbolTable <- symbolTable.Remove(name)
                                                                                symbolTable <- symbolTable.Add(name, (Variable, [], [Num tval]))
                                                                                (tLst, tval)
                                                                            else
                                                                                Console.WriteLine("Variable {0} expected type {1} but got type {2}", name, tList.Head.toNumber().TypeToString(), tval.TypeToString())
                                                                                raise typeError
        | Var name :: Assign :: tail -> let tLst, tval = E tail
                                        symbolTable <- symbolTable.Add(name, (Variable, [], [Num tval]))
                                        (tLst, tval)
        | DataType Auto :: Var name :: Assign :: tail -> let tLst,tval = E tail
                                                         symbolTable <- symbolTable.Add(name, (Variable, [], [Num tval]))
                                                         (tLst, tval)
        | DataType Integer :: Var name :: Assign :: tail -> let tLst,tval = E tail
                                                            if tval.GetType() = (Int 0).GetType() then
                                                                symbolTable <- symbolTable.Add(name, (Variable, [], [Num tval]))
                                                                (tLst, tval)
                                                            else
                                                                Console.WriteLine("Value "+ tval.ToString() + " not an integer")
                                                                raise typeError
        | DataType Float :: Var name :: Assign :: tail -> let tLst,tval = E tail
                                                          if tval.GetType() = (Flt 0.0).GetType() then
                                                               symbolTable <- symbolTable.Add(name, (Variable, [], [Num tval]))
                                                               (tLst, tval)
                                                          else
                                                              Console.WriteLine("Value " + tval.ToString() + " not a float")
                                                              raise typeError
        | DataType Fraction :: Var name :: Assign :: tail -> let tLst,tval = E tail
                                                             if tval.GetType() = (Frac (0,1)).GetType() then
                                                               symbolTable <- symbolTable.Add(name, (Variable, [], [Num tval]))
                                                               (tLst, tval)
                                                             else
                                                               Console.WriteLine("Value " + tval.ToString() + " not an fraction number")
                                                               raise typeError
        //---------------------------------------------------------------------------------------------------------------------------------------------------------
        | Func :: Var name :: Lpar :: tail when symbolTable.ContainsKey(name) ->    let paramList, tList = getPSignature ([], tail)
                                                                                    symbolTable <- symbolTable.Remove(name)
                                                                                    symbolTable <- symbolTable.Add(name, (Function, paramList, tList))                                                                        
                                                                                    (tList, Int 0)
        | Func :: Var name :: Lpar ::tail ->    let paramList, tLst = getPSignature ([], tail)
                                                symbolTable <- symbolTable.Add(name, (Function, paramList, tLst))

                                                (tLst, Int 0)
        //---------------------------------------------------------------------------------------------------------------------------------------------------------
        | Var name :: tail when symbolTable.ContainsKey(name) -> FN (name, tail)

        | Var name :: tail when not (symbolTable.ContainsKey(name)) -> Console.WriteLine("Undefined variable or function " + name)
                                                                       raise undefinedVarError
        //---------------------------------------------------------------------------------------------------------------------------------------------------------
        | For :: Lpar :: Num (Int count) :: Rpar :: Lbrace :: tail ->   if count < 1 then
                                                                            raise countLessThanOneException
                                                                        else
                                                                            for i = 1 to (count-1) do
                                                                                Console.WriteLine(tail)
                                                                                Console.WriteLine(L tail)
                                                                            let tLst, tval = L tail
                                                                            match tLst with
                                                                            | Rbrace :: tail -> (tail, tval)
                                                                            | _ ->  printTList tLst |> ignore
                                                                                    raise unclosedBracesError                            
        | For :: Lpar :: Arith Sub :: Num (Int count) :: tail -> raise countLessThanOneException
        
        | DataType Boolean :: Var name :: Assign :: tail -> let tLst, tVal = E tail
                                                            if tVal.GetType() = (Bool false).GetType() then
                                                              symbolTable <- symbolTable.Add(name, (Variable, [], [Num tVal]))
                                                              (tLst, tVal)
                                                            else
                                                              Console.WriteLine("Value " + tVal.ToString() + " not an fraction number")
                                                              raise typeError 
        | Lpar :: tail -> let tLst, tval = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                                            
                          | _ -> raise unclosedParensError
        | _ -> Console.WriteLine("Unexpected syntax at:")
               for t in tList do Console.Write(t.ToString() + " ")
               raise parseError                          

    and FN (name, tail:terminal list) =
        let b, p, t = symbolTable.[name]
        match b with
        | Variable ->   match t.Head with
                        | Num value -> (tail, value)
                        | _ -> raise NaNError
        | Function ->   match tail.Head with
                        | Lpar   -> let paramsToSub, tailEnd = getP tail.Tail
                                    let substitutedTList = subP (paramsToSub, p, t)                                                             
                                    let fTail, fTVal = L substitutedTList
                                    (tailEnd, fTVal)                                                                  
                        | _      -> Console.WriteLine(t.Head);
                                    Console.ReadLine() |> ignore
                                    raise parseError

    and getPSignature (pList, tList) =
        match tList with
        | Var name :: tail -> getPSignature ((List.append pList [Var name]), tail)
        | Rpar :: Assign :: tail -> (pList, tail)
        | _ -> Console.WriteLine("Non-Parameter specified in declaration - Unexpected syntax at:")
               for t in tList do Console.Write(t.ToString() + " ")
               raise parseError
    and getP inTList = 
        let rec scan tList =
            match tList with
            | Var name :: tail ->   let b, p, t = symbolTable.[name] 
                                    match b with
                                    | Variable -> t.Head :: scan tail
                                    | Function -> let tList, fnResult = FN (name, tail)    //Duplicated symbolTable call, questionable efficiency???
                                                  Num fnResult :: scan tList
            // | Num (Int value) :: tail -> Num (Int value) :: scan tail
            | Num value :: tail -> Num value :: scan tail
            | Rpar :: tail ->            []
            | _ -> raise parseError
        let rec getTailEnd tList = 
            match tList with
            | Lpar :: tail ->               getTailEnd tail
            | Var name :: tail ->           getTailEnd tail
            | Num number :: tail ->         getTailEnd tail
            | Rpar :: tail ->               tail
            | _ ->  printTList tList |> ignore
                    raise parseError
        (scan inTList, getTailEnd inTList)
    and subP (inParamsToSub, inPList, inTList) =
        let pArray = Array.ofList inPList
        let rec scan tList =
            match tList with
            | [] -> []
            | Var name :: tail ->   let i = indexOf (Var name, pArray)
                                    if (i <> -1) then
                                        let swap = inParamsToSub.[i]
                                        swap :: scan tail
                                    else
                                        raise unmatchedParamError
            | head :: tail -> head :: scan tail
        scan inTList
    L tList

let test (input:string, correctOut) =
    let oList = lexer input
    let out = parseNeval oList
    if snd out = correctOut then true
    else printfn "input: %s, Correct Result: %f, Interpreter Out: %f" input (number.fltVal(correctOut)) (number.fltVal(snd out))
         false
    
let testInputs =
    printfn "Tests Start"
    if
        test ("15+987", Int 1002) &&
        test ("987-15", Int 972) &&
        test ("15*987", Int 14805) &&
        test ("5/2", Int 2) &&
        test ("5.0/2", Flt 2.5) &&
        test ("1156.55+1.2", Flt 1157.75) &&
        test ("9%4", Int 1) &&
        test ("-2*3^2", Int -18)
        then printfn "All tests passed"
        else printfn "Some of the tests failed"
        
let guiIntegration (inputString: string) = 
    let oList = lexer inputString
    let Out = parseNeval oList
    snd Out
        
//let main argv  =
//    Console.WriteLine("Simple Interpreter\n-----------------")
    
//    let input:string = getInputString()

//    let oList = lexer input
//    let sList = printTList oList;
//    Console.WriteLine();

//    //let pList = printTList (parser oList)
//    let Out = parseNeval oList
    
//    Console.WriteLine("Result = {0}", snd Out)
//    //testInputs
//    Console.WriteLine(variables)
//    Console.ReadLine()
//    0

[<EntryPoint>]
let rec main' argv  =
    let input:string = getInputString()
    match str2lst input with
    | 'e' :: 'x' :: 'i' :: 't' :: tail -> 0
    | '\\' :: 'c' :: 'l' :: 'e' :: 'a' :: 'r' :: tail -> symbolTable <- Map.empty
                                                         Console.WriteLine("Cleared symbol table")
                                                         main' argv
    | _ -> let oList = lexer input
           let sList = printTList oList;

           //let pList = printTList (parser oList)
           let Out = parseNeval oList
    
           Console.WriteLine("Result = {0}", snd Out)
           //testInputs
           Console.WriteLine("Symbol table:")
           Console.WriteLine(symbolTable)
           //for entry in symbolTable do
               //Console.Write("{0} {1} = {2} ", entry.Value.TypeToString(), entry.Key, entry.Value.ToString())
           // Console.WriteLine(variables)
           Console.WriteLine();
           main' argv |> ignore
           0

//| for :: Lpar :: Int :: Rpar :: Lbrace :: body :: Rbrace