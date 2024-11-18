//fn branch

// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    | Func  //NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! 
    | Rem
    | Pow
    | Exp
    | Log of log
    | Lpar
    | Rpar
    | Neg
    | Arith of arith
    | Num of number
    | Var of string
    | Assign
    | Trig of trig
and number =
    Int of int | Flt of float
    static member fltVal n = match n with
                             | Flt f -> f
                             | Int i -> float i
    static member (+) (x: number, y: number) = match (x, y) with
                                               | Int x, Int y -> Int (x + y)
                                               | _ -> Flt (number.fltVal x + number.fltVal y)
    static member (-) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x - y)
                                             | _ -> Flt (number.fltVal x - number.fltVal y)
    static member (*) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x * y)
                                             | _ -> Flt (number.fltVal x * number.fltVal y)
    static member (/) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x / y)
                                             | _ -> Flt (number.fltVal x / number.fltVal y)
    static member (%) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x % y)
                                             | _ -> Flt (number.fltVal x % number.fltVal y)
    static member Pow (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (pown x y)
                                             | _ -> Flt (number.fltVal x ** number.fltVal y)
    static member (~-) (n:number) = match n with
                                    | Int n -> Int -n
                                    | Flt n -> Flt -n
    static member (~+) (n:number) = match n with
                                    | Int n -> Int +n
                                    | Flt n -> Flt +n
    static member Floor (n:number) = match n with
                                     | Flt n -> Flt (Math.Floor(n))
                                     | _ -> n

and trig =
     Sin | Cos | Tan | ASin | ACos | ATan
and arith =
    Add | Sub | Mul | Div | FlrDiv
and log =
    LogN | LogOther

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isletter c = System.Char.IsLetter c
let isletterordigit c = System.Char.IsLetterOrDigit c
let tanUndefinedList = [for i in -1000.0 .. 1000.0 do yield ((Math.PI/2.0) + Math.PI*i)]


let lexError = System.Exception("invalid symbol in expression")
let varError = System.Exception("incorrect var assignment")
let intVal (c:char) = (int)((int)c - (int)'0')
let floatVal (c:char) = (float)((int)c - (int)'0')
let strVal (c:char) = (string)c
let parseError = System.Exception("Parser error")
let divisionByZeroError = System.Exception("Division by zero is undefined")
let tanUndefinedError = System.Exception("Tan call will result in undefined behavior.")
let logInputError = System.Exception("Input Error By User for function Log and Ln")
let unclosedBracketsError = System.Exception("Syntax error Brackets must be closed")
let undefinedVarError = System.Exception("Syntax error Variable is not defined")

let checkAgainstTanList(x:float) =
    tanUndefinedList |> List.contains x
let checkPositive (x:float) =
    if x > 0.0 then true else false
let checkLogEdgeCase (newBase:float) =
    if (newBase = 1.0) then true else false  
let checkBetweenAtanValues(x:float) =
    let out = fun input -> (input >= -(Math.PI/2.0) && input <= (Math.PI/2.0))
    out x 
let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

and scFloat(iStr, iVal, weight) =
    match iStr with
    c :: tail when isdigit c ->
        scFloat(tail, iVal + weight * floatVal c, weight / 10.0)
    | _ -> (iStr, iVal)

let rec scName(remain : list<char>, word : string) =
    match remain with
    c :: tail when isletterordigit c -> let cStr = (string)c
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
        | 'l'::'n'::tail -> Log LogN :: scan tail
        | 'l'::'o'::'g'::tail -> Log LogOther :: scan tail 
        | 'a'::'s'::'i'::'n'::tail -> Trig ASin :: scan tail
        | 'a'::'c'::'o'::'s'::tail -> Trig ACos :: scan tail
        | 'a'::'t'::'a'::'n'::tail -> Trig ATan :: scan tail
        | 's'::'i'::'n'::tail -> Trig Sin :: scan tail
        | 'c'::'o'::'s'::tail -> Trig Cos :: scan tail
        | 't'::'a'::'n'::tail -> Trig Tan :: scan tail
        | 'f'::'n'::tail -> Func :: scan tail //NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! NEW! 
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | '='::tail -> Assign:: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c)
                                      match iStr with
                                      | '.' :: c :: tail when isdigit c -> let (iStr, iVal) = scFloat(c :: tail, (float)iVal, 0.1)
                                                                           Num (Flt iVal) :: scan iStr
                                      | _ -> Num (Int iVal) :: scan iStr
                                      // Num iVal :: scan iStr
        | c :: tail when isletter c -> let (iStr, oStr) = scName(tail, (string)c)
                                       Var oStr :: scan iStr
        | _ -> Console.WriteLine("Unexpected symbol '" + input.[0].ToString() + "'")
               raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:
// version 1.0
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> |  "%" <F> <Topt> ||<empty>
// <F>        ::= <NR> <Fopt>
// <Fopt>     ::= "^" <NR> <Fopt> | "-" <NR> | "Sin" <F> | "Cos" <F> | "Tan" <F> | "Asin" <F> | "Acos" <F> | "ATan" <F> | <empty> 
// <NR>       ::= "Var" <value> "Assign" <NR> | "Var" <value> | "Num" <value> | "Flt" <value> | "(" <E> ")"


let parser tList = 
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
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
        | Num (Int value) :: tail -> tail
        | Num (Flt value) :: tail -> tail
        | Exp :: tail -> (NR >> Fopt) tail
        | Log LogN :: tail -> (NR >> Fopt) tail
        | Log LogOther :: tail -> (NR >> Fopt) tail
        | Trig Sin :: tail -> (F >> Topt) tail
        | Trig Cos :: tail -> (F >> Topt) tail
        | Trig Tan :: tail -> (F >> Topt) tail
        | Trig ASin :: tail -> (F >> Topt) tail
        | Trig ACos :: tail -> (F >> Topt) tail
        | Trig ATan :: tail -> (F >> Topt) tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise unclosedBracketsError
        | _ -> raise parseError
    E tList

let mutable variables = Map.empty   //acts as the symbol table currently (may want revision, very rudimentary)
let mutable functions = Map.empty   //secondary table, to be merged with variables as a true symbol table

let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Arith Add :: tail -> let (tLst, tval) = T tail 
                               Eopt (tLst, value + tval)
        | Arith Sub :: tail -> let (tLst, tval) = T tail
                               Eopt (tLst, value - tval)
        | _ -> (tList, value)
    and T tList = (F >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Arith Mul :: tail -> let (tLst, tval) = F tail
                               Topt (tLst, value * tval)
        | Arith Div :: tail -> let (tLst, tval) = F tail
                               if tval = Int 0 || tval = Flt 0.0 then raise divisionByZeroError else Topt (tLst, value / tval)
        | Arith FlrDiv :: tail -> let (tLst, tval) = F tail
                                  if tval = Int 0 || tval = Flt 0.0 then raise divisionByZeroError else Topt (tLst, number.Floor(value / tval))
        | Rem :: tail -> let (tLst, tval) = F tail
                         Topt (tLst, value % tval)                  
        | _ -> (tList, value)
    and F tList = (NR >> Fopt) tList
    and Fopt (tList, value) =
        match tList with
        | Pow :: tail -> let (tLst, tval) = NR tail
                         Fopt (tLst, number.Pow(value, tval))
        | _ -> (tList, value)
    and NR tList =
        match tList with
        | Arith Sub :: tail -> let (tList, tval) = NR tail
                               (tList, -tval)
        | Arith Add :: tail -> let (tList, tval) = NR tail
                               (tList, +tval)
        | Num (Int value) :: tail -> (tail, Int value)
        | Num (Flt value) :: tail -> (tail, Flt value)
        | Log LogN :: tail -> let (tLst, tval) = NR tail
                              if checkPositive (number.fltVal(tval)) then (tLst, Flt (Math.Log(number.fltVal(tval))))
                              else raise logInputError                  
        | Log LogOther :: tail -> let (tLst, tval) = NR tail
                                  if checkPositive (number.fltVal(tval)) && (checkPositive (number.fltVal(snd (NR tLst)))) && not(checkLogEdgeCase (number.fltVal(tval)))
                                  then (tLst.Tail, Flt (Math.Log(number.fltVal(snd (NR tLst)), number.fltVal(tval)))) 
                                  else raise logInputError
        | Lpar :: tail -> let (tList, tval) = E tail
                          match tList with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise unclosedBracketsError
        | Exp :: tail -> let (tLst, tval) = NR tail
                         (tLst, Flt (Math.Exp(number.fltVal(tval))))                  
        | Trig Sin :: tail -> let (tLst, tval) = NR tail
                              if Math.Round((Math.Sin(number.fltVal(tval) * (Math.PI / 180.0))), 10) = 0.0 then
                               (tLst, Flt 0.0) 
                              else (tLst, Flt (Math.Sin(number.fltVal(tval) * (Math.PI / 180.0))))
        | Trig Cos :: tail -> let (tLst, tval) = NR tail
                              if Math.Round((Math.Cos(number.fltVal(tval) * (Math.PI / 180.0))), 10) = 0.0 then
                                (tLst, Flt 0.0) 
                              else (tLst, Flt (Math.Cos(number.fltVal(tval) * (Math.PI / 180.0))))                                
        | Trig Tan :: tail -> let (tLst, tval) = NR tail
                              if checkAgainstTanList (number.fltVal(tval) * (Math.PI / 180.0)) = false then
                                if Math.Round((Math.Tan(number.fltVal(tval) * (Math.PI / 180.0))), 10) = 0.0 then
                                  (tLst, Flt 0.0) 
                                else (tLst, Flt (Math.Tan(number.fltVal(tval) * (Math.PI / 180.0))))
                              else raise tanUndefinedError
        | Trig ASin :: tail -> let (tLst, tval) = NR tail
                               if Math.Round ((Math.Asin(number.fltVal(tval) * (Math.PI / 180.0))), 10) = 0.0 then
                                 (tLst, Flt 0.0)
                               else (tLst, Flt (Math.Asin(number.fltVal(tval) * (Math.PI / 180.0))))
        | Trig ACos :: tail -> let (tLst, tval) = NR tail
                               if Math.Round((Math.Acos(number.fltVal(tval) * (Math.PI / 180.0))), 10) = 0.0 then
                                 (tLst, Flt 0.0)  
                               else (tLst, Flt (Math.Acos(number.fltVal(tval) * (Math.PI / 180.0))))
        | Trig ATan :: tail -> let (tLst, tval) = NR tail
                               if checkBetweenAtanValues (number.fltVal(tval)) then
                                   if Math.Round((Math.Atan(number.fltVal(tval) * (Math.PI / 180.0))),10) = 0.0 then 
                                     (tLst, Flt 0.0)  
                                   else (tLst, Flt (Math.Atan(number.fltVal(tval) * (Math.PI / 180.0))))
                               else raise tanUndefinedError
        | Var name :: Assign :: tail when variables.ContainsKey(name) -> let tVal = snd (E tail)
                                                                         variables <- variables.Remove(name)
                                                                         variables <- variables.Add(name, tVal)
                                                                         (tail, tVal)
        | Var name :: Assign :: tail -> let tVal = snd (E tail)
                                        variables <- variables.Add(name, tVal)
                                        (tail, tVal)
        | Var name :: tail when variables.ContainsKey(name) -> (tail, variables.[name])
        | Var name :: tail when not (variables.ContainsKey(name)) -> Console.WriteLine("Undefined variable " + name)
                                                                     raise undefinedVarError

        | Func :: Var name :: tail -> let (paramList, tList) = P ([], tail)
                                      functions <- functions.Add(name, (paramList, tList))
                                      Console.WriteLine(functions[name])   //test
                                      (tList, (Int)0)

        | _ -> Console.WriteLine("Unexpected syntax at:")
               for t in tList do Console.Write(t.ToString() + " ")
               raise parseError
    and P (pList, tList) =
        match tList with
        | Var name :: tail -> P ((List.append pList [Var name]), tail)
        | Assign :: tail -> (pList, tail)
        | _ -> Console.WriteLine("Non-Parameter specified in declaration - Unexpected syntax at:")
               for t in tList do Console.Write(t.ToString() + " ")
               raise parseError
    E tList


let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []
    
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
        
    


[<EntryPoint>]
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

let rec main' argv  =
    let input:string = getInputString()
    match str2lst input with
    | 'e' :: 'x' :: 'i' :: 't' :: tail -> 0
    | _ -> let oList = lexer input
           let sList = printTList oList;           

           //let pList = printTList (parser oList)
           let Out = parseNeval oList
    
           Console.WriteLine("Result = {0}", snd Out)
           //testInputs
           Console.WriteLine("Symbol table:")
           for entry in variables do
               Console.Write("var " + entry.Key + " = " + entry.Value.ToString() + " ")
           // Console.WriteLine(variables)
           Console.WriteLine();
           main' argv
           0

    