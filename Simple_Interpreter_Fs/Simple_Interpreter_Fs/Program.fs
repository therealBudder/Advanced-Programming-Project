// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

let rec squareRoot ((inputValue:float), (power:float)) =
   match power with
   power when power > 1.0 = true -> squareRoot ((Math.Sqrt(inputValue)), power-1.0)
   | _ -> inputValue  

type terminal = 
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
    | Null
    | Pi
    | Abs
and number =
    Int of int | Flt of float | Rat of float * float
    static member fltVal n = match n with
                             | Flt f -> f
                             | Int i -> float i
                             | Rat (upper,lower) -> upper/lower 
    static member (+) (x: number, y: number) = match (x, y) with
                                               | Int x, Int y -> Int (x + y)
                                               | Rat (upper, lower), Int y ->
                                                   Rat (upper + lower * float y, lower)
                                               | Int y, Rat (upper, lower) ->
                                                   Rat (upper + lower * float y, lower)    
                                               | Rat (upper, lower), Flt y ->
                                                   Rat (upper + lower * y, lower)
                                               | Flt y, Rat (upper, lower) ->
                                                   Rat (upper + lower * y, lower)    
                                               | Rat (upper, lower), Rat (upperTwo, lowerTwo) ->
                                                   Rat (upper*lowerTwo + lower * upperTwo, lower*lowerTwo)    
                                               | _ -> Flt (number.fltVal x + number.fltVal y)
    static member (-) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x - y)
                                             | Rat (upper, lower), Int y ->
                                                   Rat (upper - lower * float y, lower)
                                             | Int y, Rat (upper, lower) ->
                                                   Rat (upper - lower * float y, lower)      
                                             | Rat (upper, lower), Flt y ->
                                                   Rat (upper - lower * y, lower)
                                             | Flt y, Rat (upper, lower) ->
                                                   Rat (upper - lower * y, lower)    
                                             | Rat (upper, lower), Rat (upperTwo, lowerTwo) ->
                                                   Rat (upper*lowerTwo - lower * upperTwo, lower*lowerTwo) 
                                             | _ -> Flt (number.fltVal x - number.fltVal y)
    static member (*) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x * y)
                                             | Rat (upper, lower), Int y ->
                                                   Rat (upper * float y, lower)
                                             | Int y, Rat (upper, lower) ->
                                                   Rat (upper * float y, lower)      
                                             | Rat (upper, lower), Flt y ->
                                                   Rat (upper * y, lower)
                                             | Flt y, Rat (upper, lower) ->
                                                   Rat (upper * y, lower)      
                                             | Rat (upper, lower), Rat (upperTwo, lowerTwo) ->
                                                   Rat (upper * upperTwo, lower*lowerTwo) 
                                             | _ -> Flt (number.fltVal x * number.fltVal y)
    static member (/) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x / y)
                                             | Rat (upper, lower), Int y ->
                                                   Rat (upper, lower * float y)
                                             | Int y, Rat (upper, lower) ->
                                                   Rat (lower *  float y, upper)      
                                             | Rat (upper, lower), Flt y ->
                                                   Rat (upper, lower * y)
                                             | Flt y, Rat (upper, lower) ->
                                                   Rat (lower * y, upper)      
                                             | Rat (upper, lower), Rat (upperTwo, lowerTwo) ->
                                                   Rat (upper * lowerTwo, lower*upperTwo) 
                                             | _ -> Flt (number.fltVal x / number.fltVal y)
    static member (%) (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (x % y)
                                             | Rat (upper, lower), Int y ->
                                                   Flt ((upper/ lower) % float y)
                                             | Int y, Rat (upper, lower) ->
                                                   Flt ((lower * float y) % upper)
                                             | Rat (upper, lower), Flt y ->
                                                   Flt ((upper/ lower) % y)
                                             | Flt y, Rat (upper, lower) ->
                                                   Flt ((lower * y) % upper)      
                                             | Rat (upper, lower), Rat (upperTwo, lowerTwo) ->
                                                   Flt ((upper * lowerTwo / lower) % upperTwo)       
                                             | _ -> Flt (number.fltVal x % number.fltVal y)
    static member Pow (x:number, y:number) = match (x, y) with
                                             | Int x, Int y -> Int (pown x y)
                                             | Rat (upper, lower), Int y ->
                                                   Rat (Math.Pow(upper, float y), Math.Pow(lower, float y))
                                             | Int y, Rat (upper, lower) ->
                                                   Int (pown ((int)(squareRoot(float y, lower))) ((int)upper))
                                             | Rat (upper, lower), Flt y ->
                                                   Rat (Math.Pow(upper,y), Math.Pow(lower,y))
                                             | Flt y, Rat (upper, lower) ->
                                                   Flt (Math.Pow(squareRoot(y, lower), upper))     
                                             | Rat (upper, lower), Rat (upperTwo, lowerTwo) ->
                                                   Rat ((Math.Pow(squareRoot(upper, lowerTwo), upperTwo)),(Math.Pow(squareRoot(lower, lowerTwo), upperTwo)))  
                                             | _ -> Flt (number.fltVal x ** number.fltVal y)
    static member (~-) (n:number) = match n with
                                    | Int n -> Int -n
                                    | Rat (upper, lower) -> Rat (-upper, lower)
                                    | Flt n -> Flt -n
    static member (~+) (n:number) = match n with
                                    | Int n -> Int +n
                                    | Rat (upper, lower) -> Rat (+upper, lower)
                                    | Flt n -> Flt +n
    static member Floor (n:number) = match n with
                                     | Flt n -> Flt (Math.Floor(n))
                                     | Rat (upper, lower) -> Flt (Math.Floor(upper/lower))
                                     | _ -> n
    static member Abs (n:number) = match n with
                                   | Int n -> Int (Math.Abs(n))
                                   | Flt n -> Flt (Math.Abs(n))
                                   | Rat (upper, lower) -> Flt (Math.Abs(upper/lower))
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
let sinUndefinedError = System.Exception("Sin call will result in undefined behavior.")
let cosUndefinedError = System.Exception("Cos call will result in undefined behavior.")
let logInputError = System.Exception("Input Error By User for function Log and Ln")
let unclosedBracketsError = System.Exception("Syntax error Brackets must be closed")
let undefinedVarError = System.Exception("Syntax error Variable is not defined")

let checkAgainstTanList(x:float) =
    tanUndefinedList |> List.contains x
let checkPositive (x:float) =
    if x > 0.0 then true else false
let checkLogEdgeCase (newBase:float) =
    if (newBase = 1.0) then true else false  
let checkBetweenATrigValues(x:float) =
    let out = fun input -> (input >= -(1.0) && input <= (1.0))
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
and scRad(iVal, iValString, iVal1, iVal1String, iVal2String, weight, iStr) =
    let upper = match iValString with
                c :: tail when isdigit c ->
                  scFloat(iValString.Tail, iVal + weight * floatVal c, weight / 10.0)
                | _ -> (iValString, iVal)            
    let lowerPre = match iVal1String with
                   c :: tail when isdigit c -> scInt(tail, 10*iVal1+(intVal c))
                   | _ -> (iVal1String, iVal1)
    let lower = match iVal2String with
                   c :: tail when isdigit c ->
                   scFloat(iVal2String.Tail, (float) (snd lowerPre) + weight * floatVal c, weight / 10.0)
                   | _ -> (iVal2String, float iVal1)
    let rec burnIStr inCharList =
        match inCharList with
        | c :: tail when isblank c = false -> burnIStr(inCharList.Tail)
        | _ -> (inCharList)
    (upper, lower, (burnIStr iStr))               

let isReservedWord(inString) =
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
    | _ -> Null

let rec scName(remain : list<char>, word : string) =
    match remain with
    c :: tail when isletterordigit c -> let cStr = (string)c
                                        scName(tail, word + cStr)
    | _ -> (remain, word)
let getFloatRadian value =
     number.fltVal(value) * (Math.PI / 180.0)
let getFloatDegrees value =
    (value) * (180.0/Math.PI)     
let getListPart(valueEnd, listIn) =     
    listIn |> List.take (listIn |> List.tryFindIndex (fun elem -> elem = valueEnd)).Value 
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
        | '='::tail -> Assign:: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c)
                                      match iStr with
                                      | '.' :: c :: '/' :: cNext :: '.' :: CNextAfter :: tail when isdigit c -> let (iVal, iVal2, iStr) = scRad(float iVal, (getListPart('/',iStr).Tail), intVal cNext ,(getListPart('.',iStr)),CNextAfter::tail,0.1, iStr)
                                                                                                                Num (Rat (snd iVal,snd iVal2)) :: scan iStr 
                                      | '.' :: c :: tail when isdigit c -> let (iStr, iVal) = scFloat(c :: tail, (float)iVal, 0.1)
                                                                           Num (Flt iVal) :: scan iStr
                                      
                                      | _ -> Num (Int iVal) :: scan iStr
                                      // Num iVal :: scan iStr
        | c :: tail when isletter c -> let (iStr, oStr) = scName(tail, (string)c)
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
//<number> ::= "Flt" | "Int"
//<log> ::= "logN" | "logOther"

// version 1.0
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> |  "%" <F> <Topt> ||<empty>
// <F>        ::= <NR> <Fopt>
// <Fopt>     ::= "^" <NR> <Fopt> | "-" <NR> | <trig> <F> | <log> <F> | "Exp" <F> |<empty> 
// <NR>       ::= "Var" <value> "Assign" <NR> | "Var" <value> | <number> <value> | "(" <E> ")"




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
        | Num (Rat (upper, lower)) :: tail -> (tail, Flt (number.fltVal (Rat (upper, lower))))
        | Abs :: tail -> let (tLst, tval) = NR tail
                         (tLst, number.Abs(tval))
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
                              if Math.Round(getFloatDegrees(Math.Sin(getFloatRadian tval)), 10) = 0.0 then
                                (tLst, Flt 0.0) 
                              else (tLst, Flt (getFloatDegrees(Math.Sin(getFloatRadian tval))))
        | Trig Cos :: tail -> let (tLst, tval) = NR tail
                              if Math.Round(getFloatDegrees(Math.Cos(getFloatRadian tval)), 10) = 0.0 then
                                (tLst, Flt 0.0) 
                              else (tLst, Flt (getFloatDegrees(Math.Cos(getFloatRadian tval))))                                
        | Trig Tan :: tail -> let (tLst, tval) = NR tail
                              if checkAgainstTanList (number.fltVal(tval) * (Math.PI / 180.0)) = false then
                                if Math.Round((getFloatDegrees(Math.Tan(getFloatRadian tval))), 10) = 0.0 then
                                  (tLst, Flt 0.0) 
                                else (tLst, Flt (getFloatDegrees(Math.Tan(getFloatRadian tval))))
                              else raise tanUndefinedError
        | Trig ASin :: tail -> let (tLst, tval) = NR tail
                               if checkBetweenATrigValues (number.fltVal(tval)) then 
                                if Math.Round ((getFloatDegrees(Math.Asin(getFloatRadian tval))), 10) = 0.0 then
                                 (tLst, Flt 0.0)
                                else (tLst, Flt (getFloatDegrees(Math.Asin(getFloatRadian tval))))
                               else raise sinUndefinedError
        | Trig ACos :: tail -> let (tLst, tval) = NR tail
                               if checkBetweenATrigValues (number.fltVal(tval)) then 
                                if Math.Round(getFloatDegrees((Math.Acos(getFloatRadian tval))), 10) = 0.0 then
                                 (tLst, Flt 0.0)  
                                else (tLst, Flt (getFloatDegrees(Math.Acos(getFloatRadian tval))))
                               else raise cosUndefinedError 
        | Trig ATan :: tail -> let (tLst, tval) = NR tail
                               if Math.Round(getFloatDegrees(Math.Atan(getFloatRadian tval)),10) = 0.0 then 
                                 (tLst, Flt 0.0)  
                               else (tLst, Flt (getFloatDegrees(Math.Atan(getFloatRadian tval))))
        | Var name :: Assign :: tail when variables.ContainsKey(name) -> let tVal = snd (E tail)
                                                                         variables <- variables.Remove(name)
                                                                         variables <- variables.Add(name, tVal)
                                                                         (tail, tVal)
        | Var name :: Assign :: tail -> let tVal = snd (E tail)
                                        variables <- variables.Add(name, tVal)
                                        (tail, tVal)
        | Var name :: tail when variables.ContainsKey(name) -> (tail, variables.[name])
        | Pi :: tail -> (tail, Flt Math.PI)
        | Var name :: tail when not (variables.ContainsKey(name)) -> Console.WriteLine("Undefined variable " + name)
                                                                     raise undefinedVarError
        | _ -> Console.WriteLine("Unexpected syntax at:")
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

    
    
    // 