// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar | Neg | Num of int | Flt of float | Var of string | Assign

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isletter c = System.Char.IsLetter c
let isletterordigit c = System.Char.IsLetterOrDigit c

let lexError = System.Exception("error: invalid symbol in expression")
let varError = System.Exception("error: incorrect var assignment")
let intVal (c:char) = (int)((int)c - (int)'0')
let floatVal (c:char) = (float)((int)c - (int)'0')
let strVal (c:char) = (string)c
let parseError = System.Exception("Parser error")
let divisionByZeroError = System.Exception("Division by Zero Detected")

let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

and scFloat(iStr, iVal, weight) =
    match iStr with
    c :: tail when isdigit c ->
        scFloat(tail, iVal + weight * floatVal c, weight / 10.0)
    | _ -> (iStr, iVal)

let rec scStr(remain : list<char>, word : string) =
    match remain with
    c :: tail when isletterordigit c -> let cStr = (string)c
                                        scStr(tail, word + cStr)
    | _ -> (remain, word)

let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Rem :: scan tail
        | '^'::tail -> Pow :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | '='::tail -> Assign:: scan tail
        | c :: tail when isblank c ->  scan tail
        | c :: tail when isdigit c ->  let (iStr, iVal) = scInt(tail, intVal c)
                                       match iStr with
                                       | '.' :: c :: tail when isdigit c -> let (iStr, iVal) = scFloat(c :: tail, (float)iVal, 0.1)
                                                                            Flt iVal :: scan iStr
                                       | _ -> Num iVal :: scan iStr
                                       // Num iVal :: scan iStr
        | c :: tail when isletter c -> let (iStr, oStr) = scStr(tail, (string)c)
                                       Var oStr :: scan iStr
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:
// version 1.0
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> |  "%" <F> <Topt> |<empty>
// <F>        ::= <NR> <Fopt>
// <Fopt>     ::= "^" <NR> <Fopt> | "-" <NR> | <empty> 
// <NR>       ::= "Num" <value> | "Flt" <value> | "(" <E> ")"

// Proposed BNF amendment:
// <NR>       ::= "Var" <value> "Assign" <NR> | "Var" <value> | "Num" <value> | "Flt" <value> | "(" <E> ")"

let parser tList = 
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (F >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (F >> Topt) tail
        | Div :: tail -> (F >> Topt) tail
        | Rem :: tail -> (F >> Topt) tail
        | _ -> tList
    and F tList = (NR >> Fopt) tList
    and Fopt tList =
        match tList with
        | Pow :: tail -> (NR >> Fopt) tail
        | _ -> tList
    and NR tList =
        match tList with
        | Sub :: tail -> tail
        | Num value :: tail -> tail
        | Flt value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let mutable variables = Map.empty


let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value + tval)
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value - tval)
        | _ -> (tList, value)
    
    and T tList = (F >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = F tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = F tail
                         if tval = 0.0 then raise divisionByZeroError else Topt (tLst, value / tval)
        | Rem :: tail -> let (tLst, tval) = F tail
                         Topt (tLst, value % tval)
        | _ -> (tList, value)
    
    and F tList = (NR >> Fopt) tList
    and Fopt (tList, value) =
        match tList with
        | Pow :: tail -> let (tLst, tval) = NR tail
                         Fopt (tLst, (Math.Pow(value,tval)))
        | _ -> (tList, value)
    
    and NR tList =
        match tList with
        | Sub :: tail -> let (tList, tval) = NR tail
                         (tList, -tval)
        | Num value :: tail -> (tail, (float)value)
        | Flt value :: tail -> (tail, value)
        | Lpar :: tail -> let (tList, tval) = E tail
                          match tList with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | Var name :: Assign :: tail -> Console.WriteLine(name)
                                        variables <- variables.Add(name, snd (E tail))
                                        (tail, 0)
        | _ -> raise parseError
    E tList

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []

let testFunctionInt input correctOut =
    let oList = lexer input
    let out = parseNeval oList
    printfn "input: %s, Correct Result: %d, Interpreter Out: %f" input correctOut (snd out)
let testFunctionFloat input correctOut =
    let oList = lexer input
    let out = parseNeval oList
    printfn "input: %s, Correct Result: %f, Interpreter Out: %f" input correctOut (snd out)
    
let testInputs =
    printfn "Tests Start"
    testFunctionInt "2+7" 9
    testFunctionInt "15+987" 1002
    testFunctionInt "9-2" 7
    testFunctionInt "987-15" 972
    testFunctionInt "2*7" 14
    testFunctionInt "15*987" 14580
    testFunctionInt "8/2" 4
    testFunctionInt "987/3" 329
    testFunctionFloat "1156.55+1.2" 1157.75
    testFunctionFloat "9/4" 2.25
    printfn "Tests Finished"
    
    
[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    //let pList = printTList (parser oList)
    let Out = parseNeval oList
    Console.WriteLine("Result = {0}", snd Out)
    //testInputs
    Console.WriteLine(variables)
    Console.ReadLine()
    0