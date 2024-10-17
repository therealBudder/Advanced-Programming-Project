// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

open System

type terminal = 
    Add | Sub | Mul | Div | Rem | Pow | Lpar | Rpar  | UnaryM | Dec | Num of int | Flt of float


let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isZero c = if c = 0 then true else false
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let floatVal (c:char) = (float)((int)c - (int)'0')
let parseError = System.Exception("Parser error")
let divisonByZeroError = System.Exception("Division by Zero")
let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

and scFloat(iStr, iVal, weight) =
    match iStr with
    c :: tail when isdigit c ->
        scFloat(tail, iVal + weight * floatVal c, weight / 10.0)
    | _ -> (iStr, iVal)

let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | ' '::'-'::tail -> UnaryM :: scan tail
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Rem :: scan tail
        | '^'::tail -> Pow :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c)
                                      match iStr with
                                      | '.' :: c :: tail when isdigit c -> let (iStr, iVal) = scFloat(c :: tail, (float)iVal, 0.1)
                                                                           Flt iVal :: scan iStr
                                      | _ -> Num iVal :: scan iStr
                                      // Num iVal :: scan iStr
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:

// version 0
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"


// version 1
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <F> <Topt>
// <Topt>     ::= "*" <F> <Topt> | "/" <F> <Topt> |  "%" <F> <Topt> |<empty>
// <F>        ::= <NR> <Fopt>
// <Fopt>     ::= "^" <NR> <Fopt> | "-" <NR> |<empty> 
// <NR>       ::= "Num" <value> | "Flt" <value> | "(" <E> ")"


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
        | UnaryM :: tail -> (NR >> Fopt) tail
        | _ -> tList
    and NR tList =
        match tList with 
        | Num value :: tail -> tail
        | Flt value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

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
                         if isZero tval = false then Topt (tLst, value / tval) else raise divisonByZeroError
        | Rem :: tail -> let (tLst, tval) = F tail
                         Topt (tLst, value % tval)
        | _ -> (tList, value)
    and F tList = (NR >> Fopt) tList
    and Fopt (tList, value) =
        match tList with
        | Pow :: tail -> let (tLst, tval) = NR tail
                         Fopt (tLst, Math.Pow(value, tval))
        | UnaryM :: tail -> let (tLst, tval) = NR tail
                            Fopt (tLst, (-1.0 * tval)) 
        | _ -> (tList,(int) value)
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, (float)value)
        | Flt value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []


[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    Console.WriteLine("Result = {0}", snd Out)
    0
