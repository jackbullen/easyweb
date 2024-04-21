(*
    CSC 330 Programming Languages
    Assignment 6
    Spring 2024
    Jack Bullen
*)

signature BASKET =
sig
  type Items (* Individual items      (toast, coffee, chocolate) *)
  type Pairs (* Pairs of items        ((t, co), (t, ch), (co, ch)) *)
  val first_pass: (real * TextIO.instream * char) -> (int * int * Items)    
                                                (* nrecords, minsup, items *)
  val second_pass: (int * Items * TextIO.instream  *char) -> Pairs 
  val display_table : (int * Items * Pairs * int) -> int
end

structure Key =
struct
  type ord_key = string
  val compare = String.compare
end

structure KeyPair =
struct
  type ord_key = (string * string)
  fun compare ((a1, b1): ord_key, (a2, b2): ord_key) =
    case String.compare (a1, a2) of
      EQUAL => String.compare (b1, b2)
    | order => order
end

structure ItemMap = RedBlackMapFn (Key)
structure PairMap = RedBlackMapFn (KeyPair)

structure Basket :> BASKET =
struct

type Items = int ItemMap.map
type Pairs = int PairMap.map
type FirstPassResult = int * int * Items
type ProcessedCSV = int * Items
type Pair = string * string
type TableRow = real * string * int * real * string * int * real * int * real * real
(*            pair_prop name1   num1  sup1   name2    num2  sup2 pair_ct conf   lift  *)

fun trim(input: string): string = 
  if String.isPrefix " " input then trim(String.substring(input, 1, String.size input - 1))
  else if String.isSuffix " " input then trim(String.substring(input, 0, String.size input - 1))
  else input

fun clean_string(input: string): string =
  let 
    val input = trim(input)
    val chars = String.explode input
    val chars = List.filter(fn x => x <> #"\n") chars
  in 
    String.implode chars
  end

(* FIRST PASS
   Get all individual items within threshold and their count *)
fun add_items(items: string list, out: ProcessedCSV): ProcessedCSV = 
  let 
    val (nrecords, popItems) = out
  in
    case items of
      [] => out
    | x::xs => 
      let 
        val xIsIn = ItemMap.find(popItems, x)
      in 
        case xIsIn of
          NONE => add_items(xs, (nrecords, ItemMap.insert(popItems, x, 1)))
        | SOME t => add_items(xs, (nrecords, ItemMap.insert(popItems, x, t+1)))
      end
  end

fun process_lines_first(lines: TextIO.instream, delim: char, out: ProcessedCSV): ProcessedCSV =
  case TextIO.inputLine lines of
    NONE => out
  | SOME line =>
      let
        val items = String.tokens (fn x => x = delim) line
        val (nrecords, popItems) = add_items(map clean_string items, out)
      in
        process_lines_first(lines, delim, (nrecords+1, popItems))
      end

fun first_pass (threshold: real, lines: TextIO.instream, delim: char): (int * int * Items) =
  let
    val (nrecords, popItems) = process_lines_first(lines, delim, (0, ItemMap.empty))
    val keys = ItemMap.listKeys(popItems)
    val minSupport = Real.floor (threshold * Real.fromInt(nrecords)) 
    val popItems = ItemMap.filter(fn x => x>=minSupport) popItems
  in
    (nrecords, minSupport, popItems)
  end

(* SECOND PASS 
   Get all unique pairs of items from first pass and their count *)
fun min(x: string, y: string): string = if x > y then y else x
fun max(x: string, y: string): string = if x > y then x else y

(* Generate a list of pairs from a list of items and a current item *)
(* e.g. li = [1,2], curr = 3  ==>  [(1,3), (2,3)] *)
fun get_pairs(li: string list, curr: string): Pair list =
  map (fn x => (min(x, curr), max(x, curr))) li

(* Generate a list of unique pairs from a list of items using get_pairs *)
fun unique_ord_pairs(li: string list, out: Pair list): Pair list =
  case li of
    [] => out
  | x::xs => 
    let
      val curr_pairs = get_pairs(xs, x)
    in
      unique_ord_pairs(xs, out@curr_pairs)
    end

(* 
    add_pairs(items, popItems, out) 

    params:
      items: list of items from a transaction
      popItems: dictionary of popular items from first pass
      out: accumulator of pairs and their counts

    returns:
      out: dictionary of pairs and their counts
*)
fun add_pairs(items: string list, popItems: Items, out: Pairs): Pairs =
  let
    val items = map clean_string items
    val valid_keys = ItemMap.listKeys(popItems)
    val items = List.filter (fn x => List.exists (fn y => y = x) valid_keys) items
    val pairs = unique_ord_pairs(items, [])
    fun inserter(pairs: Pair list, out: Pairs): Pairs =
      case pairs of
      [] => out
      | (x,y)::xs => 
        let 
          val xyIsIn = PairMap.find(out, (x,y))
        in 
          case xyIsIn of
            NONE => inserter(xs, PairMap.insert(out, (x,y), 1))
          | SOME t => inserter(xs, PairMap.insert(out, (x,y), t+1))
        end
  in
    inserter(pairs, out)
  end

(* Process the transactions in csv file using add_pairs *)
fun process_lines_second(lines: TextIO.instream, delim: char, popItems: Items, out: Pairs): Pairs =
  case TextIO.inputLine lines of
    NONE => out
  | SOME line =>
      let
        val items = String.tokens (fn x => x = delim) line
        val out = add_pairs(items, popItems, out)
      in
        process_lines_second(lines, delim, popItems, out)
      end

fun second_pass(support: int, popItems: Items, lines: TextIO.instream, delim:char): Pairs  =
  let
    val pairs = process_lines_second(lines, delim, popItems, PairMap.empty)
  in
    PairMap.filter (fn x => x>=support) pairs
  end  

(* PRINT TABLE *)
val MINIMUM_WIDTH = 10
val DATA_FORMAT_STRING = "%9.6f %s %6d %8.6f %s %6d %9.3f %9.3f\n"
val TITLE_FORMAT_STRING = " SuppPair-%s - Freq- Support-%s - FreqPair-  Conf.  - Lift\n"

fun str_format (format_string: string) (strings: string list) (width: int): string =
  let
    val padded_strings = map (fn x => StringCvt.padRight #" " width x) strings
  in
    Format.format format_string (map (fn x => Format.STR x) padded_strings)
  end

fun data_format (format_string: string) (data: TableRow) (width: int): string =
  let 
    val (pair_prop, name1, num1, sup1, name2, num2, sup2, pair_ct, conf, lift) = data
    val pair_prop = Format.REAL pair_prop
    val name1 = Format.STR (StringCvt.padRight #" " width name1)
    val num1 = Format.INT num1
    val sup1 = Format.REAL sup1
    val name2 = Format.STR (StringCvt.padRight #" " width name2)
    val num2 = Format.INT num2
    val sup2 = Format.REAL sup2
    val pair_ct = Format.INT pair_ct
    val conf = Format.REAL conf
    val lift = Format.REAL lift
  in 
    Format.format format_string [pair_prop, name1, num1, sup1, name2, pair_ct, conf, lift]
  end

(* Duplicate pairs and keep the same count 
   e.g. [(1,2), 3] ==> [(1,2), 3], [(2,1), 3] *)
fun pairmapDublin(items: Pairs): Pairs =
  let 
    fun Dublin(items: Pairs, out: Pairs): Pairs =
      case PairMap.listItemsi(items) of
        [] => out
      | x::xs => 
        let
          val (key, value) = x
          val (name1, name2) = key
          val out = PairMap.insert(out, (name1, name2), value)
          val out = PairMap.insert(out, (name2, name1), value)
          val (old, _) = PairMap.remove(items, key)
        in
          Dublin(old, out)
        end
  in
    Dublin(items, PairMap.empty)
  end

fun final_printing(toPrint: int, 
                   nTransactions: int, 
                   num_items: int, 
                   num_pairs: int) = 
  let
    val _ = print("\n")
    val _ = print("Number items printed: ")
    val _ = print(Int.toString toPrint)
    val _ = print("\n")
    val _ = print("\n")
    val _ = print("Number transactions: ")
    val _ = print(Int.toString nTransactions)
    val _ = print("\n")
    val _ = print("Number popular items: ")
    val _ = print(Int.toString num_items)
    val _ = print("\n")
    val _ = print("Number popular pairs: ")
    val _ = print(Int.toString num_pairs)
    val _ = print("\n")
  in 0 end

(* Two reals are equal if within eps = 0.00001 *)
fun cmp_real(x: real, y: real): bool =
  let 
    val diff = Real.-(x, y)
    val delta = 0.00001
  in
    Real.<= (Real.abs diff, delta)
  end

(* Sorting rules 
    1. Confidence
    2. Lift
    3. Frequency of first item
    4. Name of first item
    5. Name of second item 
*)
fun sort_output(li: TableRow list): TableRow list =
  let
    fun compare ((_, name1a, freq1a, _, name2a, _, _, _, confa, lifta): TableRow,
                 (_, name1b, freq1b, _, name2b, _, _, _, confb, liftb): TableRow) =
      if not (cmp_real (confa, confb)) then confa < confb
      else if not (cmp_real (lifta, liftb)) then lifta < liftb
      else if freq1a <> freq1b then freq1a > freq1b
      else if String.compare (name1a, name1b) <> EQUAL then name1a > name1b
      else name2a > name2b
  in
    ListMergeSort.sort compare li
  end

fun display_table(nTransactions: int,
                  popItems: Items, 
                  popPairs: Pairs,
                  toPrint:int
                ):int =
  let 
    val popPairs = pairmapDublin(popPairs)
    val pair_val_list = PairMap.listItems(popPairs)
    val pair_name_list = PairMap.listKeys(popPairs)
    val item_val_list = ItemMap.listItems(popItems)
    val item_name_list = ItemMap.listKeys(popItems)
    val num_pairs = length pair_val_list
    val num_items = length item_val_list
    fun get_metrics(pair_name_list: KeyPair.ord_key list, pair_val_list: int list, items: Items, n: int, out: TableRow list) =
      case (pair_name_list, pair_val_list) of
        ([], []) => out
      | (names::rest_names, value::rest_vals) => 
        let 
          val (name1, name2) = names
          val pair_ct = value
          val pair_prop = Real.fromInt(pair_ct) / Real.fromInt(nTransactions)
          val num1 = ItemMap.lookup(items, name1)
          val num2 = ItemMap.lookup(items, name2)
          val sup1 = Real.fromInt(num1) / Real.fromInt(nTransactions)
          val sup2 = Real.fromInt(num2) / Real.fromInt(nTransactions)
          val conf = pair_prop / sup2
          val lift = pair_prop / (sup1*sup2)
          val table_row = (pair_prop, name1, num1, sup1, name2, num2, sup2, pair_ct, conf, lift)
        in
          get_metrics(rest_names, rest_vals, items, n+1, table_row::out)
        end
      | (_, _) => out
    val rows = get_metrics(pair_name_list, pair_val_list, popItems, 0, [])
    val sorted_rows = sort_output(rows)
    fun get_min_width(rows: TableRow list, min_width: int, n: int, toPrint: int): int =
      if n = toPrint then min_width else
      case rows of
        [] => min_width
      | (pair_prop, name1, num1, sup1, name2, num2, sup2, pair_ct, conf, lift)::rest => 
        let
          val new_min = Int.max(min_width, String.size name1)
          val new_min = Int.max(new_min, String.size name2)
        in
          get_min_width(rest, new_min, n+1, toPrint)
        end
    val width = get_min_width(sorted_rows, MINIMUM_WIDTH, 0, toPrint)
    val _ = print(str_format TITLE_FORMAT_STRING ["Item", "With"] width) 
    fun printer(rows: TableRow list, items: Items, n: int) =
      if n = toPrint then final_printing(toPrint, nTransactions, num_items, num_pairs) else
      case rows of
          [] => 
            let 
              val toPrint = Int.min(toPrint, n)
            in
              final_printing(toPrint, nTransactions, num_items, num_pairs)
            end
      | x::xs => 
        let
          val (pair_prop, name1, num1, sup1, name2, num2, sup2, pair_ct, conf, lift) = x
          val row_string = data_format DATA_FORMAT_STRING (pair_prop, name1, num1, sup1, name2, num2, sup2, pair_ct, conf, lift) width
          val _ = print(row_string)
        in
          printer(xs, items, n+1)
        end
  in
    printer(sorted_rows, popItems, 0)
  end
end    

fun main filename = let
  val delim = #","     (* Delimiter in CSV file *)
  val threshold = 0.02 (* Threshold for including items in analysis *)
  val toPrint = 20     (* Number of items to print in output table *)

  val instream = TextIO.openIn filename
  val (nrecords, minSupport, popItems) = Basket.first_pass(threshold, instream, delim)

  val instream = TextIO.openIn filename 
  val pairs = Basket.second_pass(minSupport, popItems, instream, delim)
  
  val _ = Basket.display_table(nrecords, popItems, pairs, toPrint)
in
  TextIO.closeIn instream;
  pairs
end;
val _ = main "daily.csv"