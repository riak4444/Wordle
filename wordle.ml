(*
WORDLE GAME
Author: Ria Konar
*)
(* Install opam package "csv" with "opam install csv" *)
#require "csv"


type game_state = { w_h : string list;
                    kb : string list;
                    the_word: string }

(* initial state *)
let state =
  { w_h = ["_____";"_____";"_____";"_____";"_____";"_____"];
    kb = ["QWERTYUIOP";" ASDFGHJKL";"  ZXCVBNM"];
    the_word = "OCAML"
  }



let word_bank_src = Csv.load "word_bank.csv"

(* helper function upper: change  a character to be uppercase*)
let upper c = Char.uppercase_ascii c

(* helper function upper_l: 
  takes a list of strings
  returns a lists of the strings in uppercase*)
let rec upper_l l = 
  match l with
  | [] -> []
  | h::t -> String.map upper h :: upper_l t

(* helper function concat_l:
  takes a list of lists
  returns a list of the concatenated lists *)
let rec concat_l l = 
  match l with
  | [] -> []
  | h::t -> h @ concat_l t

let process_data src =
	(* TODO *)
	upper_l (concat_l src)


let process_data_fold src =
	(* TODO *)
	upper_l (List.fold_right (@) src [])


let word_bank = process_data word_bank_src



(* helper function check_word:
  takes a list of strings and a word (string)
  returns a bool indicating whether or not the word was in the list *)
let rec check_word l w =
  match l with
  | [] -> false
  | h::t ->
    if h=w then true
    else check_word t w

let is_word_valid w =
	(* TODO *)
	check_word word_bank w


(* helper function add_spaces:
  takes string, string length(5), index(0) 
  returns a list of the letters in the string with spaces in between*)
let rec add_spaces s n i =
  match n with
  | 0 -> []
  | m -> String.sub s i 1 :: " " :: add_spaces s (n-1) (i+1)

(* helper function lst_to_str:
  takes a list of letters and a string(which will be "")
  returns a string of the concatenated characters in the list*)
let rec lst_to_str l s =
  match l with
  | [x] -> ""
  | h::t -> h ^ (lst_to_str t s)

let space_out_word s =
	(* TODO *)
	lst_to_str (add_spaces s (String.length s) 0) ""
		


(* helper function rem_let:
takes string, String length, index(0), and char letter
returns a string with letter replaced by a space*)
let rec rem_let s n i letter =
  match n with
  | 0 -> ""
  | m -> 
    if (String.get s i)=letter (* if current char == letter *)
    then " " ^ rem_let s (n-1) (i+1) letter (* then add space and recursively call rem_let *)
    else (String.sub s i 1) ^ (rem_let s (n-1) (i+1) letter) (* otherwise return the current letter and recursively call rem_let*)

let rec rm_letter kb letter =
	(* TODO *)
  (* iterate through the list kb and call rem_let on each string *)
	match kb with
  | [] -> []
  | h::t -> (rem_let h (String.length h) 0 letter) :: rm_letter t letter



let rec new_kb kb word idx =
	(* TODO *)
	match idx with
  | n when n < 5 -> (* do on each character of word *)
    if (String.contains state.the_word (String.get word idx)) (* if target has the current letter in the guess *)
    then new_kb kb word (idx+1) (* do nothing -- recursively call *)
    else new_kb (rm_letter kb (String.get word idx)) word (idx+1) (* wrong letter -- remove it and recursively call*)
  | _ -> kb (* return when string index is out of bounds *)


(* helper function match_letters:
  takes the current status, the word guessed, and index(0)
  returns a string with updated status from the new guess*)
let rec match_letters blanks word i =
  match i with
  | 5 -> "" (* reached end of string *)
  | n when n<5 -> (* for each char in the string *)
    if (String.get word i)=(String.get state.the_word i) (* if guess and index == word at index *)
    then (String.sub word i 1) ^ match_letters blanks word (i+1) (* then add letter and recusively call *)
    else (String.sub blanks i 1) ^ match_letters blanks word (i+1) (* else add blanks at index and recursively call *)

let fill_in_the_blanks blanks word =
	(* TODO *)
	match_letters blanks word 0



let rm_dup strlst = 
	(* TODO *)
  (*get through string list from left to right
    add elements to unique if not already there*)
  List.fold_left (fun unique x -> if List.mem x unique then unique else unique @ [x]) [] strlst



(* helper function correct_letters:
  takes a word guess and an index(0)
  returns list containing letters from the guess that are in the target word*)
let rec correct_letters word i =
  match i with
  | 5 -> [] (* reached end of word *)
  | n when n<5 -> (* do on each letter *)
    if (String.contains state.the_word (String.get word i)) && ((String.get word i)!=(String.get state.the_word i)) (* if target contains the current letter *)
    then (String.sub word i 1) :: correct_letters word (i+1) (* then add letter to the list -- recursive call *)
    else correct_letters word (i+1) (* else continue to rest of word with recursive call *)

let get_good_letters word =
	(* TODO *)
	correct_letters word 0



(* helper function: space_letters 
  takes a string of blanks, a list of good letters, an index (0), and a list for added letters ([])
  returns a string of the letters in good_letters, in the target, and not in blanks, and with spaces in between *)
let rec space_letters blanks good_letters i letters =
  match i with
  | 5 -> "" (* reached end of blanks *)
  | 4 -> (* last letter -- don't add space*)
    (* if current char of blanks is blank and the current char of target is in good_letters *)
    if ((String.get blanks i)='_') && (List.mem (String.sub state.the_word i 1) good_letters) && (not (List.mem (String.sub state.the_word i 1) letters)) && (not (String.contains blanks (String.get state.the_word i)))
    (* then concatenate the current letter in target to the rest -- recursive call *)
    then (String.sub state.the_word i 1) ^ (space_letters blanks good_letters (i+1) ((String.sub state.the_word i 1)::letters))
    (* else don't add anything -- just recusively call*)
    else space_letters blanks good_letters (i+1) letters
  | n when n<4 -> (* same thing on first four letters, but add a space after the letter this time *)
    if ((String.get blanks i)='_') && (List.mem (String.sub state.the_word i 1) good_letters) && (not (List.mem (String.sub state.the_word i 1) letters)) && (not (String.contains blanks (String.get state.the_word i)))
    then (String.sub state.the_word i 1) ^ " " ^ (space_letters blanks good_letters (i+1) ((String.sub state.the_word i 1)::letters))
    else space_letters blanks good_letters (i+1) letters

let pcl_helper blanks good_letters =
	(* TODO *)
	space_letters blanks (rm_dup (good_letters)) 0 []
	

let print_correct_letters blanks good_letters =
  begin
    print_endline "--------------------------------";
    print_endline ("-- letters in right spots: "^(space_out_word blanks));
    print_endline ("-- correct letters       : "^(pcl_helper blanks good_letters));
    print_endline "--------------------------------"
  end


let print_word str =
	print_endline (space_out_word str)


let print_word_history wh =
	begin
		print_word (List.nth wh 0);
		print_word (List.nth wh 1);
		print_word (List.nth wh 2);
		print_word (List.nth wh 3);
		print_word (List.nth wh 4);
		print_word (List.nth wh 5)
	end


let game_over_billboard =
  ["---------------";
   "-- GAME OVER --";
   "---------------"]

let not_valid_word_billboard =
  [ "----------------------------------------";
    "-- Not a valid word!					 --";
    "-- Reenter your guess(5 letter word): "]

let you_won_billboard =
  [ "-----------------";
    "-- YOU WON !!! --";
    "-----------------";
    ""]

let enter_your_guess_billboard =
  [ "";
    "= = = = = = = = = = = = = = =";
    "";
    "Enter your guess (5 letter word): "]

let print_hud state word_entered blnk gl = 
  if word_entered = state.the_word
  then
    begin
      List.iter print_endline you_won_billboard;
      List.iter print_word state.w_h; (* print word history *)
      failwith "END GAME"
    end
  else
    begin
      print_correct_letters blnk gl;
      print_endline "";
      List.iter print_word state.w_h; (* print word history *)
      print_endline "";
      List.iter print_word state.kb; (* print keyboard *)
      List.iter print_endline enter_your_guess_billboard
    end


let rec game_loop state blnk gl num_tries=
  if num_tries = 6
  then 
    begin
      List.iter print_endline game_over_billboard;
      failwith "END GAME"
    end
  else
    let s = read_line ()
    in let s_upper = String.uppercase_ascii s
    in 
    if is_word_valid s_upper
    then
      (let new_blnk = fill_in_the_blanks blnk s_upper
       in let new_gl = gl @ (get_good_letters s_upper)
       in let new_word_history = (List.mapi (fun i word -> if i=num_tries then s_upper else word) state.w_h)
       in let new_key_board = (new_kb state.kb s_upper 0)
       in let new_state = {state with
                           w_h=new_word_history; kb=new_key_board}
       in
       begin
         print_hud new_state s_upper new_blnk new_gl;
	 game_loop new_state new_blnk new_gl (num_tries+1)
       end)
    else
      (match s_upper with
       |"QUIT" | "EXIT" -> failwith "Quit"
       | _ ->
	 begin
	   List.iter print_endline not_valid_word_billboard;
	   game_loop state blnk gl num_tries
         end
      )


let () =
  begin
    print_hud state "_____" "_____" [];
    game_loop state "_____" [] 0
  end
