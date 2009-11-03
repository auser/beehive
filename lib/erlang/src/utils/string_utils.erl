-module (string_utils).
-author("Serge: onerlang.blogspot.com").
-purpose("string replace functionality").
-export([sub/3,gsub/3,test/0]).
-import (string, [len/1, str/2, left/2,right/2,concat/2]).

sub(Str,Old,New) ->

   Lstr = len(Str),
   Lold = len(Old),
   Pos  = str(Str,Old),
   if 
      Pos =:= 0 -> 
                   Str;
      true      ->
           LeftPart = left(Str,Pos-1),
           RitePart = right(Str,Lstr-Lold-Pos+1),
           concat(concat(LeftPart,New),RitePart)
   end.

gsub(Str,Old,New) ->
  Acc = sub(Str,Old,New),
  subst(Acc,Old,New,Str).

subst(Str,_Old,_New, Str) -> Str;
subst(Acc, Old, New,_Str) ->
         Acc1 = sub(Acc,Old,New),
         subst(Acc1,Old,New,Acc).

test() ->
   io:format("~p ~p ~p ~p ~p ~p ~p ~n",
     [
      "SELECT * FROM people WHERE first='John' OR last='John'" =:=
  gsub("SELECT * FROM people WHERE first=$1 OR last=$1","$1","'John'"),
      "aBc" =:= sub("abc","b","B"),
      "Abc" =:= sub("abc","a","A"),
      "abC" =:= sub("abc","c","C"),
      "aac" =:= gsub("bbc","b","a"),
      "abc" =:= gsub("abc","d","C"),
      "abc" =:= sub("abc","d","D")]).