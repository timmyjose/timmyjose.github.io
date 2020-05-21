---
layout: post
author: Timmy Jose
title: "Basic grep implementation in Ada"
date: 2020-05-08
---

I have decided to learn Ada (as well!) seriously, and have been working through the (mostly good) book by Andrew Shvets (Beginning Ada - From Novice to Professional).

I decided to take a little breather by writing a simple grep implementation in Ada. 

Here is the code:

```ada
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Task_Identification;

procedure Grep is
  type StringArray is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

  function Grep_File (File_Name: String; Pattern: String; Match_Count: in out Integer) return StringArray is
    File: Ada.Text_IO.File_Type;
    Matches: StringArray (1 .. 10000);
  begin
    declare 
    begin
      Ada.Text_IO.Open (
        File => File, 
        Mode => Ada.Text_IO.In_File, 
        Name => File_Name
        );
    exception
      when Ada.Text_IO.Name_Error => Ada.Text_IO.Put_Line ("Could not find file " & File_Name);
    end;

    declare
      Line_Number: Positive := 1;
      Counter: Positive := 1;
      Unbounded_Line: Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
    begin
      loop
        exit when Ada.Text_IO.End_Of_File (File);

        Unbounded_Line := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Text_IO.Get_Line (File));

        if Ada.Strings.Unbounded.Index (Unbounded_Line, Pattern) /= 0 then
          declare
            Match: Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
          begin
            Ada.Strings.Unbounded.Append (Match, Positive'Image (Line_Number));
            Ada.Strings.Unbounded.Append (Match, " ");
            Ada.Strings.Unbounded.Append (Match, Unbounded_Line);
            Matches (Counter) := Match;
            Counter := Counter + 1;
            Match_Count := Match_Count + 1;
          end;
        end if;
        Line_Number := Line_Number + 1;
      end loop;
    end;
    Ada.Text_IO.Close (File);
    return Matches;
  end Grep_File;

  Pattern: Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
begin
  if Ada.Command_Line.Argument_Count = 0 then
    Ada.Text_IO.Put_Line ("Usage: " & Ada.Command_Line.Command_Name & " FILE [FILE*]");
    Ada.Task_Identification.Abort_Task (Ada.Task_Identification.Current_Task);
  end if;

  Ada.Text_IO.Put_Line ("Enter the pattern to search for:");
  Pattern := Ada.Strings.Unbounded.To_Unbounded_String(Ada.Text_IO.Get_Line);

  for Idx in 1 .. Ada.Command_Line.Argument_Count loop
    declare
      Matches: StringArray (1 .. 10000);
      Match_Count: Integer := 0;
      Match_File_Name: String := Ada.Command_Line.Argument (Idx);
    begin
      Matches := Grep_File (Ada.Command_Line.Argument (Idx), Ada.Strings.Unbounded.To_String (Pattern), Match_Count);
      Ada.Text_IO.Put_Line ("For File " & Match_File_Name);
      Ada.Text_IO.New_Line;

      for Match_Idx in 1 .. Match_Count loop
        declare
          Match: Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
        begin
          Match := Matches (Match_Idx);
          Ada.Text_IO.Unbounded_IO.Put_Line (Match);
        end;
      end loop;
      Match_Count := 0;
    end;
    Ada.Text_IO.New_Line (2);
  end loop;
end Grep;
```
```

Sample run:

```bash
~/dev/playground$ gnatmake -g grep.adb
gcc -c -g grep.adb
gnatbind -x grep.ali
gnatlink grep.ali -g
~/dev/playground$ ./grep
Usage: ./grep FILE [FILE*]

Execution terminated by abort of environment task
~/dev/playground$ ./grep *.lisp
Enter the pattern to search for:
defun
For File add-nums.lisp

 1 (defun main ()


For File add.lisp

 1 (defun add ()
 7 (defun greet (name)
 10 (defun main ()


For File dispatch.lisp

 22 (defun main ()


For File factorial.lisp

 1 (defun factorial (n)


For File hello-world.lisp

 1 (defun hello-world ()


For File mat-max.lisp

 1 (defun main ()
 5 (defun mat-max (mat)


For File palindrome.lisp

 3 (defun palindromep (string)
 11 (defun palindromep-list (lst)


For File replace.lisp

 1 (defun my-replace (lst source target)


For File scratch.lisp

 1 (defun palindromep (list)
 10 (defun palindromep-equivalent (list)

```

[<](2020-04-20-guessing-game-in-kotlin)
[Home](/index.html)
[>](2020-05-20-expr-eval-in-ada)
