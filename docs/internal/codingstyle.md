# Allegro.pas Coding Style Guide #

## Introduction ##

This is the **Allegro.pas Coding Style Guide**.  This follows the [Embarcadero's
Object Pascal Style Guide](https://edn.embarcadero.com/article/10280) but with
some differences;  this document also explains the reasons of these differences.

Allegro.pas code will follow this Style Guide.  You should follow these
conventions if you want to submit code to the project.

Note that these are conventions based primarily on matters of taste.  I don't
believe they are right and others are wrong.  Please feel free to use any style
as far as it is consistent.



## Source files ##

### Source-file naming ###

Each Allegro.pas unit implements an Allegro add-on (except core subsystems
`al5base`, `al5strings` and `al5opengl` that are separated units for
legibility).

Names for units should be prefixed with "`al5`" following the name of the
add-on/subsystem it implements.  Names of both file and unit should be
lowercase.

### Source-file organization ###

All units should contain the following elements in the following order:

1. Unit Name
1. Block comment with unit description.
1. Copyright and license block comment.
1. Allegro configuration file.
1. Interface section
1. Implementation
1. A closing end and a period. 

Each add-on may use more than one header file, so the Pascal unit should
include all header declarations in same order including a comment with the
header file name so it should be easy to compare with the original file to find
bugs or changes.



## Naming convention ##

Public names and identifiers should be the same than the original Allegro API
ones.  In case such identifier is a reserver word (such as Pascal keywords or
RTL subroutines) then change to an alternate name (this will happend on
parameter and field names only so you may just add "`f`" or "`a`" as prefix to
fix it).

Keywords should be lowercase.  Types should be UPPER_CASE.  Everything else
should be cased the same way than in the original API.

Pointers should have "`ptr`" as sufix.



## White usage space ##

### Blank spaces ###

Blanks should not be used:

1. Before or after a .(dot) operator.
1. Between am unary operator and its operand.
1. After an opening parenthesis `(` or before a closing parenthesis `)`.
1. After an opening square bracket `[` or before a closing square bracket `]`.
1. Before a colon or semicolon.

Note that the original Style Guide states that it also should not be used
_between a method name and its opening parenthesis_ and _between a cast and the
expression being cast_, though I think sources are way more easy to read if you
insert a space in such places.

Examples of correct usage:

~~~
function MyFunc (var Value: Integer);
MyPointer := @MyRecord;
MyInteger := MyIntegerArray[5];
MyValue   := integer (MyVariable);
~~~

Examples of incorrect usage:

~~~
function MyFunc( var Value : Integer ) ;
MyPointer := @ MyRecord;
MyInteger := MyIntegerArray [ 5 ] ;
MyValue:=integer(MyVariable);
~~~

### Indentation ###

You should always indent two spaces for all indentation levels.  In other
words, the first level of indentation is two spaces, the second level four
spaces, the third level 6 spaces, etc. Never use tab characters.

There are few exceptions.  The reserved words `unit`, `interface`,
`implementation`, `initialization` and `finalization` should always be flush
with the margin.  The final `end` statement at the end of a unit should be
flush with the margin.  In the project file, the word `program`, and the main
`begin` and `end` block should all be flush with the margin.  The code inside
the `begin..end` block, should be indented at least two spaces.

Comments should have one less indentation than the code if available.

For example:

~~~~
(* Good indentation example. *)
  procedure Example (Parm: Integer);
  begin
    if Parm = Value then
      DoSomething (Param)
    else begin
      DoElse (Param);
      while Param > 0 do
      begin
        DoOther (Param);
        DEC (Param)
      end
    end;
  { Other more strict way.  It shows also more comment indentation. }
    if Parm = Value then
      DoSomething (Param)
    begin
      begin
        DoElse (Param);
     { This comments the loop. }
        while Param > 0 do
          begin
            DoOther (Param);
            DEC (Param);
          end
      end
  end;
~~~~



### Empty lines ###

Use empty lines to separate logical blocks.  Use 3 empty lines to separate
each `record`, `procedure` or `function` with others so it's easy to find where it
starts and ends.  So do not put more than one empty line to separate code
inside a `begin...end` code block.



### Continuation Lines ###

Lines should be limited to 80 columns.  Lines longer than 80 columns should be
broken into one or more continuation lines, as needed.  All the continuation
lines should be aligned and indented from the first line of the statement, and
indented two characters.  Always place `begin` statements on their own line.

Examples:

~~~
(* CORRECT *)
  function CreateWindowEx (
    ExStyle: DWORD;
    ClassName, WindowName: PChar;
    Style: DWORD;
    X, Y, nWidth, nHeight: Integer;
    WndParent: HWND;
    Menu: HMENU;
    Instance: HINST;
    lpParam: Pointer
  ): HWND; stdcall;

{ CORRECT }
  if (X = Y) or (Y = X)
  or (Z = P) or (F = J)
  then
  begin
    S := J
  end;
~~~



## Comments ##

Except for the unit description, unit should include the same comments than the
header file only.

Use `(*...*)` block comments only, using `{...}` to deactivate code.  Try to
not use nested comments because they're a common source of problems and
mistakes.  Deactivate nested comments from your compiler options to be sure.

Do not use C++ style comments (`//...`) just because they're ugly.  An exception
for C++ comments may be to temporally deactivate code.
