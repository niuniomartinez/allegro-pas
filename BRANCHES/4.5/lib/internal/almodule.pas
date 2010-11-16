UNIT almodule;
(*<Functions and procedures to help install and uninstall modules. This is
  for internal use only. *)

{$include _alcfg.inc}

INTERFACE

USES
  albase;

(* This procedure adds the given method to an internal list of methods wich
  will be called by @link(al_exit).  The methods are called in same order as
  inserted.

  A module should call this procedure in the installation function passing the
  uninstalation procedure.
  @return(@false if there are any problem @(i.e. no space to store the
    method@)).
  @seealso(_alRemoveExitMethod_) *)
  PROCEDURE _alAddExitMethod_ (aMethod: AL_CALLBACK_PROC);

(* Removes the given method from the interal list of methods called by
  @link(al_exit).

  A module @bold(must) call this function in the uninstallation procedure if
  it did call @link(_alAddExitMethod_) somewhere. The method @bold(must)
  be the same than the one passed to @code(_alAddExitMethod_).

  It doesn't return any value, even if it can't find the method. *)
  PROCEDURE _alRemoveExitMethod_ (aMethod: AL_CALLBACK_PROC);

(* This procedure is for internal use only.  You mus not call it @bold(never).

  It calls all uninstall methods in order and cleans the internal list. *)
  PROCEDURE _alUninstallAllModules_;



IMPLEMENTATION

(* The list of exit methods. *)
TYPE
  TExitListNodePtr = ^TExitListNode;
  TExitListNode = RECORD
    Method: AL_CALLBACK_PROC;
    Next: TExitListNodePtr
  END;

VAR
  ListOfExitMethods: TExitListNodePtr = NIL;



(* This function adds the given method to an internal list of methods wich
  will be called by @link(al_exit). *)
  PROCEDURE _alAddExitMethod_ (aMethod: AL_CALLBACK_PROC);
  VAR
    Node: TExitListNodePtr;
  BEGIN
  { If method is yet on list, it returns. }
    Node := ListOfExitMethods;
    WHILE Node <> NIL DO
    BEGIN
      IF Node^.Method = aMethod THEN
	EXIT;
      Node := Node^.Next;
    END;
  { Adds the new method. }
    new (Node);
    Node^.Method := aMethod;
    Node^.Next := NIL;
    IF ListOfExitMethods = NIL THEN
      ListOfExitMethods := Node
    ELSE
      ListOfExitMethods^.Next := Node;
  END;



(* Removes the given method from the interal list of methods called by
  @link(al_exit). *)
  PROCEDURE _alRemoveExitMethod_ (aMethod: AL_CALLBACK_PROC);
  VAR
    Node, Prev: TExitListNodePtr;
  BEGIN
    Node := ListOfExitMethods;
    Prev := NIL;
    WHILE Node <> NIL DO
    BEGIN
      IF Node^.Method = aMethod THEN
      BEGIN
	IF Prev <> NIL THEN
	  Prev^.Next := Node^.Next
	ELSE
	  ListOfExitMethods := Node^.Next;
	Dispose (Node);
	EXIT;
      END;
      Prev := Node;
      Node := Node^.Next;
    END;
  END;



(* It calls all uninstall methods in order and cleans the internal list. *)
  PROCEDURE _alUninstallAllModules_;
  VAR
    Node, Prev: TExitListNodePtr;
  BEGIN
    Node := ListOfExitMethods;
    WHILE Node <> NIL DO
    BEGIN
      Node^.Method;
      Prev := Node;
      Node := Node^.Next;
      Dispose (Prev);
    END;
    ListOfExitMethods := NIL;
  END;

END.
