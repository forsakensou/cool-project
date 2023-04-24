(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
(*class A2I {

     c2i(char : String) : Int {
	if char = "0" then 0 else
	if char = "1" then 1 else
	if char = "2" then 2 else
        if char = "3" then 3 else
        if char = "4" then 4 else
        if char = "5" then 5 else
        if char = "6" then 6 else
        if char = "7" then 7 else
        if char = "8" then 8 else
        if char = "9" then 9 else
        { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   i2c is the inverse of c2i.
*)
     i2c(i : Int) : String {
	if i = 0 then "0" else
	if i = 1 then "1" else
	if i = 2 then "2" else
	if i = 3 then "3" else
	if i = 4 then "4" else
	if i = 5 then "5" else
	if i = 6 then "6" else
	if i = 7 then "7" else
	if i = 8 then "8" else
	if i = 9 then "9" else
	{ abort(); ""; }  -- the "" is needed to satisfy the typchecker
        fi fi fi fi fi fi fi fi fi fi
     };

(*
   a2i converts an ASCII string into an integer.  The empty string
is converted to 0.  Signed and unsigned strings are handled.  The
method aborts if the string does not represent an integer.  Very
long strings of digits produce strange answers because of arithmetic 
overflow.

*)
     a2i(s : String) : Int {
        if s.length() = 0 then 0 else
	if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
        if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
           a2i_aux(s)
        fi fi fi
     };

(*
  a2i_aux converts the usigned portion of the string.  As a programming
example, this method is written iteratively.
*)
     a2i_aux(s : String) : Int {
	(let int : Int <- 0 in	
           {	
               (let j : Int <- s.length() in
	          (let i : Int <- 0 in
		    while i < j loop
			{
			    int <- int * 10 + c2i(s.substr(i,1));
			    i <- i + 1;
			}
		    pool
		  )
	       );
              int;
	    }
        )
     };

(*
    i2a converts an integer to a string.  Positive and negative 
numbers are handled correctly.  
*)
    i2a(i : Int) : String {
	if i = 0 then "0" else 
        if 0 < i then i2a_aux(i) else
          "-".concat(i2a_aux(i * ~1)) 
        fi fi
    };
	
(*
    i2a_aux is an example using recursion.
*)		
    i2a_aux(i : Int) : String {
        if i = 0 then "" else 
	    (let next : Int <- i / 10 in
		i2a_aux(next).concat(i2c(i - next * 10))
	    )
        fi
    };

};*)


class CommandNode{
   command : String;

   isNum() : Bool { true };
   getCommand() : String { command };
   operation( list : List) : List { list };
   init(c : String) : CommandNode {
      {
         command <- c;
         self;
      }
   };
};

class CommandNodePlus inherits CommandNode{
   isNum() : Bool { false };

   operation(list: List) : List {
      (let num1 : Int <- (new A2I).a2i(list.head().getCommand())
         in
            (let num2 : Int <- (new A2I).a2i(list.tail().head().getCommand())
               in
               {
                  list.tail().tail().cons(new CommandNode.init(((new A2I).i2a(num1 + num2))));
               }
            )
      )
   };
};

class CommandNodeSwap inherits CommandNode{
   isNum() : Bool { false };

   operation(list: List) : List {
      
      (let node1 : CommandNode <- list.head() 
         in
            (let node2 : CommandNode <- list.tail().head()
               in
               {
                  list.tail().tail().cons(node1).cons(node2);
               }
            )
      )
      
   };
};



class List{

   isNil(): Bool {true};

   head(): CommandNode { {abort(); new CommandNode.init("");} };

   tail(): List { { abort(); self; } };

   cons(node : CommandNode) : List{
      (new Cons).init(node, self)
   };

};

class Cons inherits List{
   car : CommandNode;
   cdr : List;

   isNil() : Bool { false };

   head() : CommandNode { car };
   tail() : List { cdr };

   init(node : CommandNode, rest : List) : List{
      {
         car <- node;
         cdr <- rest;
         self;
      }
   };
};

class Main inherits IO {

   myStack : List;

   x : Bool;

   currentString : String;

   currentNode : CommandNode;

   execute() : List {    
      let top : CommandNode <- myStack.head() in 
         {
            if(not top.isNum()) then{
               myStack <- myStack.tail();
               myStack <- top.operation(myStack);
            }else{
               out_string("isNum\n");
            }
            fi;
            myStack;
         }

   };

   printStack(stack : List) : Object {
      if stack.isNil() then out_string("")
      else{
         out_string(stack.head().getCommand());
         out_string("\n");
         printStack(stack.tail());
      }
      fi
   };

   main() : Object {
      {
      x <- false;
      myStack <- new List;
      while(not x) loop
         {
            out_string(">");
            
            currentString <- in_string();
            if(currentString = "x") then{
               x <- true;
            }else{
               if(currentString = "+") then{
                  currentNode <- new CommandNodePlus.init(currentString);
                  myStack <- myStack.cons(currentNode);
               }else{
                  if(currentString = "s") then{
                     currentNode <- new CommandNodeSwap.init(currentString);
                     myStack <- myStack.cons(currentNode);
                  }else{
                     if(currentString = "e") then{
                        myStack <- execute();
                     }else{
                        if(currentString = "d") then{
                           printStack(myStack);
                        }else{
                           currentNode <- new CommandNode.init(currentString);
                           myStack <- myStack.cons(currentNode);
                        }
                        fi;
                     }
                     fi;
                  }
                  fi;
               }
               fi;
            }
            fi;
         }
      pool;
      }
   };

};
