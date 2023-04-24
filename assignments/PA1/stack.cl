(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
class Node
{
   oper : String;
   next : Node;

   init(op : String, ne : Node) : Node
   {
      {
         oper <- op; 
         next <- ne;
         self;
      }
   };
   getoperation() : String
   {
      {
         oper;
      }
   };
   getnext() : Node
   {
      {
         next;
      }
   };
};

class Main inherits A2I
{
   topnode : Node;

   push(str : String) : Node
   {
      let addnode : Node, nil : Node in
      {
         if (isvoid topnode) then
         {
            addnode <- (new Node).init(str, nil);
         }
         else 
         {
            addnode <- (new Node).init(str, topnode);
         }
         fi;
         topnode <- addnode;
         addnode;
      }
   };

   pop() : Object
   {
      {
         topnode <- topnode.getnext();
      }
   };

   
   execute() : Object
   {
      let num1 : Int, num2 : Int, change1 : String, change2 : String in
      {
         if (isvoid topnode) then
         {
            0;
         } else
         if (topnode.getoperation() = "+") then
         {
            pop();
            num1 <- a2i(topnode.getoperation());
            pop();
            num2 <- a2i(topnode.getoperation());
            pop();
            push(i2a(num1 + num2));
         } else 
         if (topnode.getoperation() = "s") then
         {
            pop();
            change1 <- topnode.getoperation();
            pop();
            change2 <- topnode.getoperation();
            pop();
            push(change1);
            push(change2);
         } else
         {
            0;
         }
         fi fi fi;
      }
   };
   
   dump() : Object
   {
      let node: Node <- topnode in {
         while (not (isvoid node)) loop
         {
            (new IO).out_string(node.getoperation());
            (new IO).out_string("\n");
            node <- node.getnext();
         }
         pool;
      }
   };

   deal(input : String) : Object
   {
      { 
         if (input = "x") then
         {
            (new IO).out_string("stop!\n");
            abort();
         } else
         if (input = "d") then
         {
            dump();
            (*(new IO).out_string("d\n");*)
         } else
         if (input = "e") then
         {
            execute();
            (*(new IO).out_string("e\n");*)
         } else
         {
            push(input);
            (*(new IO).out_string("c\n");*)
         }
         fi fi fi;
      }  
   };

   main() : Object 
   {
      let input : String in
      {
         while true loop{
            (new IO).out_string(">");
            input <- (new IO).in_string();
            deal(input);
         }
         pool;
      }
   };     
};
