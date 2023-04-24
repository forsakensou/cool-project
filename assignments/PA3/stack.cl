(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
 
 (*定义了一个stack node类，用于产生一个链表结构以表示stack*)
class Node
{
   oper : String;
   next : Node;
   (*初始化函数*)
   init(op : String, ne : Node) : Node
   {
      {
         oper <- op; 
         next <- ne;
         self;
      }
   };
   (*取操作*)
   getoperation() : String
   {
      {
         oper;
      }
   };
   (*取下个节点*)
   getnext() : Node
   {
      {
         next;
      }
   };
};

(*定义主函数，由于需要a2i和i2a函数，因此直接使用继承方式继承A2I*)
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
   
   (*pop从栈顶pop出元素*)
   pop() : Object
   {
      {
         topnode <- topnode.getnext();
      }
   };

   (*execute对应”e”命令，进行执行操作，如果不符合e的要求就保持不变*)   
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

   (*dump对应”d”操作，输出stack中的所有内容*)   
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

   (*deal是处理操作，对于每次输入的字符进行判断与对应操作，需要执行的立即执行，否则压栈*)
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

   (*主函数执行*)
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