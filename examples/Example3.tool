//Expected output: null
program Inheritance{
 println(new Person().getName());
}

class Person{
 var id : String;
 var name : String;

 def getName() : String = {
   return name;
 }
 def getId() : String = {
   return id;
 }
 def setName(n : String) : Int = {
   name = n; return 0;
 }
 def setId(i : String) : Int = {
   id = i; return 0;
 }
 def talk() : Int = {
   println("bla bla bla");
   return 0;
 }
}