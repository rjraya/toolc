//Expected output: 9 8 7 6 5 4 3 2 1 0 0
program Example1{
 println(new Ex1().compute(10));
}

class Ex1{
 var aux : Int;

 def compute(num : Int) : Int = {
  var num_aux : Int;

  while(!(num < 0) && !(num == 0)){
   aux = num;
   num = num - 1;
   println(num);
  }
  num_aux = num;
  return num;
 }
}

