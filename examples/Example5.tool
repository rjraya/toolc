//Expected output: 9 8 7 6 5 4 3 2 1 0 1 0
program Example5{
 println(new Ex5().compute(10));
}

class Ex5{
 def compute(num : Int) : Int = {
  var aux : Int;
  var num_aux : Int;

  while(!(num < 0) && !(num == 0)){
   aux = num;
   num = num - 1;
   println(num);
  }
  num_aux = num;
  do(this.print(num_aux));
  return num;
 }

 def print(num:Int) : Int = {
  println(num+1);
  return num+1;
 }
}