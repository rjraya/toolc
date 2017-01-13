//Expected output: 1022
program CountChange {
 println(new countChangeApp().initCoins().countChange(300));
}

class countChangeApp{
 var array: Int[];

 def initCoins() :countChangeApp = {
  array = new Int[7];
  array[0] = 5;
  array[1] = 10;
  array[2] = 20;
  array[3] = 50;
  array[4] = 100;
  array[5] = 200;
  array[6] = 500;

  return this;
 }

 def countChange(money:Int):Int = {
  return this.computeCountChange(money,array);
 }
    
 def computeCountChange(money: Int,coins :Int[]): Int = { 
    var answer : Int;
    if (money < 0 || coins.length == 0) answer = 0;
    else if (money == 0) answer = 1;
    else answer = this.computeCountChange(money-coins[0],coins) + this.computeCountChange(money,this.tail(coins));
    return answer;
 }

 def tail(array: Int[]): Int[] = {
  var tail : Int[];
  var i : Int;

  if(0 < array.length){
   tail = new Int[array.length - 1];
   i = 0;
   while(i < array.length - 1){
    tail[i] = array[i+1];
    i = i+1;
   }
  }else{
   tail = new Int[array.length];
  }
  return tail;
 }
}
    
