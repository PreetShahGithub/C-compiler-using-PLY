# include < stdio.h >
int main () {
  int a = 50, b = 60;
  if(a > 40){
    if(a<b){
      a = a + 2;
    }
    else{
      b = b + 2;
    }
    a = a - 6;
  }
  b = 2*b;
}
