int main() begin
  int x := 5;
  float y := 4;
  int c := 3;
  array a float[2];
  //array b;
  //matrix m;
  //a := a ++ y;
  //b := b ++ 1.0;

  //scan(x);
  print(x);
  print(a);
  addElement::a(2);
  addElement::a(3.7);
  print(getElement::a(1));
  float ff := getElement::a(0);
  print(ff);
  //float pp := getElement::a(2);
  //print(pp);
  //addElement::a(5.7);
  print(a);

end main
