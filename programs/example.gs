int main() begin
  int x := 5;
  float y := 4;
  int c := 3;
  array a;
  array b;
  matrix m;
  a := a ++ y;
  b := b ++ 1.0;

  //scan(x);
  print(x);
  print(a);
  a := a ++ 999.0;
  b := b ++ 2.0;
  print(a);
  print(b);
  //m := m ++ a;
  m := m ++ b;
  //print(m);

end main
