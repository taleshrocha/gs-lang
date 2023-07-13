record rational_t begin
  int numerador := 2;
  int denominador := 15;
end record

int main() begin
  record rational_t rational;
  print(rational);

  rational.numerador := 2;
  print(rational);

  rational.denominador := 7;
  print(rational);

  int a := 2, b := 6;
  rational.numerador := 15;

  print(a);
  print(rational);
  print(b);

end main
