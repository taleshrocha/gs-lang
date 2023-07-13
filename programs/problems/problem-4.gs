record rational_t begin
  int numerador := 2;
  int denominador := 15;
end record

int main() begin
  record rational_t rational;

  rational.numerador := 2;

  int b := rational.numerador;

end main
