int main() begin
  float x := 5;
  float y := 4;
  int c := 3;

  if (x>=y)
    if (x>y)
      print((x**2 - y) + c);
    end if

    if (y>x)
      print(x);
    end if
  end if

  if (c>y)
    print(c);
    print(y);
  end if

  print(c+y);
end main