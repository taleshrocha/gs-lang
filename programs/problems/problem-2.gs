int main() begin
  int i1, i2, i3, i4;
  int x;

  while(x >= 0) 
    scan(x);

    if(x >= 0 and x <= 25)
      i1++;
    elif(x >= 26 and x <= 50)
      i2++;
    elif(x >= 51 and x <= 75)
      i3++;
    elif(x >= 76 and x <= 100)
      i4++;
    end if

  end while

  print("Interval 1");
  print(i1);
  print("Interval 2");
  print(i2);
  print("Interval 3");
  print(i3);
  print("Interval 4");
  print(i4);
end main
