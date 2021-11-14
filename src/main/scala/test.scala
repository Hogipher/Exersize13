// Tests

package base

object tests extends App {


// test Plus
test("""
output(3+5);
""",List(),List(8))

// test Block with stmts
test("""
begin
  output(1);
  output(3);
  output(2);
end
""",List(),List(1,3,2))

// test Var
test("""
begin
  var y := 3;
  output(y*y);
end
""",List(),List(9))

// test Assign
test("""
begin
  var z := 10;
  z := z + 5;
  z := z + 1;
  output(z);
end
""",List(),List(16))

// test nested variable scopes
test("""
begin
  var x := input();
  output(x);
  begin
    var x := x + 2;
    output(x);
  end
  output(x);
end
""",List(5),List(5,7,5))

test("""
begin
  var x := 10;
  if x = 10 then output(x);
end
""",List(),List(10))

test("""
begin
  var x := 11;
  if x = 10 then output(x);
  else output(123);
end
""",List(),List(123))

test("""
begin
  array x [5];
  x [1] := 30;
  output(x [0]);
  output(x [1]);
end
""",List(),List(0,30))

test("""
begin
  var x := 1;
  begin
    var x := 3;
    output(x);
    while x = 3 do x := x - 1;
    output(x);
  end
end
""",List(),List(3,2))

test("""
begin
  proc multiply(x) is output(x*x);
  multiply(3);
end
""",List(),List(9))

test("""
begin
  var x := 3;
  proc multiply(ref x) is output(x*x);
  multiply(x);
end
""",List(),List(9))

test("""
begin
  var y := 5;
  var x := 6;
  proc multiply(ref x) is output(x*x);
  multiply(y);
end
""",List(),List(25))

test("""
begin
  var y := 5;
  proc reAssign(ref x) is x := 6;
  reAssign(y);
  output(y);
end
""",List(),List(6))

test("""
begin
  var y := 5;
  proc scopeCheck(x) is x := 3;
  scopeCheck(y);
  output(y);
end
""",List(),List(5))

test("""
begin
  array x [5];
  proc scopeCheck(ref t) is t := 3;
  scopeCheck(x [1]);
  output(x [1]);
end
""",List(),List(3));

}
