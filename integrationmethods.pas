unit integrationmethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParseMath, matrix, math;

type

  { TIntegrationMethods }

  TIntegrationMethods = class
    public
      Parse : TParseMath;
      function Trapecios(ValueX,ValueY:Real;ValueN:Integer;Expression:String;Area:Boolean):Real;
      function Simpson13(ValueX,ValueY:Real;ValueN:Integer;Expression:String;Area:Boolean):Real;
      function Simpson38C(ValueX,ValueY:Real;ValueN:Integer;Expression:String;Area:Boolean):Real;
      function GaussQuadrature(ValueX,ValueY:Real;Expression:String):Real;
      function Romberg(xA, xB, eError: Real; equation: String): Real;
      constructor Create;
  end;

implementation

function TIntegrationMethods.Trapecios(ValueX,ValueY:Real;ValueN:Integer;Expression:String;Area:Boolean):Real;
var
  i:Integer;
  auxX,auxY,r,h:Real;
begin

  Parse.Expression:=Expression;
  r:= 0;
  h:= (ValueY-ValueX)/ValueN;

  Parse.AddVariable('x',ValueX);
  auxX:= Parse.Evaluate();

  Parse.NewValue('x',ValueY);
  auxY:=Parse.Evaluate();

  if (Area) then
     r:= (abs(auxX)+abs(auxY))/2
  else
     r:= (auxX+auxY)/2;
  ValueX:=ValueX+h;


  for i:=0 to ValueN-2 do begin
      Parse.NewValue('x',ValueX);
      if (Area) then begin
         r:= r + abs(Parse.Evaluate())
      end
      else
         r:= r + Parse.Evaluate();
      ValueX:=ValueX+h;
  end;
  result:= h*r;
end;

constructor TIntegrationMethods.Create;
begin
  Parse:= TParseMath.create();
end;

function TIntegrationMethods.Simpson13(ValueX,ValueY:Real;ValueN:Integer;Expression:String;Area:Boolean):Real;
var
  i:Integer;
  impar,par,h,auxX,auxY:Real;
begin

  Parse.Expression:=Expression;
  h:= (ValueY-ValueX)/(2*ValueN);
  impar:=0;
  par:=0;

  Parse.AddVariable('x',ValueX);
  auxX:= Parse.Evaluate();

  if Area then begin
     auxX:= abs(auxX);
  end;
  Parse.NewValue('x',ValueY);
  auxY:= Parse.Evaluate();

  if Area then begin
     i:=1;
     while i <2*ValueN do begin
        Parse.NewValue('x',ValueX+i*h);
        impar := impar + abs(Parse.Evaluate());
        i:=i+2;
     end;

     i:=2;
     Parse.NewValue('x',ValueX);
     while i <(2*ValueN)-1 do begin
        Parse.NewValue('x',ValueX+i*h);
        par := par + abs(Parse.Evaluate());
        i:=i+2;
     end;
  end

  else begin
     i:=1;
     while i <2*ValueN do begin
        Parse.NewValue('x',ValueX+i*h);
        impar:= impar + Parse.Evaluate();
        i:=i+2;
     end;

     i:=2;
     Parse.NewValue('x',ValueX);
     while i <(2*ValueN)-1 do begin
        Parse.NewValue('x',ValueX+i*h);
        par:= par + Parse.Evaluate();
        i:=i+2;
     end;
  end;


  if Area then begin
     auxY:= abs(auxY);
  end;

  result:=(h/3)*((auxX+auxY)+(4*impar)+(2*par));
end;

function TIntegrationMethods.Simpson38C(ValueX,ValueY:Real;ValueN:Integer;Expression:String;Area:Boolean):Real;
var
  i:Integer;
  auxX,auxY,n,p,h:Real;
begin

  Parse.Expression:=Expression;

  h:= (ValueY-ValueX)/(3*ValueN);
  n:=0;
  p:=0;

  Parse.AddVariable('x',ValueX);
  Parse.NewValue('x',ValueX);
  auxX:=Parse.Evaluate();

  if Area then
     auxX:=abs(auxX);

  Parse.NewValue('x',ValueY);
  auxY:=Parse.Evaluate();


  if Area then begin
     for i := 1 to 3*ValueN-1 do begin
         Parse.NewValue('x',ValueX+i*h);
         if(i mod 3 = 0) then begin
              n:= n + abs(Parse.Evaluate())
         end
         else begin
              p:= p + abs(Parse.Evaluate());
         end;
     end;
  end
  else begin
     for i := 1 to 3*ValueN-1 do begin
         Parse.NewValue('x',ValueX+i*h);
         if(i mod 3 = 0) then begin
              n:= n + Parse.Evaluate()
         end
         else begin
              p:= p + Parse.Evaluate();
         end;
     end;
  end;

  if Area then
     auxY:=abs(auxY);
  result :=(3/8*h)*((auxX+auxY)+(2*n)+(3*p));
end;

function TIntegrationMethods.GaussQuadrature(ValueX, ValueY: Real;Expression: String): Real;
var x1,x2,temp1,temp2:Real;
begin
  Parse.Expression:=Expression;
  x1:= sqrt(1/3);
  x2:= -sqrt(1/3);
  temp1:= (ValueX + ValueY)/2 + (ValueY - ValueX)/2*x1;
  temp2:= (ValueX + ValueY)/2 + (ValueY - ValueX)/2*x2;
  Parse.AddVariable('x',temp1);
  temp1:= Parse.Evaluate();
  Parse.NewValue('x',temp2);
  temp2:= Parse.Evaluate();
  Result:= (temp1 + temp2)*(ValueY-ValueX)/2 ;
end;

function TIntegrationMethods.Romberg(xA, xB, eError: Real; equation: String): Real;
var  fa,fb,error:Real;
     n,m,iter,j,k:Integer;
     matrix:TDMatrix;

     function trapEq(n:Integer;a,b:Real):Real;
     var  i:Integer;
          h,x,sum:real;
     begin
       h:= (b-a)/n;
       x:= a;
       parse.NewValue('x',x);
       sum:= Parse.Evaluate();
       for i:= 1 to n-1 do begin
         x:= x + h;
         parse.NewValue('x',x);
         sum:= sum + 2 * Parse.Evaluate();
       end;
       Parse.NewValue('x',b);
       sum:= sum + Parse.Evaluate();
       Result:=(b-a)*(sum/(2*n));
     end;

begin
  Parse.Expression:=equation;
  m:= 10;
  matrix:= TDMatrix.Create(m+1,m+1);

  Parse.AddVariable('x',xA);
  fa:= Parse.Evaluate();
  Parse.NewValue('x',xB);
  fb:= Parse.Evaluate();


  n:= 1;
  matrix.data[0]:= trapEq(n,xA,xB);
  iter:= 0;
  error:= 1;
  while (iter < 20) or (error >= eError) do begin
    iter:= iter + 1;
    n:= round(power(2,iter));

    Parse.NewValue('x',xA);
    fa:= Parse.Evaluate();
    Parse.NewValue('x',xB);
    fb:= Parse.Evaluate();
    matrix.data[(iter+1)*(m+1) + 1]:= trapEq(n,xA,xB);
    for k:= 2 to iter + 1 do begin
      j:= 2 + iter - k;
      matrix.data[j*(m+1)+k]:= (power(4,k-1)*matrix.data[(j+1)*(m+1)+(k-1)]-matrix.data[j*(m+1)+(k-1)])/(power(4,k-1)-1)
    end;
    error:= abs((matrix.data[1*(m+1)+(iter+1)]- matrix.data[2*(m+1)+iter+1]));
  end;
  Result:= matrix.data[1*(m+1)+iter+1];

end;



end.

