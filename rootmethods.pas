unit rootmethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParseMath, Dialogs, math, primitive, matrix;

type

  { TRootMethods }

  TRootMethods = Class
    public
      primitives: TPrimitive;
      parse: TParseMath;
      data: TStringList;
      vars: TStringList;
      equations: TStringList;
      points: TStringList;
      constructor Create;
      destructor Destroy; override;
      function evaluate(xA: Double): Double;
      function bisection(xA, xB, eError: Double; equation: String): Double;
      function falsePosition(xA, xB, eError: Double; equation: String): Double;
      function newton(xA, eError: Double; equation, pEquation: String): Double;
      function secant(xA, eError: Double; equation: String): Double;
      function fixedPoint(xA,eError: Double;equation, pEquation: String): Double;
      function gFixedPoint(xA, yA, eError: Double): Double;
      function gNewton(eError: Double): Double;
  end;

implementation

constructor TRootMethods.Create();
begin
  parse:= TParseMath.create();
  //parse.AddVariable('x',0.0);
  //parse.AddVariable('y',0.0);
  primitives:= TPrimitive.Create;
  data:= TStringList.Create;
  equations:= TStringList.Create;
  vars:= TStringList.Create;
  points:= TStringList.Create;
end;

destructor TRootMethods.Destroy;
begin
  parse.destroy;
  data.Destroy;
  points.Destroy;
  equations.Destroy;
  primitives.Destroy;
end;

function TRootMethods.evaluate(xA: Double): Double;
begin
  try
    parse.NewValue('x', xA);
    Result:= parse.Evaluate();
  except
    ShowMessage('La funcion no es continua');
    WriteLn('La funcion no es continua');
    data.Add('La funcion no es continua');
    Exit;
  end;
end;

function TRootMethods.bisection(xA, xB, eError: Double; equation: String): Double;
var  error,tmpXn: Double;

  function xn(xA,xB: Double): Double;
  begin
    result:= (xA + xB)/2
  end;

begin
  parse:= TParseMath.create();
  parse.AddVariable('x',0.0);
  parse.AddVariable('y',0.0);

  parse.Expression:= equation;

  //Verificamos si la raiz esta en f(xA).
  if evaluate(xA) = 0 then begin
    data.Add(FloatToStr(xA));
    Result:= xA;
    Exit;
  end;

  //Verificamos si la raiz esta en f(xB).
  if evaluate(xB) = 0 then begin
    data.Add(FloatToStr(xB));
    Result:= xB;
    Exit;
  end;

  //Verificamos si se cumple el teorema de Bolzano.
  if evaluate(xA)*evaluate(xB) > 0 then begin
    ShowMessage('No se cumple el teorema de Bolzano');
    WriteLn('No se cumple el teorema de Bolzano');
    data.Add('No se cumple el teorema de Bolzano');
    Exit;
  end;

  //Realizamos el metodo de Biseccion.
  tmpXn:= xn(xA, xB);

  data.Add(FloatToStr(xA));
  data.Add(FloatToStr(xB));
  data.Add(FloatToStr(tmpXn));
  data.Add(FloatToStr(sign(evaluate(xA)*evaluate(tmpXn))));

  if evaluate(xA)*evaluate(tmpXn) > 0 then xA:= tmpXn else xB:= tmpXn;
  error:= 1;
  while eError < error do begin

    //Verificamos si la raiz esta en f(xA).
    if evaluate(xA) = 0 then begin
      data.Add(FloatToStr(xA));
      Result:= xA;
      Exit;
    end;

    //Verificamos si la raiz esta en f(xB).
    if evaluate(xB) = 0 then begin
      data.Add(FloatToStr(xB));
      Result:= xB;
      Exit;
    end;

    //Continuamos con las iteraciones del metodo.
    error:= abs(tmpXn - xn(xA, xB));
    tmpXn:= xn(xA, xB);
    data.Add(FloatToStr(xA));
    data.Add(FloatToStr(xB));
    data.Add(FloatToStr(tmpXn));
    data.Add(FloatToStr(sign(evaluate(xA)*evaluate(tmpXn))));
    data.Add(FloatToStr(error));
    if evaluate(xA)*evaluate(tmpXn) > 0 then xA:= tmpXn else xB:= tmpXn;
  end;
  Result:= tmpXn;
  parse.destroy;
end;

function TRootMethods.falsePosition(xA, xB, eError: Double; equation: String): Double;
var  error,tmpXn: Double;

  function xn(xA,xB: Double): Double;
  var fxA,fxB: Double;
  begin
    parse.NewValue('x', xA);
    fxA:= parse.evaluate();

    parse.NewValue('x', xB);
    fxB:= parse.evaluate();

    result:= xA - fxA*(xB - xA)/(fxB - fxA)
  end;
begin
  parse:= TParseMath.create();
  parse.AddVariable('x',0.0);
  parse.AddVariable('y',0.0);

  parse.Expression:= equation;

  //Verificamos si la raiz esta en f(xA).
  if evaluate(xA) = 0 then begin
    data.Add(FloatToStr(xA));
    Result:= xA;
    Exit;
  end;

  //Verificamos si la raiz esta en f(xB).
  if evaluate(xB) = 0 then begin
    data.Add(FloatToStr(xB));
    Result:= xB;
    Exit;
  end;

  //Verificamos si se cumple el teorema de Bolzano.
  if evaluate(xA)*evaluate(xB) > 0 then begin
    ShowMessage('No se cumple el teorema de Bolzano');
    WriteLn('No se cumple el teorema de Bolzano');
    data.Add('No se cumple el teorema de Bolzano');
  end;

  //Realizamos el metodo de Falsa Posicion.
  tmpXn:= xn(xA, xB);

  //Añadimos A, B, Xn y el Signo a nuestro registro
  data.Add(FloatToStr(xA));
  data.Add(FloatToStr(xB));
  data.Add(FloatToStr(tmpXn));
  data.Add(FloatToStr(sign(evaluate(xA)*evaluate(tmpXn))));

  if evaluate(xA)*evaluate(tmpXn) > 0 then xA:= tmpXn else xB:= tmpXn;
  error:= 1;
  while eError < error do begin
    //Verificamos si la raiz esta en f(xA).
    if evaluate(xA) = 0 then begin
      data.Add(FloatToStr(xA));
      Result:= xA;
      Exit;
    end;
    //Verificamos si la raiz esta en f(xB).
    if evaluate(xB) = 0 then begin
      data.Add(FloatToStr(xB));
      Result:= xB;
      Exit;
    end;
    //Continuamos con las iteraciones del metodo.
    error:= abs(tmpXn - xn(xA, xB));
    tmpXn:= xn(xA, xB);
    data.Add(FloatToStr(xA));
    data.Add(FloatToStr(xB));
    data.Add(FloatToStr(tmpXn));
    data.Add(FloatToStr(sign(evaluate(xA)*evaluate(tmpXn))));
    data.Add(FloatToStr(error));
    if evaluate(xA)*evaluate(tmpXn) > 0 then xA:= tmpXn else xB:= tmpXn;
  end;
  Result:= tmpXn;
  parse.destroy;
end;

function TRootMethods.newton(xA, eError: Double; equation, pEquation: String): Double;
var  error,tmpXn: Double;

  function xn(xA :Double; dEquation: String ): Double;
  var fxA,fpxA: Double;
  begin
    fxA:= evaluate(xA);

    parse.Expression:= dEquation;
    fpxA:= parse.evaluate();

    if fpxA = 0 then begin
      ShowMessage('La recta tangente es paralela al eje x');
      Exit;
    end;

    result:= xA - (fxA/fpxA);
  end;
begin
  parse:= TParseMath.create();
  parse.AddVariable('x',0.0);
  parse.AddVariable('y',0.0);

  parse.Expression:= equation;
  //Verificamos si la raiz esta en f(xA).
  if evaluate(xA) = 0 then begin
    data.Add(FloatToStr(xA));
    Result:= xA;
    Exit;
  end;

  //Realizamos el metodo de Newton.
  data.Add(FloatToStr(xA));
  error:= 1;
  while eError < error do begin
    tmpXn:= xn(xA,pEquation);
    error:= abs(tmpXn - xA);
    xA:= tmpXn;
    parse.Expression:= equation;
    //Verificamos si la raiz esta en f(xA).
    if evaluate(xA) = 0 then begin
      data.Add(FloatToStr(xA));
      Result:= xA;
      Exit;
    end;
    //Continuamos con las iteraciones del metodo.
    data.Add(FloatToStr(xA));
    data.Add(FloatToStr(error));
  end;
  Result:= tmpXn;
  parse.destroy ;
end;

function TRootMethods.secant(xA, eError: Double; equation: String): Double;
var  error,tmpXn: Double;

  function xn(xA, eError :Double): Double;
  var fxA, fxAp, fxAm: Double;
  begin
    parse.NewValue('x', xA);
    fxA:= parse.evaluate();

    parse.NewValue('x', xA - eError/10);
    fxAm:= parse.evaluate();

    parse.NewValue('x', xA + eError/10);
    fxAp:= parse.evaluate();

    if fxAp - fxAm = 0 then begin
      ShowMessage('La recta tangente es paralela al eje x');
      Exit;
    end;

    result:= xA - ( 2*(eError/10)*fxA)/(fxAp - fxAm);
  end;
begin
  parse:= TParseMath.create();
  parse.AddVariable('x',0.0);
  parse.AddVariable('y',0.0);

  parse.Expression:= equation;
  //Verificamos si la raiz esta en f(xA).
  if evaluate(xA) = 0 then begin
    data.Add(FloatToStr(xA));
    Result:= xA;
    Exit;
  end;

  //Realizamos el metodo de la Secante.
  data.Add(FloatToStr(xA));
  error:= 1;
  while eError < error do begin
    tmpXn:= xn(xA,eError);
    error:= abs(tmpXn - xA);
    xA:= tmpXn;

    //Verificamos si la raiz esta en f(xA).
    if evaluate(xA) = 0 then begin
      data.Add(FloatToStr(xA));
      Result:= xA;
      Exit;
    end;
    //Continuamos con las iteraciones del metodo.
    data.Add(FloatToStr(xA));
    data.Add(FloatToStr(error));
  end;
  Result:= tmpXn;
  parse.destroy;
end;

function TRootMethods.fixedPoint(xA,eError: Double;equation, pEquation: String): Double;
var
  tmpXn, error: Double;
begin
  parse:= TParseMath.create();
  parse.AddVariable('x',0.0);
  parse.AddVariable('y',0.0);

     parse.Expression:= pEquation;
     parse.NewValue('x',xA);
     if (parse.evaluate() < 0) or (parse.Evaluate() > 1) then begin
       ShowMessage('La funcion tiene más probabilidades de diverger');
       WriteLn('La funcion tiene más probabilidades de diverger');
       data.Add('La funcion tiene más probabilidades de diverger');
       Exit;
     end;

     data.Add(FloatToStr(xA));
     parse.Expression:= equation;
     error:=1;
     while error > eError do begin
       tmpXn:= evaluate(xA);
       data.Add(FloatToStr(tmpXn));
       error:= abs(xA - tmpXn);
       xA:= tmpXn;
       data.Add(FloatToStr(error));
       //Errors.Add(FloatToStr(abs(error/varXn)));
       //Errors.Add(FloatToStr(abs(error/varXn*100)));
     end;
     Result:= xA;
     parse.destroy;
end;

function TRootMethods.gFixedPoint(xA, yA, eError: Double): Double;
var tmpXn, tmpYn, error: Double;
begin

  parse:= TParseMath.create();
  parse.AddVariable('x',0.0);
  parse.AddVariable('y',0.0);

  parse.NewValue('x',xA);
  parse.NewValue('y',yA);
  data.Add(FloatToStr(xA));
  data.Add(FloatToStr(yA));
  error:= 1;
  while error > eError do begin
    parse.Expression:= equations[0];
    tmpXn:= parse.Evaluate();
    parse.NewValue('x',tmpXn);
    parse.Expression:= equations[1];
    tmpYn:= parse.Evaluate();
    parse.NewValue('y',tmpYn);
    error:= sqrt(power(tmpXn - xA,2) + power(tmpYn - yA,2));
    xA:= tmpXn;
    yA:= tmpYn;
    data.Add(FloatToStr(tmpXn));
    data.Add(FloatToStr(tmpYn));
    data.Add(FloatToStr(error));
  end;
  Result:= yA;
  parse.destroy;
end;

function TRootMethods.gNewton(eError: Double): Double;
var i, it,spaces:Integer;
    error,aux:Real;
    xnMatrix,ijbMatrix,fxMatrix,xaMatrix:TDMatrix;
begin
  error:=0;
  //Funcion1.Text:='cos(x)+exp(y)-x';
  //Funcion2.Text:='sin(5*x)+x*y-y';
  xaMatrix:= TDMatrix.Create(points.Count,1);
  xnMatrix:= TDMatrix.Create(points.Count,1);
  fxMatrix:= TDMatrix.Create(points.Count,1);
  ijbMatrix := TDMatrix.Create(points.Count,points.Count);
  spaces:=4;

  for i:=0 to vars.Count-1 do begin
    parse.AddVariable(vars[i],StrToFloat(points[i]));
  end;

  with xaMatrix do begin
    for i:= 0 to rowCount*colCount-1 do
      data[i]:= StrToFloat(points[i]);
  end;

  with fxMatrix do begin
    for i:= 0 to rowCount*colCount-1 do begin
      parse.Expression:= equations[i];
      data[i]:= parse.Evaluate();
    end;
  end;

  primitives.equations:= equations;
  primitives.vars:= vars;
  primitives.values:= points;
  //primitives.loadVars();
  ijbMatrix:= primitives.jacobian().inverse();
  //{

  xnMatrix:= xaMatrix.substract(ijbMatrix.multiply(fxMatrix));

  for i:= 0 to xaMatrix.rowCount-1 do begin
    error:= error + power(xnMatrix.data[i]-xaMatrix.data[i],2);
  end;
  error:= sqrt(error);

  data.Add(FormatFloat('0.########',xaMatrix.data[0]));
  data.Add(FormatFloat('0.########',xnMatrix.data[0]));
  data.Add(FormatFloat('0.########',ijbMatrix.data[0]));
  data.Add(FormatFloat('0.########',ijbMatrix.data[1]));
  data.Add(FormatFloat('0.########',xaMatrix.data[1]));
  data.Add(FormatFloat('0.########',xnMatrix.data[1]));
  data.Add(FormatFloat('0.########',ijbMatrix.data[2]));
  data.Add(FormatFloat('0.########',ijbMatrix.data[3]));
  data.Add(FormatFloat('0.########',error));

  while error > eError do begin
    xaMatrix:= xnMatrix;

    for i:=0 to vars.Count-1 do begin
        points[i]:= FloatToStr(xaMatrix.data[i]);
        parse.NewValue(vars[i],xaMatrix.data[i]);
    end;

    with fxMatrix do begin
      for i:= 0 to rowCount*colCount-1 do begin
        parse.Expression:= equations[i];
        data[i]:= parse.Evaluate();
      end;
    end;
    primitives.values:= points;
    ijbMatrix:= primitives.jacobian().inverse();

    xnMatrix:= xaMatrix.substract(ijbMatrix.multiply(fxMatrix));

    error:= 0;
    for i:= 0 to xaMatrix.rowCount-1 do begin
      error:= error + power(xnMatrix.data[i]-xaMatrix.data[i],2);
    end;
    error:= sqrt(error);
    data.Add(FormatFloat('0.########',xaMatrix.data[0]));
    data.Add(FormatFloat('0.########',xnMatrix.data[0]));
    data.Add(FormatFloat('0.########',ijbMatrix.data[0]));
    data.Add(FormatFloat('0.########',ijbMatrix.data[1]));
    data.Add(FormatFloat('0.########',xaMatrix.data[1]));
    data.Add(FormatFloat('0.########',xnMatrix.data[1]));
    data.Add(FormatFloat('0.########',ijbMatrix.data[2]));
    data.Add(FormatFloat('0.########',ijbMatrix.data[3]));
    data.Add(FormatFloat('0.########',error));
  end;
  //}
end;

end.

