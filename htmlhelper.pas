unit htmlhelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, rootmethods;

type

  { THtmlHelper }

  THtmlHelper = class
    public
    miMethods: TRootMethods;
    xA, xB, eError: Double;
    equation, pEquation, method: String;
    function cmtMakeTable(): String;
    function omtMakeTable(): String;
    function smtMakeTable(): String;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor THtmlHelper.Create;
begin
  miMethods:= TRootMethods.Create;
end;

destructor THtmlHelper.Destroy;
begin
  miMethods.Destroy;
end;

function THtmlHelper.cmtMakeTable(): String;
var i: Integer;
begin
  if LowerCase(method) = 'bi'then begin
    miMethods.bisection(xA, xB, eError, equation);
  end

  else if LowerCase(method) = 'fp'then begin
    miMethods.falsePosition(xA, xB, eError, equation);
  end;

  Result:= '<!DOCTYPE html><html><body>'
         + '<style>'
         + 'table, th, td {'
         + 'border: 1px solid black;'
         + 'border-collapse: collapse;'
         + '}</style>'
         +   '<table border="1">'
         +     '<tr>'
         +       '<th>A</th>'
         +       '<th>B</th>'
         +       '<th>Xn</th>'
         +       '<th>Signo</th>'
         +       '<th>Error</th>'
         +     '</tr>';

  i:= 0;
  with miMethods do begin
    //Result:= method + '</br>' + equation + '</br>';
  {
    while i < data.Count do begin
      Result:= Result + data[i] + '</br>';
      i:= i + 1;
    end;
    Result:= Result + IntToStr(data.Count);
    end;
  }
  //{
    if data.Count < 4 then begin
      Result:= Result + 'La funcion no es continua';
    end;

    Result:= Result
             + '<tr>'
             +   '<td>' + data[0] + '</td>'
             +   '<td>' + data[1] + '</td>'
             +   '<td>' + data[2] + '</td>'
             +   '<td>' + data[3] + '</td>'
             +   '<td>' + '-' + '</td>'
             + '</tr>';
    while i < floor((data.Count-4)/5) do begin
      Result:= Result + '<tr>'
                      +   '<td>' + data[i*5 + 4] + '</td>'
                      +   '<td>' + data[i*5 + 5] + '</td>'
                      +   '<td>' + data[i*5 + 6] + '</td>'
                      +   '<td>' + data[i*5 + 7] + '</td>'
                      +   '<td>' + data[i*5 + 8] + '</td>'
                      + '</tr>';
      i:= i + 1;
    end;
    if (data.Count-4) mod 5 <> 0 then begin
      Result:= Result + '<tr>'
                      +   '<td>' + data[i*5 + 4] + '</td>'
                      +   '<td> - </td>'
                      + '</tr>';
    end;
  end;
    Result:= Result + '</table>' + '</body></html>';
  //}
end;

function THtmlHelper.omtMakeTable(): String;
var i: Integer;
begin
  if LowerCase(method) = 'nw'then begin
    miMethods.newton(xA, eError, equation, pEquation);
  end

  else if LowerCase(method) = 'sc'then begin
    miMethods.secant(xA, eError, equation);
  end

  else if LowerCase(method) = 'fx'then begin
    miMethods.fixedPoint(xA, eError, equation, pEquation);
  end;

  Result:= '<!DOCTYPE html><html><body>'
         + '<style>'
         + 'table, th, td {'
         + 'border: 1px solid black;'
         + 'border-collapse: collapse;'
         + '}</style>'
         +   '<table border="1">'
         +     '<tr>'
         +       '<th>Xn</th>'
         +       '<th>Error</th>'
         +     '</tr>';

  i:= 0;
  with miMethods do begin
    //Result:= method + '</br>' + equation + '</br>';
  {
    while i < data.Count do begin
      Result:= Result + data[i] + '</br>';
      i:= i + 1;
    end;
    Result:= Result + IntToStr(data.Count);
    end;
  }
  //{
    if data.Count < 1 then begin
      Result:= Result + 'La funcion no es continua';
      Exit;
    end;

    Result:= Result
             + '<tr>'
             +   '<td>' + data[0] + '</td>'
             +   '<td>' + '-' + '</td>'
             + '</tr>';
    while i < floor((data.Count-1)/2) do begin
      Result:= Result + '<tr>'
                      +   '<td>' + data[i*2 + 1] + '</td>'
                      +   '<td>' + data[i*2 + 2] + '</td>'
                      + '</tr>';
      i:= i + 1;
    end;
    if (data.Count-1) mod 2 <> 0 then begin
      Result:= Result + '<tr>'
                      +   '<td>' + data[i*2 + 1] + '</td>'
                      +   '<td> - </td>'
                      + '</tr>';
    end;
  end;
    Result:= Result + '</table>' + '</body></html>';
  //}
end;

function THtmlHelper.smtMakeTable(): String;
var i: Integer;
begin
  //{
  if LowerCase(method) = 'gfx'then begin
    //miMethods.newton(xA, eError, equation, pEquation);

    with miMethods do begin
    equations.Add(equation);
    equations.Add(pEquation);
    gFixedPoint(xA,xB,eError);

    Result:= '<!DOCTYPE html><html><body>'
           + '<style>'
           + 'table, th, td {'
           + 'border: 1px solid black;'
           + 'border-collapse: collapse;'
           + '}</style>'
           + '<table border="1">'
           +   '<tr>'
           +     '<th>Xn</th>'
           +     '<th>Yn</th>'
           +     '<th>Error</th>'
           +   '</tr>'
           +   '<tr>'
           +     '<td>' + data[0] + '</td>'
           +     '<td>' + data[1] + '</td>'
           +   '</tr>';

    i:= 0;
    while i < round((data.Count-2)/3) do begin
      Result:= Result + '<tr>'
                      +   '<td>' + data[i*3 + 2] + '</td>'
                      +   '<td>' + data[i*3 + 3] + '</td>'
                      +   '<td>' + data[i*3 + 4] + '</td>'
                      + '</tr>';
      i:= i + 1;
    end;
  end;
    Result:= Result + '</table>' + '</body></html>';

  end
  else if LowerCase(method) = 'gnw'then begin
    with miMethods do begin
      points.Add(FloatToStr(xA));
      points.Add(FloatToStr(xB));
      vars.Add('x');
      vars.Add('y');
      equations.Add(equation);
      equations.Add(pEquation);
      gNewton(eError);
      Result:= '<!DOCTYPE html><html><body>'
             + '<style>'
             + 'table, th, td {'
             + 'border: 1px solid black;'
             + 'border-collapse: collapse;'
             + '}</style>'
             + '<table border="1">'
             +   '<tr>'
             +     '<th>Xn</th>'
             +     '<th>Xn+1</th>'
             +     '<th colspan="2">IJac</th>'
             +     '<th>error</th>'
             +   '</tr>'
             +   '<tr>'
             +     '<td>' + data[0] + '</td>'
             +     '<td>' + data[1] + '</td>'
             +     '<td>' + data[2] + '</td>'
             +     '<td>' + data[3] + '</td>'
             +     '<td></td>'
             +   '</tr>'
             +   '<tr>'
             +     '<td>' + data[4] + '</td>'
             +     '<td>' + data[5] + '</td>'
             +     '<td>' + data[6] + '</td>'
             +     '<td>' + data[7] + '</td>'
             +     '<td>' + data[8] + '</td>'
             +   '</tr>';

      i:= 0;
      while i < floor((data.Count-9)/9) do begin
        Result:= Result +   '<tr>'
                        +     '<td>' + data[i*9 + 9] + '</td>'
                        +     '<td>' + data[i*9 + 10] + '</td>'
                        +     '<td>' + data[i*9 + 11] + '</td>'
                        +     '<td>' + data[i*9 + 12] + '</td>'
                        +     '<td></td>'
                        +   '</tr>'
                        +   '<tr>'
                        +     '<td>' + data[i*9 + 13] + '</td>'
                        +     '<td>' + data[i*9 + 14] + '</td>'
                        +     '<td>' + data[i*9 + 15] + '</td>'
                        +     '<td>' + data[i*9 + 16] + '</td>'
                        +     '<td>' + data[i*9 + 17] + '</td>'
                        +   '</tr>';
        i:= i + 1;
      end;
    end;
  end;
  //}
end;

end.

