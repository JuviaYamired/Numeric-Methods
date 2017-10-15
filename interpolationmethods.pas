unit interpolationmethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, matrix;

type

  { TInterpolationMethods }

  TInterpolationMethods = class
    public
      function lagrange(data: String):String;
      function newton(data: String):String;
  end;

implementation

{ TInterpolationMethods }

function TInterpolationMethods.lagrange(data: String): String;
var i, j, tmpIni, tmpEnd: Integer;
    aux, denominator: Real;
    numerator,polynome,dsign:String;
    dataValues: TStringList;
begin
  dataValues:= TStringList.Create;
  while data <> '}' do begin
    tmpIni:= pos('(',data);
    tmpEnd:= pos(',',data);
    dataValues.Add(data.Substring(tmpIni,tmpEnd-tmpIni-1));
    tmpIni:= tmpEnd;
    tmpEnd:= pos(')',data);
    dataValues.Add(data.Substring(tmpIni,tmpEnd-tmpIni-1));
    data:= data.Substring(tmpEnd,data.Length - tmpEnd);
  end;

  denominator:=1;
  i:= 0;
  while i < dataValues.Count do begin
    j:= 0;
    numerator:='';
    denominator:=1;
    while j < dataValues.Count do begin
      if j = i then j:= j + 2;
      if j< dataValues.Count then begin
        numerator:= numerator + '(x - ' + dataValues[j] + ')*';
        denominator := denominator * (StrToFloat(dataValues[i]) - StrToFloat(dataValues[j]));
        j:= j + 2;
      end;
    end;
    numerator:=Copy(numerator,0,Length(numerator)-1);
    aux:=StrToFloat(dataValues[i+1])*(1/denominator);
    if (Sign(aux)>0) then
      dsign:=' +'
    else
      dsign:=' -';
    polynome:= polynome + dsign + FloatToStr(abs(aux)) + ' * ' + numerator;
    i:= i + 2;
  end;
  polynome:=StringReplace(polynome,',','.',[rfReplaceAll,rfIgnoreCase]);

  Result:= polynome;
end;

function TInterpolationMethods.newton(data: String): String;
var dataValues: TDMatrix;
    i, j, k, p, q, tmpIni, tmpEnd, rowCount,colCount: Integer;
begin
  rowCount:= 0;
  for i:= 0 to data.Length do begin
    if data[i] = ',' then
      rowCount:= rowCount + 1;
  end;
  colCount:= rowCount + 1;
  dataValues:= TDMatrix.Create(rowCount,colCount);

  i:= 0;
  while data <> '}' do begin
    tmpIni:= pos('(',data);
    tmpEnd:= pos(',',data);
    dataValues.data[i*colCount + 0]:= StrToFloat(data.Substring(tmpIni,tmpEnd - tmpIni - 1));
    //dataValues.Add(data.Substring(tmpIni,tmpEnd-tmpIni-1));
    tmpIni:= tmpEnd;
    tmpEnd:= pos(')',data);
    dataValues.data[i*colCount + 1]:= StrToFloat(data.Substring(tmpIni,tmpEnd - tmpIni - 1));
    //dataValues.Add(data.Substring(tmpIni,tmpEnd-tmpIni-1));
    data:= data.Substring(tmpEnd,data.Length - tmpEnd);
    i:= i + 1;
  end;

  k:= 2;
  p:= 1;
  with dataValues do begin
    k:= 1;
    p:= 1;
    for i:= 2 to colCount - 1 do begin
      for j:= 0 to rowCount - k - 1 do begin
        data[j*colCount + i]:= (data[(j+1)*colCount + i-1] - data[j*colCount + i-1])/
                               (data[(j+k)*colCount] - data[j*colCount]);
      end;

      k:= k + 1;
    end;

    Result:= FloatToStr(data[1]) + ' ';
    k:= 1;
    for i:= 2 to colCount - 1 do begin
      Result:= Result + '+' + FloatToStr(data[i]);
      j:= 0;
      while j < k do begin
        Result:= Result + '*(x-' + FloatToStr(data[j*colCount + 0]) + ')';
        j:= j + 1;
      end;
      k:= k + 1;
    end;
  end;

  {Result:= '' ;
  for i:= 0 to dataValues.colCount*dataValues.rowCount - 1 do begin
    if i mod colCount = 0 then
      Result:= Result + '</br>';
    Result:= Result + FloatToStr(dataValues.data[i]) + ',';
  end;}
end;

end.

