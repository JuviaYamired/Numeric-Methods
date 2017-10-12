unit matrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;
type
  TDMatrix = class
    type realArray = array of Real;
    public
    data: realArray;
    colCount,rowCount: Integer;
    function add(number:Real):TDMatrix;
    function add(matrixA:TDMatrix):TDMatrix;
    function substract(number:Real):TDMatrix;
    function substract(matrixA:TDMatrix):TDMatrix;
    function mpower(exponent:integer):TDMatrix;
    function multiply(number:Real):TDMatrix;
    function multiply(matrixA:TDMatrix):TDMatrix;
    function divide(number:Real):TDMatrix;
    function divide(matrixA:TDMatrix):TDMatrix;
    function cofactor(row,col:Integer):TDMatrix;
    function determinant():Real;
    function adjoint():TDMatrix;
    function tranponse():TDMatrix;
    function inverse():TDMatrix;
    constructor Create(cRowCount,cColCount:Integer);
    constructor Create(matrixA:TDMatrix);
  end;
implementation
uses math;

constructor TDMatrix.Create(cRowCount,cColCount:Integer);
var i:Integer;
begin
     self.RowCount:=cRowCount;
     self.ColCount:=cColCount;
     SetLength(self.data,rowCount*colCount);
     i:=0;
     while i < RowCount * colCount do begin
             data[i]:= 0;
             i:=i+1;
     end;
end;

constructor TDMatrix.Create(matrixA:TDMatrix);
var i,j:Integer;
begin
     self.rowCount:=matrixA.rowCount;
     self.colCount:=matrixA.colCount;
     SetLength(self.data,matrixA.rowCount*matrixA.colCount);
     for i:=0 to rowCount*colCount do
         data[i]:= matrixA.data[i];
end;

function TDMatrix.add(number:Real):TDMatrix;
var i: Integer;
begin
  result:= TDMatrix.Create(self.rowCount,self.rowCount);
  for i:=0 to self.colCount * self.rowCount do begin
    result.data[i]:= self.data[i] + number;
  end;
end;

function TDMatrix.add(matrixA:TDMatrix):TDMatrix;
var i:Integer;
begin
  Result.Create(self.rowCount,self.colCount);
  if ( self.colCount <> matrixA.colCount ) or ( self.rowCount <> matrixA.rowCount ) then
    Exit;
  for i := 0 to self.colCount * self.rowCount-1 do begin
    Result.data[i]:= self.data[i] + matrixA.data[i];
  end;
end;

function TDMatrix.substract(number:Real):TDMatrix;
var i: Integer;
begin
  result:= TDMatrix.Create(self.rowCount,self.rowCount);
  for i:=0 to self.colCount * self.rowCount do begin
    result.data[i]:= self.data[i] - number;
  end;
end;

function TDMatrix.substract(matrixA:TDMatrix):TDMatrix;
var i:Integer;
begin
  Result:= TDMatrix.Create(self.rowCount,self.colCount);
  //if ( self.colCount <> matrixA.colCount ) or ( self.rowCount <> matrixA.rowCount ) then
  //  Exit;
  for i := 0 to self.colCount * self.rowCount-1 do begin
    Result.data[i]:= self.data[i] - matrixA.data[i];
  end;
end;

function TDMatrix.mpower(exponent:integer):TDMatrix;
var i:integer;
begin
  for i:= 1 to exponent do begin
      result.data:= self.multiply(self).data;
  end;
end;

function TDMatrix.multiply(number:Real):TDMatrix;
var i: Integer;
begin
  result:= TDMatrix.Create(self.rowCount,self.rowCount);
  for i:=0 to self.colCount * self.rowCount do begin
    result.data[i]:= self.data[i] * number;
  end;
end;

function TDMatrix.multiply(matrixA:TDMatrix):TDMatrix;
var i,j,k:Integer;
    sum: Real;
begin
  if self.colCount <> matrixA.rowCount then
    Exit;
  result:= TDMatrix.Create(self.rowCount,self.colCount);
  for i := 0 to self.rowCount-1 do begin
    for j := 0 to matrixA.colCount-1 do begin
      sum := 0;
      for k := 0 to self.colCount-1 do begin
        sum := sum + self.data[i*self.colCount + k] * matrixA.data[j + k*matrixA.colCount];
      end;
      result.data[j + i*matrixA.colCount] := sum;
    end;
  end;
end;

function TDMatrix.divide(number:Real):TDMatrix;
var i: Integer;
begin
  result:= TDMatrix.Create(self.rowCount,self.rowCount);
  for i:=0 to self.colCount * self.rowCount do begin
    result.data[i]:= self.data[i] / number;
  end;
end;

function TDMatrix.divide(matrixA:TDMatrix):TDMatrix;
begin
  if self.colCount <> matrixA.rowCount then
    Exit;
  result:= TDMatrix.Create(self.rowCount,self.colCount);
  result.data:= self.multiply(matrixA.inverse()).data;
end;

function TDMatrix.cofactor(row,col:Integer):TDMatrix;
var i,j,p,q : Integer;
begin
     i:=0;
     j:=0;
     p:=0;
     q:=0;
     result:= TDMatrix.Create(self.rowCount - 1,self.colCount - 1);
     while (i < self.RowCount) and (p < self.RowCount-1) do
     begin
         j:=0;
         q:=0;
         if(i = row) then
           i:=i+1;
         while (j < self.ColCount) and (q < self.ColCount-1)  do
         begin
             if(j = col) then
               j:=j+1;
             Result.data[p*result.rowCount+q]:= self.data[i*self.rowCount+j];
             j:=j+1;
             q:=q+1;
         end;
         i:=i+1;
         p:=p+1;
     end;
end;

function TDMatrix.determinant():Real;
var i: integer;
begin
     result:= 0;
     if (self.rowCount = 1) and (self.colCount = 1) then
       result:= self.data [0];
     if (self.rowCount = 2) and (self.colCount = 2) then
       result:= self.data[0] * self.data[3] - self.data[1] * self.data[2];
     if (self.rowCount > 2) and (self.colCount > 2) and (self.rowCount = self.colCount) then
       for i:= 0 to self.rowCount-1 do
         result:=result + power(-1,i+2) * self.data[i] * self.cofactor(0,i).determinant();
end;

function TDMatrix.tranponse():TDMatrix;
var i,j:Integer;
begin
     result:=TDMatrix.Create(self.colCount,self.rowCount);
     for i:=0 to self.rowCount-1 do
         for j:=0 to self.colCount-1 do
             result.data[i * rowCount + j]:= self.data[j* self.colCount + i];
end;

function TDMatrix.adjoint():TDMatrix;
var i,j:Integer;
begin
     result:= TDMatrix.Create(self.rowCount,self.colCount);
     for i:=0 to self.rowCount-1 do
         for j:=0 to self.colCount-1 do
             result.data[i*self.rowCount + j]:= power(-1,(i+1)+(j+1)) * self.cofactor(i,j).determinant;
     result.data:= result.Tranponse().data;
end;

function TDMatrix.Inverse():TDMatrix;
var i,j:integer;
    det:Real;
begin
     result:= TDMatrix.Create(self.colCount,self.rowCount);
     result.data:= self.adjoint().data;
     det := self.determinant();
     if det = 0 then begin
       result.data:= self.data;
       ShowMessage('La determinante es igual a 0');
       Exit;
     end;
     for i:=0 to result.rowCount-1 do
         for j:=0 to result.colCount-1 do
             result.data[i* result.rowCount +j]:= result.data[i* result.rowCount +j]/det;
end;

end.

