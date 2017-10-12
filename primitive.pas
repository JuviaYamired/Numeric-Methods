unit primitive;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,ParseMath, matrix;

type

  { TPrimitive }

  TPrimitive = class
    public
    parse : TParseMath;
    equations : TStringList;
    vars : TStringList;
    values : TStringList;
    procedure loadVars();
    function jacobian():TDMatrix;
    function resolveIn(variable, equation:String;error:Double):Double;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TPrimitive.Create;
begin

  //parse:= TParseMath.create();
  equations:= TStringList.Create;
  vars := TStringList.Create();
  values := TStringList.Create();
end;

destructor TPrimitive.Destroy;
begin
   vars.Destroy;
   values.Destroy;
   //parse.destroy;
end;

procedure TPrimitive.loadVars;
var i: Integer;
begin
  for i:= 0 to vars.Count-1 do begin
    parse.AddVariable(vars[i],StrToFloat(values[i]));
  end;
end;

function TPrimitive.jacobian(): TDMatrix;
var i,j: Integer;
begin
  Result:= TDMatrix.Create(vars.Count,vars.Count);
  for i:= 0 to Result.rowCount-1 do begin
    for j:= 0 to Result.colCount-1 do begin
      Result.data[i*Result.rowCount + j]:= resolveIn(self.vars[j],self.equations[i],0.00000001);
    end;
  end;
end;

function TPrimitive.resolveIn(variable, equation:String; error: Double):Double;
var i,posVar:Integer;
    auxCalc: Double;
begin
  parse:=TParseMath.create();
  loadVars();

  parse.Expression:= equation;
  posVar:= vars.IndexOf(variable);

  //{
  parse.NewValue(vars[posVar],StrToFloat(values[posVar])+error);
  auxCalc:= parse.Evaluate();
  parse.NewValue(vars[posVar],StrToFloat(values[posVar])-error);
  auxCalc:= auxCalc - parse.Evaluate();
  auxCalc:= auxCalc/(2*error);
  Result:= auxCalc;

  parse.destroy;
   //}
end;

end.

