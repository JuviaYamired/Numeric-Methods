unit edomethods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParseMath;
type
  TEdoMethods = Class
    Public
    Parse : TParseMath;
    data: TStringList;
    xyData: String;
    constructor create();
    function useEuler(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
    function useHeun(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
    function useRungeKutta3(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
    function useRungeKutta4(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
    function useDormandPrince(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
  end;

implementation

constructor TEdoMethods.create();
begin
    Parse:= TParseMath.create();
    data:=  TStringList.Create;
end;

function TEdoMethods.useEuler(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
begin
     Parse.Expression:= evalExpression;
     Parse.AddVariable('x',valCoordX);
     Parse.AddVariable('y',valCoordY);

     xyData:='{(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

     if valFindCoordY < valCoordX then begin
         valAccuracyH:= -valAccuracyH;
         while valCoordX + valAccuracyH > valFindCoordY do begin
             valCoordY:= valCoordY + valAccuracyH * (Parse.Evaluate());
             valCoordX:= valCoordX + valAccuracyH;

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end
     else begin
         while valCoordX + valAccuracyH < valFindCoordY do begin
             valCoordY:= valCoordY + valAccuracyH * (Parse.Evaluate());
             valCoordX:= valCoordX + valAccuracyH;

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end;
     xyData:= xyData + '}';
     Result:= valCoordY;
end;

function TEdoMethods.useHeun(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
var valActualEval,valFutureEval:Real;
begin
     Parse.Expression:= evalExpression;
     Parse.AddVariable('x',valCoordX);
     Parse.AddVariable('y',valCoordY);

     xyData:='{(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

     if valFindCoordY < valCoordX then begin
         valAccuracyH:= -valAccuracyH;
         while valCoordX + valAccuracyH > valFindCoordY do begin
             valActualEval:= Parse.Evaluate();
             valCoordX:= valCoordX + valAccuracyH;

             parse.NewValue('x',valCoordX);
             parse.NewValue('y',valCoordY+valAccuracyH*valActualEval);

             valFutureEval:= Parse.Evaluate();
             valCoordY:= valCoordY + valAccuracyH * ((valActualEval+valFutureEval)/2);

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end
     else begin
         while valCoordX + valAccuracyH < valFindCoordY do begin
             valActualEval:= Parse.Evaluate();
             valCoordX:= valCoordX + valAccuracyH;

             parse.NewValue('x',valCoordX);
             parse.NewValue('y',valCoordY+valAccuracyH*valActualEval);

             valFutureEval:= Parse.Evaluate();
             valCoordY:= valCoordY + valAccuracyH * ((valActualEval+valFutureEval)/2);

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end;
     xyData:= xyData + '}';
     Result:= valCoordY;
end;

function TEdoMethods.useRungeKutta3(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
var k1,k2,k3:Real;
begin
     Parse.Expression:= evalExpression;
     Parse.AddVariable('x',valCoordX);
     Parse.AddVariable('y',valCoordY);

     xyData:='{(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

     if valFindCoordY < valCoordX then begin
         valAccuracyH:= -valAccuracyH;
         while valCoordX + valAccuracyH > valFindCoordY do begin
             k1:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH/2);
             Parse.NewValue('y',valCoordY+(k1*valAccuracyH/2));
             k2:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH);
             Parse.NewValue('y',valCoordY-(k1*valAccuracyH)+(2*k2*valAccuracyH));
             k3:=Parse.Evaluate();

             valCoordX:=valCoordX+valAccuracyH;
             valCoordY:=valCoordY+(valAccuracyH/6)*(k1+4*k2+k3);

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end
     else begin
         while valCoordX + valAccuracyH < valFindCoordY do begin
             k1:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH/2);
             Parse.NewValue('y',valCoordY+(k1*valAccuracyH/2));
             k2:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH);
             Parse.NewValue('y',valCoordY-(k1*valAccuracyH)+(2*k2*valAccuracyH));
             k3:=Parse.Evaluate();

             valCoordX:=valCoordX+valAccuracyH;
             valCoordY:=valCoordY+(valAccuracyH/6)*(k1+4*k2+k3);

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end;
     xyData:= xyData + '}';
     Result:= valCoordY;
end;

function TEdoMethods.useRungeKutta4(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
var k1,k2,k3,k4:Real;
begin
     Parse.Expression:= evalExpression;
     Parse.AddVariable('x',valCoordX);
     Parse.AddVariable('y',valCoordY);

     xyData:='{(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

     if valFindCoordY < valCoordX then begin
         valAccuracyH:= -valAccuracyH;
         while valCoordX + valAccuracyH > valFindCoordY do begin
             k1:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH/2);
             Parse.NewValue('y',valCoordY+k1/2);
             k2:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH/2);
             Parse.NewValue('y',valCoordY+k2/2);
             k3:=Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH);
             Parse.NewValue('y',valCoordY+k3);
             k4:=Parse.Evaluate();

             valCoordX:=valCoordX+valAccuracyH;
             valCoordY:=valCoordY+(valAccuracyH/6)*(k1+2*k2+2*k3+k4);

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end
     else begin
         while valCoordX + valAccuracyH < valFindCoordY do begin
             k1:=valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH/2);
             Parse.NewValue('y',valCoordY+k1/2);
             k2:=valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH/2);
             Parse.NewValue('y',valCoordY+k2/2);
             k3:=valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX+valAccuracyH);
             Parse.NewValue('y',valCoordY+k3);
             k4:=valAccuracyH*Parse.Evaluate();

             valCoordX:=valCoordX+valAccuracyH;
             valCoordY:=valCoordY+(1/6)*(k1+2*k2+2*k3+k4);

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end;
     xyData:= xyData + '}';
     Result:= valCoordY;
end;

function TEdoMethods.useDormandPrince(evalExpression:String;valCoordX,valCoordY,valFindCoordY,valAccuracyH:Real):Real;
var k1,k2,k3,k4,k5,k6:Real;
begin
     Parse.Expression:= evalExpression;
     Parse.AddVariable('x',valCoordX);
     Parse.AddVariable('y',valCoordY);

     xyData:='{(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

     if valFindCoordY < valCoordX then begin
         valAccuracyH:= -valAccuracyH;
         while valCoordX + valAccuracyH > valFindCoordY do begin
             k1:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + valAccuracyH/5);
             Parse.NewValue('y',valCoordY + k1/5);
             k2:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + 3*valAccuracyH/10);
             Parse.NewValue('y',valCoordY + 3*k1/40 + 9*k2/40);
             k3:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + 4*valAccuracyH/5);
             Parse.NewValue('y',valCoordY + 44*k1/45 - 56*k2/15 +32*k3/9);
             k4:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + 8*valAccuracyH/9);
             Parse.NewValue('y',valCoordY + 19372*k1/6561 - 25360*k2/2187 + 64448*k3/6561 - 212*k4/729);
             k5:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + valAccuracyH);
             Parse.NewValue('y',valCoordY + 9017*k1/3168 - 355*k2/33 - 46732*k3/5247 + 49*k4/176 - 5103*k5/18656);
             k6:= valAccuracyH*Parse.Evaluate();

             valCoordX:= valCoordX + valAccuracyH;
             valCoordY:= valCoordY + 35*k1/384 + 500*k3/1113 + 125*k4/192 - 2187*k5/6784 + 11*k6/84;

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end
     else begin
         while valCoordX + valAccuracyH <= valFindCoordY do begin
             k1:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + valAccuracyH/5);
             Parse.NewValue('y',valCoordY + k1/5);
             k2:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + 3*valAccuracyH/10);
             Parse.NewValue('y',valCoordY + 3*k1/40 + 9*k2/40);
             k3:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + 4*valAccuracyH/5);
             Parse.NewValue('y',valCoordY + 44*k1/45 - 56*k2/15 +32*k3/9);
             k4:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + 8*valAccuracyH/9);
             Parse.NewValue('y',valCoordY + 19372*k1/6561 - 25360*k2/2187 + 64448*k3/6561 - 212*k4/729);
             k5:= valAccuracyH*Parse.Evaluate();

             Parse.NewValue('x',valCoordX + valAccuracyH);
             Parse.NewValue('y',valCoordY + 9017*k1/3168 - 355*k2/33 - 46732*k3/5247 + 49*k4/176 - 5103*k5/18656);
             k6:= valAccuracyH*Parse.Evaluate();

             valCoordX:= valCoordX + valAccuracyH;
             valCoordY:= valCoordY + 35*k1/384 + 500*k3/1113 + 125*k4/192 - 2187*k5/6784 + 11*k6/84;

             xyData:= xyData + ';(' + FloatToStr(valCoordX) + ',' + FloatToStr(valCoordY) + ')';

             Parse.NewValue('x',valCoordX);
             Parse.NewValue('y',valCoordY);
         end;
     end;
     xyData:= xyData + '}';
     Result:= valCoordY;
end;

end.
