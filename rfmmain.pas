unit rfmmain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, Interfaces, matrix, rootmethods, htmlhelper, primitive;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; Var Handled: Boolean);
  private

  public
    srvPrimitive: TPrimitive;
    htmlCreator: THtmlHelper;
    rMethods: TRootMethods;
  end;

var
  FPWebModule1: TFPWebModule1;

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  htmlCreator:= THtmlHelper.Create;
  rMethods:= TRootMethods.Create();
  srvPrimitive:= TPrimitive.Create;
end;

procedure TFPWebModule1.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; Var Handled: Boolean);
var  a,b,error: Double;
     result, equation, pEquation, method: String;
     max: TDMatrix;
begin
  a:= StrToFloat(ARequest.QueryFields.Values['a']);
  b:= StrToFloat(ARequest.QueryFields.Values['b']);
  error:= StrToFloat(ARequest.QueryFields.Values['error']);
  equation:= ARequest.QueryFields.Values['equation'];
  pEquation:= ARequest.QueryFields.Values['pequation'];
  method:= ARequest.QueryFields.Values['method'];
  htmlCreator.xA:= a;
  htmlCreator.xB:= b;
  htmlCreator.eError:= error;
  htmlCreator.equation:= equation;
  htmlCreator.pEquation:= pEquation;
  htmlCreator.method:= method;
  AResponse.ContentType:= 'text/html; charset= utf-8' ;
  if (method = 'gnw') or (method = 'gfx') then
     AResponse.Contents.Text := htmlCreator.smtMakeTable()
  else if (method = 'bi') or (method = 'fp') then
     AResponse.Contents.Text := htmlCreator.cmtMakeTable()
  else if (method = 'nw') or (method = 'sc') or (method = 'fx') then
     AResponse.Contents.Text := htmlCreator.omtMakeTable();

  {
  with srvPrimitive do begin
    equations.Add(equation);
    equations.Add(pEquation);
    vars.Add('x');
    vars.Add('y');
    values.Add(FloatToStr(a));
    values.Add(FloatToStr(b));
  end;

  srvPrimitive.loadVars();
  max:= srvPrimitive.jacobian().inverse();

  Result:= '<!DOCTYPE html><html><body>'
         + '<style>'
         + 'table, th, td {'
         + 'border: 1px solid black;'
         + 'border-collapse: collapse;'
         + '}</style>'
         +   '<table border="1">'
         +     '<tr>'
         +       '<td>'+ FloatToStr(max.data[0]) +'</td>'
         +       '<td>'+ FloatToStr(max.data[1]) +'</td>'
         +     '</tr>'
         +     '<tr>'
         +       '<td>'+ FloatToStr(max.data[2]) +'</td>'
         +       '<td>'+ FloatToStr(max.data[3]) +'</td>'
         +     '</tr>'
         + '</table>' + '</body></html>';
  AResponse.Contents.Text := result;
  //}
  Handled := true;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

