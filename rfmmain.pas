unit rfmmain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, Interfaces,interpolationmethods, edomethods;

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; Var Handled: Boolean);
  private

  public
    srvEdo: TEdoMethods;
    srvInterpolacion: TInterpolationMethods;
  end;

var
  FPWebModule1: TFPWebModule1;

implementation

{$R *.lfm}

{ TFPWebModule1 }

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  srvEdo:= TEdoMethods.create();
  srvInterpolacion:= TInterpolationMethods.Create;
end;

procedure TFPWebModule1.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; Var Handled: Boolean);
var  x,fx,limit,error: Double;
     result, equation, method: String;
begin
  x:= StrToFloat(ARequest.QueryFields.Values['x']);
  fx:= StrToFloat(ARequest.QueryFields.Values['fx']);
  error:= StrToFloat(ARequest.QueryFields.Values['error']);
  limit:= StrToFloat(ARequest.QueryFields.Values['limit']);
  equation:= ARequest.QueryFields.Values['equation'];
  method:= ARequest.QueryFields.Values['method'];

  AResponse.ContentType:= 'text/html; charset= utf-8';
  if method = 'euler'then
     srvEdo.useEuler(equation,x,fx,limit,error);
  if method = 'heun' then
     srvEdo.useHeun(equation,x,fx,limit,error);
  if method = 'dormand' then
     srvEdo.useDormandPrince(equation,x,fx,limit,error);
  if method = 'runge4' then
     srvEdo.useRungeKutta4(equation,x,fx,limit,error);

  AResponse.Contents.Text:= srvEdo.xyData;
  Handled := true;
end;

initialization
  RegisterHTTPModule('TFPWebModule1', TFPWebModule1);
end.

