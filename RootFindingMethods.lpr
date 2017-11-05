program RootFindingMethods;

{$mode objfpc}{$H+}

uses
  fpCGI, rfmmain, htmlhelper, primitive, interpolationmethods, 
integrationmethods, edomethods;

begin
  Application.Title:='cgiproject1';
  Application.Initialize;
  Application.Run;
end.

