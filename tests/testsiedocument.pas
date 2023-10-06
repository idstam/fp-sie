unit TestSieDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, USieDocument;

type

  TTestSieDocument = class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TTestSieDocument.TestHookUp;
var
  doc: TSieDocument;
begin
  doc := TSieDocument.Create();
  AssertEquals('yyyyMMdd', doc.DateFormat);
end;



initialization

  RegisterTest(TTestSieDocument);
end.
