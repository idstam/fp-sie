unit TestSieDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, USieDocument;

type

  TTestSieDocument = class(TTestCase)
  published
    procedure TestGetVersion;
    procedure TestHookUp;
  end;

implementation

procedure TTestSieDocument.TestGetVersion;
var
  doc: TSieDocument;
  appFolder:string;
  version:integer;
begin
  appFolder := ExtractFilePath(ExtractFilePath(ParamStr(0)));
  version := TSieDocument.GetSieVersion(appfolder + DirectorySeparator + 'sie_test_files' + DirectorySeparator + 'BokslutSIE1.se');
  AssertEquals(1, version);

  version := TSieDocument.GetSieVersion(appfolder + DirectorySeparator + 'sie_test_files' + DirectorySeparator + 'MAMUT_SIE3_EXPORT.SE');
  AssertEquals(3, version);


end;


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
