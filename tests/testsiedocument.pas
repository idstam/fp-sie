unit TestSieDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, USieClasses,USieDocumentReader;

type

  TTestSieDocument = class(TTestCase)
  published
    procedure TestGetVersion;
    procedure TestCompany;
    procedure TestHookUp;
  end;

implementation

procedure TTestSieDocument.TestGetVersion;
var

  appFolder: string;
  version: integer;
begin
  appFolder := ExtractFilePath(ExtractFilePath(ParamStr(0)));


  version := TSieDocumentReader.GetSieVersion(appfolder + DirectorySeparator +
    'sie_test_files' + DirectorySeparator + 'BL0001_typ1.SE');
  AssertEquals(1, version);

  version := TSieDocumentReader.GetSieVersion(appfolder + DirectorySeparator +
    'sie_test_files' + DirectorySeparator + 'MAMUT_SIE3_EXPORT.SE');
  AssertEquals(3, version);

end;

procedure TTestSieDocument.TestCompany;
var
  doc: TSieDocument;
  reader: TSieDocumentReader;
  appFolder: string;

begin
  appFolder := ExtractFilePath(ExtractFilePath(ParamStr(0)));
  reader := TSieDocumentReader.Create();
  doc := reader.ReadDocument(appfolder + DirectorySeparator +    'sie_test_files' + DirectorySeparator + 'BL0001_typ1.SE');
  AssertEquals('Flottbrovägen 14', doc.FNAMN.Street);

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
