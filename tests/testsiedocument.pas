unit TestSieDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  USieClasses, USieDocumentReader, USieCallbacks, USieDocumentWriter;

type

  TTestSieDocument = class(TTestCase)
  published
    procedure TestGetVersion;
    procedure TestCompany;
    procedure TestSimpleWrite;
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
  reader := TSieDocumentReader.Create(TSieCallbackBase.Create);
  doc := reader.ReadDocument(appfolder + DirectorySeparator +
    'sie_test_files' + DirectorySeparator + 'BL0001_typ1.SE', False, False, False, False, true);
  AssertEquals('Flottbrovägen 14', doc.FNAMN.Street);

end;

procedure TTestSieDocument.TestSimpleWrite;
var
  doc: TSieDocument;
  reader: TSieDocumentReader;
  appFolder: string;
  outFileName:string;
  writer:TSieDocumentWriter;
begin
  try
    appFolder := ExtractFilePath(ExtractFilePath(ParamStr(0)));
    reader := TSieDocumentReader.Create(TSieCallbackBase.Create);
    doc := reader.ReadDocument(appfolder + DirectorySeparator +
      'sie_test_files' + DirectorySeparator + 'BL0001_typ1.SE', False, False, False, False, true);
    outFileName := GetTempFileName(appFolder, '');
    DeleteFile(outFileName);


    writer := TSieDocumentWriter.Create();
    writer.Write(doc, outFileName);
    AssertTrue(FileExists(outFileName));
  finally
    DeleteFile(outFileName);
  end;

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
