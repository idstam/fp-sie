program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, USieDocumentReader, USieDocumentWriter, USieClasses,
  USieCallbacks, USieDataItem, CustApp;
  { you can add units after this }

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  rdr:TSieDocumentReader;
  writer:TSieDocumentWriter;
  aDoc:TSieDocument;
  cb:TSieCallbackBase;
  fileName:string;
begin
  fileName := ParamStr(1);
  fileName := 'C:\Users\johan\src\idstam\fp-sie\tests\sie_test_files\BL0001_typ4.SE';
  writeln('Reading ', fileName);
  cb := TSieCallbackBase.Create();
  rdr := TSieDocumentReader.Create(cb);
  aDoc := rdr.ReadDocument(fileName, false, false, false, false, true);
  writer := TSieDocumentWriter.Create();
  DeleteFile('C:\Users\johan\src\idstam\fp-sie\src\ut.se');
  writeLn('Writing ut.se');
  writer.Write(aDoc, 'C:\Users\johan\src\idstam\fp-sie\src\ut.se');

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

